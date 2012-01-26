#! /usr/bin/env python
import getopt,sys,os
import signal
try:
    from cStringIO import StringIO
except ImportError: # Python 3
    from io import StringIO
import re
import glob
import AUTOExceptions,parseC,parseS,gc
try:
    import subprocess
except ImportError:
    import popen2

# A few global variables for the signal handler
alarm_demo=""
demo_killed=0
demo_max_time=-1

signals = {
    "SIGHUP": "Hangup",
    "SIGINT": "Interrupt",
    "SIGQUIT": "Quit",
    "SIGILL": "Illegal instruction",
    "SIGTRAP": "Trace/breakpoint trap",
    "SIGABRT": "Aborted",
    "SIGBUS": "Bus error",
    "SIGFPE": "Floating point exception",
    "SIGKILL": "Killed",
    "SIGUSR1": "User defined signal 1",
    "SIGSEGV": "Segmentation fault",
    "SIGUSR2": "User defined signal 2",
    "SIGPIPE": "Broken pipe",
    "SIGALRM": "Alarm clock",
    "SIGTERM": "Terminated",
    "SIGSTKFLT": "Stack fault",
    "SIGCHLD": "Child exited",
    "SIGCONT": "Continued",
    "SIGSTOP": "Stopped (signal)",
    "SIGTSTP": "Stopped",
    "SIGTTIN": "Stopped (tty input)",
    "SIGTTOU": "Stopped (tty output)",
    "SIGURG": "Urgent I/O condition",
    "SIGXCPU": "CPU time limit exceeded",
    "SIGXFSZ": "File size limit exceeded",
    "SIGVTALRM": "Virtual timer expired",
    "SIGPROF": "Profiling timer expired",
    "SIGWINCH": "Window changed",
    "SIGIO": "I/O possible",
    "SIGPWR": "Power failure",
    "SIGSYS": "Bad system call"
}

class runAUTO:
    def __init__(self,**kw):
        # Set the signal handler
        if hasattr(signal,'SIGALRM') and demo_max_time > 0:
            signal.signal(signal.SIGALRM, self.__handler)

        self.options={}
        self.options["log"] = None
        self.options["err"] = None
        self.options["auto_dir"] = None
        self.options["demos_dir"] = None
        self.options["equation"] = "all"
        self.options["verbose"] = "yes" # note: this is ignored!
        self.options["clean"] = "no"
        self.options["dir"] = "."
        self.options["executable"] = None
        self.options["command"] = None
        self.options["makefile"] = None
        self.options["constants"] = parseC.parseC()
        self.options["solution"] = parseS.AUTOSolution()
        self.options["homcont"] = None
        self.options["selected_solution"] = None

        kw = self.config(**kw)

    def config(self,**kw):
        """     Change the options for this runner object"""
        # first the normal parameters, then IRS=... style parameters
        for key in self.options:
            if key in kw:
                value = kw[key]
                del kw[key]
                if key == 'log':
                    if self.options[key] is None:
                        self.stdout = sys.stdout
                    sys.stdout = value or self.stdout
                elif key == 'err':
                    if self.options[key] is None:
                        self.stderr = sys.stderr
                    sys.stderr = value or self.stderr
                elif key == 'constants':
                    # do not completely replace existing constants data but
                    # leave the special keys such as unames, parnames, etc,
                    # intact
                    self.options[key].update(value)
                    value = self.options[key]
                elif key == 'solution' and not hasattr(value, "load"):
                    t=kw.get('t')
                    if "t" in kw and value is not None:
                        del kw["t"]
                    value = parseS.AUTOSolution(value,t=t)
                self.options[key] = value

        self.options["constants"].update(**kw)

    def __analyseLog(self,text):
        # now we also want to look at the log information to try and determine
        # where the data was written to
        files = ["fort.7", "fort.8", "fort.9"]
        v = self.options["constants"].get("sv")
        if v is None:
            value = re.findall("(Saved as|Appended to) \*\.(\w*)",text)
            if len(value):
                v = value[-1][1]
        if v is not None:
            files = ["b."+v, "s."+v, "d."+v]
        # return as the output data the last filename which was
        # either saved or appended to or
        # otherwise we assume it is fort.7 and fort.8
        self.fort7_path = os.path.join(self.options["dir"],files[0])
        self.fort8_path = os.path.join(self.options["dir"],files[1])
        self.fort9_path = os.path.join(self.options["dir"],files[2])

    def __popen(self,args,stdin=None,stderr=None):
        # subprocess.Popen wrapper:
        return subprocess.Popen(args, stdin=stdin, stdout=subprocess.PIPE, 
                                stderr=stderr, bufsize=1,
                                universal_newlines=True)

    def __handler(self, signum, frame):
        global demo_killed,alarm_demo,demo_max_time
        sys.stdout.write('Demo taking too long: '+alarm_demo+'\n')
        sys.stdout.write('Finding processes to kill...\n')
        if "subprocess" in sys.modules:
            p1 = self.__popen(["ps","ww"])
            p2 = self.__popen(["grep",alarm_demo+".exe"], p1.stdout)
            p3 = self.__popen(["grep","-v","grep"], p2.stdout)
            pids = p3.communicate()[0]
            p1.stdout.close()
            p2.stdout.close()
        else:
            cout,cin = popen2.popen2(
                "ps ww | grep %s.exe | grep -v grep"%(alarm_demo,))
            cin.close()
            pids = cout.read()
            cout.close()
        pids = pids.splitlines()
        for pid in pids:
            sys.stdout.write('Killing: %s\n'%pid)
            pid = pid.split()
            pid = int(pid[0])
            command = "/bin/kill -KILL %d\n"%(pid,)
            sys.stdout.write(command)
            if hasattr(os,"kill"):
                os.kill(pid,signal.SIGKILL)
            else:
                os.system("sh -c '%s'"%command)
        sys.stdout.flush()
        # Restart the alarm to make sure everything gets killed
        if hasattr(signal,"alarm"):
            signal.alarm(20)

    def runDemo(self,d):
        """     This function compiles the demo, then calls the runMakefile
        method, and then cleans up"""
        global demo_killed

        # Garbage collect just before the run to make sure we're not
        # running out of memory quickly.
        gc.collect()
        if self.options["auto_dir"] is None:
            if "AUTO_DIR" in os.environ:
                self.options["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTOExceptions.AUTORuntimeError("AUTO_DIR not set as option or as environment variable")

        if self.options["demos_dir"] is None:
            self.options["demos_dir"] = os.path.join(self.options["auto_dir"],
                                                     "demos")
        self.options["dir"] = os.path.join(self.options["demos_dir"],d)

        var = self.__getmakevars()
        var["FFLAGS"] = var["FFLAGS"] + " " + var["OPT"]
        var["CFLAGS"] = var["CFLAGS"] + " " + var["OPT"]
        del var["OPT"]
        for envvar in var:
            os.environ[envvar] = var[envvar]
        sys.stderr.write("===%s start===\n"%(d,))
        curdir = os.getcwd()
        os.chdir(self.options["dir"])
        if os.path.exists(d+".exe"):
            os.remove(d+".exe")
        cmd = "make -e %s.exe AUTO_DIR=%s"%(d,self.options["auto_dir"])
        if "subprocess" in sys.modules:
            p = self.__popen(cmd.split(), stderr=subprocess.PIPE)
            stdout,stderr = p.stdout,p.stderr
        else:
            stdout,stdin,stderr = popen2.popen3(cmd)
            stdin.close()

        sys.stdout.write(stdout.read())
        sys.stderr.write(stderr.read())
        stdout.close()
        stderr.close()

        self.runMakefile()

        if self.options["clean"] == "yes":
            os.chdir(self.options["dir"])
            cmd = "make -e clean"
            if "subprocess" in sys.modules:
                p = self.__popen(cmd.split(), stderr=subprocess.PIPE)
                stdout,stderr = p.stdout,p.stderr
            else:
                stdout,stdin,stderr = popen2.popen3(cmd)
                stdin.close()
            sys.stdout.write(stdout.read())
            sys.stderr.write(stderr.read())
            stdout.close()
            stderr.close()
        os.chdir(curdir)

        if demo_killed != 0:
            sys.stdout.write("***Demo was killed because it took too long***\n")
            sys.stderr.write("***Demo was killed because it took too long***\n")

        sys.stderr.write("===%s end===\n"%(d,))

    def __setup(self):
        """     This function sets up self.options["dir"] by possibly
        creating the fort.12 file.  The "constants", "solution",
        and "homcont" options
        can be anything with a writeFilename method.  NOTE:  The
        values set here will often be overridden by
        runMakefile (thought almost never by runExecutable
        or runCommand)"""
        solution = self.options["selected_solution"]
        constants = solution.c
        if os.path.exists("fort.2"):
            os.remove("fort.2")
        if os.path.exists("fort.3"):
            os.remove("fort.3")
        if constants["homcont"] is not None:
            constants["homcont"].writeFilename("fort.12")
        elif os.path.exists("fort.12"):
            os.remove("fort.12")

    def __newer(self,sources,target):
        targettime = os.stat(target).st_mtime
        for src in sources:
            if os.stat(src).st_mtime > targettime:
                return True
        return False

    def __getmakevars(self):
        # do the same as $AUTO_DIR/cmds/cmds.make but in Python
        # first get the configure-set variables
        auto_dir = self.options["auto_dir"]
        f = open(os.path.join(auto_dir,"cmds","cmds.make"),"r")
        var = {}
        for line in f:
            line = line.split()
            if len(line) < 2 or line[1] != '=':
                continue
            if line[0] == "SRC":
                break
            for key in ["CC","FC","CFLAGS","FFLAGS","OPT"]:
                if line[0] == key:
                    if len(line) == 2:
                        v = ""
                    else:
                        v = " ".join(line[2:])
                        v = v.replace("$(AUTO_DIR)",auto_dir)
                    var[key] = v
        f.close()
        return var

    def __make(self,equation,fcon=False):
        var = self.__getmakevars()
        # figure out equation file name
        src = ""
        for ext in [".f90",".f",".c"]:
            if os.path.exists(equation+ext):
                src = equation+ext
        if src == "":
            raise AUTOExceptions.AUTORuntimeError(
                "Neither the equation file %s.f90, nor %s.f, nor %s.c exists."%(
                equation,equation,equation))
        # compile
        if not os.path.exists(equation+'.o') or self.__newer([src],
                                                             equation+'.o'):
            if src[-1] == 'c':
                cmd = "%s %s %s -c %s -o %s.o"%(var["CC"],var["CFLAGS"],
                                                var["OPT"],src,equation)
            else:
                cmd = "%s %s %s -c %s -o %s.o"%(var["FC"],var["FFLAGS"],
                                                var["OPT"],src,equation)
            sys.stdout.write(cmd+"\n")
            self.runCommand(cmd)
        # link
        auto_dir = self.options["auto_dir"]
        libdir = os.path.join(auto_dir,"lib")
        if fcon:
            srcdir = os.path.join(auto_dir,"src")
            incdir = os.path.join(auto_dir,"include")
            libs = os.path.join(srcdir,"fcon.f")
            deps = [libs] + [os.path.join(incdir,"fcon.h")]
            var["FFLAGS"] = var["FFLAGS"] + " -I" +  incdir
            execfile = "fcon"
        else:
            libs = os.path.join(libdir,"*.o")
            deps = glob.glob(libs) + [equation+'.o']
            execfile = equation + ".exe"
        if not os.path.exists(execfile) or self.__newer(deps,execfile):
            if src[-1] == 'c':
                cmd = "%s -L%s %s %s %s.o -o %s %s -lauto_c"%(var["FC"],libdir,
                                   var["FFLAGS"],var["OPT"],equation,execfile,libs)
            else:
                cmd = "%s %s %s %s.o -o %s %s"%(var["FC"],var["FFLAGS"],var["OPT"],
                                                    equation,execfile,libs)
            sys.stdout.write(cmd+"\n")
            cmd = cmd.replace(libs, " ".join(deps[:-1]))
            self.runCommand(cmd)
        return os.path.exists(equation+'.exe') and not self.__newer(deps,equation+'.exe')

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        self.config(**kw)
        ret = self.options["solution"].load(constants=self.options["constants"],
                                            homcont=self.options["homcont"])
        self.options["selected_solution"] = ret
        return ret

    def run(self):
        """Run AUTO.

        Run AUTO from the solution with the given AUTO constants.
        Returns a bifurcation diagram of the result.
        """
        self.__setup()
        solution = self.options["selected_solution"]
        constants = solution.c
        if self.options["makefile"] is None:
            if self.options["auto_dir"] is None:
                if "AUTO_DIR" not in os.environ:
                    raise AUTOExceptions.AUTORuntimeError(
                        "AUTO_DIR not set as option or as environment variable")
                self.options["auto_dir"]=os.environ["AUTO_DIR"]
            curdir = os.getcwd()
            os.chdir(self.options["dir"])
            if (constants["IRS"] and
                self.options["selected_solution"].coordnames == []):
                raise AUTOExceptions.AUTORuntimeError(
                    "Restart label IRS=%s not found."%constants["IRS"])
            if "e" not in constants:
                raise AUTOExceptions.AUTORuntimeError(
                    "The equation file argument is missing.")
            equation = constants["e"]
            if self.__make(equation):
                line = "Starting %s ...\n"%equation
                sys.stdout.write(line)
                self.__analyseLog(line)
                for filename in [self.fort7_path,self.fort8_path,
                                 self.fort9_path]:
                    if os.path.exists(filename):
                        os.remove(filename)
                command = os.path.join(".",equation + ".exe")
                prefix = os.environ.get("AUTO_COMMAND_PREFIX")
                if prefix is not None:
                    command = " ".join((prefix, command))
                self.runCommand(command, solution)
                if os.path.exists("fort.3"):
                    os.remove("fort.3")
                line = "%s ... done\n"%equation
                sys.stdout.write(line)
            os.chdir(curdir)
        else:
            self.runMakefile()
        self.__outputCommand()
        import bifDiag
        return bifDiag.bifDiag(self.fort7_path,self.fort8_path,
                               self.fort9_path,constants)

    def runMakefileWithSetup(self,equation=None):
        self.__setup()
        self.runMakefile(equation)
        self.__outputCommand()
    def runMakefile(self,equation=None):        
        """     This function expects self.options["dir"] to be a directory with a Makefile in it and
        a equation file all ready to run (i.e. the Makefile does all of the work,
        like with the demos).  Basically it runs:
        cd dir
        make equation"""
        if equation is None:
            if not(self.options["equation"] is None):
                equation = self.options["equation"]
            else:
                raise AUTOExceptions.AUTORuntimeError("No equation set")
        else:
            self.options["equation"] = equation

        if self.options["auto_dir"] is None:
            if "AUTO_DIR" in os.environ:
                self.options["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTOExceptions.AUTORuntimeError("AUTO_DIR not set as option or as environment variable")

        if self.options["makefile"] == "$AUTO_DIR/cmds/cmds.make fcon":
            self.__make(equation,fcon=True)
        else:
            if self.options["makefile"] == "":
                executable = ("make -e %s AUTO_DIR=%s"%
                              (self.options["equation"],
                               self.options["auto_dir"]))
            else:
                executable = ("make -f %s -e %s AUTO_DIR=%s"%
                              (self.options["makefile"],
                               self.options["equation"],
                               self.options["auto_dir"]))
            path = os.environ["PATH"]
            os.environ["PATH"] = path+os.pathsep+"."
            self.runExecutable(executable)
            os.environ["PATH"] = path

    def runExecutableWithSetup(self,executable=None):
        self.__setup()
        self.runExecutable(executable)
        self.__outputCommand()
    def runExecutable(self,executable=None):
        """     This function expects self.options["dir"] to be a directory with an executable in it and
        a equation file all ready to run.
        Basically it runs:
        cd dir
        executable"""
        if executable is None:
            if not(self.options["executable"] is None):
                executable = self.options["executable"]
            else:
                raise AUTOExceptions.AUTORuntimeError("No executable set")
        else:
            self.options["executable"] = executable

        curdir = os.getcwd()
        os.chdir(self.options["dir"])
        self.runCommand(executable)
        os.chdir(curdir)

    def runCommandWithSetup(self,command=None):
        self.__setup()
        self.runCommand(command,self.options["selected_solution"])
        self.__outputCommand()
    def runCommand(self,command=None,solution=None):
        """     This is the most generic interface.  It just takes a string as a command
        and tries to run it. """
        global demo_killed,alarm_demo,demo_max_time
        gc.collect()
        if command is None:
            if not(self.options["command"] is None):
                command = self.options["command"]
            else:
                raise AUTOExceptions.AUTORuntimeError("No command set")
        else:
            self.options["command"] = command
        alarm_demo = self.options["dir"]
        if demo_max_time > 0 and hasattr(signal,"alarm"):
            signal.alarm(demo_max_time)
        if hasattr(os,"times"):
            user_time = os.times()[2]
        command = os.path.expandvars(command)
        if self.options["makefile"] is None and sys.stdout is sys.__stdout__:
            try:
                status = self.__runCommand_noredir(command, solution)
            except KeyboardInterrupt:
                if hasattr(signal, 'SIGINT'):
                    status = -signal.SIGINT
                else:
                    status = 1
            except OSError:
                if hasattr(signal, 'SIGKILL'):
                    status = -signal.SIGKILL
                else:
                    status = 1
        else:
            status = self.__runCommand_redir(command, solution)
        if hasattr(signal,"alarm"):
            signal.alarm(0)
        if hasattr(os,"times"):
            user_time = os.times()[2]
        else:
            user_time = 1.0
        if status != 0:
            # in case of error, write constants to fort.2 to enable
            # easier debugging.
            if solution is not None:
                f = open('fort.2', 'w')
                self.__write_constants_solution(f, solution)
                f.close()
            if status < 0:
                status = abs(status)
                for s in signals:
                    if hasattr(signal,s) and status == getattr(signal,s):
                        raise AUTOExceptions.AUTORuntimeError(signals[s])
                raise AUTOExceptions.AUTORuntimeError("Signal %d\n"%status)
            raise AUTOExceptions.AUTORuntimeError("Error running AUTO")

    def __write_constants_solution(self, f, solution):
        solution.c.write(f,new=True)
        f.write("s='/'\n")
        solution.write(f,mlab=True)

    def __runCommand_noredir(self,command,solution=None):
        sys.stdout.flush()
        args = os.path.expandvars(command).split()
        if solution is None:
            if "subprocess" in sys.modules:
                return subprocess.call(args)
            elif hasattr(os,"spawnlp"):
                return os.spawnlp(os.P_WAIT, args[0], *args)
            else:
                return os.system(command)
        if "subprocess2" in sys.modules:
            obj = subprocess.Popen(args, stdin=subprocess.PIPE,
                                   universal_newlines=True)
            stdin = obj.stdin
        else:
            stdin = os.popen(command, "w")
            status = 0
        self.__write_constants_solution(stdin, solution)
        stdin.close()
        if "subprocess2" in sys.modules:
            status = obj.wait()
        return status

    def __runCommand_redir(self,command,solution=None):
        global demo_killed
        tmp_out = []
        if "subprocess" in sys.modules or hasattr(popen2,"Popen3"):
            # The command runs here.
            # This is done as the object version so I can use the "poll" method
            # later on to see if it is still running.
            if "subprocess" in sys.modules:
                args = os.path.expandvars(command).split()
                demo_object = self.__popen(args, subprocess.PIPE,
                                           subprocess.PIPE)
                stdin, stdout, stderr = (demo_object.stdin, demo_object.stdout,
                                         demo_object.stderr)
                teststatus = None
            else:
                demo_object = popen2.Popen3(command,1,1)
                stdin, stdout, stderr = (demo_object.tochild,
                             demo_object.fromchild, demo_object.childerr)
                teststatus = -1
            if solution is not None:
                self.__write_constants_solution(stdin, solution)
            stdin.close()
            status = demo_object.poll()
            while status == teststatus:
                try:
                    line = stdout.readline()
                    sys.stdout.write(line)
                    sys.stdout.flush()
                    tmp_out.append(line)
                except IOError:
                    demo_killed = 1
                status = demo_object.poll()
        else:
            stdout, stdin, stderr = popen2.popen3(command)
            self.__write_constants_solution(stdin, solution)
            stdin.close()
            status = 0
        line = stdout.readline()
        # Read the rest of the data from stdout
        while len(line) > 0:
            tmp_out.append(line)
            sys.stdout.write(line)
            sys.stdout.flush()
            line = stdout.readline()
        self.__analyseLog("".join(tmp_out))
        sys.stderr.write(stderr.read())
        stdout.close()
        stderr.close()
        return status

    def __outputCommand(self):
        # Check to see if output files were created.
        # If not, there must have been an error
        if (not os.path.isfile(self.fort7_path) or
            os.path.getsize(self.fort7_path) == 0 or
            not os.path.isfile(self.fort8_path) or
            not os.path.isfile(self.fort9_path)):
            f = open('fort.2', 'w')
            self.__write_constants_solution(f,
                                            self.options["selected_solution"])
            f.close()
            raise AUTOExceptions.AUTORuntimeError("Error running AUTO")

def test():
    log = StringIO()
    stdout = sys.stdout
    class teeStringIO(object):
        def write(self,s):
            stdout.write(s)
            log.write(s)
        def flush(self):
            stdout.flush()
    class quiet(object):
        def write(self,s): pass
        def flush(self): pass
    runner = runAUTO(clean="yes",log=teeStringIO(),err=quiet(),
                     makefile="",
                     demos_dir=os.path.join(os.environ["AUTO_DIR"],"python"))
    runner.runDemo("wav")
    stdout.write(log.getvalue()+"\n")
    log.truncate(0)
    runner.config(equation="clean",log=log)
    runner.runDemo("wav")
    stdout.write(log.getvalue()+"\n")
    log.truncate(0)
    runner.config(equation="first")
    runner.runDemo("wav")
    stdout.write(log.getvalue()+"\n")
    runner.config(log=None, err=None)
    

if __name__ == "__main__":
    #Parse command line arguements
    opts_list,args=getopt.getopt(sys.argv[1:],"l:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]
    log = None
    err = None
    if "-l" in opts:
        log = open(opts["-l"],"w")
        err = open(opts["-l"]+"errors","w")

    runner = runAUTO()
    if len(args) == 0:
        test()
    if len(args) == 1:    
        runner.runDemo(args[0],log=log,err=err)
    if len(args) == 2:
        runner.runDemo(args[0],part=args[1],log=log,err=err)
        
    


