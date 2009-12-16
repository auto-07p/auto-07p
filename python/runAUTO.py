#! /usr/bin/env python
import getopt,sys,os
import signal, os, time
try:
    from cStringIO import StringIO
except ImportError: # Python 3
    from io import StringIO
import re
import glob,stat
import AUTOExceptions,parseC,parseH,gc
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
        if hasattr(signal,'SIGALRM'):
            signal.signal(signal.SIGALRM, self.__handler)

        self.options={}
        c = parseC.parseC()
        c["auto_dir"] = None
        c["log"] = None
        c["err"] = None
        c["dir"] = "."
        c["makefile"] = None
        c["homcont"] = None
        self.options["demos_dir"] = None
        self.options["equation"] = "all"
        self.options["verbose"] = "yes" # note: this is ignored!
        self.options["clean"] = "no"
        self.options["executable"] = None
        self.options["command"] = None
        self.options["constants"] = c
        self.options["solution"] = None

        self.config(**kw)
            
    def config(self,**kw):
        """     Change the options for this runner object"""
        for key in list(kw):
            if key in self.options and key != "constants":
                self.options[key] = kw[key]
                del kw[key]
        self.options["constants"].update(**kw)

    def __printLog(self,text):
        # Write out the log information to the appropriate place
        f = self.options["constants"]["log"] or sys.stdout
        f.write(text)
        f.flush()

    def __analyseLog(self,text):
        # now we also want to look at the log information to try and determine
        # where the data was written to
        files = ["fort.7", "fort.8", "fort.9"]
        v = None
        c = self.options["constants"]
        if c is not None and c["sv"] is not None:
            v = c["sv"]
        else:
            value = re.findall("(Saved as|Appended to) \*\.(\w*)",text)
            if len(value):
                v = value[-1][1]
        if v:
            files = ["b."+v, "s."+v, "d."+v]
        # return as the output data the last filename which was
        # either saved or appended to or
        # otherwise we assume it is fort.7 and fort.8
        self.fort7_path = os.path.join(self.options["constants"]["dir"],files[0])
        self.fort8_path = os.path.join(self.options["constants"]["dir"],files[1])
        self.fort9_path = os.path.join(self.options["constants"]["dir"],files[2])

    def __printErr(self,text):
        f = self.options["constants"]["err"] or sys.stderr
        f.write(text)
        f.flush()

    def __popen(self,args,stdin=None,stderr=None):
        # subprocess.Popen wrapper:
        return subprocess.Popen(args, stdin=stdin, stdout=subprocess.PIPE, 
                                stderr=stderr, bufsize=1,
                                universal_newlines=True)

    def __handler(self, signum, frame):
        global demo_killed,alarm_demo,demo_max_time
        self.__printLog('Demo taking too long: '+alarm_demo+'\n')
        self.__printLog('Finding processes to kill...\n')
        if "subprocess" in sys.modules:
            p1 = self.__popen(["ps","ww"])
            p2 = self.__popen(["grep",alarm_demo+".exe"], p1.stdout)
            p3 = self.__popen(["grep","-v","grep"], p2.stdout)
            cout = p3.stdout
        else:
            cout,cin = popen2.popen2(
                "ps ww | grep %s.exe | grep -v grep"%(alarm_demo,))
            cin.close()
        pids = cout.read()
        if "subprocess" in sys.modules:
            p1.stdout.close()
            p2.stdout.close()
        cout.close()
        pids = pids.splitlines()
        for pid in pids:
            self.__printLog('Killing: %s\n'%pid)
            pid = pid.split()
            pid = int(pid[0])
            command = "/bin/kill -KILL %d\n"%(pid,)
            self.__printLog(command)
            if hasattr(os,"kill"):
                os.kill(pid,signal.SIGKILL)
            else:
                os.system("sh -c '%s'"%command)
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
        if self.options["constants"]["auto_dir"] is None:
            if "AUTO_DIR" in os.environ:
                self.options["constants"]["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTOExceptions.AUTORuntimeError("AUTO_DIR not set as option or as environment variable")

        if self.options["demos_dir"] is None:
            self.options["demos_dir"] = os.path.join(self.options["constants"]["auto_dir"],
                                                     "demos")
        self.options["constants"]["dir"] = os.path.join(self.options["demos_dir"],d)

        self.__printErr("===%s start===\n"%(d,))
        curdir = os.getcwd()
        os.chdir(self.options["constants"]["dir"])
        if os.path.exists(d+".exe"):
            os.remove(d+".exe")
        cmd = "make -e %s.exe AUTO_DIR=%s"%(d,self.options["constants"]["auto_dir"])
        if "subprocess" in sys.modules:
            p = self.__popen(cmd.split(), stderr=subprocess.PIPE)
            stdout,stderr = p.stdout,p.stderr
        else:
            stdout,stdin,stderr = popen2.popen3(cmd)
            stdin.close()

        self.__printLog(stdout.read())
        self.__printErr(stderr.read())
        stdout.close()
        stderr.close()

        self.runMakefile()

        if self.options["clean"] == "yes":
            os.chdir(self.options["constants"]["dir"])
            cmd = "make -e clean"
            if "subprocess" in sys.modules:
                p = self.__popen(cmd.split(), stderr=subprocess.PIPE)
                stdout,stderr = p.stdout,p.stderr
            else:
                import popen2
                stdout,stdin,stderr = popen2.popen3(cmd)
                stdin.close()
            self.__printLog(stdout.read())
            self.__printErr(stderr.read())
            stdout.close()
            stderr.close()
        os.chdir(curdir)

        if demo_killed != 0:
            self.__printLog("***Demo was killed because it took too long***\n")
            self.__printErr("***Demo was killed because it took too long***\n")

        self.__printErr("===%s end===\n"%(d,))

    def __setup(self):
        """     This function sets up self.options["constants"]["dir"] by creating
        fort.2, fort.3 and fort.12 files.  The "constants", "solution",
        and "homcont" options 
        can be anything with a writeFilename method.  NOTE:  The
        values set here will often be overridden by
        runMakefile (thought almost never by runExecutable
        or runCommand)"""
        if os.path.exists("fort.2"):
            os.remove("fort.2")
        self.options["constants"].writeFilename("fort.2")

        solution = self.options["solution"]
        if os.path.exists("fort.3"):
            os.remove("fort.3")
        if solution is None:
            open("fort.3","wb").close()
        else:
            solution.writeFilename("fort.3",mlab=True)

        if os.path.exists("fort.12"):
            os.remove("fort.12")
        if self.options["constants"]["homcont"] is not None:
            self.options["constants"]["homcont"].writeFilename("fort.12")

    def __newer(self,sources,target):
        targettime = os.stat(target)[stat.ST_MTIME]
        for src in sources:
            if os.stat(src)[stat.ST_MTIME] > targettime:
                return True
        return False

    def __make(self,equation,fcon=False):
        # do the same as $AUTO_DIR/cmds/cmds.make but in Python
        # first get the configure-set variables
        auto_dir = self.options["constants"]["auto_dir"]
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
            self.__printLog(cmd+"\n")
            self.runCommand(cmd)
        # link
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
            self.__printLog(cmd+"\n")
            cmd = cmd.replace(libs, " ".join(deps[:-1]))
            self.runCommand(cmd)
        return os.path.exists(equation+'.exe') and not self.__newer(deps,equation+'.exe')

    def load(self,**kw):
        """Load solution with the given AUTO constants.
        Returns a shallow copy with a copied set of updated constants
        """
        self.config(**kw)
        c = self.options["constants"].copy()
        c['e'] = self.options["equation"][14:]
        if hasattr(self.options["solution"],'load'):
            return self.options["solution"].load(**c)
        else:
            if 't' in kw:
                c['t'] = kw['t']
            import parseS
            return parseS.AUTOSolution(self.options["solution"],**c).load()

    def run(self,**kw):
        """Run AUTO.

        Run AUTO from the solution with the given AUTO constants.
        Returns a bifurcation diagram of the result.
        """
        self.config(**kw)

        # figure out equation file name
        equation = self.options["constants"]["e"]
        e = None
        for ext in [".f90",".f",".c"]:
            if os.path.exists(equation+ext):
                e = equation
                break
        if e is None:
            if equation == "":
                raise AUTOExceptions.AUTORuntimeError(
                "The equation file argument is missing.")
            raise AUTOExceptions.AUTORuntimeError(
                "Neither the equation file %s.f90, nor %s.f, nor %s.c exists."%(
                equation,equation,equation))
        self.options["equation"] = "EQUATION_NAME=%s"%equation

        return self.runMakefileWithSetup()

    def runMakefileWithSetup(self,equation=None):
        self.__setup()
        self.runMakefile(equation)
        return self.__outputCommand()
    def runMakefile(self,equation=None):        
        """     This function expects self.options["constants"]["dir"] to be a directory with a Makefile in it and
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

        if self.options["constants"]["auto_dir"] is None:
            if "AUTO_DIR" in os.environ:
                self.options["constants"]["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTOExceptions.AUTORuntimeError("AUTO_DIR not set as option or as environment variable")

        if self.options["constants"]["makefile"] == "$AUTO_DIR/cmds/cmds.make":
            curdir = os.getcwd()
            os.chdir(self.options["constants"]["dir"])
            equation = self.options["constants"]["e"]
            if self.__make(equation):
                line = "Starting %s ...\n"%equation
                self.__printLog(line)
                self.__analyseLog(line)
                for filename in [self.fort7_path,self.fort8_path,
                                 self.fort9_path]:
                    if os.path.exists(filename):
                        os.remove(filename)
                self.runCommand(os.path.join(".",equation + ".exe"))
                if os.path.exists("fort.2"):
                    os.remove("fort.2")
                if os.path.exists("fort.3"):
                    os.remove("fort.3")
                line = "%s ... done\n"%equation
                self.__printLog(line)
            os.chdir(curdir)
        elif self.options["constants"]["makefile"] == "$AUTO_DIR/cmds/cmds.make fcon":
            self.__make(equation,fcon=True)
        else:
            if self.options["constants"]["makefile"] is None:
                executable = ("make -e %s AUTO_DIR=%s"%
                              (self.options["equation"],
                               self.options["constants"]["auto_dir"]))
            else:
                executable = ("make -f %s -e %s AUTO_DIR=%s"%
                              (self.options["constants"]["makefile"],
                               self.options["equation"],
                               self.options["constants"]["auto_dir"]))
            path = os.environ["PATH"]
            os.environ["PATH"] = path+os.pathsep+"."
            self.runExecutable(executable)
            os.environ["PATH"] = path

    def runExecutableWithSetup(self,executable=None):
        self.__setup()
        self.runExecutable(executable)
        return self.__outputCommand()
    def runExecutable(self,executable=None):
        """     This function expects self.options["constants"]["dir"] to be a directory with an executable in it and
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
        os.chdir(self.options["constants"]["dir"])
        self.runCommand(executable)
        os.chdir(curdir)

    def runCommandWithSetup(self,command=None):
        self.__setup()
        self.runCommand(command)
        return self.__outputCommand()
    def runCommand(self,command=None):
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
        alarm_demo = self.options["constants"]["dir"]
        if demo_max_time > 0 and hasattr(signal,"alarm"):
            signal.alarm(demo_max_time)
        if hasattr(os,"times"):
            user_time = os.times()[2]
        command = os.path.expandvars(command)
        if (self.options["constants"]["log"] is None and
            self.options["constants"]["err"] is None):
            try:
                status = self.__runCommand_noredir(command)
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
            status = self.__runCommand_redir(command)
        if hasattr(signal,"alarm"):
            signal.alarm(0)
        if hasattr(os,"times"):
            user_time = os.times()[2]
        else:
            user_time = 1.0
        if status != 0:
            if status < 0:
                status = abs(status)
                found = False
                for s in signals:
                    if hasattr(signal,s) and status == getattr(signal,s):
                        raise AUTOExceptions.AUTORuntimeError(signals[s])
                raise AUTOExceptions.AUTORuntimeError("Signal %d\n"%status)
            raise AUTOExceptions.AUTORuntimeError("Error running AUTO")

    def __runCommand_noredir(self,command=None):
        args = os.path.expandvars(command).split()
        if "subprocess" in sys.modules:
            return subprocess.call(args)
        elif hasattr(os,"spawnlp"):
            return os.spawnlp(os.P_WAIT, args[0], *args)
        else:
            return os.system(command)

    def __runCommand_redir(self,command=None):
        global demo_killed
        tmp_out = []
        if "subprocess" in sys.modules or hasattr(popen2,"Popen3"):
            # The command runs here.
            # This is done as the object version so I can use the "poll" method
            # later on to see if it is still running.
            if "subprocess" in sys.modules:
                args = os.path.expandvars(command).split()
                demo_object = self.__popen(args, stderr=subprocess.PIPE)
                stdout, stderr = demo_object.stdout, demo_object.stderr
                teststatus = None
            else:
                demo_object = popen2.Popen3(command,1,1)
                demo_object.tochild.close()
                stdout, stderr = demo_object.fromchild, demo_object.childerr
                teststatus = -1
            status = demo_object.poll()
            while status == teststatus:
                try:
                    line = stdout.readline()
                    self.__printLog(line)
                    tmp_out.append(line)
                except IOError:
                    demo_killed = 1
                status = demo_object.poll()
        else:
            stdout, stdin, stderr = popen2.popen3(command)
            stdin.close()
            status = 0
        line = stdout.readline()
        # Read the rest of the data from stdout
        while len(line) > 0:
            tmp_out.append(line)
            self.__printLog(line)
            line = stdout.readline()
        self.__analyseLog("".join(tmp_out))
        self.__printErr(stderr.read())
        stdout.close()
        stderr.close()
        return status

    def __outputCommand(self):
        # Check to see if output files were created.
        # If not, there must have been an error
        if (os.path.isfile(self.fort7_path) and
            os.path.isfile(self.fort8_path) and
            os.path.isfile(self.fort9_path)):
            import bifDiag
            return bifDiag.bifDiag(self.fort7_path,self.fort8_path,
                                   self.fort9_path,self.options["constants"])
        raise AUTOExceptions.AUTORuntimeError("Error running AUTO")

def test():
    log = StringIO()
    class teeStringIO(object):
        def write(self,s):
            sys.stdout.write(s)
            log.write(s)
        def flush(self):
            sys.stdout.flush()
    class quiet(object):
        def write(self,s): pass
        def flush(self): pass
    runner = runAUTO(clean="yes",log=teeStringIO(),err=quiet(),
                     auto_dir=os.path.join(os.environ["AUTO_DIR"],"..","97"))
    runner.runDemo("wav")
    print(log.getvalue())
    log.truncate(0)
    runner.config(equation="clean",log=log)
    runner.runDemo("wav")
    print(log.getvalue())
    log.truncate(0)
    runner.config(equation="first")
    runner.runDemo("wav")
    print(log.getvalue())
    

if __name__ == "__main__":
    import getopt,os

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
        
    


