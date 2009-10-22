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
    def __init__(self,cnf=None,**kw):
        if isinstance(cnf,self.__class__):
            for k,v in cnf.__dict__.items():
                self.__dict__[k] = v
            if kw != {}:
                self.options = self.options.copy()
                if self.options["constants"] is not None:
                    try:
                        self.options["constants"] = parseC.parseC(
                            self.options["constants"])
                    except IOError:
                        pass
                if self.options["homcont"] is not None:
                    try:
                        self.options["homcont"] = parseH.parseH(
                            self.options["homcont"])
                    except IOError:
                        pass
                self.config(**kw)
            return

        # Set the signal handler
        if hasattr(signal,'SIGALRM'):
            signal.signal(signal.SIGALRM, self.__handler)
        self.internalLog = None
        self.internalErr = None

        self.options={}
        self.options["log"] = None
        self.options["err"] = None
        self.options["auto_dir"] = None
        self.options["demos_dir"] = None
        self.options["equation"] = "all"
        self.options["redir"] = "yes"
        self.options["verbose"] = "no"
        self.options["verbose_print"] = None
        self.options["clean"] = "no"
        self.options["dir"] = "."
        self.options["executable"] = None
        self.options["command"] = None
        self.options["makefile"] = None
        self.options["constants"] = None
        self.options["solution"] = None
        self.options["homcont"] = None

        self.config(**kw)
            
    def __getattr__(self,attr):
        if self.options is not None:
            c = self.options["constants"]
            if attr == "c" and c is not None:
                return c
        raise AttributeError

    def __setattr__(self,attr,item):
        if attr == "c":
            self.options["constants"] = item
        else:
            self.__dict__[attr] = item

    def verbose_write(self,s):
        if self.options["verbose_print"] is None:
            sys.stdout.write(s)
        else:
            self.options["verbose_print"].write(s)

    def verbose_flush(self):
        if self.options["verbose_print"] is None:
            sys.stdout.flush()
        else:
            self.options["verbose_print"].flush()

    def config(self,**kw):
        """     Change the options for this runner object"""
        # first the normal parameters, then IRS=... style parameters
        for key in kw:
            if key in self.options:
                value = kw[key]
                ovalue = self.options[key]
                # preserve unames and parnames if not in a constants file
                if (key == "constants" and value is not None and
                    ovalue is not None and (ovalue["unames"] is not None or
                                            ovalue["parnames"] is not None)):
                    value = parseC.parseC(value)
                    for k in ["unames", "parnames"]:
                        if value[k] is None:
                            value[k] = ovalue[k]
                self.options[key] = value
        for key in kw:
            value = kw[key]
            if (key in self.options or key in ['t','LAB','PT','BR','TY']
                or key[:7] == 'Active '):
                continue
            if value is None:
                continue
            if self.options["constants"] is None:
                self.options["constants"]=parseC.parseC()
            if (self.options["homcont"] is not None and
                  key in self.options["homcont"]):
                self.options["homcont"][key] = value
            elif key in self.options["constants"]:
                self.options["constants"][key] = value
            else:
                raise AUTOExceptions.AUTORuntimeError(
                    "Unknown option: %s"%(key,))

    def __printLog(self,text):
        # Write out the log information to the appropriate place
        if not(self.options["log"] is None):
            self.options["log"].write(text)
        self.internalLog.write(text)
        # now we also want to look at the log information to try and determine
        # where the data was written to
        files = ["fort.7", "fort.8", "fort.9"]
        v = None
        c = self.options["constants"]
        if c is not None and "sv" in c:
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
        self.fort7_path = os.path.join(self.options["dir"],files[0])
        self.fort8_path = os.path.join(self.options["dir"],files[1])
        self.fort9_path = os.path.join(self.options["dir"],files[2])

    def __printErr(self,text):
        if not(self.options["err"] is None):
            try:
                self.options["err"].write(text)
            except ValueError: # closed file
                pass
        self.internalErr.write(text)

    def __popen(self,args,stdin=None,stderr=None):
        # subprocess.Popen wrapper:
        return subprocess.Popen(args, stdin=stdin, stdout=subprocess.PIPE, 
                                stderr=stderr, bufsize=1,
                                universal_newlines=True)

    def __handler(self, signum, frame):
        global demo_killed,alarm_demo,demo_max_time
        self.verbose_write('Demo taking too long: '+alarm_demo)
        self.verbose_write('Finding processes to kill...')
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
        pids = pids.splitlines(pids)
        for pid in pids:
            self.verbose_write('Killing: '+str(pid))
            pid = pid.split()
            pid = int(pid[0])
            command = "/bin/kill -KILL %d"%(pid,)
            self.verbose_write(command)
            if hasattr(os,"kill"):
                os.kill(pid,signal.SIGKILL)
            else:
                os.system("sh -c '%s'"%command)
        # Restart the alarm to make sure everything gets killed
        self.verbose_flush()
        if hasattr(signal,"alarm"):
            signal.alarm(20)

    def __resetInternalLogs(self):
        # Garbage collect just before the run to make sure we're not
        # running out of memory quickly.
        gc.collect()
        if self.internalLog is None:
            self.internalLog = StringIO()
        else:
            self.internalLog.close()
            self.internalLog = StringIO()
        if self.internalErr is None:
            self.internalErr = StringIO()
        else:
            self.internalErr.close()
            self.internalErr = StringIO()
    def __rewindInternalLogs(self):
        self.internalLog.seek(0)
        self.internalErr.seek(0)
        
    def runDemo(self,d):
        self.__resetInternalLogs()
        self.__runDemo(d)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]

    def __runDemo(self,d):
        """     This function compiles the demo, then calls the runMakefile
        method, and then cleans up"""
        global demo_killed

        if self.options["auto_dir"] is None:
            if "AUTO_DIR" in os.environ:
                self.options["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTOExceptions.AUTORuntimeError("AUTO_DIR not set as option or as environment variable")
        else:
            os.environ["AUTO_DIR"]=self.options["auto_dir"]


        if self.options["demos_dir"] is None:
            self.options["demos_dir"] = os.path.join(self.options["auto_dir"],
                                                     "demos")
        self.options["dir"] = os.path.join(self.options["demos_dir"],d)

        self.__printErr("===%s start===\n"%(d,))
        curdir = os.getcwd()
        os.chdir(self.options["dir"])
        if os.path.exists(d+".exe"):
            os.remove(d+".exe")
        cmd = "make -e %s.exe"%d
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

        self.__runMakefile()

        if self.options["clean"] == "yes":
            os.chdir(self.options["dir"])
            cmd = "make -e clean"
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
        os.chdir(curdir)

        if demo_killed != 0:
            self.__printLog("***Demo was killed because it took too long***\n")
            self.__printErr("***Demo was killed because it took too long***\n")

        self.__printErr("===%s end===\n"%(d,))

    def __setup(self):
        """     This function sets up self.options["dir"] by creating
        fort.2, fort.3 and fort.12 files.  The "constants", "solution",
        and "homcont" options 
        can be anything with a writeFilename method.  NOTE:  The
        values set here will often be overridden by
        runMakefile (thought almost never by runExecutable
        or runCommand)"""
        if os.path.exists("fort.2"):
            os.remove("fort.2")
        if self.options["constants"] is None:
            open("fort.2","wb").close()
            self.options["constants"] = parseC.parseC()
        else:
            self.options["constants"].writeFilename("fort.2")
        solution = self.options["solution"]

        # figure out equation file name
        equation = self.options["equation"][14:]
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
        self.options["constants"]["e"] = e
        self.options["constants"].writeFilename("fort.2")

        if os.path.exists("fort.3"):
            os.remove("fort.3")
        if solution is None:
            open("fort.3","wb").close()
        else:
            solution.writeFilename("fort.3",mlab=True)

        if os.path.exists("fort.12"):
            os.remove("fort.12")
        if self.options["homcont"] is not None:
            self.options["homcont"].writeFilename("fort.12")

    def __newer(self,sources,target):
        targettime = os.stat(target)[stat.ST_MTIME]
        for src in sources:
            if os.stat(src)[stat.ST_MTIME] > targettime:
                return True
        return False

    def __make(self,equation,fcon=False):
        # do the same as $AUTO_DIR/cmds/cmds.make but in Python
        # first get the configure-set variables
        f = open(os.path.join(os.path.expandvars("$AUTO_DIR"),
                              "cmds","cmds.make"),"r")
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
                        v = v.replace("$(AUTO_DIR)",os.environ["AUTO_DIR"])
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
            if self.options["verbose"] == "yes":
                self.verbose_write(cmd+"\n")
            self.__printLog(cmd+"\n")
            self.__runCommand(cmd)
        # link
        libdir = os.path.join(os.path.expandvars("$AUTO_DIR"),"lib")
        if fcon:
            srcdir = os.path.join(os.path.expandvars("$AUTO_DIR"),"src")
            incdir = os.path.join(os.path.expandvars("$AUTO_DIR"),"include")
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
            if self.options["verbose"] == "yes":
                self.verbose_write(cmd+"\n")
            self.__printLog(cmd+"\n")
            cmd = cmd.replace(libs, " ".join(deps[:-1]))
            self.__runCommand(cmd)
        return os.path.exists(equation+'.exe') and not self.__newer(deps,equation+'.exe')

    def run(self,**kw):
        """Run AUTO.

        Run AUTO from the solution with the given AUTO constants.
        Returns a bifurcation diagram of the result.
        """
        self.config(**kw)
        log,err,data = self.runMakefileWithSetup()
        if self.options["err"] is None:
            # log was already written if the runner is verbose
            if self.options["verbose"] == "no":
                sys.stdout.write(log.read())
            sys.stdout.write(err.read())
        return data

    def runMakefileWithSetup(self,equation=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runMakefile(equation)
        data = self.__outputCommand()
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,data]
    def runMakefile(self,equation=None):
        self.__resetInternalLogs()
        self.__runMakefile(equation)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]
    def __runMakefile(self,equation=None):        
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
        else:
            os.environ["AUTO_DIR"]=self.options["auto_dir"]


        if self.options["makefile"] is None:
            executable = "make -e %s"%self.options["equation"]
            self.__runExecutable(executable)
        elif self.options["makefile"] == "$AUTO_DIR/cmds/cmds.make":
            curdir = os.getcwd()
            os.chdir(self.options["dir"])
            equation = self.options["constants"]["e"]
            if self.__make(equation):
                line = "Starting %s ...\n"%equation
                if self.options["verbose"] == "yes":
                    self.verbose_write(line)
                self.__printLog(line)
                for filename in [self.fort7_path,self.fort8_path,
                                 self.fort9_path]:
                    if os.path.exists(filename):
                        os.remove(filename)
                self.__runCommand(os.path.join(".",equation + ".exe"))
                if os.path.exists("fort.2"):
                    os.remove("fort.2")
                if os.path.exists("fort.3"):
                    os.remove("fort.3")
                line = "%s ... done\n"%equation
                if self.options["verbose"] == "yes":
                    self.verbose_write(line)
                self.__printLog(line)
            os.chdir(curdir)
        elif self.options["makefile"] == "$AUTO_DIR/cmds/cmds.make fcon":
            self.__make(equation,fcon=True)
        else:
            executable = "make -f %s -e %s"%(self.options["makefile"],self.options["equation"])
            self.__runExecutable(executable)

    def runExecutableWithSetup(self,executable=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runExecutable(executable)
        data = self.__outputCommand()
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,data]
    def runExecutable(self,executable=None):
        self.__resetInternalLogs()
        self.__runExecutable(executable)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]
    def __runExecutable(self,executable=None):
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
        self.__runCommand(executable)
        os.chdir(curdir)

    def runCommand(self,command=None):
        self.__resetInternalLogs()
        self.__runCommand(command)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]
    def runCommandWithSetup(self,command=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runCommand(command)
        data = self.__outputCommand()
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,data]
    def __runCommand(self,command=None):
        """     This is the most generic interface.  It just takes a string as a command
        and tries to run it. """
        global demo_killed,alarm_demo,demo_max_time
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
        if self.options["verbose"] == "yes" and self.options["redir"] == "no":
            try:
                status = self.__runCommand_noredir(command)
            except KeyboardInterrupt:
                if hasattr(signal, 'SIGINT'):
                    status = -signal.SIGINT
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
        self.verbose_flush()
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
                    if self.options["verbose"] == "yes":
                        self.verbose_write(line)
                        self.verbose_flush()
                    tmp_out.append(line)
                except:
                    demo_killed = 1
                status = demo_object.poll()
            if status != 0 and self.options["verbose"] == "yes":
                self.verbose_write(stderr.read())
                self.verbose_flush()
        else:
            stdout, stdin, stderr = popen2.popen3(command)
            stdin.close()
            status = 0
        line = stdout.readline()
        # Read the rest of the data from stdout
        while len(line) > 0:
            tmp_out.append(line)
            if self.options["verbose"] == "yes":
                self.verbose_write(line)
                self.verbose_flush()
            line = stdout.readline()
        self.__printLog("".join(tmp_out))
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
                                   self.fort9_path,**self.options)
        raise AUTOExceptions.AUTORuntimeError("Error running AUTO")

def test():
    runner = runAUTO(verbose="yes",clean="yes")
    [log,err]=runner.runDemo("wav")
    print(log.read())
    runner.config(equation="clean",verbose="no")
    [log,err]=runner.runDemo("wav")
    print(log.read())
    runner.config(equation="first")
    [log,err]=runner.runDemo("wav")
    print(log.read())
    

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
        
    


