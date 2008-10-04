#! /usr/bin/env python
import getopt,sys,os,string
import signal, os, time
import cStringIO
import re
import types
import glob,stat
import AUTOExceptions,parseC,parseH,parseBandS
try:
    import subprocess
except ImportError:
    import popen2

# A few global variables for the signal handler
alarm_demo=""
demo_killed=0
demo_max_time=-1

class runAUTO:
    def __init__(self,cnf={},**kw):
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
        self.options["verbose"] = "no"
        self.options["verbose_print"] = sys.stdout
        self.options["clean"] = "no"
        self.options["dir"] = "."
        self.options["executable"] = None
        self.options["command"] = None
        self.options["makefile"] = None
        self.options["constants"] = parseC.parseC()
        self.options["solution"] = None
        self.options["homcont"] = None
        self.options["copy setup"] = None

        self.outputFort7 = None
        self.outputFort8 = None

        self.__parseOptions(kw)
        self.__parseOptions(cnf)
            
    def __parseOptions(self,dict):
        # first the normal parameters, then IRS=... style parameters
        for key in dict.keys():
            if self.options.has_key(key):
                self.options[key] = dict[key]
        for key in dict.keys():
            if self.options.has_key(key):
                pass
            elif self.options["constants"].has_key(key):
                if key == "DS" and (dict[key] == '-' or dict[key] == '+'):
                    if dict[key] == '-':
                        self.options["constants"][key] = -self.options["constants"][key]
                else:
                    self.options["constants"][key] = dict[key]
            elif (not self.options["homcont"] is None and
                  self.options["homcont"].has_key(key)):
                self.options["homcont"][key] = dict[key]
            else:
                raise "Unknown option: %s"%(key,)

    def __printLog(self,text):
        # Write out the log information to the appropriate place
        if not(self.options["log"] is None):
            self.options["log"].write(text)
        self.internalLog.write(text)
        # now we also want to look at the log information to try and determine
        # where the data was written to
        value = re.findall("(Saved as|Appended to) \*\.(\w*)",text)
        if len(value):
            # return as the output data the last filename which was
            # either saved or appended to
            self.fort7_path = os.path.join(self.options["dir"],"b.%s"%(value[-1][1]))
            self.fort8_path = os.path.join(self.options["dir"],"s.%s"%(value[-1][1]))
            self.fort9_path = os.path.join(self.options["dir"],"d.%s"%(value[-1][1]))
        else:
            # Otherwise we assume it is fort.7 and fort.8
            self.fort7_path = os.path.join(self.options["dir"],"fort.7")
            self.fort8_path = os.path.join(self.options["dir"],"fort.8")
            self.fort9_path = os.path.join(self.options["dir"],"fort.9")

    def __printErr(self,text):
        if not(self.options["err"] is None):
            self.options["err"].write(text)
        self.internalErr.write(text)

    def getFort7(self):
        self.outputFort7.seek(0)
        return self.outputFort7

    def getFort8(self):
        self.outputFort8.seek(0)
        return self.outputFort8
        
    def __handler(self, signum, frame):
        global demo_killed,alarm_demo,demo_max_time
        self.options["verbose_print"]('Demo taking too long: '+alarm_demo)
        self.options["verbose_print"]('Finding processes to kill...')
        if "subprocess" in sys.modules.keys():
            p1 = subprocess.Popen(["ps","ww"], stdout=subprocess.PIPE)
            p2 = subprocess.Popen(["grep",alarm_demo+".exe"], stdin=p1.stdout,
                                  stdout=subprocess.PIPE)
            p3 = subprocess.Popen(["grep","-v","grep"], stdin=p2.stdout,
                                  stdout=subprocess.PIPE)
            cout = p3.stdout
        else:
            cout,cin = popen2.popen2(
                "ps ww | grep %s.exe | grep -v grep"%(alarm_demo,))
            cin.close()
        pids = cout.read()
        if "subprocess" in sys.modules.keys():
            p1.stdout.close()
            p2.stdout.close()
        cout.close()
        pids = string.split(pids,"\n")
        pids = pids[:-1]
        for pid in pids:
            self.options["verbose_print"]('Killing: '+str(pid))
            pid = string.split(pid)
            pid = int(pid[0])
            command = "/bin/kill -KILL %d"%(pid,)
            self.options["verbose_print"](command)
            if hasattr(os,"kill"):
                os.kill(pid,signal.SIGKILL)
            else:
                os.system("sh -c '%s'"%command)
        # Restart the alarm to make sure everything gets killed
        self.options["verbose_print"].flush()
        if hasattr(signal,"alarm"):
            signal.alarm(20)

    def __resetInternalLogs(self):
        if self.internalLog is None:
            self.internalLog = cStringIO.StringIO()
        else:
            self.internalLog.close()
            self.internalLog = cStringIO.StringIO()
        if self.internalErr is None:
            self.internalErr = cStringIO.StringIO()
        else:
            self.internalErr.close()
            self.internalErr = cStringIO.StringIO()
    def __rewindInternalLogs(self):
        self.internalLog.seek(0)
        self.internalErr.seek(0)
        
    def runDemo(self,d):
        self.__resetInternalLogs()
        self.__runDemo(d)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,self.data]

    def __runDemo(self,d):
        """     This function compiles the demo, then calls the runMakefile
        method, and then cleans up"""
        global demo_killed

        if self.options["auto_dir"] is None:
            if os.environ.has_key("AUTO_DIR"):
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
        if "subprocess" in sys.modules.keys():
            p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE,
                                 stderr=subprocess.PIPE)
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
            if "subprocess" in sys.modules.keys():
                p = subprocess.Popen(cmd.split(), stdout=subprocess.PIPE,
                                     stderr=subprocess.PIPE)
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

    def config(self,cnf={},**kw):
        """     Change the options for this runner object"""
        self.__parseOptions(kw)
        self.__parseOptions(cnf)

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
        if (self.options["constants"] is None):
            raise AUTOExceptions.AUTORuntimeError("tried to explicitly setup but parameter not set as an option")
        else:
            self.options["constants"].writeFilename("fort.2")

        if os.path.exists("fort.3"):
            os.remove("fort.3")
        if (self.options["solution"] is None):
            open("fort.3","wb").close()
        else:
            self.options["solution"].writeFilename("fort.3")

        if os.path.exists("fort.12"):
            os.remove("fort.12")
        if not (self.options["homcont"] is None):
            self.options["homcont"].writeFilename("fort.12")

    def __newer(self,sources,target):
        targettime = os.stat(target)[stat.ST_MTIME]
        for src in sources:
            if os.stat(src)[stat.ST_MTIME] > targettime:
                return True
        return False

    def __make(self,equation):
        # do the same as $AUTO_DIR/cmds/cmds.make but in Python
        # first get the configure-set variables
        f = open(os.path.join(os.path.expandvars("$AUTO_DIR"),
                              "cmds","cmds.make"),"rb")
        var = {}
        while True:
            line = string.split(f.readline())
            if len(line) < 3:
                continue
            if line[0] == "SRC":
                break
            for key in ["CC","FC","CFLAGS","FFLAGS","OPT"]:
                if line[0] == key:
                    v = string.join(line[2:])
                    v = string.replace(v,"$(AUTO_DIR)",os.environ["AUTO_DIR"])
                    var[key] = v
        f.close()
        # figure out equation file name
        src = ""
        for ext in [".f90",".f",".c"]:
            if os.path.exists(equation+ext):
                src = equation+ext
        if src == "":
            print "Neither the equation file %s.f90, nor %s.f, nor %s.c exists."%(
                equation)
            return
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
                self.options["verbose_print"].write(cmd+"\n")
            self.__printLog(cmd+"\n")
            self.__runCommand(cmd)
        # link
        libdir = os.path.join(os.path.expandvars("$AUTO_DIR"),"lib")
        libs = os.path.join(libdir,"*.o")
        deps = glob.glob(libs) + [equation+'.o']
        if not os.path.exists(equation+'.exe') or self.__newer(deps,equation+'.exe'):
            if src[-1] == 'c':
                cmd = "%s -L%s %s %s %s.o -o %s.exe %s -lauto_c"%(var["FC"],libdir,
                                   var["FFLAGS"],var["OPT"],equation,equation,libs)
            else:
                cmd = "%s %s %s %s.o -o %s.exe %s"%(var["FC"],var["FFLAGS"],var["OPT"],
                                                    equation,equation,libs)
            if self.options["verbose"] == "yes":
                self.options["verbose_print"].write(cmd+"\n")
            self.__printLog(cmd+"\n")
            cmd = string.replace(cmd, libs, string.join(deps[:-1]," "))
            self.__runCommand(cmd)
        return os.path.exists(equation+'.exe') and not self.__newer(deps,equation+'.exe')

    def runMakefileWithSetup(self,equation=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runMakefile(equation)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,self.data]
    def runMakefile(self,equation=None):
        self.__resetInternalLogs()
        self.__runMakefile(equation)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,self.data]
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
            if os.environ.has_key("AUTO_DIR"):
                self.options["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTOExceptions.AUTORuntimeError("AUTO_DIR not set as option or as environment variable")
        else:
            os.environ["AUTO_DIR"]=self.options["auto_dir"]


        if self.options["makefile"] is None:
            executable = "make -e %s"%self.options["equation"]
        elif self.options["makefile"] == "$AUTO_DIR/cmds/cmds.make":
            curdir = os.getcwd()
            os.chdir(self.options["dir"])
            equation = self.options["equation"][14:]
            if self.__make(equation):
                line = "Starting %s ...\n"%equation
                if self.options["verbose"] == "yes":
                    self.options["verbose_print"].write(line)
                self.__printLog(line)
                self.__runCommand(os.path.join(".",equation + ".exe"))
                if os.path.exists("fort.2"):
                    os.remove("fort.2")
                if os.path.exists("fort.3"):
                    os.remove("fort.3")
                line = "%s ... done\n"%equation
                if self.options["verbose"] == "yes":
                    self.options["verbose_print"].write(line)
                self.__printLog(line)        
            os.chdir(curdir)
            return
        else:
            executable = "make -f %s -e %s"%(self.options["makefile"],self.options["equation"])
        self.__runExecutable(executable)

    def runExecutableWithSetup(self,executable=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runExecutable(executable)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,self.data]
    def runExecutable(self,executable=None):
        self.__resetInternalLogs()
        self.__runExecutable(executable)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,self.data]
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
        return [self.internalLog,self.internalErr,self.data]
    def runCommandWithSetup(self,command=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runCommand(command)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr,self.data]
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
        # The command runs here.
        # This is done as the object version so I can use the "poll" method
        # later on to see if it is still running.
        if hasattr(os,"times"):
            user_time = os.times()[2]
        tmp_out = []
        command = os.path.expandvars(command)
        if 'subprocess' in sys.modules.keys() or hasattr(popen2,"Popen3"):
            if 'subprocess' in sys.modules.keys():
                args = os.path.expandvars(command).split()
                demo_object = subprocess.Popen(args, stdout=subprocess.PIPE, 
                                               stderr=subprocess.PIPE)
                stdout, stderr = demo_object.stdout, demo_object.stderr
                teststatus = None
            else:
                demo_object = popen2.Popen3(command,1,1)
                demo_object.tochild.close()
                stdout, stderr = demo_object.fromchild, demo_object.childerr
                teststatus = -1
            alarm_demo = self.options["dir"]
            if demo_max_time > 0 and hasattr(signal,"alarm"):
                signal.alarm(demo_max_time)
            status = demo_object.poll()
            while status == teststatus:
                try:
                    line = stdout.readline()
                    if self.options["verbose"] == "yes":
                        self.options["verbose_print"].write(line)
                        self.options["verbose_print"].flush()
                    tmp_out.append(line)
                except:
                    demo_killed = 1
                status = demo_object.poll()
            if status != 0:
                if self.options["verbose"] == "yes":
                    self.options["verbose_print"].write(stderr.read())
                    self.options["verbose_print"].flush()
                raise AUTOExceptions.AUTORuntimeError("Error running AUTO")
        else:
            stdout, stdin, stderr = popen2.popen3(command)
            stdin.close()
        line = stdout.readline()
        # Read the rest of the data from stdout
        while len(line) > 0:
            tmp_out.append(line)
            if self.options["verbose"] == "yes":
                self.options["verbose_print"].write(line)
                self.options["verbose_print"].flush()
            line = stdout.readline()
        self.__printLog(string.join(tmp_out,""))
        self.__printErr(stderr.read())
        stdout.close()
        stderr.close()
        if hasattr(signal,"alarm"):
            signal.alarm(0)
        if hasattr(os,"times"):
            user_time = os.times()[2]
        else:
            user_time = 1.0

        # Check to see if output files were created.
        # If not, set the two output streams to be None.
        if not self.outputFort7 is None:
            self.outputFort7.close()
        if os.path.isfile(self.fort7_path):
            self.outputFort7 = open(self.fort7_path,"r")
        else:
            self.outputFort7 = None
        if not self.outputFort8 is None:
            self.outputFort8.close()
        if os.path.isfile(self.fort8_path):
            self.outputFort8 = open(self.fort8_path,"r")
        else:
            self.outputFort8 = None
        self.data = None
        #if (self.outputFort7 and self.outputFort8 and 
        #    os.path.isfile(self.fort9_path)):
        #    self.data = parseBandS.parseBandS(self.fort7_path,self.fort8_path,
        #                                      self.fort9_path)

def test():
    runner = runAUTO(verbose="yes",clean="yes")
    [log,err,data]=runner.runDemo("ab")
    print log.read()
    runner.config(equation="clean",verbose="no")
    [log,err,data]=runner.runDemo("ab")
    print log.read()
    runner.config(equation="first")
    [log,err,data]=runner.runDemo("ab")
    print log.read()
    

if __name__ == "__main__":
    import getopt,os

    #Parse command line arguements
    opts_list,args=getopt.getopt(sys.argv[1:],"l:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]
    log = None
    err = None
    if opts.has_key("-l"):
        log = open(opts["-l"],"w")
        err = open(opts["-l"]+"errors","w")

    runner = runAUTO()
    if len(args) == 0:
        test()
    if len(args) == 1:    
        runner.runDemo(args[0],log=log,err=err)
    if len(args) == 2:
        runner.runDemo(args[0],part=args[1],log=log,err=err)
        
    


