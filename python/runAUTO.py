#! /usr/bin/env python
import getopt,sys,os,popen2,string
import signal, os, fcntl, time
import cStringIO
import re
import types
from AUTOExceptions import *

# A few global variables for the signal handler
alarm_demo=""
demo_killed=0
demo_max_time=-1

class runAUTO:
    def __init__(self,cnf={},**kw):
        # Set the signal handler
        signal.signal(signal.SIGALRM, self.__handler)
        self.internalLog = None
        self.internalErr = None
        
        self.options={}
        self.options["log"] = None
        self.options["err"] = None
        self.options["auto_dir"] = None
        self.options["time_flag"] = "-p"
        self.options["equation"] = "all"
        self.options["verbose"] = "no"
        self.options["verbose_print"] = sys.stdout
        self.options["clean"] = "no"
        self.options["dir"] = "."
        self.options["executable"] = None
        self.options["command"] = None
        self.options["makefile"] = None
        self.options["constants"] = None
        self.options["solution"] = None
        self.options["homcont"] = None
        self.options["copy setup"] = None

        self.outputFort7 = None
        self.outputFort8 = None

        self.__parseOptions(kw)
        self.__parseOptions(cnf)
            
    def __parseOptions(self,dict):
        for key in dict.keys():
            if self.options.has_key(key):
                self.options[key] = dict[key]
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
            self.fort7_path = os.path.join(self.options["dir"],"p.%s"%(value[-1][1]))
            self.fort8_path = os.path.join(self.options["dir"],"q.%s"%(value[-1][1]))
        else:
            # Otherwise we assume it is fort.7 and fort.8
            self.fort7_path = os.path.join(self.options["dir"],"fort.7")
            self.fort8_path = os.path.join(self.options["dir"],"fort.8")

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
        cout,cin = popen2.popen2("ps ww | grep %s.exe | grep -v grep"%(d,))
        pids = cout.read()
        pids = string.split(pids,"\n")
        pids = pids[:-1]
        for pid in pids:
            self.options["verbose_print"]('Killing: '+str(pid))
            pid = string.split(pid)
            pid = string.atoi(pid[0])
            command = "/bin/kill -KILL %d"%(pid,)
            self.options["verbose_print"](command)
            os.system(command)
        # Restart the alarm to make sure everything gets killed
        self.options["verbose_print"].flush()
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
        return [self.internalLog,self.internalErr]

    def __runDemo(self,d):
        """     This function compiles the demo, then calls the runMakefile
        method, and then cleans up"""
        global demo_killed

        if self.options["auto_dir"] is None:
            if os.environ.has_key("AUTO_DIR"):
                self.options["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTORuntimeError("AUTO_DIR not set as option or as environment variable")
        else:
            os.environ["AUTO_DIR"]=self.options["auto_dir"]

        
        self.options["dir"] = os.path.join(self.options["auto_dir"],"demos",d)

        self.__printErr("===%s start===\n"%(d,))
        stdout,stdin,stderr = popen2.popen3("cd %s; rm -f %s.exe;make -e %s.exe;"%
                                            (self.options["dir"],d,d))

        self.__printLog(stdout.read())
        self.__printErr(stderr.read())

        self.__runMakefile()

        if self.options["clean"] == "yes":
            stdout,stdin,stderr = popen2.popen3("cd %s; make -e clean; cd ..;"%
                                                (self.options["dir"],))
        self.__printLog(stdout.read())
        self.__printErr(stderr.read())

        if demo_killed != 0:
            self.__printLog("***Demo was killed because it took too long***\n")
            self.__printErr("***Demo was killed because it took too long***\n")

        self.__printErr("===%s end===\n"%(d,))

    def config(self,cnf={},**kw):
        """     Change the options for this runner object"""
        self.__parseOptions(kw)
        self.__parseOptions(cnf)

    def __setup(self):
        """     This function sets up self.options["dir"] by creating a
        fort.2, fort.3 amd fort.12 files.  The "constants", "solution",
        and "homcont" options 
        can be anything with a writeFilename method.  NOTE:  The
        values set here will often be overridden by
        runMakefile (thought almost never by runExecutable
        or runCommand)"""
        os.system("rm -f fort.2")
        if (self.options["constants"] is None):
            raise AUTORuntimeError("tried to explicitly setup but parameter not set as an option")
        else:
            self.options["constants"].writeFilename("fort.2")

        os.system("rm -f fort.3")
        if (self.options["solution"] is None):
            os.system("touch fort.3")
        else:
            self.options["solution"].writeFilename("fort.3")

        os.system("rm -f fort.12")
        if (self.options["homcont"] is None):
            os.system("touch fort.12")
        else:
            self.options["homcont"].writeFilename("fort.12")

    def runMakefileWithSetup(self,equation=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runMakefile(equation)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]
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
                raise AUTORuntimeError("No equation set")
        else:
            self.options["equation"] = equation

        if self.options["auto_dir"] is None:
            if os.environ.has_key("AUTO_DIR"):
                self.options["auto_dir"]=os.environ["AUTO_DIR"]
            else:
                raise AUTORuntimeError("AUTO_DIR not set as option or as environment variable")
        else:
            os.environ["AUTO_DIR"]=self.options["auto_dir"]


        if self.options["makefile"] is None:
            executable = "make -e %s;"%self.options["equation"]
        else:
            executable = "make -f %s -e %s;"%(self.options["makefile"],self.options["equation"])
        self.__runExecutable(executable)

    def runExecutableWithSetup(self,executable=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runExecutable(executable)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]
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
            if not(self.opions["executable"] is None):
                executable = self.opions["executable"]
            else:
                raise AUTORuntimeError("No executable set")
        else:
            self.options["executable"] = executable

        command = "(cd %s;/usr/bin/time %s %s)"%(self.options["dir"],self.options["time_flag"],executable)
        self.__runCommand(command)

    def runCommand(self,command=None):
        self.__resetInternalLogs()
        self.__runCommand(command)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]
    def runCommandWithSetup(self,command=None):
        self.__resetInternalLogs()
        self.__setup()
        self.__runCommand(command)
        self.__rewindInternalLogs()
        return [self.internalLog,self.internalErr]
    def __runCommand(self,command=None):
        """     This is the most generic interface.  It just takes a string as a command
        and trys to run it. """
        global demo_killed,alarm_demo,demo_max_time
        if command is None:
            if not(self.options["command"] is None):
                command = self.options["command"]
            else:
                raise AUTORuntimeError("No command set")
        else:
            self.options["command"] = command
        # The command runs here.
        # This is done as the object version so I can use the "poll" method
        # later on to see if it is still running.
        demo_object = popen2.Popen3(command,1,1)

        stdout = demo_object.fromchild
        stdin  = demo_object.tochild
        stderr = demo_object.childerr
        alarm_demo = self.options["dir"]
        if(demo_max_time > 0):
            signal.alarm(demo_max_time)
        tmp_out = cStringIO.StringIO()
        while (demo_object.poll() == -1):
            try:
                line = stdout.readline()
                if self.options["verbose"] == "yes":
                    self.options["verbose_print"].write(line)
                    self.options["verbose_print"].flush()
                tmp_out.write(line)
                time.sleep(0.1)
            except:
                demo_killed = 1
        line = stdout.readline()
        # Read the rest of the data from stdout
        while len(line) > 0:
            tmp_out.write(line)
            if self.options["verbose"] == "yes":
                self.options["verbose_print"].write(line)
                self.options["verbose_print"].flush()
            line = stdout.readline()
        # Rewind the tmp_out so I can read from it now
        tmp_out.seek(0,0)
        self.__printLog(tmp_out.read())
        for line in stderr.readlines():
            tokens = string.split(line)
            if len(tokens) > 0:
                if tokens[0] == "real" and len(tokens)==2:
                    pass
                elif tokens[0] == "user" and len(tokens)==2:
                    #user_time = string.atof(tokens[1])
                    user_time=1.0
                    pass
                elif tokens[0] == "sys" and len(tokens)==2:
                    pass
                else:
                    self.__printErr(line)
        signal.alarm(0)

        # Check to see if output files were created.
        # If not, set the two output streams to be None.
        if os.path.isfile(self.fort7_path):
            self.outputFort7 = open(self.fort7_path,"r")
        else:
            self.outputFort7 = None
        if os.path.isfile(self.fort8_path):
            self.outputFort8 = open(self.fort8_path,"r")
        else:
            self.outputFort8 = None
        

def test():
    runner = runAUTO(verbose="yes",clean="yes")
    [log,err]=runner.runDemo("ab")
    print log.read()
    runner.config(equation="clean",verbose="no")
    [log,err]=runner.runDemo("ab")
    print log.read()
    runner.config(equation="first")
    [log,err]=runner.runDemo("ab")
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
        
    


