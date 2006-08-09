#! /usr/bin/env python

import sys
import os

# Now, for machines which use python 1.5.1 instead of 1.5.2 we need
# to include a few files from the 1.5.2 distribution
if int(sys.version[0]) <= 1 and int(sys.version[2]) <= 5 and int(sys.version[4]) < 2 :
    sys.path.append(os.environ["AUTO_DIR"]+'/python/python-1.5.1_compatibility')

import code
import string
import AUTOclui
import getopt
import re

class AUTOInteractiveConsole(code.InteractiveConsole):
    def __init__(self,locals,filename=None):
        code.InteractiveConsole.__init__(self,locals)

    def raw_input(self, prompt=None):
        line = raw_input(prompt)
        line = self.processShorthand(line)
        return line

    def demofile(self,name):
        lines = open(name,"r")
        lines = lines.readlines()
        for line in lines:
            # we only wait if the current line is not a comment
            if len(string.strip(line)) >0 and string.strip(line)[0] != "#":
                sys.stdout.write(sys.ps1+line[:-1])
                raw_input()
            else:
                sys.stdout.write(line)
            self.runsource(runner.processShorthand(line[:-1]))

    def execfile(self,name):
        lines = open(name,"r")
        lines = lines.readlines()
        source = ""
        for line in lines:
            source = source + self.processShorthand(line[:-1]) +"\n"
        self.runsource(source,name,"exec")
        

    def processShorthand(self,line):
        """    Given a line of python input check to see if it is
        a AUTO shorthand command, if so, change it to
        its real python equivalent. """
        list = string.split(line)
        spaces = re.match(" *",line)

        shortCommands = ["ls","cd","help","cat","man"]
        shortCommandsNoArgument = ["q","quit"]
            
        if len(list) > 0:
            if list[0]=="shell":
                return spaces.group()+"shell('" + string.strip(line[len(spaces.group())+5:]) +"')"
            elif list[0][0]=="!":
                return spaces.group()+"shell('" + string.strip(line[len(spaces.group())+1:]) +"')"
            elif list[0] in shortCommands:
                if len(list) == 2:
                    command = spaces.group() + list[0] + "('%s')"%list[1]
                else:
                    command = spaces.group() + list[0] + "('')"
                return command
            elif list[0] in shortCommandsNoArgument:
                command = spaces.group() + list[0] + "()"
                return command
            else:
                return line
        return line

def test():
    _testFilename("../demos/python/fullTest.auto","test_data/fullTest.log")
    _testFilename("../demos/python/tutorial.auto","test_data/tutorial.log")
    import os
    if os.environ.has_key("DISPLAY"):
        _testFilename("../demos/python/plotter.auto","test_data/plotter.log")

def _quicktest():
    _testFilename("../demos/python/fullTest.auto","test_data/fullTest.log")
    
def _testFilename(inputname,outputname):
    import commands,os
    old_path = os.getcwd()
    log = open("log","w")
    import AUTOclui
    runner = AUTOInteractiveConsole(AUTOclui.exportFunctions(log))
    runner.execfile(inputname)
    log.close()
    os.chdir(old_path)
    status,output = commands.getstatusoutput("diff --ignore-matching-lines='gcc.*' --ignore-matching-lines='.*Total Time.*' log %s"%outputname)
    if status != 0:
        raise AUTORegressionError("Error: log files differ")
    os.system("rm -f log")
    

if __name__ == "__main__":
    sys.ps1="AUTO> "    
    opts_list,args=getopt.getopt(sys.argv[1:],"dqtT:L:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]
    demo_mode = 'no'

    if opts.has_key("-t"):
        test()
        sys.exit()
    elif opts.has_key("-q"):
        _quicktest()
        sys.exit()
    elif opts.has_key("-T"):
        _testFilename(opts["-T"],opts["-L"])
        sys.exit()
    elif opts.has_key("-d"):
        demo_mode = 'yes'
        
    import AUTOclui
    try:
        import readline
    except:
        pass

    runner = AUTOInteractiveConsole(AUTOclui.exportFunctions())
    __builtins__.execfile = runner.execfile
    __builtins__.demofile = runner.demofile

    if len(args) > 0:
        for arg in args:
            if demo_mode == 'no':
                runner.execfile(arg)
            else:
                sys.stdout.write("Python %s on %s\n%s\n(%s)\n" %
                           (sys.version, sys.platform, sys.copyright,
                            runner.__class__.__name__))
                runner.demofile(arg)
    else:
        runner.interact()
 



    
