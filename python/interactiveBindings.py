#! /usr/bin/env python

import sys
import os
import code
import getopt
import re
import AUTOExceptions
import keyword
try:
    import __builtin__
except ImportError:
    import builtins as __builtin__ # Python 3

class AUTOInteractive(object):
    def __init__(self,locals,filename=None):
        locals["execfile"] = self.execfile
        locals["ex"] = self.ex
        locals["auto"] = self.auto
        locals["demofile"] = self.demofile
        locals["dmf"] = self.dmf
        self.execfilename = filename
        self.stopdemo = False
        self.oldhelp = __builtin__.help
        locals["help"] = self.help
        self.atcommands = [cmd 
                           for cmd in os.listdir(os.environ["AUTO_DIR"]+"/cmds")
                           if cmd[0]=='@' and cmd[-1]!='~']

    def showtraceback(self):
        try:
            raise
        except AUTOExceptions.AUTORuntimeError:
            import traceback
            e = sys.exc_info()[1]
            l = traceback.extract_tb(sys.exc_info()[2])
            found = False
            for entry in l:
                if entry[0] == self.execfilename:
                    self.write("%s, line %s:\n"%(entry[0],entry[1]))
                    found = True
            if found:
                self.write("\n")
            self.write("AUTO Runtime Error: %s\n"%e)
            self.stopdemo = True
        except:
            self.parentclass.showtraceback(self)

    def demofile(self,name):
        """Execute an AUTO CLUI script, line by line (demo mode).

    Type demofile('xxx.auto') to step through the script xxx.auto, which
    will proceed each time you press Enter.

Aliases: demofile dmf"""
        import AUTOclui
        AUTOInteractiveConsole(AUTOclui.exportFunctions())._demofile(name)

    def _demofile(self,name):
        oldname = self.execfilename
        self.execfilename = name
        try:
            f = open(name,"r")
        except IOError:
            raise AUTOExceptions.AUTORuntimeError(sys.exc_info()[1])
        lines = f.readlines()
        f.close()
        runline = ''
        for line in lines:
            while len(line) > 0 and line[-1] in "\r\n":
                line = line[:-1]
            # we only wait if the current line is not a comment
            if len(line.strip()) >0 and line.strip()[0] != "#":
                sys.stdout.write(sys.ps1+line)
                raw_input()
            else:
                sys.stdout.write(line+"\n")
            runline = runline + self.processShorthand(line) + "\n"
            if not self.runsource(runline):
                if self.stopdemo:
                    self.stopdemo = False
                    self.execfilename = oldname
                    raise AUTOExceptions.AUTORuntimeError('Demo interrupted')
                runline = ''
        self.execfilename = oldname

    def dmf(self,name):
        """Execute an AUTO CLUI script, line by line (demo mode).

    Type dmf('xxx.auto') to step through the script xxx.auto, which
    will proceed each time you press Enter.

Aliases: demofile dmf"""
        self.demofile(name)

    def execfile(self,name=None):
        """Execute an AUTO CLUI script.

    Type execfile('xxx.auto') to run the script xxx.auto, keeping
    all local state."""
        if name is None:
            automain()
            return
        oldname = self.execfilename
        try:
            lines = open(name,"r")
        except IOError:
            if oldname is None:
                # to give a simple message for "auto notexists.auto".
                try:
                    raise AUTOExceptions.AUTORuntimeError(sys.exc_info()[1])
                except:
                    self.showtraceback()
                    return
            else:    
                raise AUTOExceptions.AUTORuntimeError(sys.exc_info()[1])
        self.execfilename = name
        source = ""
        for line in lines:
            while len(line) > 0 and line[-1] in "\r\n":
                line = line[:-1]
            source = source + self.processShorthand(line) + "\n"
        lines.close()
        self.runsource(source,name,"exec")
        self.execfilename = oldname

    def ex(self,name=None):
        """Execute an AUTO CLUI script.

    Type ex('xxx.auto') to run the script xxx.auto.

Aliases: auto ex"""
        self.auto(name)

    def auto(self,name=None):
        """Execute an AUTO CLUI script.

    Type auto('xxx.auto') to run the script xxx.auto.

Aliases: auto ex"""
        import AUTOclui
        if name is not None:
            AUTOInteractiveConsole(AUTOclui.exportFunctions()).execfile(name)
        else:
            self.execfile()

    def help(self,*args,**kwds):
        if len(args) == 0 and len(kwds) == 0:
            print('Press ENTER and then type "man" for help about the AUTO Python CLUI.')
        self.oldhelp(*args,**kwds)

    def processShorthand(self,line):
        """    Given a line of python input check to see if it is
        a AUTO shorthand command, if so, change it to
        its real python equivalent. """
        lst = line.split()

        shortCommands = ["ls","cd","cat"]
        shortUnixCommands = ["clear","less","mkdir","rmdir","rm"]
        # these work both as functions and for the shell
        shortDuplCommands = ["cp","mv"]

        if len(lst) == 0:
            return line

        llen = len(line)-len(line.lstrip())
        lspaces = ' '*llen
        command = None
        if lst[0][0]=="!":
            command = "shell('" + line[llen+1:].strip() +"')"
        elif len(lst) == 1 or lst[1][0] != '(':
            if lst[0]=="shell":
                command = "shell('"+line[llen+5:].strip()+"')"
            elif lst[0] in self.atcommands:
                command = ("shell('" + os.path.join(
                        os.environ["AUTO_DIR"],"cmds",line.strip()) + "')")
            elif (lst[0] in shortUnixCommands or
                  (lst[0] in shortDuplCommands and
                   (len(lst) != 2 or (len(lst) == 2 and lst[1][0] == '-')))):
                command = "shell('" + line.strip() + "')"
            elif lst[0] in shortCommands:
                if len(lst) == 2:
                    command = "%s('%s')"%tuple(lst)
                else:
                    command = "%s()"%lst[0]
        if command is not None:
            return lspaces+command
        return line


class AUTOInteractiveConsole(AUTOInteractive,code.InteractiveConsole):
    def __init__(self,locals,filename=None):
        self.parentclass = code.InteractiveConsole
        AUTOInteractive.__init__(self,locals,filename)
        code.InteractiveConsole.__init__(self,locals)
        self.line_split = re.compile(r'^(\s*[,;/]?\s*)'
                                     r'([\?\w\.]+\w*\s*)'
                                     r'(\(?.*$)')
        self.re_exclude_auto = re.compile(r'^[<>,&^\|\*/\+-]'
                                          '|^is |^not |^in |^and |^or ')

    def raw_input(self, prompt=None):
        line = raw_input(prompt)
        line = self.processShorthand(line)
        return line

    def split_user_input(self,line):
        """Split user input into pre-char, function part and rest."""
        #shamelessly stolen from IPython
        lsplit = self.line_split.match(line)
        if lsplit is None:  # no regexp match returns None
            #print "match failed for line '%s'" % line  # dbg
            try:
                iFun,theRest = line.split(None,1)
            except ValueError:
                #print "split failed for line '%s'" % line  # dbg
                iFun,theRest = line,''
            pre = re.match('^(\s*)(.*)',line).groups()[0]
        else:
            pre,iFun,theRest = lsplit.groups()

        # iFun has to be a valid python identifier, so it better be only pure
        #ascii, no unicode:
        try:
            iFuntmp = iFun.encode('ascii')
            if isinstance(iFuntmp, str):
                iFun = iFuntmp
        except UnicodeEncodeError:
            try:
                theRest = iFun+unicode(' ')+theRest
                iFun = unicode('')
            except NameError:
                theRest = iFun+' '+theRest
                iFun = ''
            
        return pre,iFun.strip(),theRest

    def _ofind(self, oname):
        """Find an object in the available namespaces.

        self._ofind(oname) -> obj
        """
        #shamelessly stolen from IPython        
        oname = oname.strip()
        # initialize results to 'null'
        found = 0; obj = None;

        # Look for the given name by splitting it in parts.  If the head is
        # found, then we look for all the remaining parts as members, and only
        # declare success if we can find them all.
        oname_parts = oname.split('.')
        oname_head, oname_rest = oname_parts[0],oname_parts[1:]
        # Namespaces to search in:
        for ns in [ self.locals, __builtin__.__dict__ ]:
            if keyword.iskeyword(oname_head):
                break
            try:
                obj = ns[oname_head]
            except KeyError:
                continue
            else:
                for part in oname_rest:
                    if keyword.iskeyword(part):
                        break
                    try:
                        obj = getattr(obj,part)
                    except:
                        # Blanket except b/c some badly implemented objects
                        # allow __getattr__ to raise exceptions other than
                        # AttributeError, which then crashes IPython.
                        break
                else:
                    # If we finish the for loop (no break), we got all members
                    found = 1
                    break  # namespace loop

        # Last try: special-case some literals like '', [], {}, etc:
        if not found and oname_head in ["''",'""','[]','{}','()']:
            obj = eval(oname_head)
            
        return obj
    

    def handle_auto(self, pre, iFun, theRest, obj):
        """Handle lines which can be auto-executed, quoting if requested."""
        #shamelessly stolen from IPython, too
        if pre == ',':
            # Auto-quote splitting on whitespace
            newcmd = '%s("%s")' % (iFun,'", "'.join(theRest.split()))
        elif pre == ';':
            # Auto-quote whole string
            newcmd = '%s("%s")' % (iFun,theRest)
        elif pre == '/':
            newcmd = '%s(%s)' % (iFun,",".join(theRest.split()))
        else:
            # Auto-paren.
            if len(theRest) > 0 and theRest[0] == '[':
                if hasattr(obj,'__getitem__'):
                    # Don't autocall in this case: item access for an object
                    # which is BOTH callable and implements __getitem__.
                    newcmd = '%s %s' % (iFun,theRest)
                    auto_rewrite = False
                else:
                    # if the object doesn't support [] access, go ahead and
                    # autocall
                    newcmd = '%s(%s)' % (iFun.rstrip(),theRest)
            elif len(theRest) > 0 and theRest[-1] == ';':
                newcmd = '%s(%s);' % (iFun.rstrip(),theRest[:-1])
            else:
                newcmd = '%s(%s)' % (iFun.rstrip(), theRest)

        return newcmd

    def processShorthand(self,line):
        """Do some IPython-style transformations before passing to Python."""
        # First the short shell commands
        line = AUTOInteractive.processShorthand(self,line)

        pre,cmd,theRest = self.split_user_input(line)
        if (not(theRest and theRest[0] in '!=()') and
            (pre == ',' or pre == ';' or pre == '/' or
             not self.re_exclude_auto.match(theRest))):
            obj = self._ofind(cmd)
            if obj is not None and hasattr(obj, '__call__'):
                command = (line[:(len(line) - len(line.lstrip()))]+
                           self.handle_auto(pre,cmd,theRest,obj))
                return command
        return line

    def close(self):
        self.locals['_runner'].config(log=None, err=None)

def adjust_sys_path():
    # if the path starts with '' (Python2.2 with current directory)
    # then add the AUTO python directory to sys.path
    if sys.path[0] == '':
        sys.path.insert(1,os.path.join(os.environ["AUTO_DIR"],"python"))
    else:
        #add current directory to sys.path
        sys.path.insert(0, "")

def test():
    adjust_sys_path()
    _testFilename("../demos/python/fullTest.auto","test_data/fullTest.log")
    _testFilename("../demos/python/tutorial.auto","test_data/tutorial.log")
    try:
        import Tkinter
        try:
            Tkinter.Tk().destroy()
            _testFilename("../demos/python/plotter.auto","test_data/plotter.log")
        except Tkinter.TclError:
            pass
    except ImportError:
        pass

def _quicktest():
    _testFilename("../demos/python/fullTest.auto","test_data/fullTest.log")
    
def _testFilename(inputname,outputname):
    import AUTOclui, AUTOutil, runAUTO
    old_path = os.getcwd()
    log = open("log","w")

    os.environ["LANG"]="C"
    console = AUTOInteractiveConsole(AUTOclui.exportFunctions(log=log))
    console.execfile(inputname)
    console.close()
    log.close()
    os.chdir(old_path)
    cmd = ["diff","-b",
           "--ignore-matching-lines='.*ab\.o.*'",
           "--ignore-matching-lines='.*cir\.o.*'",
           "--ignore-matching-lines='.*wav\.o.*'",
           "--ignore-matching-lines='   [0-9][0-9 ]  .*'",
           "--ignore-matching-lines='Finished running:.*'",
           "--ignore-matching-lines='.*Location of special point.*'",
           "--ignore-matching-lines='[uU]sing .*'",
           "--ignore-matching-lines='.*Total Time.*'","log",outputname]
    status, output = AUTOutil.getstatusoutput(cmd)
    if status != 0:
        raise AUTOExceptions.AUTORegressionError(
            "Error: log files differ:\n%s"%output)
    os.system("rm -f log")

def autoipython(funcs):
    # Use IPython in combination with AUTO
    # First import the shell class
    try:
        import IPython.Shell
    except:
        print("Sorry, ipython is not available on this system.")
        return

    import IPython
    from IPython.iplib import InteractiveShell

    # Now create an instance of the embeddable shell. The first argument is a
    # string with options exactly as you would type them if you were starting
    # IPython at the system command line. Any parameters you want to define for
    # configuration can thus be specified here.

    args = ['-pi1','AUTO In [\\#]: ',
            '-pi2','AUTO    .\\D.: ',
            '-po','Out[\#]: ',
            '-noconfirm_exit',
            '-autocall','2']

    banner = ['Python %s\n'
              'Type "copyright", "credits" or "license" '
              'for more information.\n'
              % (sys.version.split('\n')[0],),
              "IPython %s -- An enhanced Interactive Python."
              % (IPython.Release.version,),
              """?       -> Introduction to IPython's features.
%magic  -> Information about IPython's 'magic' % functions.
help    -> Python's own help system.
object? -> Details about 'object'. ?object also works, ?? prints more.

Welcome to the AUTO IPython CLUI
man     -> List of AUTO CLUI commands"""]

    class AUTOInteractiveShell(AUTOInteractive,InteractiveShell):
        def __init__(self,name,**kw):
            self.parentclass = InteractiveShell
            AUTOInteractive.__init__(self, kw["user_ns"], None)
            InteractiveShell.__init__(self,name,**kw)

        def prefilter(self,line,continue_prompt):
            if not continue_prompt:
                line = self.processShorthand(line)
            line = InteractiveShell.prefilter(self,line,continue_prompt)
            return line

    ipshell = IPython.Shell.IPShell(args,
                           user_ns = funcs, user_global_ns = funcs,
                           shell_class = AUTOInteractiveShell)
    ipshell.IP.user_ns['help'] = ipshell.IP.help
    ipshell.mainloop(banner = '\n'.join(banner))

def automain(name=None):
    import AUTOclui
    sys.ps1="AUTO> "    
    opts_list,args=getopt.getopt(sys.argv[1:],"c:diqtT:L:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]
    demo_mode = 'no'

    use_ipython = 0
    if "-t" in opts:
        test()
        sys.exit()
    elif "-q" in opts:
        _quicktest()
        sys.exit()
    elif "-i" in opts:
        use_ipython = 1
    elif "-T" in opts:
        adjust_sys_path()
        _testFilename(opts["-T"],opts["-L"])
        sys.exit()
    elif "-d" in opts:
        demo_mode = 'yes'

    try:
        import readline
    except:
        pass

    adjust_sys_path()
    funcs = AUTOclui.exportFunctions()
    runner = AUTOInteractiveConsole(funcs)

    if len(args) > 0:
        for arg in args:
            if demo_mode == 'no':
                runner.execfile(arg)
            else:
                sys.stdout.write("Python %s on %s\n%s\n(%s)\n" %
                           (sys.version, sys.platform, sys.copyright,
                            runner.__class__.__name__))
                runner._demofile(arg)
    elif use_ipython:
        autoipython(funcs)
    elif "-c" in opts:
        source = ""
        for line in opts["-c"].split("\n"):
            source = source + runner.processShorthand(line) +"\n"
        runner.runsource(source,"-c","exec")
    else:
        runner.interact()

if __name__ == "__main__":
    automain()

