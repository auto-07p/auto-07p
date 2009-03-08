#! /usr/bin/env python

import sys
import os
import code
import getopt
import re
import AUTOExceptions
import __builtin__

class AUTOInteractiveConsole(code.InteractiveConsole):
    def __init__(self,locals,filename=None):
        locals["execfile"] = self.execfile
        locals["ex"] = self.ex
        locals["auto"] = self.auto
        locals["demofile"] = self.demofile
        locals["dmf"] = self.dmf
        try:
            self.oldhelp = __builtin__.help
        except:
            pass
        locals["help"] = self.help

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

    def demofile(self,name):
        """Execute an AUTO CLUI script, line by line (demo mode).

    Type demofile('xxx.auto') to step through the script xxx.auto, which
    will proceed each time you press Enter.

Aliases: demofile dmf"""
        lines = open(name,"r")
        lines = lines.readlines()
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
                runline = ''

    def dmf(self,name):
        """Execute an AUTO CLUI script, line by line (demo mode).

    Type dmf('xxx.auto') to step through the script xxx.auto, which
    will proceed each time you press Enter.

Aliases: demofile dmf"""
        self.demofile(name)

    def execfile(self,name=None):
        """Execute an AUTO CLUI script.

    Type execfile('xxx.auto') to run the script xxx.auto.

Aliases: auto ex"""
        if name is None:
            automain()
            return
        lines = open(name,"r")
        lines = lines.readlines()
        source = ""
        for line in lines:
            while len(line) > 0 and line[-1] in "\r\n":
                line = line[:-1]
            source = source + self.processShorthand(line) + "\n"
        self.runsource(source,name,"exec")

    def ex(self,name=None):
        """Execute an AUTO CLUI script.

    Type ex('xxx.auto') to run the script xxx.auto.

Aliases: auto ex"""
        self.execfile(name)

    def auto(self,name=None):
        """Execute an AUTO CLUI script.

    Type auto('xxx.auto') to run the script xxx.auto.

Aliases: ex"""
        self.execfile(name)

    def help(self,*args,**kwds):
        if "oldhelp" in self.__dict__.keys():
            if len(args) == 0 and len(kwds) == 0:
                print 'Press ENTER and then type "man" for help about the AUTO Python CLUI.'
            apply(self.oldhelp,args,kwds)
        else:
            apply(self.locals['man'],args,kwds)

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
            iFun = iFun.encode('ascii')
        except AttributeError:
            pass
        except UnicodeEncodeError:
            theRest = iFun+unicode(' ')+theRest
            iFun = unicode('')
            
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
            try:
                obj = ns[oname_head]
            except KeyError:
                continue
            else:
                for part in oname_rest:
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
        """    Given a line of python input check to see if it is
        a AUTO shorthand command, if so, change it to
        its real python equivalent. """
        lst = line.split()
        spaces = re.match(" *",line)

        shortCommands = ["ls","cd","cat"]
        shortUnixCommands = ["clear","less","mkdir","rmdir","cp","mv","rm"]

        if len(lst) > 0:
            pre,cmd,theRest = self.split_user_input(line)
            if lst[0]=="shell":
                return spaces.group()+"shell('" + line[len(spaces.group())+5:].strip() +"')"
            elif lst[0][0]=="!":
                return spaces.group()+"shell('" + line[len(spaces.group())+1:].strip() +"')"
            elif lst[0][0]=="@" or lst[0] in shortUnixCommands:
                return spaces.group()+"shell('" + line[len(spaces.group()):].strip() +"')"
            elif lst[0] in shortCommands:
                if len(lst) == 2:
                    command = spaces.group() + lst[0] + "('%s')"%lst[1]
                else:
                    command = spaces.group() + lst[0] + "()"
                return command
            elif (not(theRest and theRest[0] in '!=()') and
                  (pre == ',' or pre == ';' or pre == '/' or
                  not self.re_exclude_auto.match(theRest))):
                obj = self._ofind(cmd)
                if not obj is None and cmd != 'print' and callable(obj):
                    command = (line[:(len(line) - len(line.lstrip()))]+
                               self.handle_auto(pre,cmd,theRest,obj))
                    return command
            return line
        return line

def test():
    _testFilename("../demos/python/fullTest.auto","test_data/fullTest.log")
    _testFilename("../demos/python/tutorial.auto","test_data/tutorial.log")
    if os.environ.has_key("DISPLAY"):
        _testFilename("../demos/python/plotter.auto","test_data/plotter.log")

def _quicktest():
    _testFilename("../demos/python/fullTest.auto","test_data/fullTest.log")
    
def _testFilename(inputname,outputname):
    import commands
    import AUTOclui
    old_path = os.getcwd()
    log = open("log","w")
    runner = AUTOInteractiveConsole(AUTOclui.exportFunctions(log))
    runner.execfile(inputname)
    log.close()
    os.chdir(old_path)
    status,output = commands.getstatusoutput("diff --ignore-matching-lines='gfortran.*' --ignore-matching-lines='.*Total Time.*' log %s"%outputname)
    if status != 0:
        raise AUTOExceptions.AUTORegressionError("Error: log files differ")
    os.system("rm -f log")

def autoipython():
    # Use IPython in combination with AUTO
    # First import the embeddable shell class
    try:
        import IPython.Shell
    except:
        print "Sorry, ipython is not available on this system."
        return

    import IPython

    # Now create an instance of the embeddable shell. The first argument is a
    # string with options exactly as you would type them if you were starting
    # IPython at the system command line. Any parameters you want to define for
    # configuration can thus be specified here.

    args = ['-pi1','AUTO In [\\#]: ',
            '-pi2','AUTO    .\\D.: ',
            '-po','Out[\#]: ',
            '-noconfirm_exit',
            '-autocall','2']

    over = { "alias" : [], "execute" : ["del help"] }
    for atalias in os.listdir(os.environ["AUTO_DIR"]+"/cmds"):
        if atalias[0]=='@' and atalias[-1]!='~':
            over["alias"].append(atalias+" "+atalias)

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

    ipshell = IPython.Shell.IPShellEmbed(args,
                           banner = '\n'.join(banner),
                           exit_msg = '',
                           rc_override = over)

    # You can then call ipshell() anywhere you need it (with an optional
    # message):
    ipshell()

def automain(name=None):
    import AUTOclui
    sys.ps1="AUTO> "    
    opts_list,args=getopt.getopt(sys.argv[1:],"c:diqtT:L:")
    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]
    demo_mode = 'no'

    use_ipython = 0
    if opts.has_key("-t"):
        test()
        sys.exit()
    elif opts.has_key("-q"):
        _quicktest()
        sys.exit()
    elif opts.has_key("-i"):
        use_ipython = 1
    elif opts.has_key("-T"):
        _testFilename(opts["-T"],opts["-L"])
        sys.exit()
    elif opts.has_key("-d"):
        demo_mode = 'yes'

    try:
        import readline
    except:
        pass

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
                runner.demofile(arg)
    elif use_ipython:
        for name,value in funcs.items():
            globals()[name] = value
        del globals()['cat'], globals()['cd'], globals()['ls']
        autoipython()
    elif opts.has_key("-c"):
        source = ""
        for line in opts["-c"].split("\n"):
            source = source + runner.processShorthand(line) +"\n"
        runner.runsource(source,"-c","exec")
    else:
        runner.interact()

if __name__ == "__main__":
    automain()

