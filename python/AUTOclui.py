#! /usr/bin/env python
import AUTOutil
import sys
import os
import AUTOCommands
import interactiveBindings
try:
    import __builtin__
    from new import function
except ImportError:
    import builtins as __builtin__ # Python 3
    from types import FunctionType

class AUTOSimpleFunctions:
    def __init__(self,outputRecorder=None):
        # Initialize the output recorder (if any)
        if outputRecorder:
            # This is here so the log gets kept
            class WriteLog(object):
                def write(self,s):
                    outputRecorder.write(s)
                    sys.stdout.write(s)
                def flush(self):
                    outputRecorder.flush()
                    sys.stdout.flush()

            writelog = WriteLog()
            AUTOCommands.info = lambda s: None
            AUTOCommands.load(verbose_print=writelog)
            AUTOCommands.info = writelog.write

        # Read in the aliases.
        self._aliases = None

        parser = AUTOutil.getAUTORC()
        if parser.has_section("AUTO_command_aliases"):
            self._aliases = {}
            for option in parser.options("AUTO_command_aliases"):
                cmd = parser.get("AUTO_command_aliases",option)
                if cmd not in self._aliases:
                    self._aliases[cmd] = []
                self._aliases[cmd].append(option)

        self._addCommands([AUTOCommands])

        # Now I resolve the aliases
        for key, aliases in self._aliases.items():
            for alias in aliases:
                f = self._copyfunction(getattr(AUTOCommands,key).fun, alias)
                setattr(AUTOSimpleFunctions, alias, staticmethod(f))
                doc = getattr(AUTOCommands,key).__doc__
                doc = self._adjustdoc(doc, alias, key)
                f.__doc__ = doc

    def _copyfunction(self, f, key):
        if 'FunctionType' in globals():
            return FunctionType(f.__code__, f.__globals__, key,
                                f.__defaults__, f.__closure__)
        else:
            return function(f.func_code, f.func_globals, key,
                            f.func_defaults or ())

    def _adjustdoc(self, doc, commandname, truecommandname = None):
        # If we were created with the nonempty string return a formatted
        # reference for the given command as the string
        if doc == None:
            return doc
        # Get rid of the LaTeX stuff from the string that gets returned.
        doc = doc.replace("\\begin{verbatim}","")
        doc = doc.replace("\\end{verbatim}","")
        doc = doc + "\n"

        doc = doc.replace("FUNC", commandname)
        # This means help was asked for an alias
        if not truecommandname is None:
            commandname = truecommandname
            doc = doc + "Command name: "+commandname+"\n"
        doc = doc + "Aliases: "
        if commandname in self._aliases:
            doc = doc + " ".join(self._aliases[commandname])
        return doc

    def _addCommands(self,moduleList):
        addaliases = self._aliases is None
        if addaliases:
            self._aliases = {}
        for module in [AUTOCommands]:
            # Now we copy the commands from the module
            for key in module.__dict__:
                cmd = getattr(module,key)
                # Check to see if it is a command
                if hasattr(cmd,"fun"):
                    if addaliases and cmd.alias is not None:
                        self._aliases[key] = [cmd.fun.__name__] + cmd.alias
                    f = self._copyfunction(cmd.fun, key)
                    setattr(AUTOSimpleFunctions, key, staticmethod(f))
                    doc = cmd.__doc__
                    doc = self._adjustdoc(doc, key)
                    f.__doc__ = doc

# Export the functions inside AUTOSimpleFunctions in a dictionary
# This also allows the setting of the log
def exportFunctions(log=None):
    AUTOSimpleFunctionsInstance = AUTOSimpleFunctions(log)
    dict = {}
    for name in AUTOSimpleFunctions.__dict__:
        if name[0] != '_':
            dict[name] = getattr(AUTOSimpleFunctionsInstance, name)
    return dict

# Export the functions inside AUTOSimpleFunctions in this modules namespace
# This is to make "from AUTOclui import *" work
# ALMOST like the AUTOInteractiveConsole.  Things that
# don't work are help, shell, !, ls, cd, and any changes
# to the aliases
if "AUTO_DIR" not in os.environ:
    absfile = os.path.abspath(__file__)
    autodir = os.path.dirname(os.path.dirname(absfile))
    os.environ["AUTO_DIR"] = autodir
funcs = exportFunctions()
runner = interactiveBindings.AUTOInteractiveConsole(funcs)
for name,value in funcs.items():
    if name not in __builtin__.__dict__:
        globals()[name] = value

def test():
    interactiveBindings._testFilename("../demos/python/fullTest.auto","test_data/fullTest.log")

if __name__ == "__main__":
    test()







