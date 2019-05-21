#! /usr/bin/env python
import AUTOExceptions

modules = ["parseB", "parseS", "parseBandS", "parseC", "parseH",
           "AUTOclui", "interactiveBindings", "AUTOCommands",
           "parseD", "bifDiag", "runDemo", "runAUTO"]

regressions = []
for module in modules:
    print("-----------------------")
    print("Testing Module:  %s\n"%module)
    mod = __import__(module)
    
    try:
        mod.test()
    except AUTOExceptions.AUTORegressionError:
        regressions.append(module)
if regressions:
    print("Found regressions in: %s"%", ".join(regressions))
else:
    print("All regression tests were successful.")
