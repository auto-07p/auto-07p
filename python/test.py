#! /usr/bin/env python

modules = ["parseB", "parseS", "parseBandS", "parseC", "parseH",
           "AUTOclui", "interactiveBindings", "AUTOCommands",
           "parseD", "bifDiag"] #runDemo,runAUTO

for module in modules:
    print("-----------------------")
    print("Testing Module:  %s\n"%module)
    exec "import %s"%(module,)
    exec "%s.test()"%(module,)
    

