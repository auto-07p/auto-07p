#! /usr/bin/env python

modules = ["parseB", "parseS", "parseBandS", "parseC", "parseH",
           "AUTOCommands",
           "runAUTO", "runDemo", "AUTOclui","interactiveBindings",
           "parseD"]

for module in modules:
    print "-----------------------"
    print "Testing Module: ",module
    print
    exec "import %s"%(module,)
    exec "%s.test()"%(module,)
    

