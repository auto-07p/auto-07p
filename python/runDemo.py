#! /usr/bin/env python
import runAUTO
import AUTOExceptions

def runDemo(demo,**kw):
    runner = runAUTO.runAUTO(**kw)
    runner.runDemo(demo)

def test():
    import os
    import commands
    log=open("log","w")
    err=open("err","w")
    runDemo("wav",log=log,err=err,
            auto_dir="%s"%(os.environ["AUTO_DIR"],),
            verbose="yes",
            clean="yes")
    log.close()
    err.close()
    
    #commands.getstatusoutput
    status,output=commands.getstatusoutput("diff --ignore-matching-lines='.*Total Time.*' log test_data/runDemo.log")
    if status != 0:
        raise AUTOExceptions.AUTORegressionError("Log files differ")

    status,output=commands.getstatusoutput("diff --ignore-matching-lines='.*Total Time.*' err test_data/runDemo.err")
    if status != 0:
        raise AUTOExceptions.AUTORegressionError("Error files differ")
    
    os.remove("log")
    os.remove("err")

if __name__ == "__main__":
    import sys
    if len(sys.argv) == 1:
        test()
    if len(sys.argv) == 2:
        runDemo(sys.argv[1])
    if len(sys.argv) == 3:
        runDemo(sys.argv[1],part=sys.argv[2])
        

