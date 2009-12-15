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
    path, auto_dir = os.environ["PATH"], os.environ["AUTO_DIR"]
    os.environ["PATH"] += os.pathsep+"."
    runDemo("ab",log=log,err=err,
            auto_dir=os.path.join(os.environ["AUTO_DIR"],"..","97"),
            verbose="yes",
            clean="yes")
    log.close()
    err.close()
    os.environ["PATH"], os.environ["AUTO_DIR"] = path, auto_dir
    
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
        

