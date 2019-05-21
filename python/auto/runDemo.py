#! /usr/bin/env python
import runAUTO
import AUTOExceptions

def runDemo(demo,**kw):
    runner = runAUTO.runAUTO(**kw)
    runner.runDemo(demo)
    runner.config(log=None, err=None)

def test():
    import os
    import sys
    import AUTOutil

    log=open("log","w")
    err=open("err","w")
    stdout=sys.stdout
    class teelog(object):
        def write(self,text):
            log.write(text)
            stdout.write(text)
        def flush(self):
            log.flush()
            stdout.flush()
    runDemo("ab",log=teelog(),err=err,makefile="",
            demos_dir=os.path.join(os.environ["AUTO_DIR"],"python"),
            clean="yes")
    log.close()
    err.close()
    
    diffopts = ["diff","-b","--ignore-matching-lines='.*Total Time.*'",
                "--ignore-matching-lines='.*ab\.o.*'",
                "--ignore-matching-lines='   [0-9]  .*'"]
    status,output=AUTOutil.getstatusoutput(
        diffopts+["log","test_data/runDemo.log"])
    if status != 0:
        raise AUTOExceptions.AUTORegressionError("Log files differ")

    status,output=AUTOutil.getstatusoutput(
        diffopts+["err","test_data/runDemo.err"])
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
        

