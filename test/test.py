#!/usr/bin/env python

#This is Randy Paffenroths attempt to make a better regression
#tester for AUTO.
import getopt
import os
import sys, shutil
import time
import platform
from interactiveBindings import AUTOInteractiveConsole
import AUTOclui
import parse_test
import runDemo

# selection
DIR1=["pp2","exp","int","dd2","opt","lin","bvp","pp3","wav","plp"]

# HomCont selection
DIR2=["san","mtn","kpr","cir","she","rev"] 

# All AUTO97 demos
DIR3=DIR1+DIR2+["ab","abc","brc","brf","bru","chu","enz","ext","ezp","frc",
                "fsh","ivp","kar","lor","lrz","nag","non","obv","ops","pd1",
                "pd2","pen","phs","ppp","pvl","spb","stw","tim","tor"]
DIR3.sort()

# Plus new demos
DIR4=DIR3+["abcb","apbp","c2c","cusp","ffn","fhh","fhn","fnb","fnc","hen",
           "kdv","lcbp","log","man","nep","p2c","pcl","ph1","pla","python",
           "python/n-body","r3b","sib",
           "snh","sspg","tfc","um2","um3","vhb"]
DIR4.sort()

# dummy stdin to redirect wait()s
class nostdin(object):
    def read(self):
        return "\n"
    def readline(self):
        return "\n"

def test(demos, versions=None, log_file=None, parse=True):
    sys.stdin = nostdin()
    if demos == "selec":
        demos = DIR1
    if demos == "hom":
        demos = DIR2
    if demos == "all97":
        demos = DIR3
    if demos == "all":
        demos = DIR4

    if log_file is None:
        hostname = platform.node()
        if '.' in hostname:
            hostname = hostname.split('.')[0]
        log_file = hostname+'_log'

    if versions is None:
        versions = ['07p']

    log_files = []
    log = {}
    err = {}
    runner = {}
    for version in versions:
        if len(log_file)!=0:
            log_files.append(log_file+version)
            log[version] = open(log_file+version,"w")
            err[version] = open(log_file+version+"errors","w")
        else:
            log[version] = None
            err[version] = None
        runner[version] = AUTOInteractiveConsole(AUTOclui.exportFunctions(
                log[version],err[version]))

    autofiles = []
    for d in demos:
        print("Doing "+d)
        for version in versions:
            print("Version "+version)
            auto_dir=os.path.join(os.environ["AUTO_DIR"],"..",version)
            demo_dir=os.path.join(auto_dir,"demos",d)
            autofiles = []
            if os.path.exists(os.path.join(demo_dir,"%s.auto"%d)):
                autofiles = ["%s.auto"%d]
            else:
                autofiles = [dirname for dirname in os.listdir(demo_dir) if
                             dirname[-5:]=='.auto']
                autofiles.sort()
            if len(autofiles) > 0:
                oldcwd = os.getcwd()
                tmpdir = os.path.join(oldcwd,'tmp')
                try:
                    shutil.rmtree(tmpdir)
                except OSError:
                    pass
                try:
                    os.mkdir(tmpdir)
                except OSError:
                    pass
                os.chdir(tmpdir)
                AUTOclui.copydemo(d)
                log[version].write("Demo %s is started\n"%d)
                for autofile in autofiles:
                    os.chdir(tmpdir)
                    runner[version].auto(autofile)
                os.chdir(oldcwd)
                log[version].write("Demo %s is done\n"%d)
            else:
                runDemo.runDemo(d, verbose="yes", log=log[version],
                                err=err[version], auto_dir=auto_dir)
    if len(autofiles) > 0:
        try:
            shutil.rmtree(tmpdir)
        except OSError:
            pass

    if len(log_file)!=0:
        for version in versions:
            runner[version].close()
            err[version].close()
            log[version].close()

    if parse:
        parse_test.parse(log_files, demos=demos)

if __name__ == '__main__':
    opts_list,args=getopt.getopt(sys.argv[1:],"cfpl:i:")

    #defaults
    versions = []
    log_file = ""

    opts={}
    for x in opts_list:
        opts[x[0]]=x[1]

    if opts.has_key("-c"):
        versions.append("2000")
    if opts.has_key("-f"):
        versions.append("97")
    if opts.has_key("-p"):
        versions.append("07p")
    if opts.has_key("-l"):
        log_file=opts["-l"]

    demos = args[0]
    test(args[0], versions, log_file, parse=False)
