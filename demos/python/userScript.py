# This is an example script for the AUTO07p command line user
# interface.  See the "Command Line User Interface" chapter in the
# manual for more details.
from auto import *

def myRun(demoname):

    demo(demoname)

    r = run(demoname)
    branchpoints = r("BP")
    for solution in branchpoints:
        bp = load(solution, ISW=-1, NTST=50)
        # Compute forwards
        print "Solution label", bp["LAB"], "forwards"
        fw = run(bp)
        # Compute backwards
        print "Solution label", bp["LAB"], "backwards"
        bw = run(bp,DS='-')
        both = fw + bw
        merged = merge(both)
        r = r + merged

    r=relabel(r)
    save(r, demoname)
    plot(r)
    wait()
