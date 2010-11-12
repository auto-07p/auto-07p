# This script computes the initial circle of solutions for mu=0
# as well as the bifurcating branches which give us the
# Lagrange points.

from AUTOclui import *

def write_lagrange(r):
    # Print all Lagrange points
    import math
    ltouz = {1: 7, 2: 8, 3: 6, 4: 2, 5: 4}
    for lnumber in (1, 2, 3, 4, 5):
        print("L"+str(lnumber)+":")
        uzlabel = 'UZ'+str(ltouz[lnumber])
        x = r(uzlabel)
        label = x["LAB"]
        for parnumber in (5, 6, 7):
            period = x.PAR(parnumber)
            if period != 0:
                print("Label: %2d (%s, TY='HB%s'); imaginary part: %.11f; "
                      "period: %.11f"%
                      (label, uzlabel, parnumber, 2*math.pi/period, period))

def compute(m=0.063):
    # m is the desired mass ratio
    r1 = run('r3b', UZR={2:m})
    write_lagrange(r1)

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    if len(sys.argv) > 1:
        compute(float(sys.argv[1]))
    else:
        compute()
