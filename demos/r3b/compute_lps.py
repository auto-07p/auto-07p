# This script computes the initial circle of solutions for mu=0
# as well as the bifurcating branches which give us the
# Lagrange points.

from AUTOclui import *
import parseD,math,sys

def write_lagrange(m, x, i, output):
    # When we determine which Lagrange point we have we save it.
    periods = []
    x["Type number"] = 3 #HB
    print "L"+str(i)+":"
    d = parseD.parseD('fort.9')
    routh = 0.5*(1-math.sqrt(69)/9)
    for evalue in d(x["Label"])["Eigenvalues"]:
        # If the real part in sufficiently small
        # we get the imaginary part
        if evalue[1] > 0 and (i <= 3 or evalue[0] == 0 or m < routh):
            # and compute the period.  If is is not in our
            # list of periods (i.e. it is not a complex conjugate
            # to one we have already computed) we add it.
	    periods.append(2*math.pi/evalue[1])
    periods.sort()
    for j in range(len(periods)):
        period = periods[j]
        label = (i-1)*2+j+1
        # above Routh's ratio we have one period for L4/L5, otherwise 3.
        if i == 5:
            label = label - 2 + len(periods)
        print "Label: %d; imaginary part: %7s; period: %10s"%(label,
	    2*math.pi/period, period)
        x["Label"] = label
        x["Branch number"] = i*10+j+1
        x["p"][10] = period
        x.write(output)

def compute(m=0.063):
    # m is the desired mass ratio

    # Load r3b.f and c.r3b into the AUTO CLUI
    load('r3b',c='r3b')

    # Add a stopping condition so we only compute the loop once
    # We tell AUTO to stop when parameter 16 is 0.991, parameter 2 is -0.1,
    # or parameter 2 is 1.1.  If parameter2 is m we just report
    # a point.
    cc('UZR',[[-16,0.991],
              [-2,-0.1],
              [2,m],
              [-2,1.1]])

    # Compute the circle.
    run()

    # Extract the 5 Lagrange points for each of the branches
    # which we will use in later calculations.

    # This command parses the solution file fort.8 and returns
    # a Python object which contains all of the data in the
    # file in an easy to use format.
    data=sl()

    # For every user defined point the fort.8 file...
    uzpoints = map(data, splabs(data, "UZ"))
    # We look at the value of one of the components
    # to determine which Lagrange point it is.

    # The solution is a Python dictionary.  One of the
    # elements of the dictionary is an array called "data"
    # which contains the values of the solution.  For example,
    # x["data"][0]["t"] is the 't' value of the first point
    # of the solution. x["data"][0]["u"] is an array of which
    # contains the value of the solution at t=0.
    def select_lp(x):
        u = x["data"][0]["u"]
        if u[1] > 0.01:
            return 4
        if u[1] < -0.01:
            return 5
        if u[0] > 1:
            return 2
        if u[0] < -0.01:
            return 3
        return 1
    lp = map(select_lp, uzpoints)

    output = open("s.start","w")
    for i in range(1,6):
        write_lagrange(m, uzpoints[lp.index(i)], i, output)
    output.close()
    print "Written to s.start"

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    if len(sys.argv) > 1:
        compute(float(sys.argv[1]))
    else:
        compute()
