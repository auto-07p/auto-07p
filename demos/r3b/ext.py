from AUTOclui import *
import sys

def get(sfile,label,step,floquet=None):
    s = load(s=sfile)
    solution = rl(s([label]))(1)
    if floquet is None:
        floquet = solution['PAR(4)']
    print "Floquet multiplier: ", floquet
    solution['PAR(4)'] = floquet
    solution['PAR(5)'] = 0
    solution['PAR(6)'] = step
    return solution

def make_s(sfile,label,step,floquet=None):
    solution = get(sfile,label,step,floquet)
    save(solution,'s.ext')

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    if len(sys.argv) < 4:
        print 'Not enough arguments given'
        print 'Usage: autox ext.py sfile label step [floquet_multiplier]'
    elif len(sys.argv) > 4:
        make_s(sys.argv[1], int(sys.argv[2]), float(sys.argv[3]),
               float(sys.argv[4]))
    else:
        make_s(sys.argv[1], int(sys.argv[2]), float(sys.argv[3]))
