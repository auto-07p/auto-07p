from AUTOclui import *
import sys

def make_s(sfile,label,step,floquet=None):
    solution = sl(sfile)(label)
    if floquet is None:
        floquet = solution['Parameters'][3]
    print "Floquet multiplier: ", floquet
    solution['Label'] = 1
    solution['Free Parameters'] = [2, 3, 4]
    solution["Parameter NULL vector"] = [0, 1, 0]
    solution['Parameters'][3] = floquet
    solution['Parameters'][4] = 0
    solution['Parameters'][5] = step
    solution.writeFilename('s.ext')
    print "Written to s.ext"

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
