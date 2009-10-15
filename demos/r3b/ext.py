from AUTOclui import *
import sys

def get(sfile,label,step,floquet=None):
    s = loadbd(s=sfile)
    solution = s(label)
    if floquet is None:
        floquet = solution.PAR(4)
    print("Floquet multiplier: %s"%floquet)
    return load(solution,LAB=1,PAR={4:floquet,5:0,6:step})

def make_s(sfile,label,step,floquet=None):
    solution = get(sfile,label,step,floquet)
    save(solution,'ext')

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    if len(sys.argv) < 4:
        print('Not enough arguments given')
        print('Usage: autox ext.py sfile label step [floquet_multiplier]')
    elif len(sys.argv) > 4:
        make_s(sys.argv[1], int(sys.argv[2]), float(sys.argv[3]),
               float(sys.argv[4]))
    else:
        make_s(sys.argv[1], int(sys.argv[2]), float(sys.argv[3]))
