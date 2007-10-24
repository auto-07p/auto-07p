from AUTOclui import *
import sys,parseD

def make_s(sfile,label,step,floquet=None):
    solution = sl(sfile)
    if floquet is None:
        #first figure out the Floquet multiplier
        diag = parseD.parseD('d.'+str(sfile))
        for d in diag:
            if d["Label"] != 0: break
        addend = d["Label"] - solution[0]["Label"]
        floquet = 1.0 + 1e-3
        for mplier in diag(label+addend)["Multipliers"]:
            if abs(mplier[0]) > abs(floquet):
                if abs(floquet) > 1.0 + 1e-3 + 1e-10:
                    print "Warning: more than one multiplier > 1"
                floquet = mplier[0]
    print "Floquet multiplier: ", floquet
    solution = solution(label)
    for point in solution['data']:
        for i in range(len(point['u'])):
            point['u'].append(0)
            point['u dot'].append(0)
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
