from AUTOclui import *

def create(sfile='flq',label=2):
    solution = sl(sfile)(label)
    output = open("man.dat", "w")
    for par in [1,2,5]:
        output.write("%19.10E\n" % (solution['Parameters'][par]))
    for u in solution[0]['u']:
        output.write("%19.10E\n" % (u))
    output.close()
    print "Written to man.dat"

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    if len(sys.argv) > 2:
        create(sys.argv[1], int(sys.argv[2]))
    elif len(sys.argv) > 1:
        create(sys.argv[1])
    else:
        create()

