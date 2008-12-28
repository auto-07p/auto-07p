from AUTOclui import *

def get(solution):
    #construct a new constant solution based on the flq solution
    PAR = solution.PAR
    eps = PAR(6)
    u = []
    s0 = solution(0)
    for i in range(6):
        u.append(s0[i]+eps*s0[i+6])
    p = {2:PAR(2),3:PAR(3),6:PAR(6),12:0}
    # init PAR(21:23) to u[:3]
    for i in range(3):
        p[21+i] = u[i]
    # init PAR(25:30) to USTART and PAR(31:36) to VSTART
    for i in range(12):
        p[25+i] = s0[i]
    return load(u,PAR=p)

def create(sfile='flq',label=2):
    solution = loadbd(s=sfile)(label)
    save(get(solution),'startman')

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    if len(sys.argv) > 2:
        create(sys.argv[1], int(sys.argv[2]))
    elif len(sys.argv) > 1:
        create(sys.argv[1])
    else:
        create()

