from AUTOclui import *

def get(solution):
    #construct a new constant solution based on the flq solution
    eps = solution.PAR(6)
    u = []
    s0 = solution(0)
    for i in range(6):
        #a doubled list: the same value for t=0 and t=1
        u.append([s0[i]+eps*s0[i+6]]*2)
    p = [0]*24
    p[1] = solution.PAR(2)
    p[2] = solution.PAR(3)
    p[5] = eps
    p[11] = 0
    p[20] = u[0][0]
    p[21] = u[1][0]
    p[22] = u[2][0]
    p.extend(list(s0))
    return load(u,t=[0,1],p=p)

def create(sfile='flq',label=2):
    solution = load(s=sfile)(label)
    save(get(solution),'startman')

# This is the Python syntax for making a script runable    
if __name__ == '__main__':
    if len(sys.argv) > 2:
        create(sys.argv[1], int(sys.argv[2]))
    elif len(sys.argv) > 1:
        create(sys.argv[1])
    else:
        create()

