#===============
# AUTO Demo cusp
#===============

# Load the files cusp.f90 and c.cusp into the AUTO
# command interpreter.
cusp = load('cusp')
# Run and store the result in the Python variable mu
mu = run(cusp)
# Run backwards, and append to mu
mu = mu + run(cusp,DS='-')
# Relabel solutions
mu = relabel(mu)
# Save to b.mu, s.mu, and d.mu
save(mu,'mu')
# Plot bifurcation diagram
p = plot(mu)
p.config(bifurcation_y=['x'])
# Set the new start label to the first LP label in b.mu and s.mu
lp1 = load(mu('LP1'), ISW=2)
# Continue from this label in two parameters
cusp = run(lp1)
cusp = cusp + run(lp1,DS='-')
# save to b.cusp, s.cusp, and d.cusp
save(cusp,'cusp')
# Plot the cusp
p = plot(cusp)
p.config(bifurcation_y=['lambda'])
#clean the directory
clean()
wait()
