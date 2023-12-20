# Computationally intensive stuff
# Takes 2-3 sec on my system

# Monte Carlo simulation to estimate Pi
# Set the number of iterations
n <- 10000000

# Set seed for reproducability
set.seed(12345)

# Generate random points
x <- runif(n, min=-1, max=1)
y <- runif(n, min=-1, max=1)

# Calculate the distance from (0,0) and check if it's within the unit circle
inside_circle <- ifelse(x^2 + y^2 <= 1, 1, 0)

# Estimate Pi
pi_estimate <- 4 * sum(inside_circle) / n

save( pi_estimate,
      file = here::here("inst","example_quarto","backend","estim_pi.Rdata"))
