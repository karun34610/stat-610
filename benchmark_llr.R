library(bench)
x = rnorm(n)
y = rnorm(x + rnorm(n))
z = seq(-1, 1, length.out = 100)
omega = 3
time_taken <- bench::mark(llr(x,y,z,omega)) 
cat("Total time taken by llr function:", time_taken$total_time, "\n")
