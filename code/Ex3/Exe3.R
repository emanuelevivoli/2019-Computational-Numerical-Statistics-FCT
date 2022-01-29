# Exercise 3

# Last edited: 09/10.2019

g <- function(x){sqrt(-log(x))/2}

# (a) Use the R function integrate() to compute the value of I
I <- integrate(g, lower = 0, upper = 1)
I$value

# (b) Describe and implement in R the Monte Carlo method of size m = 10000 for estimating 
# Ihat. Report an estimate of the variance of the Monte Carlo estimator I_MChatof I.

set.seed(456) 
m=10000 
x=runif(m,0,1)
I_MC <- mean(g(x))
error <- abs(I_MC - I$value) 
Var_I_MC <- var(g(x))/m # estimate of the variance


# (c) Describe and implement in R the Monte Carlo method of size m = 10000 based on 
# control variables for estimating I. Report an estimate of the variance of the Monte
# Carlo estimator I_C of I.

m = 10000
######
c_st <- -cov(g(x), x)/var(x)   
I_c <- mean(g(x) + c_st * (x - mean(x)))
Var_I_C <- (var(g(x)) - ((cov(g(x), x))^2/var(x)))/m



# (d) What's the percentage of variance reduction that is achieved when using I_C instead 
# of I_MC?
Var_red_p <- ((Var_I_MC - Var_I_C)/Var_I_MC)* 100

