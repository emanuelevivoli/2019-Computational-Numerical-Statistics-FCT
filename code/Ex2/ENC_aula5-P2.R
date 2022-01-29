#Exemple 2 slide 22/60
X = c(7,5,3,1,3,5,9,2,8,4,8,6)
xbar = mean(X)
n = length(X)

# assume data comes from Poisson with lambda unkown

# a) MLE(poisson) = xbar
# b) bias,var,sd of MLE(lambda_hat)
var.lambda_hat = var(X) / n; var.lambda_hat