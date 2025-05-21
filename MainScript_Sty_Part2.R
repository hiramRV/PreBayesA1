library(matlib)
#Create a matrix with N(0,1)
X= matrix(rnorm(30,mean=0, sd=1) ,nrow = 10, ncol = 3,byrow = TRUE)
X
# Given the Vector B
b = c(1,1,2)
b
#Multiply them
mu = (X%*%b)
mu

#Vector or errors. 
epsilon= matrix(rnorm(10,mean=0, sd=0.1) ,nrow = 10, ncol = 1,byrow = TRUE)
epsilon

#Vector of response observations
y = mu + epsilon
y

#Compute the Least Squares estimate
B_1 = solve((t(X)%*%X))
B_2 = t(X)%*%y
b_hat = B_1%*%B_2
b_hat

b_hat2 = solve(t(X) %*% X) %*% t(X) %*% y

#4d)
n=nrow(X)
p=ncol(X)
#Vector of residuals
error = y-X%*%b_hat
error
#Variance errors
var_e = (t(error)%*%error)/(n-p)
var_e[1]
#Covariance matrix of the least squares estimator
CoVar = var_e[1]*solve(t(X) %*% X)
# Get the errors of each row
CoVar_ind = matrix(c(sqrt(CoVar[1]),sqrt(CoVar[5]),sqrt(CoVar[9])),nrow=1,ncol=3)
CoVar_ind

sqrt(diag(CoVar))
#Last check 
lm(y~mu + epsilon)
#Test
X2= matrix(c(1,2,3,3,5,6,6,7,1),nrow = 3, ncol = 3,byrow = TRUE)
invX2 = solve(X2)
invX2%*%X2
