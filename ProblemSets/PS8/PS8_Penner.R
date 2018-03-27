# Problem set 8
# Alli Penner
library(nloptr)
library(stargazer)

# generate some data
set.seed(100)
X <- matrix(rnorm(100000*10), nrow = 100000, ncol = 10)
X[,1] <- 1

# function
eps <- rnorm(100000, sd=0.25)
beta <- c(1.5,-1,-0.25,0.75, 3.5,-2,0.5,1,1.25,2)

Y <- rep(0, length(100000))

# apply betas
for (i in 1:100000){
  Y[i] <- X[i,1]*beta[1]+X[i,2]*beta[2]+X[i,3]*beta[3]+X[i,4]*beta[4]+
          X[i,5]*beta[5]+X[i,6]*beta[6]+X[i,7]*beta[7]+X[i,8]*beta[8]+
          X[i,9]*beta[9]+X[i,10]*beta[10]+eps[i]
}

# solve
BOLS <- solve(crossprod(X), crossprod(X,Y))




# gradient boosting
alpha <- 0.0000003
maxiter <- 500

objfun <- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))
  }

gradient <- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

beta1 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# randomly initialize a value to beta
set.seed(100)
beta.All <- matrix("numeric",length(beta),maxiter)

# gradient descent method to find the minimum
iter  <- 1
beta0 <- 0*beta1
while (norm(as.matrix(beta0)-as.matrix(beta1))>1e-8) {
  beta0 <- beta1
  beta1 <- beta0 - alpha*gradient(beta0,Y,X)
  beta.All[,iter] <- beta1
  if (iter%%10000==0) {
    print(beta1)
  }
  iter <- iter+1
}

# print result and plot all xs for every iteration
print(iter)
print(paste("The minimum of f(beta,y,X) is ", beta, sep = ""))


# L-BFGS
objfun <- function(bet,Y,X) {
  return (sum((Y-X%*%bet)^2))
}

## Gradient 
gradient <- function(bet,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%bet)) )
}

# initial values
beta0 <- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

#parameters
options <- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

# Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)

# nelder mead
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e3)

## Optimize!
result <- nloptr( x0=beta0,eval_f=objfun,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)


# MLE L-BFGS

gradient <- function(theta){
  grad <- as.vector(rep(0,length(theta)))
  beta <- theta[1: (length(theta)-1)]
  sig <- theta[length(theta)]
  grad[1:(length(theta)-1)] <- -t(X)%*%(Y-X%*%beta)/(sig^2)
  grad[length(theta)] <- dim(X)[1]/sig - crossprod(Y-X%*%beta)/sig^3
  return(grad)
}
  
objfun  <- function(theta,Y,X) {
  # need to slice our parameter vector into beta and sigma components
  beta    <- theta[1:(length(theta)-1)]
  sig     <- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((y-X%*%beta)/sig)^2) ) 
  return (loglike)
}


## initial values
theta0 <- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients

## Algorithm parameters
options <- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

## Optimize!
result <- nloptr( x0=theta0,eval_f=objfun,opts=options,Y=Y,X=X)
print(result)
betahat  <- result$solution[1:(length(result$solution)-1)]
sigmahat <- result$solution[length(result$solution)]


  
  
# easy way
model <- lm(Y~X-1)

stargazer(model)


