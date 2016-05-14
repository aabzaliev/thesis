# IMPORTANT: LASSO is the same as Basis Pursuit Denoising

setwd("C:/Users/ngnt/Dropbox/master thesis/code/scripts")

options(scipen=999)
#setwd("~/Dropbox/master thesis/code/scripts")
require(tseries)
#optimization with risk-free
R <- returns[[4]]
res <- portfolio.optim(returns[[1]], pm=0.01680487, rf=R, shorts=TRUE)

#it works!


require("quadprog")
bVec <- 1
zeros <- array(0, dim = c(J,1))
#meq=1 means equality
#calculate covariance matrix
S <- cov(returns[[1]])
p <- colMeans(returns[[1]])
Amat <- matrix(1, nrow=nrow(S))
solQP <- solve.QP(S, zeros, Amat, bVec, meq = 1)
weightsQP <- solQP$solution
  
all.equal(as.numeric(var(returns[[1]] %*% solQP$solution)), as.numeric(2 * solQP$value))


#######################################################################################
# Naive method - generate 2^p constraints and solve the problem with them
A = as.matrix (expand.grid (rep (list (c(-1,1)), J)))
#add 1 for sum of the coeffcients is equal to 1 constraint
Amat = cbind(1, t(A))
lambda <- 1
# the same story for beta - why is it -1? I guess it should be written in lagrange form
# because we have inequality more than in R, but less than in theory
b = rep (-lambda, 2^J)
b = c(1, b)
solLasso <- solve.QP(S, zeros, Amat, b, meq=1)
weightsQP <- solLasso$solution

#######################################################################################
#   now lets use Tibsurani's method
# first unconstrained solution(i.e. only with constraint that weights sum up to 1)
bVec <- 1
zeros <- array(0, dim = c(J,1))
Amat <- matrix(1, nrow=nrow(S))
solQP <- solve.QP(S, zeros, Amat, bVec, meq = 1)
weightsQP <- solQP$solution

#or probably better to use analytical solution
wminp <- f + (a[1,2]/a[1,1])*g #TODO implement just with one formula from the data
#our starting weights obtained without constraint
w <- wminp
#constraint all weights to sum up to 1
Amat <- matrix(1, nrow=nrow(S))
bVec <- 1
# use a tolerance on the 1-Norm insted of hard-comparison
# this allows algorithm to be linear vs. exponential in the number of variables
optTol <- as.double(1e-5)
iter <- 0
# while the L1-norm of the parameters is above t
maxIter <- 1000

while (sum(abs(w)) >= lambda + optTol && iter < maxIter) {
  iter = iter +1
  # we get signs of w, and then in constraint they are multiplied with w itself: 
  # obviously, +*+ will be plus and -*- will be plus: therefore it is equal to absolute value
  #if they together more than lambda, then magic happens and coefficients go down gradually
  Amat <- cbind(Amat, sign(w))  
  bVec <- c(bVec, lambda)
  # important: this is negative, since solve.QP solves for constraint A%*%w >= b;
  # however, in our case we have less than(i.e. A%*%w <= b) and therefore
  # both A and b have to be multiplied by (-1): -A%*%w <= -b
  w <- solve.QP(S, zeros, -Amat, -bVec, meq = 1)$solution
}
#works!
###################################################################################
#now lets use cross-validation

# now: lasso
#write  2^p constraints
#dim2 <- dim(returns[[1]])[2]

p.cv <- colMeans(returns.cv[[1]])
names(p.cv) <- returns.cv[[2]]

#calculate OOS covariance matrix
S.cv <- cov(returns.cv[[1]])

#lets find the risk-return values of our portfolio with new data!
meanVar.cv <- as.double(sqrt(t(weightsQP)%*%S.cv%*%weightsQP))
meanVar.cv <- append(meanVar.cv, weightsQP%*%p.cv)

#higher return - higher variance!


####################################################################################
#   obtain the weights via regression - see http://enricoschumann.net/R/minvar.htm
####################################################################################
#generate one asset as regressand
y <- returns[[1]][ ,1L]
# compute excess returns
X <- returns[[1]][ ,1L] - returns[[1]][ ,2:J]

solR <- lm(y ~ X)
#the coefficient, except intercept are the weights: so we have replace first coeffcient 
# by $1 - sum of all the other coefficients$
cat("weights from qp\n")
as.vector(solQP$solution)

cat("\nweights from regression\n")
as.vector(c(1 - sum(coef(solR)[-1L]), coef(solR)[-1L]))

#interesting, does it work with the glmnet package?? i.e. lasso
library("glmnet")
fit.lasso=glmnet(X,y, lambda=0)
plot(fit.lasso,xvar="lambda",label=TRUE)

####################################################################################
#       solving system of linear equations (again http://enricoschumann.net/R/minvar.htm)
####################################################################################

x <- solve(cov(returns[[1]]), numeric(J) + 1)
x <- x / sum(x)
x
####################################################################################
# OVER 9000 PACKAGES https://cran.r-project.org/web/views/Optimization.html
####################################################################################


####################################################################################
#               now lets calculate the stuff for different values of lambda
####################################################################################

lambda_vec <- c()
