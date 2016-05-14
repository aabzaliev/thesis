setwd("C:/Users/ngnt/Dropbox/master thesis/code/scripts")
#number of portfolios on frontier
N <- 100
#Maximum daily return value considered
Rmax <- 0.01

data <- read.table(paste("./data/", "portfolio.txt", sep=""),
                   header=TRUE, sep=",", na.strings = "NA")
data <- data[,colSums(is.na(data))<nrow(data)]
#only since Frebruary
data$KMI <- NULL

data$GOOG <- NULL
is.na(data[[1]])
data <- list(data[, 2:length(data)], as.integer(N),
             as.double(Rmax))

dat <- data[[1]]

#number of observations (one less since we make returns and first observation is dropped)
N <- nrow(dat) - 1
#number of stocks
J <- ncol(dat)

ret <- (dat[1:N,1] - dat[2:(N+1),1])/dat[2:(N+1),1];
if(J > 1){
  for(j in 2:J){
    ret <- cbind(ret, (dat[1:N,j] - dat[2:(N+1),j])/
                   dat[2:(N+1),j])
  }
}
#stupid me - we have time series and we shouldnt use random sampling for CV
#train=sample(seq(T-1),T-50,replace=FALSE)
ret <- ret[,colSums(is.na(ret))<nrow(ret)]
#FB and FBBV and PYPL was not presented in the index in 2011
# KMI appeared only in February
returns <- list(ret, names(data[[1]]), data[[2]], data[[3]])

############################################################
#           divide to training and CV (for one month) set
############################################################


#returns.cv <- list(ret[I(T-50):T,], names(data[[1]]), data[[2]], data[[3]])
############################################################

#                  Analytical solution
############################################################
p <- colMeans(returns[[1]])
names(p) <- returns[[2]]
J <- ncol(returns[[1]])
M <- returns[[3]]
Rmax <- returns[[4]]

#calculate covariance matrix
S <- cov(returns[[1]])



#inverse of S
Q <- solve(S)
anyNA(Q)
#create unit vector
u <- rep(1,J)

#initialize A-matrix, empty
a <- matrix(rep(0,4),nrow=2)

#populate with values
# If both are vectors of the same length, it will return the inner product (as a matrix)
# therefore we dont need transpose in second multiplication
a[1,1] <- u%*%Q%*%u

a[1,2] <- a[2,1] <- u%*%Q%*%p

a[2,2] <- p%*%Q%*%p

#find the determinant of the matrix
d <- a[1,1]*a[2,2] - a[1,2]*a[1,2]


f <- (Q%*%( a[2,2]*u - a[1,2]*p))/d

g <- (Q%*%(-a[1,2]*u + a[1,1]*p))/d

#generate a sequence of returns to be displayed
r <- seq(0, Rmax, length=M)

#initialize weight matrix
w <- matrix(rep(0,J*M), nrow=J)

#find all the weights for all given levels of return - frontier portfolios
for(m in 1:M) w[,m] <- f + r[m]*g

#find all the variances of the portfolios, for a given return
s <- sqrt( a[1,1]*((r - a[1,2]/a[1,1])^2)/d + 1/a[1,1])

#why do we extract variances of the assets - to populate the matrix with eigenvalues later
ss <- sqrt(diag(S))

# risk-return values of minimum variance portfolio
minp <- c(sqrt(1/a[1,1]), a[1,2]/a[1,1])

#weights of the minumum variance portfolio
wminp <- f + (a[1,2]/a[1,1])*g

#risk-return value of the tangency portfolio
tanp <- c(sqrt(a[2,2])/a[1,2], a[2,2]/a[1,2])

#weights of the tangency portfolio
wtanp <- f + (a[2,2]/a[1,2])*g


#now the part for eigen-portfolios
#big omega matrix
omega <- sqrt(diag(1.0/ss))
#inverse ith
#omega <- solve(omega)

#calculate eigenvalues and eigenvectors of the function
x <- eigen(omega%*%S%*%omega)

#divide each eigenvector by volatility of the corresponding asset - almost weights
v <- omega%*%x$vec

#...and normalize by imposing a constant invested wealth - weights of the eigen-portfolios
#intuition: the weight of a given asset is inversely proportional to its volatility
# "Thus, any portfolio can be represented as a linear combination
# of the eigen-portfolios, since they are orthogonal and
# form a basis in the asset space"
for(j in 1:J) v[,j] <- v[,j]/(u%*%v[,j])
#this is with regularization
sv <- rv <- rep(0, J)
for(j in 1:J){
  rv[j] <- t(v[,j])%*%p;
  if(rv[j] < 0){
    rv[j] <- -rv[j];
    v[,j] <- -v[,j];
  }
  sv[j] <- sqrt(t(v[,j])%*%S%*%v[,j]);
}