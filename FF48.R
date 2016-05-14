setwd("C:/Users/ngnt/Dropbox/master thesis/data")

require(zoo)
# important - there should be white space before -99.99, otherwise it doesn't recognize NA's
# you can imagine how much time I spent figuring it out

data <- read.csv (file="48_Industry_Portfolios.csv", header=T, skip=11, na.strings=c(" -99.99", " -999"))


#all the columns except date have to be numeric
data[, -1] <- sapply(data[, 2:length(data)], as.character)
data[, -1] <- sapply(data[, 2:length(data)], as.numeric)

#as data column is different for different datasets(i.e. yearly and montly we just leave it as a character)
names(data)[1] <- "Date"
data$Date <- as.character(data$Date)

# now lets split the data - there are several databases inside
# just hard-coded the lines manually - is there better way to do it?

#Average Value Weighted Returns -- Annual
annual.value <- data[2153:2241,]
monthly.value$Date <- as.yearmon(monthly.value$Date, "%Y%m")

#Average Equal Weighted Returns -- Monthly
monthly.equal <- data[1077:2150, ]
monthly.equal$Date <- as.yearmon(monthly.equal$Date, "%Y%m")

#Average Value Weighted Returns -- Monthly
monthly.value <- data[1:1074, ]

#Average Equal Weighted Returns -- Annual
annual.equal <- data[2244:2332,]


# as in Brodie et al. 2009, I will use equally weighted monthly returns
moeq5 <- subset(monthly.equal, format(monthly.equal$Date, "%Y-%m") > "1976-07" & format(monthly.equal$Date, "%Y-%m") < "1986-06")
moeq5 <- as.matrix(moeq5[, -1])
nc <- ncol(moeq5)
w <- rep(1/nc, nc)
#mean(monthly.equal[541:600, 2:length(monthly.equal)], na.rm=TRUE)

#average_hist_return <- colMeans(monthly.equal[541:600, 2:length(monthly.equal)])
#average_hist_return%*%w

pftPerf <- function(x, w) {
  #x is 12 month data
  # so lets write the logic - first we calculate 1/n portfolio with equal weight for every industry
  # add 1 to every return so we will be able to calculate cumulative return
  # and afterwards multiply it with w so will have average return across all industries
  R_ave <- (x + 1)%*%w
  # and now simply make the product of all average returns across all the periods
  ColProd <- prod(R_ave)
  return(ColProd-1)
}
####################################
#   This is just exercise to see the returns: for normal calculations see function pftPerf()
####################################
# ptfValue1 <- pftPerf(moeq5[1:12,], w)
# r1 <- ptfValue1[length(ptfValue1)]/ptfValue1[1] - 1
# ptfValue2 <- pftPerf(moeq5[13:24,], w)
# r2 <- ptfValue2[length(ptfValue2)]/ptfValue2[1] - 1
# ptfValue3 <- pftPerf(moeq5[25:36,], w)
# r3 <- ptfValue3[length(ptfValue3)]/ptfValue3[1] - 1
# ptfValue4 <- pftPerf(moeq5[37:48,], w)
# r4 <- ptfValue4[length(ptfValue4)]/ptfValue4[1] - 1
# ptfValue5 <- pftPerf(moeq5[49:60,], w)
# r5 <- ptfValue5[length(ptfValue5)]/ptfValue5[1] - 1
#calculate and plot the perfomance of the equally weighted portfolio
#(r1+r2+r3+r4+r5)/5

plot(pftPerf(moeq5, w), type = "l",
       main = "Portfolio Cumulated Performance",
       xlab = "Date", ylab = "Wealth")

z <- pftPerf(moeq5[1:12, ], w)
for (i in seq(13,60, 12)) 
{
  lower <- i
  upper <- i+11
  #z contains all the cimulative returns for the 5 years
  z <- cbind(z, pftPerf(moeq5[lower:upper, ], w))
}
#the result corresponds to the paper
mu <- mean(z)

#solution without L1 regularization

require(quadprog)
S <- cov(moeq5)
p <- colMeans(moeq5)
J <- ncol(moeq5)
Amat <- matrix(1, nrow=nrow(S))
Amat <- cbind(Amat, colMeans(moeq5))
bVec <- c(1, mu)
zeros <- array(0, dim = c(J,1))

solQP <- solve.QP(S, zeros, Amat, bVec, meq = 2)
wnew<-solQP$solution
sum(solQP$solution)
w <- wnew

#now with the regularization

optTol <- as.double(1e-5)
iter <- 0
# while the L1-norm of the parameters is above t
maxIter <- 1000

lambda = 3
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
  w <- solve.QP(S, zeros, -Amat, -bVec, meq = 2)$solution
}

w[abs(w)<=zeroThreshold] = 0

zeroThreshold = 1e-4

#works!

barplot(w, ylim = c(-3, 3), las = 2, main = "Test MV Implementation")

