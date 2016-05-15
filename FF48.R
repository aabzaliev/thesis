require(quadprog)
require(zoo)

setwd("C:/Users/ngnt/Dropbox/master thesis/data")
# important - there should be white space before -99.99, otherwise it doesn't recognize NA's
# you can imagine how much time I spent figuring it out

zeroThreshold = 1e-4
optTol <- as.double(1e-5)
iter <- 0
# while the L1-norm of the parameters is above t
maxIter <- 1000

data <- read.csv (file="48_Industry_Portfolios.csv", header=T, skip=11, na.strings=c(" -99.99", " -999"))


#all the columns except date have to be numeric
data[, -1] <- sapply(data[, 2:length(data)], as.character)
data[, -1] <- sapply(data[, 2:length(data)], as.numeric)

#as date representation is different for different datasets (i.e. yearly and montly we just leave it as a character for now)
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

# as in Brodie et al. 2009, I will use equally weighted monthly returns from 1976 to 2006
moeq <- subset(monthly.equal, format(monthly.equal$Date, "%Y-%m") >= "1971-07" & format(monthly.equal$Date, "%Y-%m") <= "2006-06")
#moeq5 <- as.matrix(moeq5[, -1])

# so lets generate 5-year sequences to estimate the parameters
# to separate our dataset for different years
# get the amount of years, use of ceiling because we start from july and end in june
big_ts <- NULL
#ro_save <- 0
eval_data_save <- NULL
ro_save <- NULL
#N_year <- ceiling(moeq5$Date[length(moeq5$Date)] - moeq5$Date[1])
for (i in seq(1971, 2000))
  {
  start_date <- paste(i, "07", sep="-")
  end_date <- paste(i+5, "06", sep="-")
  # this is 5-year span for estimating sample returns, cumulative returns, variance-covariance matrix and
  # ro (target return, average annualized return estimated by the naive 1/N portfolio over these 5 years)
  subsetting_data <- subset(moeq, format(moeq$Date, "%Y-%m") >= start_date & format(moeq$Date, "%Y-%m") <= end_date)
  subsetting_data <- as.matrix(subsetting_data[, -1])*0.01
  #save the dataset for future use
  assign(paste(start_date, end_date, sep="/"), subsetting_data)
  
  # this part divide the 5 year dataset into 1year and estimate the cumulative return with 1/N portfolio for each year;
  # returns are averaged afterwards to get the average for 5 year span
  # this strategy is from Brodie et al. 2009
  # number of assets
  nc <- ncol(subsetting_data)
  # 1/N weights
  w <- rep(1/nc, nc)
  
  z <- sum(subsetting_data[1:12, ]%*%w)
  for (j in seq(13, 60, 12)) 
  {
    lower <- j
    upper <- j+11
    z <- cbind(z, sum(subsetting_data[lower:upper, ]%*%w))
  }
  
  ro <- mean(z)
  ro_save <- cbind(ro_save, ro)
  
  S <- cov(subsetting_data)
  mu <- colMeans(subsetting_data)
  J <- ncol(subsetting_data)
  #constraint that some of the coefficients is equal to 1
  Amat <- matrix(1, nrow=nrow(S))
  #adding constraint to target return
  Amat <- cbind(Amat, mu)
  #addin long-only constraint doesnt work
  #Amat <- cbind(Amat, diag(1, ncol(x)))
  
  #b = rep(0, ncol(x))
  
  bVec <- c(1, ro)
  #bVec <- c(bVec, b)
  zeros <- array(0, dim = c(J,1))
  
  solQP <- solve.QP(S, zeros, Amat, bVec, meq = 2)
  wnew<-solQP$solution
  sum(solQP$solution)
  w <- wnew

  #now we have unconstrained solution and we can start with L1 solution
  
  lambda = 100
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
  
  ############################################################
  #     the weights above are from LASSO
  ###########################################################
  
  start_eval_date <- paste(i+5, "07", sep="-")
  end_eval_date <- paste(i+6, "06", sep="-")
  eval_data <- subset(moeq, format(moeq$Date, "%Y-%m") >= start_eval_date & format(moeq$Date, "%Y-%m") <= end_eval_date)
  eval_data <- as.matrix(eval_data[, -1])*0.01
  eval_data_save <- rbind(eval_data_save, eval_data)
  
  #returns <- eval_data%*%w
  #TODO leave date column in final tseris to be able to look at it
  #big_ts <- rbind(big_ts, returns)
  
}
########################################################
#             evaluating 1/N portfolio
########################################################
# every 60 observations correspond to 5 year break-out period
# total amount of months in evaluations periods
sd_save <- NULL
m_safe <- NULL
for (i in seq(1, nrow(eval_data_save), 60))
{
  start_ind <- i
  end_ind <- i+59
  k <- eval_data_save[start_ind:end_ind, ]
  z <- sum(k[1:12, ]%*%w)
    for (j in seq(13, 60, 12)) 
    {
      lower <- j
      upper <- j+11
      z <- cbind(z, sum(k[lower:upper, ]%*%w))
      #sd_prep <- cbind(sd_prep, sqrt(sum((k[lower:upper, ]%*%w - sum(k[lower:upper, ]%*%w))^2)/(T-1)))
    }
  
  #sqrt(sum((k%*%w - z)^2)/T)
  m <-mean(z)
  m_safe <-cbind(m_safe, m)
  T <- nrow(k)
  p <- sum(k%*%w)/T
  sd_ret <- sqrt(sum((k%*%w - p)^2)/T)
  #sd_ret <- w%*%cov(k)%*%w
  sd_save <- cbind(sd_save, sd_ret)
}
  sd_ret <- sqrt(w%*%cov(eval_data_save)%*%w)
  d <- cov(k)
#m_safe contains values for the mean returns
#now lets try for the whole evaluation period, i.e. 

mean(m_safe)




#wooooooooooooorks!!!!!!!!!!

#solution without L1 regularization
require(quadprog)
S <- cov(moeq5)
p <- colMeans(moeq5)
J <- ncol(moeq5)
Amat <- matrix(1, nrow=nrow(S))
Amat <- cbind(Amat, colMeans(moeq))
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


#works!

barplot(w, ylim = c(-3, 3), las = 2, main = "Test MV Implementation")

