setwd("C:/Users/ngnt/Dropbox/master thesis/code/scripts")
#for T periods
T <- 2000

########################################################################################
#                         download the data
#######################################################################################

#TODO make functions instead of scripts and use some more fancy functions from 
#https://www.rmetrics.org/sites/default/files/2014-04-portfolioAllocation.pdf
#read the names of the stocks
stocks <- tickers$TICKER
stocks <- t(read.table("stocks.txt",sep=",", header=TRUE))
J <- length(stocks)
dir.create("data", showWarnings = FALSE)
path <- "http://ichart.finance.yahoo.com/table.csv?s="
#for every stock in stocks.txt download variables and all historical info and save it with the corresponding name 
for(j in 1:J){
  dat <- read.csv(paste(path,stocks[j],sep=""))
  write.csv(dat, file=paste("./data/",stocks[j],sep=""),
              row.names=FALSE, quote=FALSE)
}


#do the first stock manually and afterwards add all the consequent into the one table using the Closing price
dat <- read.csv(paste("./data/", stocks[1], sep=""))
price <- dat[,c(1,5)]

#### this is to subset the data for whole 2011 year
# TODO probably use another time span
#hard coded 1095:1346, because it is spmehow complicated to do it automatically
price$Date <- sapply(price$Date, as.character)
price$Date <- as.Date(price$Date)
price <- price[1095:1346, ]

#### end of my own section
if(J > 1){
  for(j in 2:J){
    dat <- read.csv(paste("./data/",
                          stocks[j], sep=""))
    #was 1:n originally
    price <- cbind(price, dat[1095:1346,5])
  }
  
}

write.table(price, file=paste("./data/", "portfolio.txt", sep=""),
            row.names=FALSE, col.names=c("Date", stocks),
            quote=FALSE, sep=",")


