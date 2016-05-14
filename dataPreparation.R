setwd("C:/Users/ngnt/Dropbox/master thesis/code/scripts")
#predefine some constants
T <- 250

########################################################################################
#                         download the data
#######################################################################################
#read the names of the stocks
stocks <- t(read.table("stocks.txt",sep=",")[1,])
J <- length(stocks)
dir.create("data", showWarnings = FALSE)
path <- "http://ichart.finance.yahoo.com/table.csv?s="
#for every stock in stocks.txt download 7 variables and all historical info and save it with the corresponding name 
for(j in 1:J){
  dat <- read.csv(paste(path,stocks[j],sep=""))
  write.csv(dat, file=paste("./data/",stocks[j],sep=""),
              row.names=FALSE, quote=FALSE)
}


#do the first stock manually and afterwards add all the consequent into the one table using the Closing price
dat <- read.csv(paste("./data/", stocks[1], sep=""))
price <- dat[1:N,5]
if(J > 1){
  for(j in 2:J){
    dat <- read.csv(paste("./data/",
                          stocks[j], sep=""))
    price <- cbind(price, dat[1:N,5])
  }
  
}
write.table(price, file=paste("./data/", "portfolio.txt", sep=""),
            row.names=FALSE, col.names=stocks,
            quote=FALSE, sep=",")


