library(XML)
u <- "http://www.cboe.com/products/indexcomponents.aspx"
tables <- readHTMLTable(u, header=TRUE)
tickers <- tables[[1]][2]
tickers <- lapply(tables[[1]][2], as.character)
write.table(tickers, file="C:/Users/ngnt/Dropbox/master thesis/code/scripts/stocks.csv", sep = ",", col.names = FALSE, row.names = FALSE)
tickers$TICKER[18] <- "BRK-B"
