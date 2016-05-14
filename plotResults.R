dat <- log(data[[1]])
M <- nrow(dat)
J <- ncol(dat)
ymax = max(dat)
ymin = min(dat)

mycolors <- rainbow(J+1)

#####################################################
#               Asset prices and returns
#####################################################
postscript(file="./results/fig1.eps", onefile=FALSE,
           horizontal=FALSE, height=10, width=5)

par(mfrow=c(2,1))

id <- c(1:nrow(dat))
#plot asset log returns
#plot only facebook
plot(id, rev(dat[,1]), ylim=c(ymin, ymax), type="l",
     col=mycolors[1], xlab="day", ylab="log(price)",
     main = "Asset Prices")
#add other assets to the plot

if(J > 1){
  for(j in 2:J){
    lines(id, rev(dat[,j]), type="l",
          col=mycolors[j]);
  }
}

legend("topleft", names(dat), cex=0.5, pch=rep(15, J),
       col=mycolors)
ret <- returns[[1]];
ymax = max(ret); ymin = min(ret);
id <- c(1:nrow(ret));
plot(id, rev(ret[,1]), ylim=c(ymin, ymax), type="l",
     col=mycolors[1], xlab="day", ylab="returns",
     main = "Asset Returns");
if(J > 1){
  for(j in 2:J){
    lines(id, rev(ret[,j]),type="l",col=mycolors[j]);
  }
}
legend("topleft", returns[[2]], cex=0.5, pch=rep(15, J),
       col=mycolors);


########################################################
#             Efficient frontier
##########################################################


postscript(file="./results/fig2.eps", onefile=FALSE,
           horizontal=FALSE, height=10, width=5);
par(mfrow=c(2,1));
plot(s, r, xlim=c(0,max(s)), ylim=c(min(r,p), max(r,p)),
     type="l", col="blue", xlab="risk", ylab="return",
     main = "Efficient Frontier, MVP1, TGP")
#add individual assets
points(ss, p, pch=19, col=mycolors)
text(ss, p, pos=4, cex=0.5, names(p))
#add dominant eigen portfolio with all weights positive
points(sv[1], rv[1], pch=15, col="black")
text(sv[1], rv[1], pos=4, cex=0.5, "DEP")

#minimum variance and tangent portfolio
points(minp[1], minp[2], pch=19, col="black")
text(minp[1], minp[2], pos=2, cex=0.5, "MVP1")
points(tanp[1], tanp[2], pch=19, col="black")
text(tanp[1], tanp[2], pos=2, cex=0.5, "TGP")
lines(c(0,max(s)), c(0,max(s)*tanp[2]/tanp[1]), lty=3)
abline(h=0, lty=2)
abline(v=0, lty=2)


#this is quite strange - the original code used f transposed insted of w
f <- t(w)
#now weights plot
plot(s, f[,1], xlim=c(0,max(s)), ylim=c(min(f),max(f)),
     col=mycolors[1], type="l",
     xlab="risk", ylab="portfolio weights",
     main = "Efficient Portfolio Weights")
if(J > 1){
  for(j in 2:J){
    lines(s, f[,j], type="l", col=mycolors[j]);
  }
}

abline(h=0, lty=2); abline(v=minp[1], lty=3)
abline(v=tanp[1], lty=3)
text(minp[1], min(f), pos=4, cex=0.5, "MVP1")
text(tanp[1], min(f), pos=4, cex=0.5, "TGP")
legend("topleft", names(p), cex=0.5, pch=rep(15, J),
       col=mycolors)


###################################################
#                   Barplots
###################################################
postscript(file="./results/fig3.eps", onefile=FALSE,
           horizontal=FALSE, height=10, width=5)
par(mfrow=c(2,1))
barplot(wminp, main="Minimum Variance Portfolio 1 (MVP1)",
        xlab="Assets", ylab="Weights",
        col=mycolors, beside=TRUE)
abline(h=0, lty=1)
legend("topleft", names(p), cex=0.5, pch=rep(15, J),
       col=mycolors)
barplot(wtanp, main="Tangency Portfolio (TGP)",
        xlab="Assets", ylab="Weights", col=mycolors,
        beside=TRUE)
abline(h=0, lty=1)
legend("topleft", names(p), cex=0.5, pch=rep(15, J),
       col=mycolors)


#I assume here should be sv insted of v[,1]
barplot(sv, main="Dominant Eigen-Portfolio (DEP)",
        xlab="Assets", ylab="Weights", col=mycolors,
        beside=TRUE)
abline(h=0, lty=1)
legend("topleft", names(p), cex=0.5, pch=rep(15, J), col=
         mycolors)
