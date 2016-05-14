#this script replicates the matlab code from coursera ML course
training.data <- read.csv("ex2data1.txt", header=FALSE)

X <- training.data[, 0:2]
Y <- training.data[, 3]
model <- glm(Y~X$V1+X$V2, family=binomial(link='logit'))
#