#dtw tutorial
#by komosinskid
library(dtw)
library(dendextend)
library(mclust)

######################
# window constraints
######################
w <- 40
dtwWindow.plot(sakoeChibaWindow, window.size=w)
dtwWindow.plot(itakuraWindow)

######################
# step patterns
######################

# formulas
symmetric1
symmetric2
symmetricP1
asymmetric
typeIIa
typeIIIc

# visualisation
par(mfrow=c(2,3))
plot(symmetric1)
plot(symmetric2)
plot(symmetricP1)
plot(asymmetric)
plot(typeIIa)
plot(typeIIIc)
title("Several types of step patterns", outer = TRUE)
par(mfrow=c(1,1))

######################
# example
######################
query <- window(aami3a,start=0,end=2)
reference <- window(aami3a,start=2.7,end=5)

d <- dtw(query, reference, keep=TRUE)
d$distance
plot(d, type="two", off=1, match.indices=40)
plot(d, type="three", match.indices=20)
#plot(d, type="density")

######################
# example
######################
set.seed(999)
n <- 200
w <- n/10
x <- numeric(n)
y <- numeric(n)
for (i in 2:n) {
  x[i] <- x[i-1] + rnorm(1)
  y[i] <- y[i-1] + rnorm(1)
}
plot(x, type="l", ylim = c(min(x,y), max(x,y)))
lines(y, col="blue")


### 1
d <- dtw(x,y, dist.method = "Euclidean", window.type = "none", step.pattern = symmetric2, keep=TRUE)
d$distance
dtwPlotTwoWay(d, offset=20, match.indices = 50)
dtwPlotThreeWay(d, match.indices = 20)
dtwPlotDensity(d)

### 2
w <- n/10
d <- dtw(x,y, dist.method = "Euclidean", window.type = "sakoechiba", window.size=w, step.pattern = symmetric2, keep=TRUE)
d$distance
dtwPlotTwoWay(d, offset=20, match.indices = 50)
dtwPlotThreeWay(d, match.indices = 20)
dtwPlotDensity(d)

### 3
d <- dtw(x,y, dist.method = "Euclidean", window.type = "itakura", step.pattern = symmetric2, keep=TRUE)
d$distance
dtwPlotTwoWay(d, offset=20, match.indices = 50)
dtwPlotThreeWay(d, match.indices = 20)
dtwPlotDensity(d)

### 4
plot(typeIIIc)
d <- dtw(x,y, dist.method = "Euclidean", window.type = "none", step.pattern = typeIIIc, keep=TRUE)
d$distance
dtwPlotTwoWay(d, offset=20, match.indices = 50)
dtwPlotThreeWay(d, match.indices = 20)
dtwPlotDensity(d)
##########

######################
# open bounds
######################
idx<-seq(0,6.28,len=100)
query<-sin(idx)+runif(100)/10
reference<-cos(idx)
plot(query, type="l")
lines(reference, col="blue")

d <- dtw(query[44:88],reference,
         keep=TRUE,step=asymmetric,
         open.end=TRUE,open.begin=TRUE)
plot(d,type="two",off=1)

######################
# cost and cumulative cost matrix
######################
lcm <- d$localCostMatrix
image(x=1:nrow(lcm),y=1:ncol(lcm),lcm)
#text(row(lcm),col(lcm),label=lcm)
lines(alignment$index1,alignment$index2)

ccm <- d$costMatrix
image(x=1:nrow(ccm),y=1:ncol(ccm),ccm)
#text(row(ccm),col(ccm),label=ccm)
lines(alignment$index1,alignment$index2)

######################
# classification with many time series
######################
db <- read.csv("train.csv", header = FALSE, dec=".", sep="")
y <- db$V61
db <- db[,-61]
plot(y)
idx <- sample(1:300, 10)
plot.ts(t(db[idx,]))

d_euclid <- dist(db)
d_dtw <- dist(db, method = "DTW", window.type="sakoechiba", window.size=20)

set.seed(123)
m_euclid <- kmeans(d_euclid, centers = 6)
m_dtw <- kmeans(d_dtw, centers = 6)

round(FM_index(y, m_euclid$cluster)[1] ,3)
round(adjustedRandIndex(y, m_euclid$cluster), 3)

round(FM_index(y, m_dtw$cluster)[1] ,3)
round(adjustedRandIndex(y, m_dtw$cluster), 3)

