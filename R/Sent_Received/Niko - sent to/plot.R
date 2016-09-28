rm(list=ls())                         # clean up workspace
print(load("data_dow.Rda"))           # taken from your data_dow.RData
str(data_dow)

## fix some column types
data_dow$ID <- as.numeric(data_dow$ID)
data_dow$myDate <- as.Date(data_dow$Date) # I need days only

## example function to check if dow was mentioned in tweet x
dowMentioned <- function(x, string="dow") string %in% unlist(strsplit(x, " " )) 
data_dow$mentioned <- as.numeric(dowMentioned(data_dow$Tweet, "dow")) # 'dow' always mentioned (as expected)

##==========================================================
## Sol 1 (simplest)
##==========================================================
(dm1 <- table(data_dow$myDate))

##==========================================================
## Sol 2 (aggregate via aggregate function)
##==========================================================
dm2 <- aggregate(mentioned ~ myDate, data = data_dow, sum)
rng <- range(dm2$myDate)

## Get Dow
library(tseries)
x <- get.hist.quote(start=rng[1], end = rng[2], instrument = "^DJI",
                    provider = "yahoo", quote="Close")

##==========================================================
## Plot Dow levels (two representations of the same thing)
##==========================================================
par(mfrow=c(2,1))
plot(dm1, lwd=4, col=4, ylab="Frequency", main="'dow' Tweets")
plot(dm1, lwd=4, col=4, type = "b", ylab="Frequency", main="'dow' Tweets")

##==========================================================
## Plot Dow levels vs. Tweets
##==========================================================
par(mfrow=c(2,1))
plot(dm2, lwd=4, col=2, type="b", ylab="Frequency", main="'dow' Tweets")
plot(x,lwd=4, col=4, type="b",  ylab="Level", main ="Dow Jones")

##==========================================================
## Plot Dow returns vs. Tweets
##==========================================================
library(zoo)                            # for diff(log(x))
par(mfrow=c(2,1))
plot(dm2, lwd=4, col=2, type="b", ylab="Frequency", main="'dow' Tweets")
plot(diff(log(x)*100),lwd=4, col=4, type="b", ylab="Percentage return", main="Returns of Dow Jones (in %)")

## ##==========================================================
## ## Sol 3 (aggregation method via cast function, most versatile function)
## ## just another way of aggregating things
## ##==========================================================
## par(mfrow=c(1,1))
## library("reshape2")                     # cast function used below
## dm3 <- dcast(data_dow, myDate ~ ., sum, value.var="mentioned") # sum of mentions per days
## colnames(dm3) <- c("Date","Mentions")
## dm3$Date <- as.Date(dm3$Date)
## plot(dm3$Date, dm3$Mentions, lwd=4, col=4, type="h", ylab="Sum", main="Frequency of 'dow' Tweets")
