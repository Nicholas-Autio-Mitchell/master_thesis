###################### ======================= ######################
######################  Lagging a time series  ######################
###################### ======================= ######################

## =========== ##
##  Test data  ##
## =========== ##

## You'll need these in the session if not in your .Rprofile
install.packages("quantmod")
library(quantmod)
library(tseries)
d <- rnorm(15)                          # test data

## Create five lags
embed(d, 6)                             # trims NAs

## Do the same with Lag() from {quantmod}
Lag(d, 1:5)                  # leaves NAs
cbind(d, Lag(d, 1:5))        # to include initial column (-> cbind)
## Also returns semi-useful column names!

## ==================================== ##
##  Time-Series Data e.g - zoo objects  ##
## ==================================== ##

zDJI <- get.hist.quote("^DJI", start = "2013-06-01", end = "2013-06-20",
               quote ="Close", retclass = "zoo", quiet = TRUE)

embed(zDJI, 6)
cbind(zDJI, Lag(zDJI, 1:5))             # retains dates in output & add colNames

## ==================================================================== ##
##  Same thing with {quantmod} - excellent for financial data/analysis  ##
## ==================================================================== ##

## Get Dow Jones data
qDJI <- getSymbols("^DJI", auto.assign = FALSE)
DJI <- subset(qDJI["20130601/20130620"], select = 6) 

embed(DJI, 5)

print(x <- cbind(DJI, Lag(DJI, 1:5)))
x[complete.cases(x)]               # chop off the padded area

## Nice built in plotting functions
chartSeries(qDJI)
chartSeries(qDJI, theme="white", TA="addVo();addBBands();addCCI()")
