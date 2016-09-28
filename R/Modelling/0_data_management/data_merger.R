###################### ======================================================== ######################
######################  Combine sentiment analysis data with market/macro data  ######################
###################### ======================================================== ######################

#' This file only merges data that has already been downloaded and is ready to use.
#' The result should contain sentiment analysis data and macroeconomic data.
#' Preprocessing (imputation, log-returns, lagged variables) happens in a separate step.

## Load sentiment analysis data -> this is simply called 'data'
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/sentiment_data.rda")

###################### ============ ######################
######################  Index Data  ######################
###################### ============ ######################

## ============================= ##
##  Append Dow Jones to SA data  ##
## ============================= ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_dow.rda")
myDJI <- dow$DJI
myDJI <- myDJI["20130114/20150911"]
## ## Alternative selection method
##myDJI <- myDJI[ index(myDJI) >= as.Date("2013-01-14") & index(myDJI) <= + as.Date("2015-09-11") ]
myDJI <- as.data.table(myDJI)
myDJI$dates <- as.Date(myDJI$index)
myDJI[, index:=NULL]
myDJI <- subset(myDJI, select = c(DJI.Adjusted, dates))
names(myDJI) <- c("DJI", "dates")
data <- merge(data, myDJI, by.x = "dates", by.y = "dates", all = TRUE)

## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))

## ======================= ##
##  Append DAX to SA data  ##
## ======================= ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_dax.rda")
myDAX <- dax$GDAXI
myDAX <- myDAX["20130114/20150911"]
## ## Alternative selection method
##myDAX <- myDAX[ index(myDAX) >= as.Date("2013-01-14") & index(myDAX) <= + as.Date("2015-09-11") ]
myDAX <- as.data.table(myDAX)
myDAX$dates <- as.Date(myDAX$index)
myDAX[, index:=NULL]
myDAX <- subset(myDAX, select = c(GDAXI.Adjusted, dates))
names(myDAX) <- c("DAX", "dates")
data <- merge(data, myDAX, by.x = "dates", by.y = "dates", all = TRUE)

## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))

## =========================== ##
##  Append FTSE100 to SA data  ##
## =========================== ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_ftse100.rda")
## Load the macro data
myFTSE <- ftse100$FTSE
myFTSE <- myFTSE["20130114/20150911"]
## ## Alternative selection method
##myFTSE <- myFTSE[ index(myFTSE) >= as.Date("2013-01-14") & index(myFTSE) <= + as.Date("2015-09-11") ]
myFTSE <- as.data.table(myFTSE)
myFTSE$dates <- as.Date(myFTSE$index)
myFTSE[, index:=NULL]
myFTSE <- subset(myFTSE, select = c(FTSE.Adjusted, dates))
names(myFTSE) <- c("FTSE", "dates")
data <- merge(data, myFTSE, by.x = "dates", by.y = "dates", all = TRUE)

## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))

## ========================== ##
##  Append S&P500 to SA data  ##
## ========================== ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_sp500.rda")
myGSPC <- sp500$GSPC
myGSPC <- myGSPC["20130114/20150911"]
## ## Alternative selection method
##myGSPC <- myGSPC[ index(myGSPC) >= as.Date("2013-01-14") & index(myGSPC) <= + as.Date("2015-09-11") ]
myGSPC <- as.data.table(myGSPC)
myGSPC$dates <- as.Date(myGSPC$index)
myGSPC[, index:=NULL]
myGSPC <- subset(myGSPC, select = c(GSPC.Adjusted, dates))
names(myGSPC) <- c("GSPC", "dates")
data <- merge(data, myGSPC, by.x = "dates", by.y = "dates", all = TRUE)

## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))

## ============================= ##
##  Append Nikkei223 to SA data  ##
## ============================= ##

## the Asian markets have different holidays (similar story with European vs. American markets)
## this means that we start introducing a lot of NA values -> Imputation becomes more important
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_nikkei225.rda")
myNikkei225 <- nikkei225$nikkei225
myNikkei225 <- as.data.table(myNikkei225)
names(myNikkei225) <- c("dates", "nikkei225")
data <- merge(data, myNikkei225, by.x = "dates", by.y = "dates", all = TRUE)

## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))

## ========================================= ##
##  Append Shanghei SE Composite to SA data  ##
## ========================================= ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_SSE_comp.rda")
mySSE_comp <- SSE_comp$SSE_comp
mySSE_comp <- as.data.table(mySSE_comp)
names(mySSE_comp) <- c("dates", "SSE_comp")
data <- merge(data, mySSE_comp, by.x = "dates", by.y = "dates", all = TRUE)


## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))

## -> Add quite a lot of NAs, may be worth dropping?

## ============================================== ##
##  Append EFA (emerging Markets ETF) to SA data  ##
## ============================================== ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_EFA.rda")
myEFA <- EFA$EFA
myEFA <- as.data.table(myEFA)
names(myEFA) <- c("dates", "EFA")
data <- merge(data, myEFA, by.x = "dates", by.y = "dates", all = TRUE)

## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))


###################### ============ ######################
######################  Macro data  ######################
###################### ============ ######################

## ====================================================================== ##
##  Unpack all macro data and create one data table to append to SA data  ##
## ====================================================================== ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/macro_data.rda")
## Initialise a variable to old all macro data
## give it the dates of the entire data set to merge each macro dat set to
myMacro <- setnames(as.data.table(data$dates), "V1", "dates")
myMacro <- merge(myMacro, macro_data$gold, by.x = "dates", by.y = "index", all = TRUE)
myMacro <- merge(myMacro, macro_data$copper, by.x = "dates", by.y = "index", all = TRUE)
myMacro <- merge(myMacro, macro_data$energy, by.x = "dates", by.y = "index", all = TRUE)
myMacro <- merge(myMacro, macro_data$currency, by.x = "dates", by.y = "index", all = TRUE)
myMacro <- merge(myMacro, macro_data$zc_yield, by.x = "dates", by.y = "index", all = TRUE)
myMacro <- merge(myMacro, macro_data$volatility, by.x = "dates", by.y = "index", all = TRUE)

data <- merge(data, myMacro, by.x = "dates", by.y = "dates", all = TRUE)

## Check how many NAs are introduced - sentiment data began with zero NAs
sum(is.na(data))
## How many rows (days) are still complete, i.e. without NA values?
sum(complete.cases(data))

###################### ================================================ ######################
######################  Remove everything from workspace except 'data'  ######################
###################### ================================================ ######################

rm(list=setdiff(ls(), "data"))
