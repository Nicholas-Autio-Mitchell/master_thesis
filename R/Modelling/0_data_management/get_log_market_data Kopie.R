###################### ============================================ ######################
######################  A script to get log returns for macro data  ######################
###################### ============================================ ######################

## Only the essentials are collected here - see "get_market_data.R" for more options

#' Each section returns a table of data that must be ready to merge via dates -
#' This means have only columns relevant for modelling and WITH a dates column

lsos()
rm(list=ls())

## ============================================================================ ##
##  Taken and adapted from data_enhancer.R - there was used for Dow Jones only  ##
## ============================================================================ ##

## Create base data set, removing weekends and col: day_names
ds <- data_to_impute[is.weekend == 0] %>%
    subset(., select = c(1, seq(3, 164)))

## Add a column for the log returns of the DJI
## filling in from the week before to keep length
getSymbols(c('^GDAXI', "^FTSE", "GSPC", "^N225", "000001.SS", "EFA"))
#' Once the variable for the SSE is auto assigned, its name begin with digits...
#' which means ESS can't read it. Need to auto.assign with a name beginning with characters
#' --> Do the same for all others to be consistent
SSE = getSymbols(Symbols =  c("000001.SS"), auto.assign = FALSE)
myDAX = getSymbols(Symbols =  c("^GDAXI"), auto.assign = FALSE)
myFTSE = getSymbols(Symbols = c("^FTSE"), auto.assign = FALSE)
myGSPC = getSymbols(Symbols = c("^GSPC"), auto.assign = FALSE)
myN225 = getSymbols(Symbols = c("^N225"), auto.assign = FALSE)
mySSE = getSymbols(Symbols = c("^000001.SS"), auto.assign = FALSE)
myEFA = getSymbols(Symbols = c("^EFA"), auto.assign = FALSE)

dax <- myDAX["20130111/20150911"]
dax <- dax[,6]
log_ret_DAX <- diff(log(dax))
ftse <- myFTSE["20130111/20150911"]
ftse <- ftse[,6]
log_ret_FTSE <- diff(log(ftse))
gspc <- myGSPC["20130111/20150911"]
gspc <- gspc[,6]
log_ret_GSPC <- diff(log(gspc))
N225 <- myN225["20130111/20150911"]
N225 <- N225[,6]
log_ret_nikkei225 <- diff(log(N225))
SSE_comp <- mySSE["20130111/20150911"]
SSE_comp <- SSE_comp[,6]
log_ret_SSE_comp <- diff(log(SSE_comp))
EFA <- myEFA["20130111/20150911"]
## EFA <- EFA[,6]   # Not required
log_ret_EFA <- diff(log(EFA))

## Merging works better as data.tables
dtDAX <- as.data.table(log_ret_DAX)
dtFTSE <- as.data.table(log_ret_FTSE)
dtGSPC <- as.data.table(log_ret_GSPC)
dtnikkei225 <- as.data.table(log_ret_nikkei225)
dtSSE_comp <- as.data.table(log_ret_SSE_comp)
dtEFA <- as.data.table(log_ret_EFA)

## Create a list with data to merge using Reduce()
lind <- sapply(c("dtDAX", "dtFTSE", "dtGSPC", "dtnikkei225", "dtSSE_comp", "dtEFA"), function(x) NULL) #logIndex
lind$dtDAX <- dtDAX
lind$dtFTSE <- dtFTSE
lind$dtGSPC <- dtGSPC
lind$dtnikkei225 <- dtnikkei225
lind$dtSSE_comp <- dtSSE_comp
lind$dtEFA <- dtEFA

## Define function to merge data by
lind_merge <- function(x, y){merge(x, y,
                            by.x = "index",
                            by.y = "index",
                            all = TRUE)}

x <- as.data.table(Reduce(lind_merge, lind))
names(x) <- c("dates", "log_ret_DAX", "log_ret_FTSE","log_ret_GSPC",
              "log_ret_nikkei225", "log_ret_SSE_comp", "log_ret_EFA")
## Get rid of the first row of NAs producd by taking log returns
x1 <-  x[2:695]
## Save the names to reapply after imputating
lind_names <- names(x1)
## Impute (only locf is )
x2 <- as.data.table(na.locf(as.xts(x1)))
names(x2) <- lind_names

indices_log_ret_data <- x2
indices_log_ret <- new.env()
indices_log_ret$indices_log_ret_data <- indices_log_ret_data
## Save for later use
save(indices_log_ret, file = "indices_log_ret.rda")

## ================================ ##
##  Macroeconomic data from Quandl  ##
## ================================ ##

#' !! It is assumed that the correct time-series is obtained!!
#' this is why we are able to select only the data-holding columns,
#' neglecting the 'dates' column and using 'cbind' instead of merge

## Extract data using Quandl package

## ## Sort by date to align with all other data before merging
## DJI_vola[order(as.Date(DJI_vola$Date, format="%Y-%m-%d")),]

## ============================ ##
##  Vola - Returns: volatility  ##
## ============================ ##

## CBOE Vola from Quandl
vola_dow <- Quandl("CBOE/VXD", type = "xts", order = "desc", collapse = "daily",
                   authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
## This data has a bank holiday in it -> must be removed to match other data
y <- (as.data.table(vola_dow))[-560,]
## Keep only the daily "Close" prices
vola_dow <- as.data.table(y)[, Close, index]

## VIX data from Quandl (volatility of S&P50)
vola_sp500 <- Quandl("YAHOO/INDEX_VIX", type = "xts", order = "desc", collapse = "daily",
                     authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
## Keep only the daily "Close" prices
vola_sp500 <- as.data.table(vola_sp500)[, Close, index]

## Combine into one data table and rename
volatility <- subset(cbind(vola_dow, vola_sp500), select = -3)
names(volatility)  <- c("index", "vola_dow", "vola_sp500")

## No NAs after merge
##sum(is.na(volatility))

## ================================= ##
##  US Treasury - Returns: zc_yield  ##
## ================================= ##

## American zero-coupon bond
zc_yield <- Quandl("FED/SVENY", type = "xts", order = "desc", collapse = "daily",
                   authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")

## We could choose only certain maturities here e.g. 1, 2, 5, 10, 15, 20 years
zc_yield  <- subset(zc_yield, select = c(SVENY01, SVENY02, SVENY05,
                                         SVENY10, SVENY15, SVENY20))
## Convert to data table and rename columns
zc_yield <- as.data.table(zc_yield)
names(zc_yield)  <- c("index", "zc_US_1Y", "zc_US_2Y", "zc_US_5Y", "zc_US_10Y", "zc_US_15Y", "zc_US_20Y")

## No NAs
##sum(is.na(zc_yield))

## ============================= ##
##  Commodities - Returns: gold  ##
## ============================= ##

## Gold from Quandl - price per troy ounce
gold <- Quandl("LBMA/GOLD", type = "xts", order = "desc", collapse = "daily",
               authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
#' There are both AM and PM prices for dollar, pound and euro
#' We could select to use only the closing prices, i.e. PM
##gold  <- subset(gold, select = c("USD (PM)", "GBP (PM)", "EURO (PM)"))
gold <- gold[,c(2, 4, 6)]

## We must rename to conform to naming rules in data.tables
names(gold) <- paste("gold", c("USD", "GBP", "EURO"), sep = "_")
## Save as data.table
gold <- as.data.table(gold)

## =============================== ##
##  Commodoties - Returns: copper  ##
## =============================== ##

## Copper prices - London Metal Exchange
copper_all <- Quandl("LME/PR_CU", type = "xts", order = "desc", collapse = "daily",
                     authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
#' this results in a selection of prices, but many are NA
#' We get complete sets for: cash buyer, cash seller & settlement, 3-months-buyer, 3-months-seller.
#' We can use these four time-series, plus the buy-sell delta for each as a proxy for volatility (in commodities)

## Keep date column for later use
copper_dates <- subset(as.data.table(copper_all), select = 1)

## Calculate the spot and 3-month prices as averages of bid/ask prices
copper_cash <- as.data.table(apply(copper_all[,c(1,2)], MARGIN = 1, function(x) mean(x)))
copper_3M <- as.data.table(apply(copper_all[,c(3,4)], MARGIN = 1, function(x) mean(x)))

## Calculate the two volatility proxies as bid/ask spread for the spot and 3-month prices respectively
copper_vol_spot <- as.data.table(copper_all[,2] - copper_all[,1])
copper_vol_3M <- as.data.table(copper_all[,4] - copper_all[,3])

## Combine all copper values
copper <- subset(cbind(copper_dates, copper_cash, copper_3M, copper_vol_spot, copper_vol_3M), select = c(1, 2, 3, 5, 7))
names(copper) <- c("index", "copper_cash", "copper_3M", "copper_vol_spot", "copper_vol_3M")

## =============================== ##
##  Commodities - Returns: energy  ##
## =============================== ##

## WTI Crude Futures - Continuous contract front month (1 mth)
oil_WTI <- Quandl("CHRIS/ICE_T1", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
oil_WTI <- as.data.table(oil_WTI[,4])

## ICE returns a pretty short time-series, missing ~ 80 days
## ## ICE Brent Crude Oil Futures - continuous contract front month (1 mth)
## oil_ICE <- Quandl("OFDP/FUTURE_B1", type = "xts", order = "desc", collapse = "daily",
##                   authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
## oil_ICE <- as.data.table(oil_ICE[,4])

## Nymex Natural Gas Futures - One month continuous forward month
natural_gas <- Quandl("OFDP/FUTURE_NG1", type = "xts", order = "desc", collapse = "daily",
                      authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
natural_gas <- as.data.table(natural_gas[,4])

## Combine oil and gas data sets -> merging by date, which creates some NAs
energy <- merge(oil_WTI, natural_gas, by.x = "index", by.y = "index", all = TRUE)
names(energy) <- c("index", "oil_WTI", "natural_gas")

## Returns some NAs - the WTI time-series is a bit longer than usual ~691 c.p. ~670
## sum(is.na(energy))
## [1] 23

## =================================================== ##
##  Currencies against the Dollar - Returns: currency  ##
## =================================================== ##

## Using Quandl
## Currency - USD vs EUR
USD_EUR <- Quandl("CURRFX/USDEUR", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
USD_EUR <- as.data.table(USD_EUR)[,Rate, index]
## Currency - USD vs GBP
USD_GBP <- Quandl("CURRFX/USDGBP", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
USD_GBP <- as.data.table(USD_GBP)[,Rate, index]
## Currency - USD vs JPY
USD_JPY <- Quandl("CURRFX/USDJPY", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
USD_JPY <- as.data.table(USD_JPY)[,Rate, index]
## Currency - USD vs AUS
USD_AUD <- Quandl("CURRFX/USDAUD", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
USD_AUD <- as.data.table(USD_AUD)[,Rate, index]
## Currency - USD vs CAD
USD_CAD <- Quandl("CURRFX/USDCAD", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-11", trim_end="2015-09-11")
USD_CAD <- as.data.table(USD_CAD)[,Rate, index]

## Create empty list with element names to assign the data.tables to it
curr_list <- vector("list", length = 5)
curr_list[[1]] = USD_EUR
curr_list[[2]] = USD_GBP
curr_list[[3]] = USD_JPY
curr_list[[4]] = USD_AUD
curr_list[[5]] = USD_CAD

myNames <- c("USD_EUR", "USD_GBP", "USD_JPY", "USD_AUD", "USD_CAD")
names(curr_list)  <- myNames

## Make sure each name is unique to allow Reduce function below to work
for( i in 1:length(myNames)) {names(curr_list[[i]]) <- c("index", myNames[i])}

## Define a function to apply our own args in merge() for use in Reduce below
curr_merge <- function(x, y) {merge(x, y, by.x = "index", by.y = "index", all = TRUE)}

## Merge all to one data table
currency <- Reduce(curr_merge, curr_list)

## Zero NA values returned, however the time-series is longer than expected: ~695
## sum(is.na(currency))

## ## The FX data can also be downloaded with Quantmod
## ## This includes extrapolated values for weekends, as Oanda also collects the (sparse!) WE data
## ## Currency - USD vs EUR
## USD_EUR <- getFX("USD/EUR", from = "2013-01-14", to = "2015-09-11", auto.assign = FALSE)
## ## Currency - USD vs GBP
## USD_GBP <- getFX("USD/GBP", from = "2013-01-14", to = "2015-09-11", auto.assign = FALSE)
## ## Currency - USD vs JPY
## USD_JPY <- getFX("USD/JPY", from = "2013-01-14", to = "2015-09-11", auto.assign = FALSE)
## ## Currency - USD vs AUS
## USD_AUS <- getFX("USD/AUS", from = "2013-01-14", to = "2015-09-11", auto.assign = FALSE)
## ## Currency - USD vs CAD
## USD_CAD <- getFX("USD/CAD", from = "2013-01-14", to = "2015-09-11", auto.assign = FALSE)


## =========================== ##
##  Combine all data and save  ##
## =========================== ##
## Create one new environent and store all macro data within
macro_raw_log_data <- new.env()
macro_raw_log_data$copper <- copper
macro_raw_log_data$gold <- gold
macro_raw_log_data$currency <- currency
macro_raw_log_data$energy <- energy
macro_raw_log_data$volatility <- volatility
macro_raw_log_data$zc_yield <- zc_yield

save(macro_raw_log_data, file = "macro_raw_log_data.rda")

###################### ========================================= ######################
######################  Create the log return for each data set  ######################
###################### ========================================= ######################

## ## Index data - already done and saved as log returns
## load("/Volumes/Mac OS Drive/Thesis/Source Code/R/raw_data/indices_log_ret.rda")

## Macro data - load, create log returns and save

#' This data must be processed in segments, i.e. one data set at a time.
#' This is because the time-series differ somewhat
#' 'xts' objects work most cleanly

## First create a list to store all the cleaned log data

macro_log_data <- sapply(c("copper", "currency", "energy", "gold", "volatility", "zc_yield"), function(x) NULL)

## ======== ##
##  Copper  ##
## ======== ##

x <- macro_raw_log_data$copper
copper <- diff(log(as.xts(x)))[-1,]
macro_log_data$copper <- as.data.table(copper)

## ========== ##
##  Currency  ##
## ========== ##

x <- macro_raw_log_data$currency
currency <- diff(log(as.xts(x)))[-1,]
macro_log_data$currency <- as.data.table(currency)

## ======== ##
##  Energy  ##
## ======== ##

x <- macro_raw_log_data$energy
energy <- diff(log(as.xts(x)))[-1,]
macro_log_data$energy <- as.data.table(energy)

## ====== ##
##  Gold  ##
## ====== ##

x <- macro_raw_log_data$gold
gold <- diff(log(as.xts(x)))[-1,]
macro_log_data$gold <- as.data.table(gold)

## ============ ##
##  Volatility  ##
## ============ ##

x <- macro_raw_log_data$volatility
volatility <- diff(log(as.xts(x)))[-1,]
macro_log_data$volatility <- as.data.table(volatility)

## ========== ##
##  ZC_Yield  ##
## ========== ##

x <- macro_raw_log_data$zc_yield
zc_yield <- diff(log(as.xts(x)))[-1,]
macro_log_data$zc_yield <- as.data.table(zc_yield)


## Check final results
ls.str(macro_log_data)
sum(is.na(macro_log_data))

## Save the macro log data
macro_log_ret <- new.env()
macro_log_ret <- macro_log_data
save(macro_log_ret, file = "macro_log_ret.rda")


###################### =============================================================== ######################
######################  Create one data table to create lagged data and merge with SA  ######################
###################### =============================================================== ######################

## Define function to merge all log macro data
lmacro_merge <- function(x, y){merge(x, y,
                            by.x = "index",
                            by.y = "index",
                            all = TRUE)}

## Put all log macro data together into one data table
x <- as.data.table(Reduce(lmacro_merge, macro_log_ret))

## Combine with the SA data (including the dates column - used to merge)
#' This data is simply some working version of the data, but could be otherwise taken from the saved files
#' it has been imputed, but still includes 

names(x) <- c("dates", "log_ret_DAX", "log_ret_FTSE","log_ret_GSPC",
              "log_ret_nikkei225", "log_ret_SSE_comp", "log_ret_EFA")

