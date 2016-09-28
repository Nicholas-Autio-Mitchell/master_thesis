###################### ============================================= ######################
######################  A script to retrieve and prepare macro data  ######################
###################### ============================================= ######################

#' Each section returns a table of data that must be ready to merge via dates -
#' This means have only columns relevant for modelling and WITH a dates column

lsos()
rm(list=ls())

## =========================================== ##
##  Load tickers for each market's components  ##
## =========================================== ##

## First the entire stock data for some markets and all their components is obtained
## Then the indices alone for several more {shanghei SE composite, Nikkei 225, EFA}

## Lists of tickers can be loaded
## load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Market Data - Tickers/index_tickers_18082015.RData")

## tickers_dow = c("^DJI", dax_tickers)
## tickers_dax = c("^GDAXI", dax_tickers)
## tickers_sp500 = c("^GSPC", sp500_tickers)
## tickers_ftse100 = c("^FTSE", ftse100_tickers)

## ============================================= ##
##  Download index data - each index separately  ##
## ============================================= ##

## This downloads the index itself plus each of its components individually

## ## Tickers are all saved in one environment to load as necessary -
## ## this returns the tickers for each market in it's own vector
## load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/tickers.rda")

## ## Save all data in environments (already done so should be in folder to simply load)

## dow <- new.env()
## getSymbols(paste(tickers_dow, sep=";"), start=start_date, end=end_date,
##            env=dow, adjust=TRUE)
## save(dow, file="market_data_dow.rda")

## dax <- new.env()
## getSymbols(paste(tickers_dax, sep=";"), start=start_date, end=end_date,
##            env=dax, adjust=TRUE)
## save(dax, file="market_data_dax.rda")

## ftse100 <- new.env()
## getSymbols(paste(tickers_ftse100, sep=";"), start=start_date, end=end_date,
##            env=ftse100, adjust=TRUE)
## save(ftse100, file="market_data_ftse100.rda")

## sp500 <- new.env()
## getSymbols(paste(tickers_sp, sep=";"), start=start_date, end=end_date,
##            env=sp500, adjust=TRUE)
## save(sp500, file="market_data_sp500.rda")

## ## the following three indices are returned without their components

## ## Shanghei SE Composite
## SSE_comp <- new.env()
## x <- getSymbols("000001.SS", auto.assign = FALSE)
## y <- x["20130114/20150911"]
## z <- subset(as.data.table(y), select = c(1, 7))   # col = 7 -> Adjusted closing prices
## names(z) <- c("index", "SSE_comp")
## SSE_comp$SSE_comp <- z
## save(SSE_comp, file = "market_data_SSE_comp.rda")
## ## Clear workspace
## ##rm(x, y, z)

## ## Nikkei 225
## nikkei225 <- new.env()
## x <- getSymbols("^N225", auto.assign = FALSE)
## y <- x["20130114/20150911"]
## z <- subset(as.data.table(y), select = c(1, 7))    # col = 7 -> Adjusted closing prices
## names(z) <- c("index", "nikkei225")
## nikkei225$nikkei225 <- z
## save(nikkei225, file = "market_data_nikkei225.rda")
## ## Clear workspace
## ##rm(x, y, z)

## ## EFA (Emerging Markets ETF representative)
## EFA <- new.env()
## x <- getSymbols("EFA", auto.assign = FALSE)
## y <- x["20130114/20150911"]
## z <- subset(as.data.table(y), select = c(1, 7))
## names(z) <- c("index", "nikkei225")   # this name is wrong... might still be like that in the data!
## EFA$EFA <- z
## save(EFA, file = "market_data_EFA.rda")

## Load the market (indice) data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_dax.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_dow.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_ftse100.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_sp500.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_SSE_comp.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_nikkei225.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/market_data_EFA.rda")
## Load the macro data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/macro_data.rda")

## If required - the sentiment analysis data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/sentiment_data.rda")

## ## ========================================= ##
## ##  Market (index) data - t-series package   ##
## ## ========================================= ##

## ## Define some variable
## start_date <- as.Date("2013-01-14")
## end_date <- as.Date("2015-09-11")
## headers <- c("Volume", "AdjClose")
## data_source <- "yahoo"
## data_freq <- "d"
## date_format <- "zoo"

## # Extract market data for the the main indices
## dow_data <- get.hist.quote(instrument = "^DJI", start_date, end_date,
##                         quote = c("Close"),
##                         provider = c("yahoo"), method = NULL, compression = "d",
##                         retclass = c("zoo"), quiet = FALSE, drop = FALSE) 

## dax_data <- get.hist.quote(instrument = tickers_dax, start_date, end_date,
##                         quote = c("Close"),
##                         provider = c("yahoo"), method = NULL, compression = "d",
##                         retclass = c("zoo"), quiet = FALSE, drop = FALSE)

## sp500_data <- get.hist.quote(instrument = tickers_sp500, start_date, end_date,
##                         quote = c("Close"),
##                         provider = c("yahoo"), method = NULL, compression = "d",
##                         retclass = c("zoo"), quiet = FALSE, drop = FALSE)

## ftse100_data <- get.hist.quote(instrument = tickers_ftse100, start_date, end_date,
##                         quote = c("Close"),
##                         provider = c("yahoo"), method = NULL, compression = "d",
##                         retclass = c("zoo"), quiet = FALSE, drop = FALSE)

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
                   authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
## This data has a bank holiday in it -> must be removed to match other data
y <- (as.data.table(vola_dow))[-560,]
## Keep only the daily "Close" prices
vola_dow <- as.data.table(y)[, Close, index]

## VIX data from Quandl (volatility of S&P50)
vola_sp500 <- Quandl("YAHOO/INDEX_VIX", type = "xts", order = "desc", collapse = "daily",
                     authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-13", trim_end="2015-09-11")
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
                   authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")

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
               authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
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
                     authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
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
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
oil_WTI <- as.data.table(oil_WTI[,4])

## ICE returns a pretty short time-series, missing ~ 80 days
## ## ICE Brent Crude Oil Futures - continuous contract front month (1 mth)
## oil_ICE <- Quandl("OFDP/FUTURE_B1", type = "xts", order = "desc", collapse = "daily",
##                   authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
## oil_ICE <- as.data.table(oil_ICE[,4])

## Nymex Natural Gas Futures - One month continuous forward month
natural_gas <- Quandl("OFDP/FUTURE_NG1", type = "xts", order = "desc", collapse = "daily",
                      authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
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
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
USD_EUR <- as.data.table(USD_EUR)[,Rate, index]
## Currency - USD vs GBP
USD_GBP <- Quandl("CURRFX/USDGBP", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
USD_GBP <- as.data.table(USD_GBP)[,Rate, index]
## Currency - USD vs JPY
USD_JPY <- Quandl("CURRFX/USDJPY", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
USD_JPY <- as.data.table(USD_JPY)[,Rate, index]
## Currency - USD vs AUS
USD_AUD <- Quandl("CURRFX/USDAUD", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
USD_AUD <- as.data.table(USD_AUD)[,Rate, index]
## Currency - USD vs CAD
USD_CAD <- Quandl("CURRFX/USDCAD", type = "xts", order = "desc", collapse = "daily",
                  authcode="b9TBeCr_F-hMURweE53C", trim_start="2013-01-14", trim_end="2015-09-11")
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


###################### ================================================ ######################
######################  Combine all macro data to append to SA results  ######################
###################### ================================================ ######################

## Create one new environent and store all macro data within
macro_data <- new.env()
macro_data$copper <- copper
macro_data$gold <- gold
macro_data$currency <- currency
macro_data$energy <- energy
macro_data$volatility <- volatility
macro_data$zc_yield <- zc_yield

save(macro_data, file = "macro_data.rda")


##' Data to combine:
##' Sentiment Analysis
##' Index Data
##' Macro Data
##' lagged values for each can also be used
