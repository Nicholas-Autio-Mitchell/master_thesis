###################### ======================================== ######################
######################  Create simple plots to depict all data  ######################
###################### ======================================== ######################

## ================================================================================= ##
##  This is the same as data_visualiser.R but updated to reflect newer data objects  ##
## ================================================================================= ##


#' We can show that there are noteworthy relationships or better, loose trends, between the market data used and the sentiment data obtained, as well as between the market returns and the traditional predictors, such as gold, or volatility.

## TO_DO -> create a new data set/add some columns to explore the data in more detail
## For example - the names of the days to use as facets, keep is.monday, avg. sentiment scores

## ================ ##
##  Load some data  ##
## ================ ##

## Load most current modelling data
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/modelling_data.rda") #27-11-2015
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/ready_to_boost.rda") #10-12-2015
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/all_nominal_data.rda") #25-01-2015

#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_dirty.rda")
## Choose which data set exactly should be used
din0 <- all_nominal_data$locf$locf_L0
din1 <- all_nominal_data$locf$locf_L1 # with one lag

## is.weekend is all zeros as they have already been removed
din0[, is.weekend:=NULL]
din1[, is.weekend:=NULL]

## See what we have available to plot
names(din0)

## ================================================= ##
##  Some common trends can be shown within our data  ##
## ================================================= ##

dev.new()

## Show evolution of Dow Returns - market is slowy climbing and the dispersion is too -- higher vola?
qplot(index, log_ret_DJI, data = din0, color = DJI, # dispersion (read: vola!) increases with abs DJI
      main = "Log returns of Dow Jones - colour scale reflects index price",
      xlab = "Date", ylab = "Log returns", geom = c("point"))
## Show evolution of Dow against gold prices in USD
qplot(index, log_ret_DJI, data = din0, color = gold_USD, # vs. gold in dollars
            main = "Log returns of Dow Jones - colour scale reflects gold prices in USD",
      xlab = "Date", ylab = "Log returns", geom = c("point"))
## Show evolution of Dow against gold prices in EUR
qplot(index, log_ret_DJI, data = din0, color = gold_EURO, # vs. EURO - can we see currency devaluation?
            main = "Log returns of Dow Jones - colour scale reflects gold priced in EUR",
      xlab = "Date", ylab = "Log returns", geom = c("point"))
## Same as before with smoothed line superimposed
qplot(index, log_ret_DJI, data = din0, color = gold_EURO, geom = c("point", "smooth"), # Add smoothing
      main = "Log returns of Dow Jones - colour scale reflects gold priced in EUR",
      xlab = "Date", ylab = "Log returns")
## Display the volatility of the Dow versus the log returns
qplot(index, log_ret_DJI, data = din0, color = vola_dow,  #
      main = "Log returns of Dow Jones\nColour scale reflects the volatility of Dow according to the market indicator (not the data itself)",
      xlab = "Date", ylab = "Log returns")
## The dow returns against a broader voliatiliy measure . the VIX
qplot(index, log_ret_DJI, data = din0, color = vola_sp500, # vs. more general market vola, S&P500
      main = "Log returns of Dow Jones\nColour scale reflects the volatility of S&P500 according to the VIX indicator",
      xlab = "Date", ylab = "Log returns")

## Some clear correlation values, as expected
cor(din0$vola_dow, din0$DJI) # positively correlated normal, higher returns = higher vola in those returns
cor(din0$vola_sp500, din0$DJI) # little correlation across wider range of stocks
cor(din0$DJI, din0$gold_USD) # neg correlation, gold used as trade off asset to stock. Market climbs gold falls
cor(din0$vola_sp500, din0$gold_USD) # Higher market vola means less certainty, so people run to gold
cor(din0$vola_dow, din0$vola_sp500) # high correlation in speed of  movement between stocks listed in America

## Inspect volatility of all data versus just mondays
qplot(vola_dow, vola_sp500, data = din0, geom = c("point", "smooth"))
qplot(vola_dow, vola_sp500, data = din0, geom = c("point", "smooth"), facets = is.monday~.) # No big difference -> MAs, over 30 days

## Inspect nominal Dow value versus just Mondays
qplot(DJI, vola_dow, data = din0, geom = c("point", "smooth"))
qplot(DJI, vola_dow, data = din0, geom = c("point", "smooth"), facets = is.monday~.) # No big difference -> MAs, over 30 days

## Inspect Dow log returns versus volatility versus just Mondays
## clearly the lower limit on Monday returns is higher and the volatility is also higher -> compensating for weekend information
qplot(log_ret_DJI, vola_dow, data = din0, geom = c("point", "smooth"))
qplot(log_ret_DJI, vola_dow, data = din0, geom = c("point", "smooth"), facets = .~ is.monday,
      main = "Dow log returns vs. Dow volatility indicator\n(left) Tuesday to Friday; (right) Mondays only",
      xlab = "Dow log returns", ylab = "Volatility of Index")
