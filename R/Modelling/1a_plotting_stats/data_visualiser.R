###################### ================================================================ ######################
######################  Various plots to investigate the data and show SA significance  ######################
###################### ================================================================ ######################

#' We can show that there are noteworthy relationships or better, loose trends, between the market data used and the sentiment data obtained, as well as between the market returns and the traditional predictors, such as gold, or volatility.

## TO_DO -> create a new data set/add some columns to explore the data in more detail
## For example - the names of the days to use as facets, keep is.monday, avg. sentiment scores

## ================ ##
##  Load some data  ##
## ================ ##

## Load most current modelling data
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/modelling_data.rda") #27-11-2015
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/ready_to_boost.rda") #10-12-2015
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_dirty.rda")
## Choose which data set exactly should be used
din <- ready_to_boost$ds_locf_L0    # with no lags
din <- as.data.table(ready_to_boost$ds_locf_lags$ds_locf_L1) # with one lag

## See what we have available to plot
names(din)

## ================================================= ##
##  Some common trends can be shown within our data  ##
## ================================================= ##

dev.new()
qplot(dates, log_ret_DJI, data = din, color = DJI) # dispersion (read: vola!) increases with abs DJI
qplot(dates, log_ret_DJI, data = din, color = gold_USD) # vs. gold in dollars
qplot(dates, log_ret_DJI, data = din, color = gold_EURO) # vs. EURO - can we see currency devaluation?
qplot(dates, log_ret_DJI, data = din, color = gold_EURO, geom = c("point", "smooth"), span = 0.25) # Add smoothing
qplot(dates, log_ret_DJI, data = din, color = vola_dow)  # 
qplot(dates, log_ret_DJI, data = din, color = vola_sp500) # vs. more general market vola, S&P500
## Some clear correlations, as expected
cor(din$vola_dow, din$DJI)
cor(din$vola_sp500, din$DJI)
cor(din$DJI, din$gold_USD)
cor(din$vola_sp500, din$gold_USD)
cor(din$vola_dow, din$vola_sp500)
qplot(vola_dow, vola_sp500, data = din)
qplot(vola_dow, vola_sp500, data = din, facets = is.monday~.) # No big difference -> MAs, over 30 days

## ============================================================= ##
##  Now explore relationships involving sentiment analysis data  ##
## ============================================================= ##

dev.new()
qplot(dates, log_ret_DJI, data = din, color = L1_avg_federal_reserve)

qplot(dates, log_ret_DJI, data = din, color = avg_Dow_Jones)
qplot(dates, log_ret_DJI, data = din, facets = is.monday~., color = avg_Dow_Jones)
qplot(dates, log_ret_DJI, data = din, facets = is.monday~., color = Dow_Jones_Sentistrength_pos)
qplot(dates, log_ret_DJI, data = din, facets = is.monday~., color = Dow_Jones_Sentiment140)
qplot(dates, log_ret_DJI, data = din, facets = is.monday~., color = Dow_Jones_num_tweets)
qplot(dates, log_ret_DJI, data = din, facets = is.monday~., color = L1_avg_Dow_Jones)

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_dirty.rda")

qplot(dates, log_ret_DJI, data = din, facets = is.monday~., color = is.monday)

## To use facets by days
qplot(dates, vola_dow, data = din, facets = is.monday~.)
dind <- data_dirty
dind$log_ret_DJI <- diff(log(dind$DJ))
## Try to use a daily proxy to vola from the sentiment analysis
qplot(dates, log_ret_DJI, data = dind, color = dind$Dow_Jones_Vader_decimal, facets = is.monday ~ .)
qplot(dates, DJI, data = dind, color = dind$Dow_Jones_Vader_decimal, facets = days_names ~ .)
qplot(dates, DJI, data = din, colour = avg_Dow_Jones, geom = c("point", "smooth"), span = 0.4)
qplot(dates, GSPC, data = din, colour = avg_Dow_Jones, geom = c("point", "smooth"), span = 0.2)
## See if simply pos+ or neg- makes it easier to realise
din$dow_posneg <- ifelse(din$avg_Dow_Jones >= 0, 1, 0)
qplot(dates, DJI, data = din, colour = dow_posneg, geom = c("point", "smooth"), span = 0.9) # not much to see
qplot(dates, DJI, data = din, colour = dow_posneg, geom = c("point", "smooth"), span = 0.5) # mostly below line
qplot(dates, DJI, data = din, colour = dow_posneg, geom = c("point", "smooth"), span = 0.1) # all below line

## Use a cubic splines to smooth the data
library(splines)
qplot(dates, DJI, data = din, colour = dow_posneg,
      method = "lm", formula = y ~ ns(x, 5),
      geom = c("point", "smooth")) # does it show anything?
qplot(dates, DJI, data = din, colour = dow_posneg,
      method = "lm", formula = y ~ ns(x, 20),
      geom = c("point", "smooth")) # We captcher most of the points
qplot(dates, DJI, data = din, colour = dow_posneg,
      method = "lm", formula = y ~ ns(x, 90),
      geom = c("point", "smooth")) # are we over-fitting here?

qplot(dates, DJI, data = din, colour = dow_posneg,
      method = "lm", formula = y ~ ns(x, 50),
      geom = c("line", "smooth")) # maybe clearer to see

## Same last two plots for the S&P500 against sentiment of the Dow
qplot(dates, GSPC, data = din, colour = dow_posneg,
      method = "lm", formula = y ~ ns(x, 90),
      geom = c("point", "smooth")) # are we over-fitting here?

qplot(dates, GSPC, data = din, colour = dow_posneg,
      method = "lm", formula = y ~ ns(x, 50),
      geom = c("line", "smooth")) # maybe clearer to see

## Density plots of the log_ret_DJI data
qplot(log_ret_DJI, ..density.., data = dind,
  geom = "histogram", binwidth = 0.0003, xlim = c(-0.035, 0.04))
## Plot the density of log returns from down jones, separated by daya
qplot(log_ret_DJI, ..density.., data = dind, facets = is.monday ~ .,
  geom = "histogram", binwidth = 0.0003, xlim = c(-0.035, 0.04))
## Really need to add the day_names column to the ready_to_boost data set!!!

## =================================================================================================== ##
##  Do some boxplots, showing a market feature with sentiment conditional on day or weekend sentiment  ##
## =================================================================================================== ##

#' Compare some boyplots and jitter plots
#' Alternate the opacity for the kitter plots if necessary - example code:

## qplot(color, price / carat, data = diamonds, geom = "jitter",
##  alpha = I(1 / 5))
## qplot(color, price / carat, data = diamonds, geom = "jitter",
##  alpha = I(1 / 50))
## qplot(color, price / carat, data = diamonds, geom = "jitter",
##  alpha = I(1 / 200))


## ================================================================================= ##
##  Try some histograms plots, filling the columns with the returns splot into days  ##
## ================================================================================= ##

#' Do Mondays on average account for most of the returns, or less? other days?
#' How can this be explained by the sentiment analysis?


## ================================== ##
##  Violin plots over the days_names  ##
## ================================== ##

#' We can plot the returns or some other factors of interest on a daily separation
#' Not quite like facetting but similar
#' The 'violin' shows the density
