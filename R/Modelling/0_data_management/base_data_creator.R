###################### ==================================================== ######################
######################  Create one subset to begin modelling a second time  ######################
###################### ==================================================== ######################

#' Here we put together all the data including the original data for indices and other macro data,
#' plus the newly weighted SA scores and the new variable including information from the weekend.
#'
#' The names of the variables are also slightly changed to those used before, so that plots are more usefully labelled
#'
#' The data is also imputed only using one methodology: LOCF
#'
## Each data set should be 697 days long: 2013-01-14 to 2015-09-11

## ======================================= ##
##  Get dates and is.weekend etc. columns  ## --> length = 971
## ======================================= ##

## We will need this data throughout this script
## Load the dates - they were created below!
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/date_columns_complete.rda")

names(date_columns) <- c("Date", "day_name", names(date_columns)[3:length(date_columns)])

## load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_weighted.rda")
## ## Get the date-related columns (dates, days_names, is.monday, is.weekend, day.number)
## myDates <- subset(weighted_sentiment, select = c(1, 2, 3, 4, 5))

## ## ---------------------------------------------- ##
## ##  Get the data for 'is.holiday' dummy variable  ##
## ## ---------------------------------------------- ##

## ## Load the bank holidays; they are saved as a vector of character dates
## load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/holidays.rda")
## holidays <- as.Date(holidays)
## ## add holidays to data_to_compute
## myDates$is.holiday <- as.numeric(myDates$dates %in% holidays)

## ## ------------------------------------------------------------------- ##
## ##  Save the dates in their own file for possible future requirements  ##
## ## ------------------------------------------------------------------- ##

## date_columns <- copy(myDates)
## save(date_columns, file = "date_columns_complete.rda")

## =================================================== ##
##  All Macro data - load and copy required variables  ## --> length = 697
## =================================================== ##

#' Here we are left with two objects. One fir the indices and one for the remaining macro data

## Newly created indice and other macro data, checked for consistency with dates
## These are the log returns, having used a preceding day (2013-01-11) to get values for our first day
## The variable names no longer reflect that they are log returns
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/indice_final.rda")
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/macro_final(no_indices).rda")


## All macro data has been made stationary by taking the log - this is no longer reflected in the naming!!
## Names removed and slightly adjusted
new_macro_names <- c("Date", "copper_cash", "copper_3M", "copper_vol_cash", "copper_vol_3M", #copper
                 "USD_EUR", "USD_GBP", "USD_JPY", "USD_AUD", "USD_CAD", #currencies
                 "oil_WTI", "natural_gas",            #energy
                 "gold_USD", "gold_GBP", "gold_EUR",  #gold
                 "vola_dow", "vola_sp500",            #Volatility
                 "zc_1Y", "zc_2Y", "zc_5Y", "zc_10Y", "zc_15Y", "zc_20Y") #US zero-coupon bonds
names(macro_final) <- new_macro_names

## Reorder the macro_final data alphabetically
setcolorder(macro_final, c("Date", sort(names(macro_final[, 2:ncol(macro_final), with=FALSE]))))

## Have a quick look at the data
names(indice_final)
names(macro_final)
dim(indice_final)
dim(macro_final)
sum(is.na(indice_final))
sum(is.na(macro_final))
identical(macro_final$Date, indice_final$Date)

## Remove trash
rm(new_macro_names)

## ===================================================== ##
##  Sentiment Data 1 - newly weighted individual scores  ## --> length = 971
## ===================================================== ##

## -------------------------------------- ##
##  The sentiment scores ('sent_scaled')  ##
## -------------------------------------- ##

load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_scaled.rda")

## Reorder into alphabetical order
setcolorder(sent_scaled, neworder = sort(names(sent_scaled)))

## ------------------------------------------------------ ##
##  Data that contains the number of actions on each day  ##
## ------------------------------------------------------ ##

load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_weighted.rda")

## Take the subset of only the number of (1) total tweets on that day and (2) total actions that day
action_names <- grep("_total_", names(weighted_sentiment), value = TRUE)
tweets_actions <- subset(weighted_sentiment, select = action_names)

## create new names to match those made for all other covariates e.g. search_term.SA_Model
new_names1 <- gsub(pattern = "_total_", replacement = ".total_", x = action_names)
names(tweets_actions) <- new_names1

## Put in alphabetical order
setcolorder(tweets_actions, sort(names(tweets_actions)))

## Apply new names to be consistent with all other naming so far
## (taken from names(tweets_actions) after the re-ordering above, so it is correct!)
new_names2 <- c("bear_market.total_actions", "bear_market.total_tweets",
               "bull_market.total_actions", "bull_market.total_tweets",
               "dow_jones.total_actions", "dow_jones.total_tweets", #this is the only real change - using lower case for dow_jones
               "dow_SPDR.total_actions", "dow_SPDR.total_tweets",
               "dow_wallstreet.total_actions", "dow_wallstreet.total_tweets",
               "federal_reserve.total_actions", "federal_reserve.total_tweets",
               "financial_crisis.total_actions", "financial_crisis.total_tweets",
               "goldman_sachs.total_actions", "goldman_sachs.total_tweets",
               "interest_rates.total_actions", "interest_rates.total_tweets",
               "market_volatility.total_actions", "market_volatility.total_tweets",
               "obama_economy.total_actions", "obama_economy.total_tweets",
               "oil_prices.total_actions", "oil_prices.total_tweets",
               "stock_prices.total_actions", "stock_prices.total_tweets")
names(tweets_actions) <- new_names2

rm(action_names, new_names1, new_names2, weighted_sentiment)

## ===================================================================================== ##
##  Sentiment Data 2 - newly weighted averages AND new variable using weekend sentiment  ## --> length = 971
## ===================================================================================== ##

## Averaged SA results plus variable with weekend sentiment
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_daily_averages_with_weekend_data.rda")

## Give new names that are consisten with all other data sets
new_SA_avg_names <- c(paste0(names(daily_avg_scores)[1:13], ".Combined"), "SA_with_weekend")
names(daily_avg_scores) <- new_SA_avg_names

## Reorder the columns alphabetcally, additionally placing the variable with weekends SA data at the front
setcolorder(daily_avg_scores, neworder = c("SA_with_weekend", sort(names(daily_avg_scores)[1:13])))

sent_avg <- daily_avg_scores            #the final object

## Remove trash
rm(new_SA_avg_names, daily_avg_scores)


## ===================================================================================== ##
##  Combine all sentiment data, remove weekends and prepare it to merge with macro data  ##
## ===================================================================================== ##

## This is a longer but safer way to do things...
## Add a date column to each of the data sets, then merge according to those dates, then remove weekends

## One data set requires all the date columns in order to have the dummy variable for weekends
sent_avg_to_merge <- cbind(date_columns, sent_avg)

## the reminder only require the Date column
sent_scaled_to_merge <- cbind(date_columns[, .(Date)], sent_scaled)
tweets_actions_to_merge <- cbind(date_columns[, .(Date)], tweets_actions)

## Create a list to use Reduce()
sent_to_merge <- sapply(c("sent_avg_to_merge", "sent_scaled_to_merge", "tweets_actions_to_merge"), function(x) NULL)

sent_to_merge$sent_avg_to_merge <- sent_avg_to_merge
sent_to_merge$sent_scaled_to_merge <- sent_scaled_to_merge
sent_to_merge$tweets_actions_to_merge <- tweets_actions_to_merge

## Custom function to use within Reduce() suppplying the arguments to merge()
custom_merge <- function(x, y) {
    output <- merge(x, y, by.x = "Date", by.y = "Date", all =TRUE)
    return(output)
}

## Merge all sentiment data
sent_final_complete <- Reduce(custom_merge, sent_to_merge)

## Check the output
dim(sent_final_complete)
sum(is.na(sent_final_complete))                    #zero

## Save a copy of the data at this stage (might be useful later e.g.for plotting)
save(sent_final_complete, file = "sentiment_data_final_complete.rda")

## Remove weekends
sent_final_weekdays <- sent_final_complete[is.weekend == 0]
## Keep the dates columns here, as they will be required somewhere for the final data table

## Sanity checks
dim(sent_final_weekdays)
sum(is.na(sent_final_weekdays))

## Save a copy
save(sent_final_weekdays, file = "sentiment_data_final_weekdays.rda")

## Rename
sent_final <- sent_final_weekdays       #final object

## Remove trash
rm(list = setdiff(ls(), c("indice_final", "macro_final", "sent_final")))

###################### =============================== ######################
######################  Merge all data into one table  ######################
###################### =============================== ######################

## The sentiment data 'sent_final' contains all the date columns that will be required in the end procuct
## The indice and macro data sets contain only a Date column with which we can merge the data

## Custom function to use within Reduce() suppplying the arguments to merge()
custom_merge <- function(x, y) {
    output <- merge(x, y, by.x = "Date", by.y = "Date", all =TRUE)
    return(output)
}

## Merge indice amd other macro data
macro_all <- merge(indice_final, macro_final, by.x = "Date", by.y = "Date")
## Merge the sentiment data with the macro_all set
all_data_final <- merge(sent_final, macro_all, by.x = "Date", by.y = "Date")

## Save this final data set before create chosen subsets and lags
save(all_data_final, file = "all_data_final.rda")

## -------------------- ##
##  Problem identified  ## --> found at this step previously. Preceding work was improved to fix it
## -------------------- ##
## These dates have no information for sentiment analysis data
#' they are not bank holidays in north america.
#' An explanation could be a probelem during scraping, but would be impossible to pinpoint where.
#' Twitter website crashing on those days, didn't load tweets for those days, or the scraper malfunctioning.
"""2014-05-11 (a Sunday) & 2014-10-18 (a Saturday)"""
#' We simply impute them with the spline method. This seems the most neutral.
#' Filling them with a value of 0 to imply neutral sentiment may actual appear as a jump
#' if for example the surounding data had been either very positive or negative.

## take out, spline impute and reinsert those dates?

## All macro data was obtained and imputed once again with checks at each stage for dates, now everything is as expected
## see 'get_log_market_data.R' for information on how this was performed
## ---------------- ##
##  Problem solved  ##
## ---------------- ##

"""
Looking at this data makes it quite clear that the 'dow_SPDR' data should be removed as it is very sparse.
We can also use the nearZV() functions to show that it doesn't contain hardly any variance.
"""


###################### ================ ######################
######################  Create subsets  ######################
###################### ================ ######################



## ========================================= ##
##  Create structure to hold all final data  ##
## ========================================= ##

base_data <- sapply(c("lag1", "lag2", "lag3", "lag4", "lag5"), function(x) NULL)
