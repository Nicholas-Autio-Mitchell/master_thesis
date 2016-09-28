
###################### ====================================================== ######################
######################  Scale all sentiment results, maintaining dispersion   ######################
###################### ====================================================== ######################

#' All sentiment scores are scaled within their relative subsets
#' The new scale is from -1 to +1
#' This is done using a linear transformation, with the factor for scaling
#' being the absolute maximum of that subet.
#' This ensures our mean and variance also scale linearly.
#' If we were to scale with a regular (normalisation) method, the mean would be shifted in a way
#' that could force it to change it's sign, which would alter our interpretation
#' of the results. A positive results means positive sentiment and vice-versa.

## Package required to calculate statistics for the SA scores of each search term
library(pastecs)

## ======================================= ##
##  Aggregate the SA results for weekends  ##
## ======================================= ##

## We use the 'data_dirty.rda' data collection
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/data_dirty.rda")
## Use the upgraded (weighted) sentiment data
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_weighted.rda")
sent <- weighted_sentiment

## ## We need the following column indices
## 11, 12, 13, 14, 15, 16
## 22, 23, 24, 25, 26, 27
## 33, 34, 35, 36, 37, 38

## Find all the column indices of the Sentiment Analysis results in order to scale them
num_col_per_search_term <- 11
n_reps <- ceiling(length(sent)/num_col_per_search_term)
diffs <- c(11, rep(c(1, 1, 1, 1, 1, 6), n_reps))
myCols <- cumsum(diffs) %>% .[. <= 148]  #121 = last columns of SA results

## Take subset of all sentiment data (we are avoiding the number of tweets etc.)
## NOTE: subsetting a data table makes a copy, that is changing the subset does not also change the original data
sub_sent <- subset(sent, select = myCols)

###################### ========================== ######################
######################  Scale the sentiment data  ######################
###################### ========================== ######################

## We break the data into is groups (search terms) in order to scale them
## We also create a descriptive statistics table for each search term
## See file: "sentiment_data_stats.rda"

## Names of the models
mod_names <- c("Emolex", "Sentiment140", "SentiStrength", "Vader", "Vader_Afinn")
## Names of the search terms
searchTerms <- c("bull_market", "bear_market", "dow_jones", "dow_SPDR", "dow_wallstreet", "federal_reserve",
                  "financial_crisis", "goldman_sachs", "interest_rates", "market_volatility", "obama_economy",
                  "oil_prices", "stock_prices")

## Initialise lists to store stats
sent_stats <- sapply(searchTerms, function(x) NULL)
sent_minmax <- sapply(searchTerms, function(x) NULL)

## ============================ ##
##  NOT RUN - some comparisons  ##
## ============================ ##

## ## Compare the results that come as a results of NOT averaging the SentiStrength scores
## rowMeans(subset(bull_market, select = seq(1, 6)))
## ## Direct comparison - visual inspection
## data.table(x, sent$bull_market_avg)
## ## identical(x, sent$bull_market_avg)
## ## FALSE
##
## ## Compare different ways to find the mean of the rows
## x <- data.table(pos = sent$bull_market_Sentistrength_pos, neg = sent$bull_market_Sentistrength_neg)
## x$original <- bull_market$SentiStrength                    #original output from above
## x[, avg := apply(.SD, 1, mean), .SDcols = c("pos", "neg")] #by reference
## x$my_avg <- rowSums(x[, .(pos, neg)])/2                    #subsetting with data.table but using rowSums
## x                                                          #have a look

## ========================================================= ##
##  Calculate stats for each search term's SA model results  ##
## ========================================================= ##

## Compute stats for all search terms and use them to scale all SA scores between +/- 1
## output is save(sent_scaled, file = "sentiment_data_scaled.rda") --> further below

## Create one data table that contains all the SA scores, ready to scale
SA_scores_non_scaled <- sapply(searchTerms, function(x) NULL)

## --------------------------------------------------------- ##
##  Complete example for the first search term: bull_market  ##
## --------------------------------------------------------- ##

## select just the bull market data (six columns, one for each SA model output)
bull_market <- c(names(sub_sent)[grep("^bull_market", names(sub_sent))]) %>%
    subset(sub_sent, select = .)

## Create one value for SentiStrength
## This also removes the scaling issue, ranging [-1 to -5] and [+1 to +5] (nothing between -1 and +1)
bull_market$SentiStrength <- rowMeans(data.table(bull_market$bull_market_Sentistrength_pos, bull_market$bull_market_Sentistrength_neg))

## Alter names and remove original SentiStrength column (the separate pos and neg values)
bull_market <- subset(bull_market, select = c("bull_market_Emolex", "bull_market_Sentiment140",
                                              "bull_market_Vader_Afinn", "bull_market_Vader", "SentiStrength")) %>%
    setcolorder(., c(1, 2, 5, 4, 3))

## Apply nicer names (Keep plotting in mind for later!)
names(bull_market) <- mod_names

## Assign the non-scaled SA results to a list to keep for later use if necessary
SA_scores_non_scaled$bull_market <- bull_market

## compute the stats (including max/min for later use)
sent_stats$bull_market <- stat.desc(bull_market)

## ----------------------------- ##
##  Repeat for all search terms  ##
## ----------------------------- ##

## Bear Market
bear_market <- c(names(sub_sent)[grep("^bear_market", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
bear_market$SentiStrength <- rowMeans(data.table(bear_market$bear_market_Sentistrength_pos, bear_market$bear_market_Sentistrength_neg))
bear_market <- subset(bear_market, select = names(bear_market)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(bear_market) <- mod_names
SA_scores_non_scaled$bear_market <- bear_market
sent_stats$bear_market <- stat.desc(bear_market)

## Dow Jones
dow_jones <- c(names(sub_sent)[grep("^Dow_Jones", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
dow_jones$SentiStrength <- rowMeans(data.table(dow_jones$Dow_Jones_Sentistrength_pos, dow_jones$Dow_Jones_Sentistrength_neg))
dow_jones <- subset(dow_jones, select = names(dow_jones)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(dow_jones) <- mod_names
SA_scores_non_scaled$dow_jones <- dow_jones
sent_stats$dow_jones <- stat.desc(dow_jones)

## Dow SPDR
dow_SPDR <- c(names(sub_sent)[grep("^dow_SPDR", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
dow_SPDR$SentiStrength <- rowMeans(data.table(dow_SPDR$dow_SPDR_Sentistrength_pos, dow_SPDR$dow_SPDR_Sentistrength_neg))
dow_SPDR <- subset(dow_SPDR, select = names(dow_SPDR)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(dow_SPDR) <- mod_names
SA_scores_non_scaled$dow_SPDR <- dow_SPDR
sent_stats$dow_SPDR <- stat.desc(dow_SPDR)

## Dow Wallstreet
dow_wallstreet <- c(names(sub_sent)[grep("^dow_wallstreet", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
dow_wallstreet$SentiStrength <- rowMeans(data.table(dow_wallstreet$dow_wallstreet_Sentistrength_pos, dow_wallstreet$dow_wallstreet_Sentistrength_neg))
dow_wallstreet <- subset(dow_wallstreet, select = names(dow_wallstreet)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(dow_wallstreet) <- mod_names
SA_scores_non_scaled$dow_wallstreet <- dow_wallstreet
sent_stats$dow_wallstreet <- stat.desc(dow_wallstreet)

## Federal Reserve
federal_reserve <- c(names(sub_sent)[grep("^federal_reserve", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
federal_reserve$SentiStrength <- rowMeans(data.table(federal_reserve$federal_reserve_Sentistrength_pos, federal_reserve$federal_reserve_Sentistrength_neg))
federal_reserve <- subset(federal_reserve, select = names(federal_reserve)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(federal_reserve) <- mod_names
SA_scores_non_scaled$federal_reserve <- federal_reserve
sent_stats$federal_reserve <- stat.desc(federal_reserve)

## Financial Crisis
financial_crisis <- c(names(sub_sent)[grep("^financial_crisis", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
financial_crisis$SentiStrength <- rowMeans(data.table(financial_crisis$financial_crisis_Sentistrength_pos, financial_crisis$financial_crisis_Sentistrength_neg))
financial_crisis <- subset(financial_crisis, select = names(financial_crisis)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(financial_crisis) <- mod_names
SA_scores_non_scaled$financial_crisis <- financial_crisis
sent_stats$financial_crisis <- stat.desc(financial_crisis)

## goldman Sachs
goldman_sachs <- c(names(sub_sent)[grep("^goldman_sachs", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
goldman_sachs$SentiStrength <- rowMeans(data.table(goldman_sachs$goldman_sachs_Sentistrength_pos, goldman_sachs$goldman_sachs_Sentistrength_neg))
goldman_sachs <- subset(goldman_sachs, select = names(goldman_sachs)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(goldman_sachs) <- mod_names
SA_scores_non_scaled$goldman_sachs <- goldman_sachs
sent_stats$goldman_sachs <- stat.desc(goldman_sachs)

## Interest Rates
interest_rates <- c(names(sub_sent)[grep("^interest_rates", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
interest_rates$SentiStrength <- rowMeans(data.table(interest_rates$interest_rates_Sentistrength_pos, interest_rates$interest_rates_Sentistrength_neg))
interest_rates <- subset(interest_rates, select = names(interest_rates)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(interest_rates) <- mod_names
SA_scores_non_scaled$interest_rates <- interest_rates
sent_stats$interest_rates <- stat.desc(interest_rates)

## Market Volatility
market_volatility <- c(names(sub_sent)[grep("^market_volatility", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
market_volatility$SentiStrength <- rowMeans(data.table(market_volatility$market_volatility_Sentistrength_pos, market_volatility$market_volatility_Sentistrength_neg))
market_volatility <- subset(market_volatility, select = names(market_volatility)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(market_volatility) <- mod_names
SA_scores_non_scaled$market_volatility <- market_volatility
sent_stats$market_volatility <- stat.desc(market_volatility)

## Obama Economy
obama_economy <- c(names(sub_sent)[grep("^obama_economy", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
obama_economy$SentiStrength <- rowMeans(data.table(obama_economy$obama_economy_Sentistrength_pos, obama_economy$obama_economy_Sentistrength_neg))
obama_economy <- subset(obama_economy, select = names(obama_economy)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(obama_economy) <- mod_names
SA_scores_non_scaled$obama_economy <- obama_economy
sent_stats$obama_economy <- stat.desc(obama_economy)

## Oil Prices
oil_prices <- c(names(sub_sent)[grep("^oil_prices", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
oil_prices$SentiStrength <- rowMeans(data.table(oil_prices$oil_prices_Sentistrength_pos, oil_prices$oil_prices_Sentistrength_neg))
oil_prices <- subset(oil_prices, select = names(oil_prices)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(oil_prices) <- mod_names
SA_scores_non_scaled$oil_prices <- oil_prices
sent_stats$oil_prices <- stat.desc(oil_prices)

## Stock Prices
stock_prices <- c(names(sub_sent)[grep("^stock_prices", names(sub_sent))]) %>%
    subset(sub_sent, select = .)
stock_prices$SentiStrength <- rowMeans(data.table(stock_prices$stock_prices_Sentistrength_pos, stock_prices$stock_prices_Sentistrength_neg))
stock_prices <- subset(stock_prices, select = names(stock_prices)[3:7]) %>%
    setcolorder(., c(1, 2, 5, 4, 3))
names(stock_prices) <- mod_names
SA_scores_non_scaled$stock_prices <- stock_prices
sent_stats$stock_prices <- stat.desc(stock_prices)

## Group all min and max values together in one list of data tables
for(i in 1:length(sent_stats)){          # i is the search term
    for(j in 1:length(sent_stats[[i]])){ # j is the SA model used
        sent_minmax[[i]][[j]] <- data.table(min = sent_stats[[i]][[j]][[4]], max = sent_stats[[i]][[j]][[5]])}}
## Give them the names of the models
for(i in 1:13){names(sent_minmax[[i]]) <- mod_names}

## Group all min and max values together in one list of data tables
for(i in 1:length(sent_stats)){          # i is the search term
    for(j in 1:length(sent_stats[[i]])){ # j is the SA model used
        sent_minmax[[i]][[j]] <- max(abs(c(sent_stats[[i]][[j]][[4]], sent_stats[[i]][[j]][[5]])))
    }
}

## ========================================================================== ##
##  Scale all the sentiment results to take daily average in an unbiased way  ##
## ========================================================================== ##

#' We use the min and max values obtained to scale the sentiment data, but we could
#' additionally recreate the stats info about all the models' SA results here after
#' they have been scaled for comparison.
#' Or to simply plot the actual results used for further analysis.

## ------------------------------------- ##
##  Create a list of of scaling factors  ##
## ------------------------------------- ##

model_list <- sapply(mod_names, function(x) NULL)
scaling_factors <- sapply(searchTerms, function(x) NULL)
for(i in 1:length(searchTerms)){scaling_factors[[i]] <- sapply(model_list, function(x) NULL)}

## Save all scaling factors in one list
for(i in 1:length(searchTerms)) {
    
    for(j in 1:length(mod_names)) {
        
        scaling_factors[[i]][[j]] <- max(abs(sent_minmax[[i]][[j]]))
    }
}

## ------------------------------------ ##
##  Save the stats and scaling factors  ##
## ------------------------------------ ##

SA_stats_and_scaling_factors <- sapply(c("stats", "scaling_factors"), function(x) NULL)
SA_stats_and_scaling_factors$stats <- sent_stats
SA_stats_and_scaling_factors$scaling_factors <- scaling_factors

save(SA_stats_and_scaling_factors, file = "SA_stats_and_scaling_factors.rda")

## -------------------------------------------------------------------------------- ##
##  Scale all the data then create one data table to combine with market data etc.  ##
## -------------------------------------------------------------------------------- ##

## Create object to hold all the scaled SA scores
SA_scores_scaled <- sapply(searchTerms, function(x) NULL)
for(i in 1:length(SA_scores_scaled)){SA_scores_scaled[[i]] <- model_list}

## Scale the data
for(i in 1:length(SA_scores_non_scaled)) {
    
    for(j in 1:length(SA_scores_non_scaled$dow_jones)) {
        
        SA_scores_scaled[[i]][[j]] <- SA_scores_non_scaled[[i]][[j]] / scaling_factors[[i]][[j]]
    }
}

## Check that the new maximums of the scaled data is indeed +/- 1
for(i in 1:length(SA_scores_scaled)) {
    
    for(i in 1:length(SA_scores_scaled$bull_market)) {
        
        print(max(abs(SA_scores_scaled[[i]][[j]])))
    }
}

## Initialise object with first 
sent_scaled <- as.data.table(sapply(SA_scores_scaled$bull_market, function(x) x))
names(sent_scaled) <- paste0(names(SA_scores_scaled)[1], ".", mod_names)
## Create one data table
for(i in 1:(length(SA_scores_scaled)-1)) {

    this_subset <- as.data.table(sapply(SA_scores_scaled[[i+1]], function(x) x))
    names(this_subset) <- paste0(names(SA_scores_scaled)[i+1], ".", mod_names)
    sent_scaled <- cbind(sent_scaled, this_subset)
}

## Save the scaled sentiment data
save(sent_scaled, file = "sentiment_data_scaled.rda")

###################### ========================================== ######################
######################  Find daily averages for sentiment scores  ######################
###################### ========================================== ######################

## Calculate the average for each day and append to sentiment data
sent$bull_market_avg <- rowMeans(subset(bull_market, select = seq(3, 7, 1))) #Leaves out SS_pos and SS_neg

## ====================================================================== ##
##  for each search term: save daily average sentiment as a new variable  ##
## ====================================================================== ##

## This finds the average over all SA models for each search term
## The output should be 971 observations ofor 13 variables (search terms)
## This can then be attached to 'data_dirty' for imputation and dummy variable creation

## As the scaled data is still all in one list, it must be converted to a data table

daily_avg_scores <- sapply(searchTerms, function(x) NULL)
daily_avg_scores <- data.table(to_remove = matrix(nrow = 971))[, as.vector(searchTerms) := as.data.table(0)]

for(i in 1:13){

    daily_avg_scores[, searchTerms[i] := subset(as.data.table(sent_scaled), select = seq(5*i - 4, 5*i)) %>%
        rowMeans(.)]
    ## daily_avg_scores[[i]] <- subset(as.data.table(sent_scaled), select = seq(5*i - 4, 5*i)) %>%
    ##     rowMeans(.)
    
}


daily_avg_scores[, to_remove.V1 := NULL]
save(daily_avg_scores, file = "sentiment_data_daily_averages.rda")

## ================================================================= ##
##  Append the daily averages to the entire data set for imputation  ##
## ================================================================= ##

## Create names for the daily_avg_scores to make more sense in the aggregated table
names(daily_avg_scores) <- paste0(names(daily_avg_scores), ".averaged")

## Join all data and reorder to have average SA results with dates and dummy variables
data_to_impute <- cbind(data_dirty, as.data.table(daily_avg_scores))
setcolorder(data_to_impute, neworder = c(1, 2, 3, 4, seq(151, 163), seq(5, 150)))

save(data_to_impute, file = "data_to_impute.rda")



###################### ================== ######################
######################  Defunct function  ######################
###################### ================== ######################

## These functions don't all do exactly what we need...

## A function to base SentiStrength data from -4 to +4, (not +/-1 to +/-5)
## This linear transformation doesn't change the variance
## THIS IS NOT USEFUL --> doesn't change the output of data once scaled to {-1:+1}
scaler1 <- function(x) {ifelse(x < 0, x+1, ifelse(x > 0, x-1, ifelse(x == 0, x, x)))}

## A function to normalise all SA data to -1 and +1
## This isn't used in the end
scaler2 <- function(input_data, new_min, new_max) {

    ## Define parameters for scaling
    old_min <- min(input_data)
    old_max <- max(input_data)
    a <- (new_max - new_min)/(old_max - old_min)
    b <- new_max - a * old_max

    ## Scale the input_data
    output_data <- a * input_data + b
    return(output_data)
}

## A scaler that keeps the relative dispersion between values
my_scaler <- function(x) {x / max(sqrt(x*x))}

## It is better to scale by a fixed factor, the maximum possible on scale, not max observation
## This is (+/-) 5 for SentiStrength
