
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


## ======================================= ##
##  Aggregate the SA results for weekends  ##
## ======================================= ##

## Find all the column indices of the Sentiment Analysis results in order to scale them
n_reps <- ceiling(length(data_dirty)/9)
diffs <- c(8, rep(c(1, 1, 1, 1, 1, 4), n_reps))
myCols <- cumsum(diffs) %>% .[. <= 121]  #121 = last columns of SA results

## Take subset of all sentiment data (we are avoiding the number of tweets etc.)
sent <- subset(data_dirty, select = myCols)

## A function to base SentiStrength data from -4 to +4, (not +/-1 to +/-5)
## This linear transformation doesn't change the variance
scaler1 <- function(x) {ifelse(x < 0, x+1, ifelse(x > 0, x-1, ifelse(x == 0, x, x)))}

## A function to normalise all SA data to -1 and +1
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

## A scaler that keep the relative dispersion between values
my_scaler <- function(x) {x / max(sqrt(x*x))}

## It is better to scale by a fixed factor, may possible on scale, not max observation
## This is (+/-) 5 for SentiStrength


###################### ========================== ######################
######################  Scale the sentiment data  ######################
###################### ========================== ######################

## We break the data into is groups (search terms) in order to scale them
## We also create a descriptive statistics table for each search term
## See file: "sentiment_data_stats.rda"

## Names of the models
mod_names <- c("Sentistrength_pos", "Sentistrength_neg", "Emolex_raw", "Sentiment140", "Vader_Afinn", "Vader_decimal")
## Names of the search terms
search_names <- c("bull_market", "bear_market", "Dow_Jones", "dow_SPDR", "dow_wallstreet", "federal_reserve",
                  "financial_crisis", "goldman_sachs", "interest_rates", "market_volatility", "obama_economy",
                  "oil_prices", "stock_prices")

## Initialise lists to store stats
sent_stats <- sapply(search_names, function(x) NULL)
sent_minmax <- sapply(search_names, function(x) NULL)

## select subset: bull market
bull_market <- c(names(sent)[grep("^bull_market", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(bull_market) <- mod_names
## Check the max and min values
sent_stats$bull_market <- stat.desc(bull_market)

## Create one value for sentistrength
## This also removes the scaling issue, ranging [-1 to -5] and [+1 to +5] (nothing between -1 and +1)
bull_market$SentiStrength <- rowMeans(data.table(bull_market$Sentistrength_pos, bull_market$Sentistrength_neg))

## Calculate the average for each day and append to sentiment data
sent$bull_market_avg <- rowMeans(subset(bull_market, select = seq(3, 7, 1))) #Leaves out SS_pos and SS_neg

## ========================================= ##
##  Repeat for the rest of the search terms  ##
## ========================================= ##

bear_market <- c(names(sent)[grep("^bear_market", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(bear_market) <- mod_names
sent_stats$bear_market <- stat.desc(bear_market)

Dow_Jones <- c(names(sent)[grep("^Dow_Jones", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(Dow_Jones) <- mod_names
sent_stats$Dow_Jones <- stat.desc(Dow_Jones)

dow_SPDR <- c(names(sent)[grep("^dow_SPDR", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(dow_SPDR) <- mod_names
sent_stats$dow_SPDR <- stat.desc(dow_SPDR)

dow_wallstreet <- c(names(sent)[grep("^dow_wallstreet", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(dow_wallstreet) <- mod_names
sent_stats$dow_wallstreet <- stat.desc(dow_wallstreet)

federal_reserve <- c(names(sent)[grep("^federal_reserve", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(federal_reserve) <- mod_names
sent_stats$federal_reserve <- stat.desc(federal_reserve)

financial_crisis <- c(names(sent)[grep("^financial_crisis", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(financial_crisis) <- mod_names
sent_stats$financial_crisis <- stat.desc(financial_crisis)

goldman_sachs <- c(names(sent)[grep("^goldman_sachs", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(goldman_sachs) <- mod_names
sent_stats$goldman_sachs <- stat.desc(goldman_sachs)

interest_rates <- c(names(sent)[grep("^interest_rates", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(interest_rates) <- mod_names
sent_stats$interest_rates <- stat.desc(interest_rates)

market_volatility <- c(names(sent)[grep("^market_volatility", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(market_volatility) <- mod_names
sent_stats$market_volatility <- stat.desc(market_volatility)

obama_economy <- c(names(sent)[grep("^obama_economy", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(obama_economy) <- mod_names
sent_stats$obama_economy <- stat.desc(obama_economy)

oil_prices <- c(names(sent)[grep("^oil_prices", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(oil_prices) <- mod_names
sent_stats$oil_prices <- stat.desc(oil_prices)

stock_prices <- c(names(sent)[grep("^stock_prices", names(sent))]) %>%
    subset(sent, select = .)
## Alter names
names(stock_prices) <- mod_names
sent_stats$stock_prices <- stat.desc(stock_prices)

## Group all min and max values together in one list of data tables
for(i in 1:13){
    for(j in 1:6){
        sent_minmax[[i]][[j]] <- data.table(min = sent_stats[[i]][[j]][[4]], max = sent_stats[[i]][[j]][[5]])}}
## Give them the names of the models
for(i in 1:13){names(sent_minmax[[i]]) <- mod_names}

sent_stats
all_stats <- sent_stats
myStats <- sapply(c("all_stats", "min_max"), function(x) NULL)
myStats$all_stats <- all_stats
myStats$min_max <- sent_minmax

sentiment_data_stats <- myStats
save(sentiment_data_stats, file = "sentiment_data_stats.rda")

## ========================================================================== ##
##  Scale all the sentiment results to take daily average in an unbiased way  ##
## ========================================================================== ##

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/sentiment_data_stats.rda")

minmax <- sentiment_data_stats$min_max

## Create a list of scaling factors that correpsond to the models in 'sent'
scaling_factors <- sapply(names(sent), function(x) NULL)

## Initialise counter to assign the scaling factors
count = 1

for(i in 1:13){
    for (j in 1:6){

        ## Find scaling factor and assign to corresponding model
        sf <- max(abs(minmax[[i]][[j]]))
        scaling_factors[[count]] <- sf
        count = count + 1
    }
}

## Create a list to hold the scaled data
scaled_sent <- sapply(names(sent), function(x) NULL)

for( i in 1:length(sent)){

    scaled_sent[[i]] <- sent[[i]] / scaling_factors[[i]]
}


##save(scaled_sent, file = "scaled_sentiment_data.rda")

###################### ========================================== ######################
######################  Find daily averages for sentiment scores  ######################
###################### ========================================== ######################
   

## ===================================================================== ##
##  for each search term: find daily average sentiment for new variable  ##
## ===================================================================== ##

## This finds the average over all SA models for each search term
## The output should be 971 observations ofor 13 variables (search terms)
## This can then be attache to 'data_dirty' for imputation and dummy variable creation

## As the scaled data is still all in one list, it musr be converted to a data table

daily_scores <- sapply(search_names, function(x) NULL)

for(i in 1:13){
    
    daily_scores[[i]] <- subset(as.data.table(scaled_sent), select = seq(6*i - 5, 6*i)) %>%
        rowMeans(.)
    
}

##save(daily_scores, file = "sentiment_data_daily_averages.rda")


## ================================================================= ##
##  Append the daily averages to the entire data set for imputation  ##
## ================================================================= ##

## Create names for the daily_scores to make more sense in the aggregated table
avg_names <- paste0("avg_", names(daily_scores))
names(daily_scores) <- avg_names

## Join all data and reorder to have average SA results with dates and dummy variables
data_to_impute <- cbind(data_dirty, as.data.table(daily_scores))
setcolorder(data_to_impute, neworder = c(1, 2, 3, 4, seq(151, 163), seq(5, 150)))

save(data_to_impute, file = "data_to_impute.rda")
