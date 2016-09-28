###################### ============================================================== ######################
######################  Create the dummy variables: Was Proceeding Weekend Positive?  ######################
###################### ============================================================== ######################

## ========= ##
##  Summary  ##
## ========= ##

""""
Three search terms were selected as representative, having the three highest correrlations with the Dow returns.
The average sentument was taken for these three on a dily basis to create a new variable.
The values for Friday Saturday and Sunday were then grouped into one mean value and replaced the Friday.
This 'Friday' value is the value that will be used to predict the next day, i.e. the immediately
following Monday, therefore there is not violation of temporal information flow.

Only three have been selected as the interpretation becomes rather difficult once more are selected.
A negative sentiment score implies different things for each of the search terms and so taking the average
would be too simplified a method to retain the underlying information. Also, the sentiment score for some search terms e.g. 'financial crisis' is negative for the vast majority of the time.

There are plots for each search terms at five separate 20-day chunks, as well as for the average.

""""

## ===================================== ##
##  Original ideas and file description  ##
## ===================================== ##


#' This file takes "sentiment_data_daily_averages.rda", which is the average daily SA scores for each search term
#' It uses ready_to_impute to create new dummy variables for each of the sentiment analysis columns
#' (this means the individual model results as well as for the averaged daily sentiments) and creates
#' the new dummy variable 'wwp.{term+model}'.
#' This looks at the average of the weekend sentiment scores i.e. the Saturday and Sunday
#' values for each model/term,
#' and if the average is positive, the Monday columns are given a value of +1, if negative, a value of -1.
#' A value of zero is given to days Tuesday-Friday
#'
#' One question remains: Do we use the average over all search terms for the new dummy variable?
#' This seems more concise
#' in capturing the idea, however maybe the terms are negatively correlated, so we lose
#' the overall effect of each term.
#' E.g. "bear market" is positive should lead to markets rising, whereas "bull market" should 
#' 
#' We also create several plots to support the idea behind weekend sentiment providing
#' information on Monday returns:
#' (1) - the sentiment of the weekend versus the returns on Mondays/whole week/week return
#' (2) - delta of friday closing versus monday opening versus sentiment (as value?)

## Required for the date-related columns
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_weighted.rda")
## Required for the daily averaged SA scores after all data was weighted using retweets/favourites
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_daily_averages.rda")

##load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data.rda")
##load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/Archive/sentiment_data.rda")

###################### ========================================== ######################
######################  Create some plots to inspect correlation  ######################
###################### ========================================== ######################

#' the final plots below (myPlot1:5) show there is clear correlation, but that the
#' sentiment generally trails the market movements. In some cases however it can be
#' seen that the sentiment movement over the weekend is a precursor to Monday returns.
#' This justifies the inclusions of the dummy variable for Was Preceeding Weekend Positive - wpwp.

## ------------------------------- ##
##  Create one table to work with  ##
## ------------------------------- ##

## Required for the date-related columns
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_weighted.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/ready_to_boost.rda")
## ## Load the bank holidays; they are saved as a vector of character dates
## load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/holidays.rda")
## holidays <- as.Date(holidays)
## ## add holidays to data_to_compute
##data_to_impute$is.holiday <- as.numeric(data_to_impute$dates %in% holidays)

## Define function to check if sentiment results were positive or negative
is.naturalnumber <- function(x, tol = .Machine$double.eps^0.5)  x > tol & abs(x - round(x)) < tol
data_to_impute$is.sent.pos <- ifelse(is.naturalnumber())


## Get the date-related columns
myDates <- subset(weighted_sentiment, select = c(1, 2, 3, 4, 5))

## Attach to the SA data
base_data <- cbind(myDates, daily_avg_scores)

## ---------------------------------------------------------------------------- ##
##  Have a look at the data - do the SA scores from the previous day say much?  ##
## ---------------------------------------------------------------------------- ##

## Create a table with relevant data
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/all_nominal_data.rda")
dow <- data.table(all_nominal_data$locf$locf_L0$index, all_nominal_data$locf$locf_L0$log_ret_DJI)
dow_dates <- merge(base_data, dow, by.x = "dates", by.y = "V1", fill = NA, all = TRUE)
names(dow_dates) <- c(names(dow_dates)[1:18], "log_ret_dow")

## Add a column with the averaged SA scores (one avg over all search terms/models)
dow_dates[, avg_SA := rowMeans(daily_avg_scores)]
## Have a look at the first 60 rows
head(dow_dates[, c(1, 2, 19, 20), with=FALSE], n=60)

## A function to replace all NAs with a given value
NA_replacer = function(DT) {
    # either of the following for loops

    # by name :
    #for (j in names(DT))
    #    set(DT,which(is.na(DT[[j]])),j,0)

    # or by number (slightly faster than by name) :
    for (j in seq_len(ncol(DT)))
        set(DT,which(is.na(DT[[j]])),j,0)

    return(DT)
}

## Plot the dow returns against each of the SA score search terms and
## the average SA scores over all search terms.
data_set <- sapply(c(names(daily_avg_scores), "average"), function(x) NULL)
## Each mini data set contain the data, the dow returns and one column of SA scores
data_set$bull_market <- dow_dates[, c(1, 19, 6), with=FALSE]        # 1
data_set$bear_market <- dow_dates[, c(1, 19, 7), with=FALSE]        # 2
data_set$dow_jones <- dow_dates[, c(1, 19, 8), with=FALSE]          # 3
data_set$dow_SPDR <- dow_dates[, c(1, 19, 9), with=FALSE]           # 4
data_set$dow_wallstreet <- dow_dates[, c(1, 19, 10), with=FALSE]    # 5
data_set$federal_reserve <- dow_dates[, c(1, 19, 11), with=FALSE]   # 6
data_set$financial_crisis <- dow_dates[, c(1, 19, 12), with=FALSE]  # 7
data_set$goldman_sachs <- dow_dates[, c(1, 19, 13), with=FALSE]     # 8
data_set$interest_rates <- dow_dates[, c(1, 19, 14), with=FALSE]    # 9
data_set$market_volatility <- dow_dates[, c(1, 19, 15), with=FALSE] # 10
data_set$obama_economy <- dow_dates[, c(1, 19, 16), with=FALSE]     # 11
data_set$oil_prices <- dow_dates[, c(1, 19, 17), with=FALSE]        # 12
data_set$stock_prices <- dow_dates[, c(1, 19, 18), with=FALSE]      # 13
data_set$average <- dow_dates[, c(1, 19, 20), with=FALSE]           # 14

## Insert number from desired data set above and run following code to see plots
data_to_plot <- 6
this.data_set <- copy(data_set[[data_to_plot]])
## Replace NAs with zeros
NA_replacer(this.data_set)


## mini_data <- melt(this.data_set, id.vars = "dates")
## qplot(x = dates, y = value, data = mini_data, geom = "line", colour = variable)

## long_data <- melt(this.data_set, id.vars = "dates")
## qplot(x = dates, y = value, data = long_data, geom = "line", colour = variable)

## Make smaller plots - must create data first
data1 <- melt(this.data_set[4:24], id.vars = "dates")
data2 <- melt(this.data_set[100:120], id.vars = "dates")
data3 <- melt(this.data_set[300:320], id.vars = "dates")
data4 <- melt(this.data_set[590:610], id.vars = "dates")
data5 <- melt(this.data_set[800:820], id.vars = "dates")

## ## Plot all five time chunks, one chunk with dow returns on one graph. scales make it diffictult to compare
## qplot(x = dates, y = value, facets = (.~variable), data = data1, geom = "line", colour = variable)
## qplot(x = dates, y = value, data = data2, geom = "line", colour = variable)
## qplot(x = dates, y = value, data = data3, geom = "line", colour = variable)
## qplot(x = dates, y = value, data = data4, geom = "line", colour = variable)
## qplot(x = dates, y = value, data = data5, geom = "line", colour = variable)

## ======================================================== ##
##  Create plots highlighting weekends for the five chunks  ##
## ======================================================== ##

## The dates for the rectangles overlayed on the plots are hard-coded.

## -------------- ##
##  Time Chunk 1  ##
## -------------- ##

lj <- copy(this.data_set)

names(lj) <- c("Date", "DJIA - log returns ", "Sentiment - ")


myPlot1 <- ggplot(data = data1, aes(x = Date, y = 10*Value, color = variable)) +
    facet_grid(variable~., scales = "free_y", legend.position = "none") +
    geom_line() +
    labs(x = "Value") +
    theme(legend.position = "none") +
    theme_bw()
    
## Create rectangles to fit over the weekend data
rect <- data.frame(xmin=as.Date(c("2013-01-19", "2013-01-26", "2013-02-02")),
                   xmax=as.Date(c("2013-01-20", "2013-01-27", "2013-02-03")), ymin=-Inf, ymax=Inf)
## Add the rectangles with an alpha < 1
myPlot1 <- myPlot1 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                               color="gold",
                               alpha=0.4,
                               inherit.aes = FALSE)

## -------------- ##
##  Time Chunk 2  ##
## -------------- ##

myPlot2 <- ggplot(data = data2, aes(x = dates, y = value, color = variable)) +
    facet_grid(variable~., scales = "free_y") +
    geom_line()
## Create rectangles to fit over the weekend data
rect <- data.frame(xmin=as.Date(c("2013-04-27", "2013-05-04", "2013-05-11")),
                   xmax=as.Date(c("2013-04-28", "2013-05-05", "2013-05-12")), ymin=-Inf, ymax=Inf)
## Add the rectangles with an alpha < 1
myPlot2 <- myPlot2 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                               color="gold",
                               alpha=0.4,
                               inherit.aes = FALSE)

## -------------- ##
##  Time Chunk 3  ##
## -------------- ##

myPlot3 <- ggplot(data = data3, aes(x = dates, y = value, colour = variable)) +
    facet_grid(variable~., scales = "free_y") +
    geom_line()
## Create rectangles to fit over the weekend data
rect <- data.frame(xmin=as.Date(c("2013-11-09", "2013-11-16", "2013-11-23")),
                   xmax=as.Date(c("2013-11-10", "2013-11-17", "2013-11-24")), ymin=-Inf, ymax=Inf)
## Add the rectangles with an alpha < 1
myPlot3 <- myPlot3 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                               color="gold",
                               alpha=0.4,
                               inherit.aes = FALSE)

## -------------- ##
##  Time Chunk 4  ##
## -------------- ##

myPlot4 <- ggplot(data = data4, aes(x = dates, y = value, color = variable)) +
    facet_grid(variable~., scales = "free_y") +
    geom_line()
## Create rectangles to fit over the weekend data
rect <- data.frame(xmin=as.Date(c("2014-08-30", "2014-09-06", "2014-09-13")),
                   xmax=as.Date(c("2014-08-31", "2014-09-07", "2014-09-14")), ymin=-Inf, ymax=Inf)
## Add the rectangles with an alpha < 1
myPlot4 <- myPlot4 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                               color="gold",
                               alpha=0.4,
                               inherit.aes = FALSE)

## -------------- ##
##  Time Chunk 5  ##
## -------------- ##

myPlot5 <- ggplot(data = data5, aes(x = dates, y = value, color = variable)) +
    facet_grid(variable~., scales = "free_y") +
    geom_line()
## Create rectangles to fit over the weekend data
rect <- data.frame(xmin=as.Date(c("2015-03-28", "2015-04-04", "2015-04-11")),
                   xmax=as.Date(c("2015-03-29", "2015-04-05", "2015-04-12")), ymin=-Inf, ymax=Inf)
## Add the rectangles with an alpha < 1
myPlot5 <- myPlot5 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                               color="gold",
                               alpha=0.4,
                               inherit.aes = FALSE)

## ------------------------------------------------------------------------------------- ##
##  Inspect these time chunks - how many weekend movements predict the Monday movement?  ##
## ------------------------------------------------------------------------------------- ##

## For 'dow jones' 6/15
myPlot1                                 # 2/3 correct
myPlot2                                 # 0/3 correct, but otherwise promising
myPlot3                                 # 3/3 correct
myPlot4                                 # 1/3 correct
myPlot5                                 # 0/3 correct

## For 'bull market' 8/15
myPlot1                                 # 1/3 correct
myPlot2                                 # 1/3 correct
myPlot3                                 # 2/3 correct
myPlot4                                 # 2/3 correct
myPlot5                                 # 2/3 correct

## for 'federal reserve' 10/15
myPlot1                                 # 2/3 correct
myPlot2                                 # 2/3 correct
myPlot3                                 # 3/3 correct
myPlot4                                 # 2/3 correct
myPlot5                                 # 1/3 correct

## For 'interest rates' 9/15
myPlot1                                 # 2/3 correct
myPlot2                                 # 2/3 correct
myPlot3                                 # 2/3 correct
myPlot4                                 # 2/3 correct
myPlot5                                 # 1/3 correct

## For 'oil prices' 6/15
myPlot1                                 # 1/3 correct
myPlot2                                 # 1/3 correct
myPlot3                                 # 1/3 correct
myPlot4                                 # 1/3 correct
myPlot5                                 # 2/3 correct

## For average of all models 5/15
myPlot1                                 # 1/3 correct
myPlot2                                 # 1/3 correct
myPlot3                                 # 1/3 correct
myPlot4                                 # 1/2 correct
myPlot5                                 # 1/3 correct
## This even suggests a negative correlation!


## ============ ##
##  Conclusion  ##
## ============ ##

#' The plots above show that there is no great consistent pattern in the weekend sentiment scores
#' with the Monday returns. At least not better than random.
#'
#' This means that the inclusion of a dummy variable, while intuitively interesting as we can avoid
#' the complete loss of data, doesn't appear as though it would introduce a great impact on the
#' predictive accuracy of the model that uses it.

## As a results, we shall create a variable that does contain the sentiment of the weekends
#' so that we do not completely lose it within the modelling.
#'
#' We create one variable using the best or most directly relevant search terms to market movements.
#' These are:
#' (1) dow jones
#' (2) dow wallstreet
#' (3) bull market
#' When looking at the signs of their SA scores and the market returns, there is a high correlation
#'

## have a quick look at the correlation among the SA scores and log_ret_DJIA
ä <- dow_dates[,6:19, with=FALSE]
tweet_cor <- cor(NA_replacer(ä))
ä1 <- ä[, 1:13, with=FALSE][2:971]
ä_dow <- ä[, 14, with=FALSE][1:970]
## Compute the correlation matrix
ä_cor <- cor(cbind(ä1, ä_dow))

## The results show that the three SA terms most correlated with market reutrns are:
## (1) stock prices
## (2) federal reserve
## (3) dow jones
##
## These also all have a relatively large amount of tweets (unlike 'dow_SPDR')
## We will use these three to create a dummy variable for the Monday returns

###################### ======================= ######################
######################  Create Dummy Variable  ######################
###################### ======================= ######################

## we use the 'daily_avg_scores' data set loaded from:
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/sentiment_data_daily_averages.rda")

## Take the three columns we are using
DT <- subset(daily_avg_scores, select = c("stock_prices", "dow_jones", "federal_reserve"))
DT[, avg := rowMeans(DT)]

## Combine the new column with dates to create new column without weekends, but taking them into account
DT1 <- cbind(myDates, DT$avg)
names(DT1) <- c(names(myDates), "avg_subset_sent")

## get indices of Mondays
friday_indices <- DT1[, which(day.number == 5)][1:138]

## Create data table with the indices of Friday to Sunday to take average over
begin_end <- data.table(begin = friday_indices, end = friday_indices+2)
indices_for_avg <- list()

for(i in 1:nrow(begin_end)) {
    indices_for_avg[[i]] <- seq(from = begin_end[i, begin], to = begin_end[i, end])
}

## Calculate the averages for the Friday-Sunday ranges
my_averages <- list()

for(i in 1:length(indices_for_avg)) {
    my_averages[[i]] <- mean(DT1[indices_for_avg[[i]], avg_subset_sent])
}

## Now we must assign these values to the Monday indices.
## These replace the row mean values for Mondays. The values for Tue-Fri remain unchanged

## Copy weekend_sentiment column to keep a copy safe
DT1[, SA_with_weekends := avg_subset_sent]

for(i in 1:length(friday_indices)) {
    DT1$SA_with_weekends[friday_indices[i]] <- my_averages[[i]]
}

## Now the column 'SA_with_weekends' can be appended to the rest of
## the averaged SA scores (the original 13 columns), and when the weekends are removes,
## we have kept some information from the weekends in the data set

## the new values for SA on Mondays (containing weekend info) will be used to predict Tuesday

daily_avg_scores[, sa_with_weekend := DT1$SA_with_weekends]


###################### ============================== ######################
######################  Finished data set saved here  ######################
###################### ============================== ######################

save(daily_avg_scores, file = "sentiment_daily_averages_with_weekend_data.rda")

###################### ============================== ######################
######################  Finished data set saved here  ######################
###################### ============================== ######################


## ======================================= ##
##  Plot the average with the dow returns  ##
## ======================================= ##

## This doesn't remove weekends and plot with the time-shift as we will do for modelling!

## Add dates and dow returns
data_set <- cbind(dow_dates[, c(1, 19), with = FALSE], daily_avg_scores$sa_with_weekend)
names(data_set) <- c("dates", "log_ret_dow", "sa_with_weekend")

## Replace the NA values within the dow returns
NA_replacer(data_set)

## Make smaller plots - must create data first
data1 <- melt(data_set[4:24], id.vars = "dates")
data2 <- melt(data_set[100:120], id.vars = "dates")
data3 <- melt(data_set[300:320], id.vars = "dates")
data4 <- melt(data_set[590:610], id.vars = "dates")
data5 <- melt(data_set[800:820], id.vars = "dates")


lj <- copy(this.data_set)

names(lj) <- c("Date", "DJIA - log returns", "New sentiment variable")
data1 <- melt(lj[4:24], id.vars = "Date")


## Create plot object
myPlot1 <- ggplot(data = data1, aes(x = Date, y = 10*value, color = variable)) +
    facet_grid(variable~., scales = "free_y") +
    geom_line(size=2.5) +
    labs(y = TeX("Value $\\left( 10^{-2} \\right)$"), x = "\nDate") +
    theme(legend.position = "none") +
    theme_bw(base_size = 24)
    
## Create rectangles to fit over the weekend data
rect <- data.frame(xmin=as.Date(c("2013-01-19", "2013-01-26", "2013-02-02")),
                   xmax=as.Date(c("2013-01-20", "2013-01-27", "2013-02-03")), ymin=-Inf, ymax=Inf)
## Add the rectangles with an alpha < 1
myPlot1 <- myPlot1 + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),
                               color="gold",
                               alpha=0.4,
                               inherit.aes = FALSE)
myPlot1




## For sentiment data including weekend averages  7/15

myPlot1                                 # 2/3 correct
myPlot2                                 # 1/3 correct
myPlot3                                 # 2/3 correct
myPlot4                                 # 1/3 correct
myPlot5                                 # 1/3 correct

## --------------- ##
##  Further usage  ##
## --------------- ##

#' this new data set will be added to the larger set containing all individual SA model results
#' plus the macro data
