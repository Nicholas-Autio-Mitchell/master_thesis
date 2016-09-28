
## =============================== ##
##  Import all data for modelling  ##
## =============================== ##

#' This file prodcues the final data.table used for boosting.
#' Tweet data is combined with the sentiment analysis results.
#' Market data is imported and appended
#' If possible, macroeconomic factors will also be imported here.

## Clear workspace and open R session
rm(list=ls())
lsos()

## Input
if (Sys.info()[['sysname']] == "Darwin") {

    src_tweets = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/input_analysed/dirty/"
    src_sentiment = "/Volumes/Mac OS Drive/Thesis/Data/sentiment_analysis/output/"
    dst = "/Volumes/Mac OS Drive/Thesis/Data/model/"

} else if (Sys.info()[['sysname']] == "win32") {

    src_tweets = "c:/Users/dev231/Documents/Thesis/Data/sentiment_analysis/input_analysed/dirty/"
    src_sentiment = "c:/Users/dev231/Documents/Thesis/Data/sentiment_analysis/output/"
    dst = "c:/Users/dev231/Documents/Thesis/Data/model/"
}

## Save names of folders for use throughout below - can be done with either src_*
myNames <- new.env()
myNames$dir_names <- list.files(src_sentiment) %>%
    gsub(" ", "_", .) %>%
    gsub("[^\\x{41}-\\x{7A}]", "", ., perl = TRUE)

## =============== ##
##  Process Dates  ##
## =============== ##

#' A mixture of German and English dates appear in the scraped tweet data.
#' Use a dictionary to translate all months into English (e.g. Juli --> Jul)
#' this needs to be done to be able to use function: as.Date.
#' It is possible necessary that Sys.setlocale(locale = "de_DE") must be applied

## The eng_Dates function requires the locale to be German
##Sys.setlocale(locale = "de_DE")

## Define function to return English dates --> used below in 'load_tweets'
eng_Dates <- function(dates) {

    ## Language names differ on OSs
    if (Sys.info()[['sysname']] == "Darwin") {
        langs <- c("de_DE", "C")
    } else if (Sys.info()[['sysname']] == "win32") {
        langs <- c("German", "English")
    }

    ## Create dictionary to translate months into English if necessary
    tab <- unlist(lapply(langs, function(lang) {
        Sys.setlocale("LC_TIME", lang)
        nms <- format(ISOdate(2000, 1:12, 1), "%b")
        setNames(month.abb, nms)
    }))
    tab <- c(tab, Juli = "Jul", Juni = "Jun", März = "Mar") ## Add a few custom translations that are required

    ## x <- as.data.table(x)
    source_month <- gsub("[^[:alpha:]]", "", dates)
    dates <- mapply(sub, source_month, tab[source_month], dates, USE.NAMES = FALSE)

}

## ================= ##
##  Load tweet data  ## --> This is the TweetID, times_retweeted, times_favourites and the dates (text, userID etc. are dropped)
## ================= ##

## Define function to import, clean and collate data
load_tweets <- function(path) {

    dir_list <- list.dirs(path) %>% .[2:length(.)]
    
    ## Create empty list with element names to assign the data_frames to
    data_list <- vector("list", length(dir_list))
    names(data_list) <- myNames$dir_names

    for (i in 1:length(myNames$dir_names)) {
        
        x <- dir(dir_list[i], pattern = "*.txt$", full.names = TRUE) %>%
            lapply(.,         # create a list of data frames,
                   function(x) fread(x, drop = 5:7, header = TRUE, sep = "\t", ## drop columns: [user_name, user_id, text]
                              stringsAsFactors = FALSE)) %>%
            rbindlist(.)
        
        ## ensure time_date column is consistent for all data
        x$time_date <- gsub("\\x{2E}{0,2}", "", x$time_date, perl = TRUE) %>%
            gsub("( PM)?", "", .) %>%
            gsub("( AM)?", "", .)

        ## Convert as date strings into English (some German months present, e.g. März, Juli, ...)
        tmp_dates <- eng_Dates(x$time_date)
        x$dates  <- as.Date(tmp_dates, format = "%H:%M - %d %b %Y")

        ## Remove the original date_time column
        x[,time_date:=NULL]
        
        ## Alter data type for later use
        x$times_retweeted <- as.numeric(x$times_retweeted)
        x$times_favourited <- as.numeric(x$times_favourited)

        data_list[[i]] <- x
        
    }
    return(data_list)
}

## Load data
tweet_DT <- load_tweets(src_tweets)
## Check structure of list
summary(tweet_DT)
## Check structure of first list-element (data.table)
glimpse(tweet_DT[[1]])
glimpse(tweet_DT[[3]])
## Inspect number of tweets for each search term (== numbers above for tweet_DT)
str(lapply(tweet_DT, dim))

## ============================== ##
##  Load sentiment analysis data  ##
## ============================== ##

## Define function to import, clean and collate data
load_sentiment <- function(path) {

    dir_list <- list.dirs(path) %>% .[2:length(.)]

    ## Create empty list with element names to assign the data_frames to
    data_list <- vector("list", length(dir_list))
    names(data_list) <- myNames$dir_names

    for (j in 1:length(myNames$dir_names)) {
        
        x <- dir(dir_list[j], pattern = "*.txt$", full.names = TRUE) %>%
            lapply(.,         # create a list of data frames,
                   function(x) fread(x , drop = c(1, 5, 9), header = TRUE, sep = "\t", ## drop columns: [Tweet-Text, Emolex_rounded, Vader_rounded]
                              stringsAsFactors = FALSE)) %>%
            rbindlist(.)

        ## Convert to more useful data types
        x$Sentistrength_pos <- as.numeric(x$Sentistrength_pos)
        x$Sentistrength_neg <- as.numeric(x$Sentistrength_neg) 
        x$Emolex_raw <- as.numeric(x$Emolex_raw)
        #x$Emolex_rounded <- as.numeric(x$Emolex_rounded)
        x$Sentiment140 <- as.numeric(x$Sentiment140)
        x$Vader_Afinn <- as.numeric(x$Vader_Afinn)
        #x$Vader_rounded <- as.numeric(x$Vader_rounded)

        data_list[[j]] <- x
        
    }
    return(data_list)
}

## Load data
sent_DT <- load_sentiment(src_sentiment)
## Check structure of list
summary(sent_DT)
## Check structure of first list-element (data.table)
glimpse(sent_DT[[1]])
glimpse(sent_DT[[7]])
## Inspect number of elements for each search term (== numbers above for tweet_DT)
str(lapply(sent_DT, dim))

## ================================ ##
##  Aggregate data frames to daily  ##
## ================================ ##

#' Define function to combine the data for each sarch term
#' these can then be combined with rbindlist() or similar

combiner <- function(tweet_table, sent_table, input_name) {

    ## Create test data frame using "bull market"
    dt <- cbind(tweet_table, sent_table)

    ## Reorder the columns as desired
    setcolorder(dt, c("tweetID", "dates", "times_retweeted", "times_favourited",
                      "Sentistrength_pos", "Sentistrength_neg", "Emolex_raw", #"Emolex_rounded",
                      "Sentiment140", "Vader_Afinn", "Vader_decimal"))  #, "Vader_rounded"))

    ## Remove any duplicates, using the tweetID column
    dt1 <- dt[!duplicated(dt$tweetID),]

    ## Check to see if any were removed (debug step)
    if (identical(dt, dt1) == TRUE){
        message(paste0("No duplicate tweet data was removed in ", input_name))
    } else {
        message(paste0("Some duplicate tweet data was removed in ", input_name))
    }

    ## Remove the tweetID column
    dt1[, tweetID := NULL]

    ## Ensure all data follows the time-series order
    dt2 <- dt1[order(dt1$dates),]

    ## Calculate the number of tweets each day before aggregatgin; to append later
    freq <- count(dt2, dt2$dates)

    ## Aggregate to a daily basis - Here we can use custom function on column basis
    var_names <- names(dt2)

    #var_names[] =

    ## Aggregate against days computing the mean for each sentiment model for each day
    dt3 <- dt2[, c(lapply(.SD, mean)), 
               by=dates]

    ## Aggregate with specific function for each variable
    ## dt3 <- dt2[, list(times_retweeted = mean(times_retweeted),
    ##                   times_favourited = mean(times_favourited),
    ##                   Sentistrength_pos = mean(Sentistrength_pos),
    ##                   Sentistrength_neg = mean(Sentistrength_neg),
    ##                   Emolex_raw = mean(Emolex_raw),
    ##                   Emolex_rounded = mean(Emolex_rounded),
    ##                   Sentiment140 = mean(Sentiment140),
    ##                   Vader_Afinn = mean(Vader_Afinn),
    ##                   Vader_decimal = mean(Vader_decimal),
    ##                   Vader_rounded = mean(Vader_rounded)),
    ##            by=dates]

    ## Add the number of tweets per day to the aggregated data table in 3rd column
    dt4 <- as.data.table(append(dt3, list(num_tweets = as.numeric(freq$n)), after = 1))

    ## Keep only the dates that are consistent across all data sets
    dt5 <- dt4[dates>="2013-01-14" & dates<="2015-09-11"]
    
    return(dt5)

}

## ==================================== ##
##  Merge all data into one data.table  ##
## ==================================== ##

## Define function to merge data.tables specifying options for merge()
myMerge <- function(x, y) {

    ## all = TRUE --> inserts NA rows into gaps in time series to retain as data 
    merge(x, y , by.x = "dates", by.y = "dates", all = TRUE)

}

## Define function to perform merge
bigMerge <- function(tweet_data, sentiment_data) {
    
    ## Create empty list with element names to assign the data.frames to
    combined_data <- vector("list", length(myNames$dir_names))
    names(combined_data) <- myNames$dir_names

    ## Create a list of data.tables with both tweet and SA data - one for each search term
    for( i in 1:length(tweet_data)) {

        ## Pull the tweet and SA data together
        combined_data[[i]] <- combiner(tweet_table = tweet_data[[i]],
                                       sent_table = sentiment_data[[i]],
                                       input_name = myNames$dir_names[i])

        ## Use names of variables exluding $dates
        var_names <- names(combined_data[[i]])[2:length(combined_data[[i]])]
        ## Apply the unique names to each variable in each data.table, e.g. "bull_market_SentiStrength_pos"
        col_names <- paste(myNames$dir_names[i], var_names, sep = "_")
        colnames(combined_data[[i]])[2:length(combined_data[[i]])] <- col_names
        
    }


    ## Merge all the data.tables by date
    data <- Reduce(myMerge, combined_data)   ## Perhaps use arg: accumulate=TRUE to debug

    ## Show what percentage of all data is NA
    message(paste0("\nRaw - 'NA' content: ",as.character(round(sum(is.na(data))/prod(dim(data))*100, digits = 2))," %"))

    ## Set all NA values (arising from the merge) to 0
    data[is.na(data)] <- 0

    ## Show what percentage of all data is NA
    message(paste0("\ncleaned - 'NA' content: ",as.character(round(sum(is.na(data))/prod(dim(data))*100, digits = 2))," %"))

    return(data)
}

## Create final data.table
data <- bigMerge(tweet_DT, sent_DT)

## Add three columns: the weekdays (char) & dummy variables for Mondays and weekends
data$days_names <- weekdays(data$dates)
data$is.monday <- as.numeric(weekdays(data$dates) %in% c('Monday'))
data$is.weekend <- as.numeric(weekdays(data$dates) %in% c('Sunday','Saturday'))
##data$is.weekend <- format(as.Date(data$dates),"%w")   ## get numbers for days: 0-6 -> Sun-Sat

## Rearrange to have date/day related columns at beginning of data.table
num_vars <- dim(data)[2]
setcolorder(data, c(1, (num_vars -2), (num_vars -1), num_vars, seq(from = 2, to = (num_vars - 3))))

## Inspect final data.table
str(data, vec.len = 3, give.length = TRUE, digits.d = 2, list.len = length(data))

## ============================ ##
##  Preliminary boosting steps  ##
## ============================ ##

## Keep z reserved for a backup of data.table with everything still contained
## Remove weekends from data (called 'bdata' from get_market_data.R)
data <- z
data <- data[is.weekend == 0]
#data_dates  <- z[is.weekend == 0]

## Remove columns not required [days_names, is.weekend]
data <- subset(data, select = -c(dates, days_names, is.weekend))

## Show how many NA values are in bdata and there positions
dim(data)
length(which(is.na(data)))
which(is.na(data))

## ============ ##
##  Imputation  ##
## ============ ##

## Conveert data to a zoo object - the dates column mst be removed
x <- zoo(data)

## Before
dim(x)
length(which(is.na(x)))
which(is.na(x))

## Replace NA values with severla methods
out1 <- na.locf(x)
#out2 <- na.approx(x)
#out3 <- na.spline(x)

## reassign to data.table to 'data'
x <- as.data.table(lapply(out1, function(x) as.numeric(x)))
data <- x

## ============================================ ##
##  We can add a column here for a lagged DOW   ##
## ============================================ ##

## This can be done later, lagging all predictors using embed()

## Create a column for Dow Returns
## Dow on Sep. 14th = 16,370.96
dow_tmp <- append(data$DJI, as.numeric("16370.96"))
dow_ret <- diff(dow_tmp)                #returns
dow_lr <- diff(log(dow_tmp))          #log-returns

## Add new column to main data.table
data <- cbind(dow_lr, data)
#data <- append(data, dow_lr, after = 1)

