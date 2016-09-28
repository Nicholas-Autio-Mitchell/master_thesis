
## ==================================================== ##
##  Rearrange the data slightly and inspect some plots  ##
## ==================================================== ##

rm(list=ls())                         # clean up workspace

search_term1 <- "bull"
search_term2 <- "market"
file_path <- "/Volumes/Mac OS Drive/Data Backup/Data/parse_output/individual/\"bull market\"/"
file_name <- "2013-01-28_til_2013-01-13.txt"
input_file <- paste(file_path, file_name, collapse = "", sep = "")

# Read in the raw data (parsed and tab-separated) <- we need to set quotes as ""
# --> http://stackoverflow.com/questions/17414776/read-csv-warning-eof-within-quoted-string-prevents-complete-reading-of-file
data <- read.table(input_file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = "", row.names = NULL)

# Check the structure for data types
str(data)

## fix some column types
data$myRetweeted <- as.numeric(x = data$times_retweeted)
data$myFavourited <- as.numeric(x = data$times_favourited)
data$myDate <- as.Date(x = data$time_date, format = "%d. %b. %Y")
data$myUserID <- as.numeric(x = data$user_id)      # Maybe leave at data type 'int'?


## example function to check if dow was mentioned in tweet x
check_presence <- function(x, string = term) string %in% unlist(strsplit(x, " "))
# search_term2_present <- function(x, string = search_term2) string %in% unlist(strsplit(x, " " ))
data$mentioned1 <- as.numeric(check_presence(data$text, search_term1))
data$mentioned2 <- as.numeric(check_presence(data$text, search_term2))

## y <- data$text

## for tweet in y {
##     data$mentioned_term <- grep(search_term1, data$text, ignore.case = TRUE)
## }

# Check that all sarch terms are present as expected - this is specific to expecting all search terms in all tweets!
sum_st1 <- sum(data$mentioned1)
sum_st2 <- sum(data$mentioned2)
if ((dim(data)[1] == sum_st1) & (dim(data)[1] == sum_st2) & (sum_st1 == sum_st2)){
    message("All tweets contain the search terms as expected")
} else {
    message("Some tweets do not contain all search terms, or there is a different problem...")
}

##==========================================================
## Sol 1 (simplest)
##==========================================================
freq_daily <- table(data$myDate)
## Can use rep.int() to extract _just_ the frequencies of each day
s <- rep.int(freq_daily, times=1)
freq_exploded <- rep.int(s, s)
##==========================================================
## Sol 2 (aggregate via aggregate function)
##==========================================================
# We only need to aggragate on one of the search terms, given that they both appear in each tweet (as checked above)
freq_dow <- aggregate(mentioned1 ~ myDate, data = data, sum)
# Have a look to see if retweets and favourites show any obvious correlations/patterns
freq_multi <- aggregate(cbind(mentioned1, myRetweeted, myFavourited) ~ myDate, data = data, sum)

#Get a range for the dates used (not important from where as they're all the same)
rng <- range(data$myDate)

## Get Dow
library(tseries)
dow <- get.hist.quote(start=rng[1], end =rng[2], instrument = "^DJI",
                    provider = "yahoo", quote=c("Open", "Close", "AdjClose"))

plot(dow, lwd=2, col=4, type="b", xlab = "Date", ylab=c("Open", "Close", "AdjClose"), main ="Dow Jones")

##==========================================================
## Plot Dow levels (two representations of the same thing)
##==========================================================
dev.new(title = "Frequencies")
par(mfrow=c(1,1))
plot(freq_daily, lwd=2, col=4, type = "h", ylab="Frequency", main = "Number tweets containing 'Dow' and 'Jones' each day")
par(new=TRUE)
plot(freq_daily, lwd=2, col=6, axes = FALSE, type = "b", xlab = "Date", ylab="Frequency")

##==========================================================
## Plot Dow levels vs. Tweets
##==========================================================
dev.new(title = "Market Movements")
par(mfrow=c(3,1))
plot(freq_daily, lwd=2, col=2, type="b", ylab="Frequency", main="Number of tweets -VS- Movement of Dow")
#par(mfrow=c(3,1))
open_price <- dow[1:9, 2:2]
plot(open_price, lwd=2, col=4, type="b", xlab = "Date")
adjClose <- dow[1:9, 3:3]
plot(adjClose, lwd=2, col=4, type="b", xlab = "Date")

##==========================================================
## Plot Dow returns vs. Tweets
##==========================================================
library(zoo)                            # for diff(log(x))
dev.new(title = "Market Returns")
par(mfrow=c(2,1))
plot(freq_dow, lwd=2, col=2, type="b", ylab="Frequency", main="'dow' Tweets")
plot(diff(log(adjClose)*100),lwd=4, col=4, type="b", ylab="Percentage return", main="Returns of Dow Jones (in %)")


## Same, but with aligned dates
dev.new(title = "Market Returns - Aligned Dates")
par(mfrow=c(2,1))
plot(freq_dow[4:14,], lwd=2, col=2, type="b", ylab="Frequency", main="'dow' Tweets")
plot(diff(log(adjClose)*100),lwd=2, col=4, type="b", ylab="Percentage return", main="Returns of Dow Jones (in %)")


## ##==========================================================
## ## Sol 3 (aggregation method via cast function, most versatile function)
## ## just another way of aggregating things
## ##==========================================================
## par(mfrow=c(1,1))
## library("reshape2")                     # cast function used below
## dm3 <- dcast(data_dow, myDate ~ ., sum, value.var="mentioned") # sum of mentions per days
## colnames(dm3) <- c("Date","Mentions")
## dm3$Date <- as.Date(dm3$Date)
## plot(dm3$Date, dm3$Mentions, lwd=4, col=4, type="h", ylab="Sum", main="Frequency of 'dow' Tweets")


