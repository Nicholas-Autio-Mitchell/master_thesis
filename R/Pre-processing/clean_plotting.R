## =================================================================================== ##
##  Combine the earlier work on cleaning tweet data with the new work to visualise it  ##
## =================================================================================== ##

rm(list=ls())

## Define some static parameters
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

myFun <- function()

## Convert to ASCII format to remove unwanted character combinations
tweets_clean <- aggregate(text, data = data, FUN = iconv(row, "latin1", "ASCII", sub=""))

## Confirm that each tweet contains both the search terms (may not be the case, either due to Twtitter or parsing)
check_presence <- function(x, string) string %in% unlist(strsplit(x, " ")) # this is case sensitive


term_presence <- aggregate(x= , ...)
