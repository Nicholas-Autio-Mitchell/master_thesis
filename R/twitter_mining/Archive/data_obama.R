## Obama

# This File ---------------------------------------------------------------

# The Twitter data chall be extracted and cleaned here, ready to be merged with data from comparable files
# The colection of data can then be used together in one model

# Introduction - the idea behind the method -------------------------------

## Sentiment analysis of Obama as a factor on an American index

# Taking the frequency alone of Obama tweets (from his account and/or from the entire Twitter platform) is rather arbitrary in itself as we do not know what factors affect to number of tweets.
# We attempt to form a time series of the sentiment (positive or negative with magnitudes)
# These quantified sentiments can then be visually inspected against the target dependent variable e.g. the Dow Jones

# Data Extraction ---------------------------------------------------------

## Initialise the paramters for the desired data:
# Create the search terms to be mined, separated by the '+' character (see: https://dev.twitter.com/rest/public/search)
searchTerms <- "Barrack Obama"
# '#DowJones' '@DowJones'
# The maximum number of tweets to be found (if less than n tweets exist, they will all be used)
maxTweets <- 20000
# The date in the past from where we would like our tweet list to begin - format: 'YYYY-MM-DD'
startDate <- '2013-01-01'
# The last day from which we would like tweets
endDate <- '2015-06-30'
# Specificy a language for the tweets e.g. "en" or "de" --> Isn't 100% reliable in returning only selected language
language <- "en"
# Set which results we want to receive. Either most recent ('recent'), most popular ('popular') or a mixture ('mixed')
resultSort <- 'recent'
# The number of times to attempt data extraction
retry = 100
# Select a geographical area if desired
geography <- "NULL"

# Mine Twitter for the desired tweets, returned as a list object
tweets <- searchTwitter(searchString = searchTerms, n = maxTweets, lang = language, since = startDate,
                        resultType = resultSort, retryOnRateLimit = retry)

# Extract the tweet text from the list received and return a character vector
tweets_text <- sapply(tweets, function(x) x$getText())
# Extract the corresponding dates of the tweets [using 'sapply' returned dates in a non human readable format]
#tweets_dates <- sapply(tweets, function(x) x$getCreated())
tweets_dates <- lapply(tweets, function(x) x$getCreated())

# Clean the data ----------------------------------------------------------

# Remove special characters, such as emojis
tweets_clean <- sapply(tweets_text, function(row) iconv(row, "latin1", "ASCII", sub=""), USE.NAMES = FALSE)

# Remove all upper case characters
tweets_clean <- tolower(tweets_clean)

# Return ampersand symbols to their readable form, i.e. not "&amp;"
tweets_clean <- gsub("&amp;", "&", tweets_clean)

# Remove all punctuation, but hold back the ampersand and hyphen symbols, as they appear in stock/index names and so are meaningful
tweets_clean <- gsub("[^[:alnum:][:space:]&-]", "", tweets_clean)

to_retain <- grep(pattern = searchTerms, x = tweets_clean, ignore.case = TRUE, useBytes = TRUE)
if (length(to_retain) > 0.7 * length(tweets_clean)) {
  tweets_clean <- tweets_clean[to_retain]
} else {
  message("More than 30% of tweets did not contain any of the search terms. None were removed. Consider changing search parameters")
}

# Remove the data corresponding to these tweets from other sets collected
tweets_dates <- tweets_dates[to_retain]

# Check that the data sizes are stil consistent with one another
if (length(tweets_clean) == length(tweets_dates)){
  message("The number of tweets and number of dates match")
}  else {
  messages("The sizes of the tweet and date containers are not linger consistent. Expect errors further on!")
}

# Remove known non-English tweets
# 'abre' here is a word that means 'opens' in Portueguese and appears in many tweets, meaning the tweet isn't English - we will remove the entire tweet.
# First find the indices of the tweets that are not English
to_remove_foreign <- grep('abre', tweets_clean, ignore.case = TRUE)
# Remove these tweets from our list, but check first that there is at least one to remove, otherwise all tweets would be removed
if (length(to_remove_foreign) > 0) {
  tweets_clean <- tweets_clean[-(to_remove_foreign)]
}

# Remove the data corresponding to these tweets from other sets collected
if (length(to_remove_foreign) > 0) {
  tweets_dates <- tweets_dates[-(to_remove_foreign)]
}

# Check that the data sizes are stil consistent with one another
if (length(tweets_clean) == length(tweets_dates)){
}  else {
  message("The sizes of the tweet and date containers are not linger consistent. Expect errors further on!")
}

# Remove all links -> they are automatically shrunk by Twitter and so do not hold any relevant information [1]
tweets_clean <- gsub("http(.*)\\b+", "", tweets_clean)

# For efficiency and neatness, remove any white space that remains after removing many characters
tweets_clean <- stripWhitespace(tweets_clean)
# Remove blank spaces at the beginning
tweets_clean <- gsub("^ ", "", tweets_clean)
# Remove blank spaces at the end
tweets_clean <- gsub(" $", "", tweets_clean)
# Remove tabs (just in case not picked up by 'stripWhiteSpace')
tweets_clean <- gsub("[ |\t]{2,}", "", tweets_clean)

# Assess and reorganise the data ------------------------------------------

# Display how many of the tweets that remain are 'retweets', meaning they are duplicates.
retweet_positions <- grep(pattern = "^rt", x = tweets_clean, ignore.case = FALSE)
retweet_percentage = length(retweet_positions) / length(tweets_clean) * 100

# Create a data table of the tweets
tweets_table <- as.data.table(tweets_clean)
# Create a data table with the dates as an intermediary step
dates_table <- as.data.table(tweets_dates, keep.rownames = TRUE)

# Convert dates table into a matrix to transpose it without losing any formatting
dates <- as.character.Date(x = dates_table[1,])
# Transpose the matrix, checking the dimensions before and after to confirm that everything went as hoped
dim(dates)
dates_transposed <- t(dates)
dim(dates_transposed)
# Convert back in to a data table in order to then merge with the tweets, so we have one object with all data
dates_to_merge <- as.data.table(dates_transposed)

# Create a new column to set unique identifiers for each of the tweets
tweets_id <- rownames(tweets_table)
# Create a new column to set unique identifiers for each of the dates
dates_id <- rownames(dates_to_merge)

# Combine the IDs of the tweets and dates to obtain one data table with tweets, their dates identified with unique IDs
if (identical(tweets_id, dates_id) == TRUE) {
  message("The unique ID for the tweets and their dates match, those from the tweets shall be used for both variables")
  tweet_data <- cbind(tweets_id, dates_to_merge, tweets_table)
  # Add a column name 
} else {
  message("The column names (and so the unique identifers) of the data sets do not match.
          a new data table containing all information cannot be create")
}

# Rename the columns of the new data table
setnames(x = tweet_data, old = c("tweets_id", "V1", "tweets_clean"), new = c("ID", "Date", "Tweet"))

# Rename the data to reflect the search terms
data_obama <- tweet_data

# Clean working environment -----------------------------------------------

# Remove all the variables that we created in producing the final output
rm(list = c("dates", "dates_id", "dates_table", "dates_to_merge", "dates_transposed", "to_remove_foreign", "to_retain", "tweet_data", "tweets_clean", "tweets_dates", "tweets_id", "tweets_table", "tweets_text"))