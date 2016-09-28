# # # # Functions used in sequence to extract, convert and clean up tweets in order to analyse them # # # #

## Open a session with the Twitter API ####
# ensure the necessary packages are installed
#install.packages('twitteR')
#install.packages('ROAuth')

# Set up the Twitter API (only needs to be done once per session)
# This can also be executed using the document 'oauth_info.R'
# First save the keys that were defined and supplied by Twitter when creating the API
consumerKey <- 'aRE1YNfXXaw15M4MIs6w9JpY9'
consumerSecret <- 'dluHwkkzokjSusKGD6soLCSYTVUTtYEi326xDuliyHr36xpCtL'
accessToken <- '3402568396-mygMgTzp0tr3on7ySLRp4PUesc1odyELnkznnsy'
accessTokenSecret <- 'OzgNs8hh2HWPdRYoFwgYiK3Hs8OzOtVNoy5eGFcgaMofs'
# Use these keys to supply the handle function provided by package 'twitteR'
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# Clear the workspace --> these are not longer needed for the session as the API must only be activated once
remove(consumerKey, consumerSecret, accessToken, accessTokenSecret)

## Initiate the search criteria and extract the data ####

# Set some parameters to be used for one session:
# Create the search terms to be mined, separated by the '+' character (see: https://dev.twitter.com/rest/public/search)
searchTerms <- "Dow Jones"
# The maximum number of tweets to be found (if less than n tweets exist, they will all be used)
maxTweets <- 1500
# The date in the past from where we would like our tweet list to begin - format: 'YYYY-MM-DD'
startDate <- '2013-01-01'
# The last day from which we would like tweets
endDate <- '2015-06-30'
# Specificy a language for the tweets e.g. "en" or "de" --> Isn't 100% reliable in returning only selected language
language <- "en"
# Set which results we want to receive. Either most recent ('recent'), most popular ('popular') or a mixture ('mixed)
resultSort <- 'recent'

# Mine Twitter for the desired tweets, returned as a list object
tweets <- searchTwitter(searchTerms, n = maxTweets, since = startDate, lang = language, resultType = resultSort)

## Preprocess the data ####

# Extract the tweet text from the list received and return a character vector
tweets_text <- sapply(tweets, function(x) x$getText())
# Extract the corresponding dates of the tweets [using 'sapply' returned dates in a non human readable format]
#tweets_dates <- sapply(tweets, function(x) x$getCreated())
tweets_dates <- lapply(tweets, function(x) x$getCreated())

# Test to ensure tweets are now saved in a text format - "character" - before applying the cleaning functions
class(tweets_text)

## Clean the tweet text data to return only text strings without any inherently irrelevant terms ####

# Remove special characters, such as emojis
tweets_clean <- sapply(tweets_text, function(row) iconv(row, "latin1", "ASCII", sub=""), USE.NAMES = FALSE)
# (seond method) Remove the UTF-8 symbols that are in text due to emojis/smileys [currently not required after sapply() above]
#tweets_no_emojis <- gsub("\\xed\xa0(.*)", "", tweets_text, useBytes = TRUE)

# Remove all upper case characters
tweets_clean <- tolower(tweets_clean)

# Return ampersand symbols to their readable form, i.e. not "&amp;"
tweets_clean <- gsub("&amp;", "&", tweets_clean)

# Remove all punctuation, but hold back the ampersand and hyphen symbols, as they appear in stock/index names and so are meaningful
tweets_clean <- gsub("[^[:alnum:][:space:]&-]", "", tweets_clean)

# Remove any tweets that do not contain (a/the) search term(s) - this can occur due to the paging error  --> e.g. 8 tweets from 200 found not to contain the search terms 'Dow Jones'
# Done by simplying retaining tweets that do indeed contain the search terms
# To avoid possible errors, ensure that at least 70% of tweets remain, otherwise notify user
to_retain <- grep(pattern = searchTerms, x = tweets_clean, ignore.case = TRUE, useBytes = TRUE)
if (length(to_retain) > 0.7 * length(tweets_clean)) {
  tweets_clean <- tweets_clean[to_retain]
} else {
  message("More than 30% of tweets did not contain any of the search terms. None were removed")
}

# Remove the data corresponding to these tweets from other sets collected
tweets_dates <- tweets_dates[to_retain]

# Remove known non-English tweets [499]
# 'abre' here is a word that means 'opens' in Portueguese and appears in many tweets, meaning the tweet isn't English - we will remove the entire tweet.
# First find the indices of the tweets that are not English
to_remove_foreign <- grep('abre', tweets_clean, ignore.case = TRUE)
# Remove these tweets from our list, but check first that there is at least one to remove, otherwise all tweets would be removed
if (length(to_remove_foreign) > 0) {
  tweets_clean <- tweets_clean[-(to_remove_foreign)]
} else {
  message("It appears all tweets are clear of select foreign words - therefore none were removed")
}

# Remove the data corresponding to these tweets from other sets collected
tweets_dates <- tweets_dates[-(to_remove_foreign)]

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

# Display how many of the tweets that remain are 'retweets', meaning they are duplicates.
retweet_positions <- grep(pattern = "^rt", x = tweets_clean, ignore.case = FALSE)
retweet_percentage = length(retweet_positions) / length(tweets_clean) * 100

## Cleaning methods that can be used in case a corpus is used for data storage ####

# Convert desiredTweets_text into a corpus, which can be more easily used for analysis
#corpus <- Corpus(VectorSource(tweets_clean))

# Begin to clean the tweets in preparation for analysis - general cleaning first, data specific after. This could all be completed using just one function 'tm-reduce', however here more verbosely for clarity.
#clean_corpus <- function(corpus) {
  
  # Remove common and potentially meaningless words (possibly create personal list here if context requires)
  #corpus_tmp <- tm_map(corpus, removeWords, c(stopwords("english"), "our", "custom", "words"))
#  corpus_tmp <- tm_map(corpus, removeWords, stopwords("english"), "the")
  # Remove white space remaining after all removals so far
  #corpus_tmp <- tm_map(corpus_tmp, stripWhitespace)  
  # Remove punctuation
  #corpus.tmp <- tm_map(corpus, content_transformer(removePunctuation))
  # Remove capitalisation i.e. all letters into lower case
  #corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
  # Remove numbers
  #corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
#  return(corpus)
#}

## Create a single data table object containing all data for later use ####

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

## Miscellaneous ####

# A method to use gsub with the tweet when they are still in list form. This can then be used for the dates as a list
tweets_list_gsub <- lapply(X = tweets_list, function(x) gsub(pattern = "#DowJones", replacement = "#JowDones", x[], ignore.case = TRUE))


## For fun - create a 'wordcloud' plot of the main words in the collected and cleaned set of tweets
# Source for some ideas: http://www.rdatamining.com/docs/text-mining-with-r-of-twitter-data-analysis 

# colors
pal <- brewer.pal(9, "BuGn")
pal <- pal[-(1:4)]

# plot word cloud
wordcloud(words = names(word.freq), freq = word.freq, min.freq = 3, random.order = F, colors = pal)

wordcloud(tweets_clean, scale = c(3.5, 1), min.freq = 5, max.words = 80, random.order = FALSE, colors = pal)
wordcloud(corpus, scale = c(3.5, 1), min.freq = 10, max.words = 80, random.color = TRUE, colors = rainbow(10))
#wordcloud(corpus, scale = c(5, 0.5), min.freq = 3, max.words = 50)
