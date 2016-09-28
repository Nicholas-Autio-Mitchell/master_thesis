## Functions used in sequence to scrap, convert and clean up tweets in order to analyse them

# Install necessary packages
install.packages('twitteR')
install.packages('RCurl')

# Set up the Twitter API (only needs to be done once per session)
# This can also be executed using the document 'oauth_info.R'
# First save the keys that were defined and supplied by Twitter when creating the API
consumerKey <- 'aRE1YNfXXaw15M4MIs6w9JpY9'
consumerSecret <- 'dluHwkkzokjSusKGD6soLCSYTVUTtYEi326xDuliyHr36xpCtL'
accessToken <- '3402568396-mygMgTzp0tr3on7ySLRp4PUesc1odyELnkznnsy'
accessTokenSecret <- 'OzgNs8hh2HWPdRYoFwgYiK3Hs8OzOtVNoy5eGFcgaMofs'
# Use these keys to supply the handle function provided by package 'twitteR'
setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)

# Set some parameters to be used for one session:
# The maximum number of tweets to be found (if less than n tweets exist, they will all be used)
maxTweets = 1500
# The date in the past from where we would like our tweet list to begin - format: 'YYY-MM-DD'
startDate = '20114-06-01'
# Create the search terms to be mined, separated by the '+' character (an example:)
searchTerms = 'devnet'

# Mine Twitter for the desired tweets
desiredTweets = searchTwitter(searchTerms, n = maxTweets, since = startDate)

# Convert desiredTweets from being a 'list' into a character vector
desiredTweets_text = sapply(desiredTweets, function(x) x$getText())
# Test to ensure tweets are now saved in a text format
str(desiredTweets_text)

# Convert desiredTweets_text into a corpus, which can be more easily used for analysis
desiredTweets_corpus <- Corpus(VectorSource(desiredTweets_text))

# Begin to clean the tweets in preparation for analysis - general cleaning first, data specific after. This could all be completed using just one function 'tm-reduce', however here more verbosely for clarity.
# Remove punctuation
desiredTweets_corpus_clean <- tm_map(desiredTweets_corpus, content_transformer(removePunctuation))
# Remove capitalisation i.e. all letters into lower case
desiredTweets_corpus_clean <- tm_map(desiredTweets_corpus_clean, content_transformer(tolower))
# Remove common and potentially meaningless words (possibly create personal list here if context requires)
desiredTweets_corpus_clean <- tm_map(desiredTweets_corpus_clean, removeWords, stopwords("english"))
# Remove numbers
desiredTweets_corpus_clean <- tm_map(desiredTweets_corpus_clean, removeNumbers)
# Remove white space remaining after all removals so far
desiredTweets_corpus_clean <- tm_map(desiredTweets_corpus_clean, stripWhitespace)


## For fun - create a 'wordcloud' plot of the main words in the collected and cleaned set of tweets
wordcloud(desiredTweets_corpus_clean, scale = c(5, 0.5), min.freq = 3, max.words = 150)






