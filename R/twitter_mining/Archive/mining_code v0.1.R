## Functions used in sequence to scrap, convert and clean up tweets in order to analyse them

# Install necessary packages
install.packages('twitteR')
install.packages('ROAuth')

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
# Create the search terms to be mined, separated by the '+' character (an example:)
searchTerms = 'Dow Jones'
# The maximum number of tweets to be found (if less than n tweets exist, they will all be used)
maxTweets = 5000
# The date in the past from where we would like our tweet list to begin - format: 'YYYY-MM-DD'
startDate = '2015-01-01'
# The last day from which we would like tweets
endDate = '2015-08-01'
# Specificy a language for the tweets e.g. "en" or "de" --> Isn't 100% reliable in returning only selected language
language = "en"
# Set which results we want to receive. Either most recent ('recent'), most popular ('popular') or a mixture ('mixed)
resultSort = 'recent'

# Mine Twitter for the desired tweets, returned as a list object
tweets = searchTwitter(searchTerms, n = maxTweets, since = startDate, lang = language, resultType = resultSort)

# Convert desiredTweets from being a 'list' into a character vector
tweets_text = sapply(tweets, function(x) x$getText())
# Test to ensure tweets are now saved in a text format - "character"
class(tweets_text)

# Remove special characters, such as emojis
tweets_clean <- sapply(tweets_text, function(row) iconv(row, "latin1", "ASCII", sub=""), USE.NAMES = FALSE)
# (seond method) Remove the UTF-8 symbols that are in text due to emojis/smileys [currently not required after sapply() above]
#tweets_no_emojis <- gsub("\xed\xa0(.*)", "", tweets_text, useBytes = TRUE)

# Remove all upper case characters
tweets_clean <- tolower(tweets_clean)

# Return ampersand symbols to their readable form, i.e. not "&amp;"
tweets_clean <- gsub("&amp;", "&", tweets_clean)

# Remove all punctuation, but hold back the ampersands, as they appear in stock/index names
tweets_clean <- gsub("[^[:alnum:][:space:]&]", "", tweets_clean)

# Remove known non-English tweets [499]
# 'abre' here is a word that means 'opens' in Portueguese and appears in many tweets, meaning the tweet isn't English - we will remove the entire tweet.
# First find the indices of the tweets that are not English
to_remove_foreign <- grep('abre', tweets_clean, ignore.case = TRUE)
# Remove these tweets from our list
tweets_clean <- tweets_clean[-(to_remove_foreign)]

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

# Convert desiredTweets_text into a corpus, which can be more easily used for analysis
corpus <- Corpus(VectorSource(tweets_clean))

# Begin to clean the tweets in preparation for analysis - general cleaning first, data specific after. This could all be completed using just one function 'tm-reduce', however here more verbosely for clarity.
clean_corpus <- function(corpus) {

# Remove common and potentially meaningless words (possibly create personal list here if context requires)
#corpus_tmp <- tm_map(corpus, removeWords, c(stopwords("english"), "our", "custom", "words"))
corpus_tmp <- tm_map(corpus, removeWords, stopwords("english"), "the")
# Remove white space remaining after all removals so far
#corpus_tmp <- tm_map(corpus_tmp, stripWhitespace)  
# Remove punctuation
#corpus.tmp <- tm_map(corpus, content_transformer(removePunctuation))
# Remove capitalisation i.e. all letters into lower case
#corpus.tmp <- tm_map(corpus.tmp, content_transformer(tolower))
# Remove numbers
#corpus.tmp <- tm_map(corpus.tmp, removeNumbers)
return(corpus)
}

## For fun - create a 'wordcloud' plot of the main words in the collected and cleaned set of tweets
wordcloud(tweets_clean, scale = c(3.5, 1), min.freq = 10, max.words = 80, random.color = TRUE, colors = rainbow(10))
wordcloud(corpus, scale = c(3.5, 1), min.freq = 10, max.words = 80, random.color = TRUE, colors = rainbow(10))
#wordcloud(corpus, scale = c(5, 0.5), min.freq = 3, max.words = 50)
