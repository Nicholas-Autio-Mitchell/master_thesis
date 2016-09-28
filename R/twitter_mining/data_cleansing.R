#convert all text to lower case
tweets_text <- tolower(tweets_text)

# Replace @UserName
tweets_text <- gsub("@\\w+", "", tweets_text)

# Remove punctuation
# Spuperceded with "x1 <- gsub("[^[:alnum:][:space:]'&]", "", x)"
tweets_text <- gsub("[[:punct:]]", "", tweets_text)

# Remove links
tweets_text <- gsub("http(.*)\\b+", "", tweets_text)
#tweets_text <- gsub("http\\w+", "", tweets_text)

# Remove tabs
tweets_text <- gsub("[ |\t]{2,}", "", tweets_text)

# Remove blank spaces at the beginning
tweets_text <- gsub("^ ", "", tweets_text)

# Remove blank spaces at the end
tweets_text <- gsub(" $", "", tweets_text)

# Remove non alpha-numeric characters, excluding ampersands
x1 <- gsub("[^[:alnum:][:space:]'&]", "", x)

# Remove all links
c1 <- gsub("http(.*)\\b+", "", c)

# Find the specific strings that need to be removed - return their location index
# 'aber' here is a word that means 'opens' in Portueguese and appears in many tweets, meaning the tweet isn't English - we will remove the entire tweet.
to_remove_foreign <- grep('abre', tweets_text, ignore.case = TRUE)
# Find the decoded versions of emojis that must be removed before further data cleansing can be performed
to_remove_emojis <- grep('\xed\xa0', tweets_clean, useBytes = TRUE)
# Find Ampersand symbols that need to be converted back into their readable forms - useBytes is very important for finding these
to_convert_ampersands <- grep('&amp;', tweets_clean, useBytes = TRUE)

# Find Ampersand symbols and convert them back from their HTML forms into human readable form
tweets_clean <- gsub("&amp;", "&", tweets_clean)