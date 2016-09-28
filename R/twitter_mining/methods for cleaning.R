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


             
