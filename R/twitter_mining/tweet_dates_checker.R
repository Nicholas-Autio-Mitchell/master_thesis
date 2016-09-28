## Short script simply to output a matrix showing where the timeline of the tweets is not completely chronological
# a value of 1 shows that the date of that tweet is larger than that of the following, meaning that the tweet at that index was created closer to the present (as the tweet at index 1 is the most recent)

# For us, this observation is not an issue as we only hope to classify the tweets into daily intervals.
# There happens due to the way in which the tweets are extracted by the twitteR package (and in general, how tweets are offered via the Twitter API).
# The pages of tweets are scanned, and during the time it takes to scan pages - a paging error.
# In a study of 459 tweets, 398 were correctly ordered chronologically, meaning an 'error rate' of 13.1 %. If this is acceptable or not is still to be decided.
# The problem that is raised on the link below, is that this paging can also lead to tweets being completely missed, due to there position on the page being shifted out of range before being collected.
# Source for in depth explanation: https://dev.twitter.com/rest/public/timelines

tweet_date_checker <- function(tweets_dates) {

# Find the maximum index required for the comparison of dates
q <- length(tweets_dates) - 1

# Initialise a vector to store the results
comparison_output <- numeric(length = q)
 
# make the comparisons of dates in a loop and save results (0 or 1)
for(i in 1:q) {
  comparison_output[i] <- tweets_dates[[i + 1]] < tweets_dates[[i]]
}
end

# Percentage of returns that are not choronologically ordered
error_rate <- ((q - sum(comparison_output)) / q) * 100 

# Create the function output
result <- list("Values" = comparison_output, "Error" = error_rate)

return(result)
}
