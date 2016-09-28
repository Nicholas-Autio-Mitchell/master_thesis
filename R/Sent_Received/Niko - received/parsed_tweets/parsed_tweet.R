rm(list=ls())
tweets <- read.table(file="2014-10-30_til_2014-08-01.txt-parsed.txt", header=TRUE,
                     sep="\t", stringsAsFactors=FALSE)


##==========================================================
## Convert to POSIX* type
##==========================================================
## Mini example with: "16:11 - 29. Okt. 2014" -> "2014-10-29 16:11:00"
## Check out: http://robinzoni.userweb.mwn.de/riskman/materials/RiskRintro.pdf, page 14
a <- "16:11 - 29. Okt. 2014"            # an example
b <- strptime(a, format="%H:%M - %d. %b. %Y") # this is now POSIX*
b
as.Date(b)

## do the same for the whole column and create a new column called "mydt"
tweets$mydt <- strptime(tweets$time_date, format="%H:%M - %d. %b. %Y")
tweets$dates <- as.Date(tweets$mydt)    # create another column for the dates only
str(tweets)                             # see the structure of tweets

## convert favs and retweets to a number
tweets$retweets <- as.numeric(gsub(" .*", "", tweets$retweets)) # number of retweets
tweets$favs <- as.numeric(gsub(" .*", "", tweets$favs)) # number of favs
str(tweets)            # note that favs and retweets are different now

##==========================================================
## aggregate per dates
##==========================================================
## date frequencies
pdf("myparsedtweets.pdf", width=8, height=6)
freq <- aggregate(favs ~ dates, data = tweets, length)
plot(freq, lwd=4, col=4, type = "l", ylab="Count", main="Number of tweets")

## retweets
retw <- aggregate(retweets ~ dates, data = tweets, sum)
plot(retw, lwd=4, col=2, type = "l", ylab="Count", main="Number of re-tweets")

## favs
favs <- aggregate(favs ~ dates, data = tweets, sum)
plot(favs, lwd=4, col="deeppink3", type = "l", ylab="Count", main="Number of favs")
dev.off()                               # corresponds to the pdf() function on line 30
                        
