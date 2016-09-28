
###################### ============================================================= ######################
######################  Make a simple plot showing linear regression with residuals  ######################
###################### ============================================================= ######################

## ----------------------------- ##
##  Load some data from twitter  ##
## ----------------------------- ##

## Choose a subset that has some nice linear data long term version of DOW over some time
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/3_binomial/subsets_lagged.rda")
dow <- subsets_lagged$Lag.1$trad_small$DOW.DP
dow_sent <- subsets_lagged$Lag.1$sent_small$dow_jones.Combined.L1

## Create date
the_dates <- seq(as.Date("2013-01-14"), as.Date("2015-09-11"), by = "days")
## Get just the weekdays (Weekends start with S so remove them)
the_weekends <- grep(pattern = "^[S]",weekdays(the_dates)) #This gets the indices of the weekends
w_days <- the_dates[-the_weekends]                         #the weekdays only, weekends removed

## Create one data table
the_dow <- as.data.table(w_days)
the_dow$stock <- as.data.table(dow)
the_dow$sentiment <- dow_sent

the_model <- lm(the_dow$stock ~ the_dow$sentiment)

plot1 <- qplot(x = the_dow$sentiment, y = the_dow$stock, main = ("Sentiment scores versus actual returns for Dow Jones"), xlab = ("Avg. sentiment for seach term: 'dow jones'"), ylab = ("Log returns of Dow Jones"), geom = "dotplot")
plot1


library(latex2exp)                      #latex in the plot

the_dow$stock1000 <- the_dow$stock * 1000
the_dow$sentiment100 <- the_dow$sentiment * 100

plot2 <- ggplot(data = the_dow, aes(x = sentiment100, y = stock1000)) +
    geom_point() +
    geom_smooth(method = lm, se = FALSE) +
    lims(x = c(-2.5, 7.5), y = c(-4., 12)) +
    ggtitle("For all data") +
    theme_bw(base_size = 30) +          #size of axis text
    theme(plot.title = element_text(size=42)) + #size of title text
    xlab(TeX("Mean sentiment score $\\left(10^{-2}\\right)$")) +
    ylab(TeX("Log returns of Dow Jones $\\left(10^{-3}\\right)$"))

    ## Preview
plot2

## Save - to place within pplot of ten points
ggsave(plot2, filename = "fig:all_data.residuals.png")

## ======================================== ##
##  Take ten points to make things clearer  ##
## ======================================== ##

## Randomly select 10 points
this_data <- the_dow[ceiling((runif(10)*100))]
this_data <- copy(the_dow[123:132])

plot3 <- ggplot(data = this_data, aes(x = 100*sentiment, y = 1000*stock, method = "lm")) +
    geom_point(size=5) +
    geom_smooth(method = lm, data = the_dow[, .(sentiment, stock)], se = FALSE) +
    lims(x = c(-2.5, 7.5), y = c(-4, 12)) +
    theme_bw(base_size = 30) +          #size of axis text
    theme(plot.title = element_text(size=42)) + #size of title text
        xlab(TeX("Mean sentiment score $\\left(10^{-2}\\right)$")) +
    ylab(TeX("Log returns of Dow Jones $\\left(10^{-3}\\right)$"))

## Preview
plot3

## Save - add lines manually with another program
ggsave(plot3, filename = "fig.basic.residuals.png")

