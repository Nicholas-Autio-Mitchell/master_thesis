###################### =================================================== ######################
######################  Plots to show relationships between starting data  ######################
###################### =================================================== ######################

#' We will show simple plots here to showcase expected as well as surprising relationships between
#' the original data. This data includes all macro data as well as the sentiment analysis results
#' and the frequency of tweets, number of favrouties and retweets and so fourth.

## ================================== ##
##  Load required data for all plots  ##
## ================================== ##

library(ggplot2)
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/all_nominal_data.rda")

###################### ================= ######################
######################  Frequency Plots  ######################
###################### ================= ######################

## The values for each day are plotted, meaning we do not attempt to compensate for lagged effects
## This is hopefully to be shown in the plots themselves

## ========================================= ##
##  Number of tweets against market returns  ##
## ========================================= ##

## Select data required
freq_data <- copy(all_nominal_data$locf$locf_L0)
subset_ <- c("DJI", grep("(_num_tweets)", names(freq_data), value = TRUE))
freq_data <- subset(freq_data, select = subset_)

g <- ggplot(freq_data, )

## =================== ##
##  Number of tweets   ##
## =================== ##


###################### ================= ######################
######################  Sentiment Plots  ######################
###################### ================= ######################

#' We plot the sentiment for each search term, evaluated by one single model again the market
#' returns. Here is might be interesting to plot lagged versions, so DJI[t] ~ Emolex[t-2] to show that there is a relationship which is delayed somewhat

## ======================== ##
##  First SA Model: Emolex  ##
## ======================== ##

## --------------------------------------------------------- ##
##  Emolex SA results for each search term again the market  ##
## --------------------------------------------------------- ##

## Get the column names
em_names <- grep("_Emolex_raw", names(freq_data), value = TRUE)
## Get the column
em_data <- freq_data[, em_names, with=FALSE]
## Attach the nominal Dow and her log returns
em_data[, c("Date", "Dow_nominal", "Dow_returns") := .(freq_data$index, freq_data$DJI, freq_data$log_ret_DJI)]

em_melt <- melt(em_data, id.vars = "Date")

## Plot everything (hard to make any comparisons)
emolex <- ggplot(data = em_melt, mapping = aes(x = Date, y = value)) +
    facet_wrap(~ variable, scales = "free_y") +
    geom_line()

ggsave(file="Emolex_vs_nominal_Dow.pdf", width = 297, height = 210, units = "mm",
       limitsize = FALSE, dpi = 900)

## --------------------------------------------------- ##
##  Plot smaller subgroups all against market returns  ##
## --------------------------------------------------- ##

##  
sub_em <- copy(em_data[, .(bull_market_Emolex_raw,
                           Dow_Jones_Emolex_raw,
                           goldman_sachs_Emolex_raw)])
## Add the dow returns and dates
sub_em[, c("Date", "Dow_returns") := .(freq_data$index, freq_data$log_ret_DJI)]
setcolorder(sub_em, c(4, 5, 1, 2, 3))

sub_em_melt <- melt(sub_em, id.vars = "Date")

## Create names for plot
my_label_value = c(
    `Date` = "Date",
    `Dow_returns` = "Dow log retuns",
    `Dow_Jones_Emolex_raw` = "Dow jones",
    `bull_market_Emolex_raw` = "bull market",
    `goldman_sachs_Emolex_raw` = "Goldman Sachs")

sub_emolex <- ggplot(data = sub_em_melt,
                     mapping = aes(x = Date, y = value),
                     colour = variable) +
    facet_wrap(~variable, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value)) +
    geom_line()

ggsave(file="Emolex_vs_LogDow_3weeks.pdf", width = 297, height = 210, units = "mm",
       limitsize = FALSE, dpi = 900)
## ------------------------------------------- ##
##  Zoom in and look at a shorter time period  ##
## ------------------------------------------- ##

short_em1 <- sub_em[Date >= "2013-05-02" & Date <= "2013-05-17", .(Date, Dow_returns, Dow_Jones_Emolex_raw)]
short_em2 <- sub_em[Date >= "2014-07-02" & Date <= "2014-07-17", .(Date, Dow_returns, Dow_Jones_Emolex_raw)]
short_em3 <- sub_em[Date >= "2015-08-02" & Date <= "2015-08-17", .(Date, Dow_returns, Dow_Jones_Emolex_raw)]

short_em_melt1 <- melt(short_em1, id.vars = "Date")
short_em_melt2 <- melt(short_em2, id.vars = "Date")
short_em_melt3 <- melt(short_em3, id.vars = "Date")

## Create names for plot
my_label_value_short = c(
    `Date` = "Date",
    `Dow_returns` = "Dow log retuns",
    `Dow_Jones_Emolex_raw` = "Sentiment on 'Dow Jones'")

short_emolex1 <- ggplot(data = short_em_melt1,
                     mapping = aes(x = Date, y = value),
                     colour = variable) +
    facet_wrap(~variable, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value_short)) + geom_line() +
    ggtitle("Comparison over 3 weeks of market returns and the Twitter sentiment measures for 'Dow Jones'\nDate range: 2013-05-02 til 2013-05-17")

short_emolex2 <- ggplot(data = short_em_melt2,
                     mapping = aes(x = Date, y = value),
                     colour = variable) +
    facet_wrap(~variable, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value_short)) + geom_line() +
    ggtitle("Comparison over 3 weeks of market returns and the Twitter sentiment measures for 'Dow Jones'\nDate range: 2014-07-02 til 2014-07-17")

short_emolex3 <- ggplot(data = short_em_melt3,
                     mapping = aes(x = Date, y = value),
                     colour = variable) +
    facet_wrap(~variable, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value_short)) + geom_line() +
    ggtitle("Comparison over 3 weeks of market returns and the Twitter sentiment measures for 'Dow Jones'\nDate range: 2015-08-02 til 2015-08-17")

## Plot it
short_emolex1
short_emolex2
short_emolex3


###################### ===================== ######################
######################  Macro data analysis  ######################
###################### ===================== ######################

## Highlight the strengths of our data set (the breadth) through plots of each of the covariates in the Macro data subset versus the market.



