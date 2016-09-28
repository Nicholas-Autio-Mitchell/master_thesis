###################### ================================================= ######################
######################  Create some descriptive statistics for all data  ######################
###################### ================================================= ######################

#' We want to summarise the data as succinctly as possible.
#' This means showing things such as:
#' - the number of tweets per day
#' - breakdown of how many tweets there are
#' - Comparison of the SA results from each of the search terms
#' -

## =========== ##
##  Load data  ##
## =========== ##
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/all_nominal_data.rda")

## ====================================== ##
##  Total number of tweets over timeline  ##
## ====================================== ##
## Select required data
my_dates <- all_nominal_data$locf$locf_L0$index
freq_data <- copy(all_nominal_data$locf$locf_L0)
subset_ <- grep("(_num_tweets)", names(freq_data), value = TRUE)
freq_data <- subset(freq_data, select = subset_)
daily_total <- rowSums(x = freq_data)
freq_data <- cbind(my_dates, daily_total, freq_data)

## total tweets per day
qplot(freq_data$my_dates, freq_data$daily_total,
      xlab = "Date", ylab = "Number",
      main = "Total number of tweets obtained per day",
      geom = c("line"), stat_smooth(method = "lm"))
      ##method = "gam", span = 0.1

## Same using ggplot instead
g <- ggplot(data = freq_data, aes(x = my_dates, y = daily_total)) +
    geom_line() +
    ylab("Total tweets (per day)\n") +
    xlab("Date") +
    geom_smooth(method = "lm") +
    theme_bw(base_size = 35)

g
## This is pretty noisy, with a slow upwards trend as Twitter grows. Interest in stock markets is maybe being expressed more often within social media - it is becoming a new platform for the communication of news and opinions.

## ================================================== ##
##  Number of tweets shown for each search term used  ##
## ================================================== ##

## Remove dow SPDR - crap anyway and we need one less variable for fit facet nicely
freq_data$dow_SPDR_num_tweets  <- NULL

## Alter the data to make is suitable for facet plotting
m_data <- freq_data[, daily_total:=NULL]
m_data <- melt(m_data, id.vars = "my_dates")

## Number of tweets on a daily basis for each of the search terms
freq_plot <- ggplot(data = m_data, mapping = aes(x = my_dates, y = value)) +
    facet_wrap( ~ variable, scales = "free") +
    geom_line() +
    ylab("Total tweets (per day)") +
    xlab("Date") +
    theme_bw()

freq_plot

## ------------------------ ##
##  Make a smaller version  ##
## ------------------------ ##

x <- copy(freq_data)
x1 <- x[, c(1, 4, 5, 14), with = FALSE]       #bear market, dow jones, oil prices
setnames(x1, old=names(x1), new=c("Date", "Bear market", "Dow Jones", "Oil prices"))
## Add the dow log retuns to plot with
x1$dow <- f
setnames(x1, old = "dow", new = "DJIA")
setcolorder(x1, neworder = c(1, 5, 4, 2, 3))
x2 <- melt(x1, id.vars = "Date")

## Number of tweets on a daily basis for each of the search terms
freq_plot <- ggplot(data = x2, mapping = aes(x = Date, y = value)) +
    facet_wrap( ~ variable, scales = "free", ncol=1, ) +
    geom_line() +
    geom_smooth(se = FALSE, span = 0.01, size = 0.5) +
    ylab("Total tweets (per day)") +
    xlab("\nDate") +
    theme_bw(base_size = 16)

freq_plot

scale_x_date(my_dates, breaks = "year")

## =================================================== ##
##  Attempts to improve facet plot of tweet frequency  ##
## =================================================== ##

## Append sum of all tweets for each 
tot_tweets <- colSums(subset(freq_data, select = -1))
tot_tweets <- as.vector(as.character(tot_tweets))
labs <- rep(labs, length(m_data$my_dates)/(ncol(freq_data)-1))

## Annotate each facet with text
ann_text <- data.frame(my_dates = rep(as.Date("2014-07-01"), 13), value = rep(4200, 13), lab = tot_tweets)
                      # cyl = factor(8,levels = c("4","6","8")))

tweet_sets <- c(

    `bull_market_num_tweets` = "'bull market'",
    `bear_market_num_tweets` = "'bear market'",
    `Dow_Jones_num_tweets` = "'Dow Jones'",
    `dow_SPDR_num_tweets` = "'dow SPDR'",
    `dow_wallstreet_num_tweets` = "'dow wallstreet'",
    `federal_reserve_num_tweets` = "'federal reserve'",
    `financial_crisis_num_tweets` = "'financial crisis'",
    `goldman_sachs_num_tweets` = "'goldman sachs'",
    `interest_rates_num_tweets` = "'interest rates'",
    `market_volatility_num_tweets` = "'market volatility'",
    `obama_economy_num_tweets` = "'obama economy'",
    `oil_prices_num_tweets` = "'oil prices'",
    `stock_prices_num_tweets` = "'stock prices'"
)

## Attempt to add the total number of tweets per tweet set on respective facet
annotate("text", tot_tweets)
geom_text(data = ann_text, label = paste0("Total tweets: ", ann_text$lab))

geom_text(data = ann_text, label <- labs) +
geom_text(aes(my_dates, value,
              label=paste("Total tweets = ", d), group=NULL),
          size = 4, color = "grey50",  parse = F)


###################### =========================== ######################
######################  Compare SA data to market  ######################
###################### =========================== ######################

## Plot the frequency of tweets against the market return

## Create data table holding tweet freq, log and nominal Dow values over entire date range
dr_all <- data.table(Date = all_nominal_data$locf$locf_L0$index,
                 Dow_Jones = log(all_nominal_data$locf$locf_L0$DJI),
                 log_Dow_Jones = all_nominal_data$locf$locf_L0$log_ret_DJI,
                 Tweet_count = log(all_nominal_data$locf$locf_L0$Dow_Jones_num_tweets
                 ))
## Create a long data set required to plot several lines on one graph
dr_melt <- melt(dr_all, id.vars = "Date")

qplot(x = Date, y = log_Dow_Jones, data = dr_all,  geom = c("point", "smooth"), stat = "smooth")

my_label_value = c(
    `Date` = "Date",
    `Dow_Jones` = "DJIA - log value",
    `log_Dow_Jones` = "DJIA - log returns",
    `Tweet_count` = "Log of tweet count for 'Dow Jones'")

g1 <- ggplot(data = dr_melt, mapping = aes(x = Date, y = value, colour = variable)) +
    facet_wrap(~variable, nrow = 3, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value)) +
    geom_line(show.legend = FALSE, size = 1.1) +
    ylab("Value") +
    xlab("Date") +
    theme_bw(base_size = 24)

g1

##ggsave(file="Tweeet_Count_Entire.pdf", width = 297, height = 210, units = "mm",
##       limitsize = FALSE, dpi = 900)


## Create the same plot but look at a few weeks instead of the whole timeline
dr_melt_mini1 <- melt(dr_all[40:54], id.vars = "Date")
dr_melt_mini2 <- melt(dr_all[240:254], id.vars = "Date")
dr_melt_mini3 <- melt(dr_all[540:554], id.vars = "Date")

## Create display and save for a shorter time period
g2 <- ggplot(data = dr_melt_mini1, mapping = aes(x = Date, y = value, colour = variable)) +
    facet_wrap(~variable, nrow = 3, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value)) +
    geom_line(show.legend = FALSE) +
    ggtitle("Comparison of daily tweets counts and (1) Dow value and (2) Dow log returns\n15 weekday timeline: 2013-03-08 til 2013-03-28") +
    ylab("Value") +
    xlab("Date")

g2                                      #Display

#ggsave(file="Tweeet_Count_A.pdf", width = 297, height = 210, units = "mm", #Save
       #limitsize = FALSE, dpi = 900)

## Create display and save for a second time period
g3 <- ggplot(data = dr_melt_mini2, mapping = aes(x = Date, y = value, colour = variable)) +
    facet_wrap(~variable, nrow = 3, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value)) +
    geom_line(show.legend = FALSE) +
    ggtitle("Comparison of daily tweets counts and (1) Dow value and (2) Dow log returns\n15 weekday timeline: 2013-12-13 til 2014-01-02") +
    ylab("Value") +
    xlab("Date")

g3

#ggsave(file="Tweeet_Count_B.pdf", width = 297, height = 210, units = "mm",
       limitsize = FALSE, dpi = 900)

## Create display and save for a third time period
g4 <- ggplot(data = dr_melt_mini3, mapping = aes(x = Date, y = value, colour = variable)) +
    facet_wrap(~variable, nrow = 3, scales = "free_y", ncol = 1, labeller = as_labeller(my_label_value)) +
    geom_line(show.legend = FALSE, size = 1.5) +
    ylab("Value\n") +
    xlab("\nDate") +
    theme_bw(base_size = 26)

g4

#ggsave(file="Tweeet_Count_C.pdf", width = 297, height = 210, units = "mm",
       limitsize = FALSE, dpi = 900)
#unlink("Tweeet_Count_C.pdf")
