
###################### ================== ######################
######################  Inspect the data  ######################
###################### ================== ######################

#' Here we can see where there is correlation and also where the NAs are
#' This can help impute the data more effectively
#' Also maybe we can see which variables can be removed.


## Load the merged data: "data_dirty"
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_dirty.rda")
## Load the bank holidays; they are saves as a vector of strings and must be converted to dates
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/holidays.rda")

## ===================================== ##
##  Inspect correlation within our data  ##
## ===================================== ##


## We must remove all NA values
corr_data <- subset(data_dirty, select = -c(1, 2, nikkei225))
corr_data <- na.locf(corr_data)
myCorr <- cor(corr_data)

## Plot correlation heatmap for entire data set
dev.new()
corrplot(myCorr, type = "upper", addCoefasPercent = TRUE, tl.cex = 0.28, mar=c(1,3,1,1))

## Look at interesting macro-economic region in more detail
cor_reg <- as.data.frame(subset(corr_data, select = (seq(120, 147, 1))))
myCorr_reg <- cor(cor_reg)
dev.new()
corrplot(myCorr_reg, type = "upper", addCoefasPercent = TRUE, tl.cex = 0.8, mar=c(2,3,4,0),
         title = "Correlations between macro-economic factors included in the model")

## Look at the correlation of the sentiment analsis results
## Here we can see the small triangle of higher correlation along the bottom axis
## This is expected, as they relate to the same search term - otherwise ~zero correlation
cor_reg <- as.data.frame(subset(corr_data, select = -(seq(120, 147, 1))))
myCorr_reg1 <- cor(cor_reg)
dev.new()
corrplot(myCorr_reg1, type = "upper", addCoefasPercent = TRUE, tl.cex = 0.35, mar=c(1,3,1,1),
         title = "Correlations between the sentiment analysis results")

## ======================================================== ##
##  Another perspective on correlation using Google Trends  ##
## ======================================================== ##

#' These simple plots help to visualise the amount of correlation that can be measured
#' within the realm of public interest, perhaps comparable to the frequency plots for tweets.
#' We see the relative number of Google searches that contained our terms or similar.
#' It helps to show in which time frames, whcih search terms appeared to be linked in the minds of the public.

## The search terms must be in groups of five or smaller
## The groups loosely follow a theme, but also are chosen to return relatively similar scaling

## First a group closely related to stock markets ('hard' traditional factors)
trends1 <- gtrends(c("dow jones", "oil prices", "interest rates", "USD EUR", "gold price"),
                   res = "week", start_date = as.Date("2013-01-14"), end_date = as.Date("2015-09-11"))

## Second a group with more general terms (macro economic)
trends2 <- gtrends(c("dow jones", "federal reserve", "financial crisis", "stock prices", "obama economy"),
                   res = "week", start_date = as.Date("2013-01-14"), end_date = as.Date("2015-09-11"))

## Thirdly the terms that are market specific but not traditional factors ('soft' traditional factors)
trends3 <- gtrends(c("dow jones", "bull market", "bear market", "market volatility", "goldman sachs"),
                   res = "week", start_date = as.Date("2013-01-14"), end_date = as.Date("2015-09-11"))

## Define custom version of plotting function
my.plot.gtrends <- function(x,
                         type = c("trend", "regions", "topmetros", "cities"),
                         region = "world",
                         resolution = c("countries", "provinces", "metros"),
                         displaymode = c("auto", "regions", "markers"),
                     ind = 1L,
                     main = "",
                         ...) {
  type <- match.arg(type)
  
  resolution <- match.arg(resolution)
  
  gvisopt <- list(region = region,
                  displayMode = "markers",
                  resolution = resolution)
  
  if (type == "trend") {
    
    df <- x$trend
    
    df <- reshape(df,
                  varying = names(df)[mapply(is.numeric, x$trend)],
                  v.names = "hit",
                  idvar = names(df)[!mapply(is.numeric, x$trend)],
                  direction = "long",
                  times = names(df)[mapply(is.numeric, x$trend)],
                  timevar = "keyword")
    
    df$start <- as.POSIXct(df$start)
    
    p <- ggplot(df, aes_string(x = "start", y = "hit", color = "keyword")) +
      geom_line() +
      xlab("Date") +
      ylab("Search hits") +
      ggtitle(main) +
      theme_bw()
    
    print(p)
    
  } else if (type == "regions") {
    
    x <- x[["regions"]][[ind]]
    
    if(all(is.na(x))) stop("Not enough search volume to show results.", 
                      call. = FALSE)
    
    df <- data.frame(loc = x[, 1], hits = x[, 2])
    
    plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
    
  } else if (type == "topmetros") {
    
    x <- x[["topmetros"]][[ind]]
    
    if(all(is.na(x))) stop("Not enough search volume to show results.", 
                      call. = FALSE)
    
    df <- data.frame(loc = x[, 1], hits = x[, 2])
    
    plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
    
  } else if (type == "cities") {
    
    x <- x[["cities"]][[ind]]
    
    if(all(is.na(x))) stop("Not enough search volume to show results.", 
                      call. = FALSE)
    
    df <- data.frame(loc = x[, 1], hits = x[, 2])
    
    plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
  }
  
  invisible(NULL)
}

## Display the trends, one per graph
dev.new()
my.plot.gtrends(trends1, main = "Dow Jones Search frequency in comparison with traditional market factors")

dev.new()
my.plot.gtrends(trends2, main = "Dow Jones Search frequency in comparison with macro-economic factors")

dev.new()
my.plot.gtrends(trends3, main = "Dow Jones Search frequency in comparison with relevant market terms")


## First steps in adding the Dow returns to the same chart...
## Plot the second plot and put axis scale on right
## plot(data_dirty$DJI, pch=15,  xlab="", ylab="", ylim=c(13500,18320), 
##     axes=FALSE, type="l", lwd = 2, col="red")
## ## a little farther out (line=4) to make room for labels
## mtext("Cell Density",side=4,col="red",line=4) 
## axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## ==================== ##
##  Where are the NAs?  ##
## ==================== ##

## Define two functions to help view NAs within whole data structure

## These packages are required
library(devtools)
##install_github("swihart/lasagnar")
library(lasagnar)   
##dfviewr(df=df.in)
library(colorspace)

## Customise function to show our data types
myPlotter <- function (X, col = rainbow_hcl(length(unique(c(X)))), axes = FALSE, 
               main = "(A)  Initial Lasagna Plot", main.adj = 0, cex.axis = 1.75, 
               gridlines = FALSE, legend = FALSE, ...)
{
    if (!legend) {
        image(t(X)[, (nrow(X):1)], col = col, axes = axes, useRaster = TRUE, 
              ...)
    }
    else {
        image.plot(t(X)[, (nrow(X):1)], col = col, axes = axes, 
                   ...)
    }
    box()
    title(main, adj = main.adj)
    if (!axes) {
        axis(1, seq(0, 1, 1/(ncol(X) - 1)), 1:ncol(X), cex.axis = cex.axis, 
             tck = 0, mgp = c(0, 0.5, 0))
        axis(4, seq(0, 1, 1/(nrow(X) - 1)), seq(as.Date("2013-01-14"), as.Date("2015-09-11"), length.out = 20), 
             las = 1, cex.axis = cex.axis, at = 1:20, labels = 1:20, tck = 0, mgp = c(0, 
                                                                                      0.2, 0))
    }
    if (gridlines) {
        axis(1, seq(1/(ncol(X) - 1) * 0.5, 1 - 1/(ncol(X) - 1) * 
                                           0.5, length = (ncol(X) - 1)), lab = NA, tck = 1, 
             lty = 1, col = "black")
        axis(2, seq(1/(nrow(X) - 1) * 0.5, 1 - 1/(nrow(X) - 1) * 
                                           0.5, length = (nrow(X) - 1)), lab = NA, tck = 1, 
             lty = 1, col = "black")
    }
}

## A slimmed down version to highlight only the NAs
## REQUIRES myPlotter ^^
myView <- function (df, col = c("red", "white", "black", "white", "black"), 
           axes = FALSE, main = "Reletive position of NA values", main.adj = 0, cex.axis = 1.75, gridlines = TRUE, 
           legend = TRUE, ...) 
{
    mat.out <- matrix(NA, nrow = nrow(df), ncol = ncol(df))
    mat.out[, sapply(df, is.POSIXct)] <- 5
    #mat.out[, sapply(df, is.factor)] <- 2
    #mat.out[, sapply(df, is.character)] <- 3
    mat.out[, sapply(df, is.numeric)] <- 4
    mat.out[is.na(df)] <- 3
    row.names(mat.out) <- 1:nrow(df)
    myPlotter(mat.out, col = col, cex = 0.67, main = main, gridlines = gridlines)
    if (legend) {
        legend("bottom", fill = col, legend = c("NA", "dates", "numeric"),
               horiz = T, xpd = NA,
               inset = c(-0.15), border = "black")
    }
}

## Unclear, but shows overview of all data types
dfviewr(data_dirty, gridlines = FALSE)

## We must remove the non-numeric predictors
d <- subset(data_dirty, select = -c(1, 2, nikkei225))

## View where the NAs are
##myPlotter(d, gridlines = FALSE)
myView(d, gridlines = FALSE)

## Remove the weekends
z <- d[is.weekend == 0, ]

## Where are the NAs after weekends are removed
##myPlotter(z, gridlines = FALSE)
myView(z, gridlines = FALSE)    # Notice the repeating pattern of non-trading days - holidays

## Have a look at the concentration of the bank holidays
x <- as.xts(data_dirty)[as.Date(holidays)]

## All bank holidays - all data
myView(x)
## Bank holidays only for market/macro data
myView(x[,120:ncol(x)])
## show names of each of the columns, i.e. the predictors
dimnames((x[,120:ncol(x)]))

## ================================================ ##
##  Conclusions - How to best carry out imputation  ##
## ================================================ ##

#' The weekends are inherently the cause of the majority of NAs
#' These must be removed, as they are not random;
#' [Missing And Random, Missing And Completely Random -> COULD be replaced]

#' We can keep some form of information from the weekend data from SA:
#' Create new dummy variables: was.sat.pos, was.sun.pos, was.weekend.pos
#' -> These will be the average of all models in each case

#' Additionally we can plot (according to notes in boook):
#' 1) Whether the weekend sentiment results are correlated with Monday-movements
#' 2) Is there are general patter? [See the four example cases in notebook]
