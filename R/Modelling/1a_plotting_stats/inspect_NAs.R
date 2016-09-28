###################### ===================================== ######################
######################  Find and plot NA-maps of a data set  ######################
###################### ===================================== ######################

## --------------------------------------------- ##
##  This code was taken from 'data_inspector.R'  ##
## --------------------------------------------- ##


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
