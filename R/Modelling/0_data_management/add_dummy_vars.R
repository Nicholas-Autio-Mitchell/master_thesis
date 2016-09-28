###################### ======================================================================== ######################
######################  Create a function to add the dummy variables for dates to a data table  ######################
###################### ======================================================================== ######################

#' The function[s] below add the dummy variables is.weekend, is.monday, is.holiday to a given data table.
#' They are added directly after the date column, which must be the first column in the table.

## Load the holidays
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/holidays.rda")
holidays <- as.Date(holidays)

## =================================================================================================== ##
##  WARNING - need to pay attention to the language used! Might need to change to English words below  ##
## =================================================================================================== ##
## ================================================================================ ##
##  WARNING - the dates columns in all L0 data sets is called 'index', not 'dates'  ## --> now corrected!
## ================================================================================ ## --> all "index"

## Define function to add dummy variable columns
add_dummy_vars <- function(data_table){

    ## Removes any existing dummy variables containing 'is.' - idea being we only need these once
    ## Does it make sense to have the lags of these dummy variables? I think not...
    data_table <- subset(data_table, select = c(grep(pattern = "^((?!is\\.).)*$", names(data_table), value = TRUE, perl = TRUE)))
    ## Save number of columns before adding new ones
    num_cols <- ncol(data_table)
    
    ## Add the new columns --> comment-out as necessary
    data_table$is.monday <- as.numeric(weekdays(data_table$index) %in% c('Montag'))
    data_table$is.holiday <- as.numeric(data_table$index %in% holidays)
    ##data_table$days_names <- weekdays(data_table$index)
    ##data_table$is.weekend <- as.numeric(weekdays(data_table$index) %in% c('Sonntag','Samstag'))
    ##data_table$is.weekend <- format(as.Date(data_table$index),"%w")   ## get numbers for days: 0-6 -> Sun-Sat

    ## Reorder the columns to add dummy variables next to the dates column
    ## This may be beter to leave commented out as it must be changed as above lines are altered
    setcolorder(data_table, c(1, seq((num_cols+1), ncol(data_table)), seq(2, num_cols)))
    
}


## ======================== ##
##  Test the function out!  ##
## ======================== ##

## ## Some test data
## y1 <- all_data_lagged$spline$spline_L3
## y2 <- dummyIndex(y1)
## ## Inspect the columns
## names(y2)

## ## Look at the bank holidays and confirm they match with the new dummy var.
## holidays
## y2[, .(index, is.holiday)]
