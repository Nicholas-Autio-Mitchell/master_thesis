###################### =================================================================== ######################
######################  Create the five subsets, ready for creating their lagged versions  ######################
###################### =================================================================== ######################

## This creates the basic five subsets used for all modelling

## Subsets selected later in the snse of 'cherry-picking' are to be created in a separate file

"""
Here is the outline of the subsets:

 | Name       | Components (dow ~ ...)                                              | Dummy Variables |
 |------------+---------------------------------------------------------------------+-----------------|
 | trad_small | Traditional factors used for market predictions.                    | No              |
 | trad_large | All macro data: indices, commodities, volatility and interest rates | Yes             |
 | sent_small | Only model averages, plus variable with weekend information         | Yes             |
 | sent_large | All individual model results, plus variable with weekend info       | Yes             |
 | combined   | contains all data: dummy variables, macroeconomic and sentiment     | Yes             |
 |            |                                                                     |                 |


The Dummy variables, when selected, are: 'day.number', 'is.monday' and 'is.holiday'


For the reasoning behind each data set, see: 'data_subsets.org'

"""

## --------------- ##
##  Load the data  ##
## --------------- ##

## Load the entire data set - one data table with all dates, indices, macro data and all sentiment analysis variables
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/all_data_final.rda")

## ================ ##
##  Create subsets  ##
## ================ ##

#' Due to the way the data was combined to create the final data set loaded above,
#' we can create the subsets using indices (column numbers)
## See the names that we are selecting from:
names(all_data_final)

## All data sets must first contain indices 1 (Date) and 113 (DOW)
## The Date column will be removed as the very last step after all legs have been created

## ------------ ##
##  trad_small  ##
## ------------ ##

## indices refer to: Date, DOW, SP500, gold_USD, oil_WTI, USD_EUR, zc_10Y
trad_small <- all_data_final[, c(1, 113, 115, 125, 127, 130, 135), with = FALSE]

## Check the constituents
names(trad_small)

## ------------ ##
##  trad_large  ##
## ------------ ##

## All macroeconomic variables
trad_large <- all_data_final[, c(1, 113, 5, 3, 6, 112, 114:140), with = FALSE]

## To see these constituents:
names(trad_large)

## ------------ ##
##  sent_small  ##
## ------------ ##

## All averaged 
sent_small <- all_data_final[, c(1, 113, 5, 3, 6, 7, 8:20), with = false]

## check constituents
names(sent_small)

## ------------ ##
##  sent_large  ##
## ------------ ##

## all individual sentiment variables
sent_large <- all_data_final[, c(1, 113, 5, 3, 6, 7, 21:111), with = FALSE]

## Check the constituents
names(sent_large)

## ---------- ##
##  combined  ##
## ---------- ##

## This contains all the columns EXCEPT non-numeric day_name, plus is.weekend (all = 0)
combined <- all_data_final[, c(1, 113, 5, 3, 6:140), with = FALSE]

## Check the constituents
names(combined)


## =============================================== ##
##  Create one list containing these base subsets  ##
## =============================================== ##

## Create named list
base_subsets <- sapply(c("trad_small", "trad_large", "sent_small", "sent_large", "combined"), function(x) NULL)

## Attach the subsets
base_subsets$trad_small <- trad_small
base_subsets$trad_large <- trad_large
base_subsets$sent_small <- sent_small
base_subsets$sent_large <- sent_large
base_subsets$combined <- combined

## Save the base subsets
save(base_subsets, file = "base_subsets.rda")

## ------------ ##
##  Next steps  ##
## ------------ ##

"""
These base subsets will be loaded into 'lag_creator.R' where the final data sets will be created.

The DOW and some dummy variables for time (that are deterministic, so known in advance) must be shifted
by one, so that when we use one row of any data table to fit a model, we are fitting the data of today to
explain the return of tomorrow.

"""
