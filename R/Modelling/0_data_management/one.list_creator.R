###################### ============================================= ######################
######################  Create one list holding all lagged subsets  #######################
###################### ============================================= ######################

#' This takes the file 'subsets_lagged.rda', which contains a list of five lists. Each of these
#' nested represents one lag value and holds all 5 subsets with that level of lag.
#'
#' This file takes that data and makes the list flat, one list of 25 (5 subsets x 5 lags) tables.
#' This is to allow parallel processing to utilise as many cores as possible until completion.

## ---------------------------------- ##
##  Load the base data to be altered  ##
## ---------------------------------- ##

load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/subsets_lagged.rda")

## ===================================== ##
##  Create one list holding all subsets  ##
## ===================================== ##

all_subsets.one_list <- unlist(subsets_lagged, recursive = FALSE)

save(all_subsets.one_list, file = "subsets_lagged.one_list.rda")


#' As this is such an uncomplicated process, it may be performed on the fly as necessary.
