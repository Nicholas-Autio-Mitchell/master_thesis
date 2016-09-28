###################### ======================================================== ######################
######################  Separate the main data sets into subsets for modelling  ######################
###################### ======================================================== ######################

#' Create sensible subsets of the data to e.g. remove multicollinearity factors
#' These subsets also contain the 6 levels of lag that have already been specified [i.e. 0-5]
#'
#' We also make use of last alterations to the data. E.g. transforming other market data to log return
#' That work was completed in 'data_creator.R'


#' An general overview of the data subsets:
#' 
#' | Name       | Components (log_ret ~ ...) | Reasoning                                    |
#' |------------+----------------------------+----------------------------------------------|
#' | dow_only   | lagged log_ret             | Most basic example for comparison            |
#' | dow_trad   | gold, oil, sp500, int rates| Traditional model factors                    |
#' | dow_macro  | all (and only) macro data  | Many macro factors handled well              |
#' | dow_SA_avg | average sentiment scores   | Sentiment analysis explains var.             |
#' | dow_SA_all | all individual SA results  | SA from certain models might perform better  |
#' | dow_best   | trad + macro + best of SA  | All data to showcase component-wise boosting |
#' |------------+----------------------------+----------------------------------------------|


## ==================================== ##
##  Guidelines for subsetting the data  ##
## ==================================== ##

#' The data is saved in several different places, depending on where it originated from.
#'
#' There are two main categories of data: (1) Sentiment data, and (2) Macroeconomic data
#' For both categories, there is the original data, plus 1-5 lags, individually saved.
#' Furthermore there are both the nominal values and the log returns for all macro economic data.
#'
#' Subsets are to be created that make use of all the data.

## How the data is separated
#' --> "all_nominal_data.rda"
#' This contains all sentiment analysis data and its lagged values - this means it
#' contains the individual search-terms and models as well as the scaled-averages
#' It also contains all original and lagged nominal values of all macroeconomic data
#' --> "indice_log_lag_imputed.rda"
#' This contains the log returns for all indices: DJI, DAX, FTSE100, ...
#' These contain a 'pre-day', meaning the zero-lag values still have the same length
#' time-series all the other zero-lag data
#' --> "macro_lag_log_imputed.rda"
#' This contains all the other macroeconomic data: gold, copper, zc_yield, ...
#' These also make use of a 'pre-day' inclusion so cover the original time-series

## =============== ##
##  Load all data  ##
## =============== ##

## Sentiment analysis data - individual (all scaled) and averaged data
#' This also includes the non logged/lagged macro data, which isn't required here
##load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/ready_to_boost.rda")   ## This had errors with the dates
##load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/all_data_lagged.rda")  ## Items in list incorrectly ordered
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/all_nominal_data.rda")
## The log returns for the indices
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/indice_lag_log_imputed.rda")
## The log return data for other macro data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/macro_lag_log_imputed.rda")

## We also make use of a function and some data in a different script
#' This allows us to add the dummy variables for dates -
#' additionally removing their lagged equivalents
source("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/add_dummy_vars.R")

## ===================================================== ##
##  Code to consistently name the dates columns 'index'  ##
## ===================================================== ##

## ## Loop through each data set in the nominal data
## for(h in 1:3){
##     for(i in 1:6){
        
##         names(all_nominal_data[[h]][[i]]) <- c("index", names(all_nominal_data[[h]][[i]])[2:length(names(all_nominal_data[[h]][[i]]))])
##     }
## }

## ## Same for the indice data
## for(h in 1:3){
##     for(i in 1:6){
        
##         names(indice_lag_log_imputed[[h]][[i]]) <- c("index", names(indice_lag_log_imputed[[h]][[i]])[2:length(names(indice_lag_log_imputed[[h]][[i]]))])
##     }
## }

## ## Same once again for the macro data
## for(h in 1:3){
##     for(i in 1:6){
        
##         names(macro_lag_log_imputed[[h]][[i]]) <- c("index", names(macro_lag_log_imputed[[h]][[i]])[2:length(names(macro_lag_log_imputed[[h]][[i]]))])
##     }
## }

## save(all_nominal_data, file="all_nominal_data.rda")
## save(indice_lag_log_imputed, file="indice_lag_log_imputed.rda")
## save(macro_lag_log_imputed, file="macro_lag_log_imputed.rda")

## ============================================= ##
##  Get quick overview of data sets as imported  ##
## ============================================= ##

## Print structure and  number of NAs found in each data set (nominal)
ls.str(all_nominal_data)
for( i in 1:3){
    for( j in 1:6){
        message(sum(is.na(all_nominal_data[[i]][[j]])))
    }
}

## Print structure and number of NAs found in each data set (macro)
ls.str(macro_lag_log_imputed)
for( i in 1:3){
    for( j in 1:6){
        message(sum(is.na(macro_lag_log_imputed[[i]][[j]])))
    }
}

## Print structure and number of NAs found in each data set (indices)
ls.str(indice_lag_log_imputed)
for( i in 1:3){
    for( j in 1:6){
        message(sum(is.na(indice_lag_log_imputed[[i]][[j]])))
    }
}


## Conclusions:
#' Indice data has fewer observations (694) than all_nominal_data (695) and macro (697)
#' There are only NAs in the {caret} subsets, where imputation is performed as a part
#' of the model-implemented preprocessing. Otherwise zero NAs, due to our imputation.

#' After we have combined these to make the desired data sets in the table above, it will be
#' necessary to match the time-series of the data sets by either removing 1(all_nominal) and
#' 3 (macro) days to match the shortest (indice) time-series. An alternative would be to once again
#' impute these values to maintain the longest time-series possible. It isn't of much significance.

## ================================================ ##
##  Create test set to then expand across all data  ##
## ================================================ ##

## Merge data into one data frame --> first only using data with lag=2
##a <- ready_to_boost$ds_locf_lags$ds_locf_L2   # not used anymore, but data still in Archive

a <- all_nominal_data$locf$locf_L4
b <- indice_lag_log_imputed$locf$locf_L4
c <- macro_lag_log_imputed$locf$locf_L4

myMerge <- function(x, y){merge(x, y, by.x = "index", by.y = "index", all = TRUE)}

myData <- Reduce(myMerge, list(a, b, c))

## Use the function in 'add_dummy_vars.R' to leave only one set of dummy vars
myData <- add_dummy_vars(myData)

## Re-impute data to keep longest time series possible (see notes above)
## The dates column is now called 'index' - results of as.xts
myData <- as.data.table(na.locf(as.xts(myData)))

## Required for every data set --> put to one side
mainDates <- myData[, .(index)]
the_dow <- subset(myData, select = c(grep("^log_ret_DJI", names(myData))))

## ================= ##
##  Create dow_only  ##
## ================= ##

dow_only <- cbind(mainDates, subset(myData, select = c(grep("log_ret_DJI", names(myData)))))
#names(dow_only) <- c("index", "L1_log_ret_DJI", "log_ret_DJI")

## ================= ##
##  Create dow_trad  ##
## ================= ##

sp500 <- subset(myData, select = c(grep("log_ret_GSPC", names(myData))))
oil <- subset(myData, select = c(grep("log_oil_WTI", names(myData))))
int_rates <- subset(myData, select = c(grep("(log_zc_US_1Y|log_zc_US_10Y)", names(myData))))
gold <- subset(myData, select = c(grep("log_gold_USD", names(myData))))

## Merge all subsets and also the first row, which contains many NAs
dow_trad <- cbind(mainDates, the_dow, sp500, oil, int_rates, gold)     ##[2:nrow(mainDates)]

## ================== ##
##  Create dow_macro  ##
## ================== ##

## This includes the lags of the Dow Returns itself, which is not the case all all subsets above
dow_ret <- subset(myData, select = c(grep("log_ret_DJI", names(myData))))
## The first 'macro' variable is log_ret_DAX. Assuming we are only using logged values
macro_data <- subset(myData, select = names(myData)[(which(
                                                        grepl("log_ret_DAX",
                                                              names(myData)))[1]):ncol(myData)])

## Merge and trim first row, which contains many NAs
dow_macro <- cbind(mainDates, dow_ret, macro_data)      ##[2:nrow(mainDates)]

## =================== ##
##  Create dow_SA_avg  ##
## =================== ##

SA_avg <- subset(myData, select = c(grep("avg_", names(myData))))

dow_SA_avg <- cbind(mainDates, the_dow, SA_avg)


## =================== ##
##  Create dow_SA_all  ##
## =================== ##

SA_all <- subset(myData, select = c(grep("(Sentistrength_pos|Sentistrength_neg|Emolex_raw|Sentiment140|Vader_Afinn|Vader_decimal)", names(myData))))

dow_SA_all <- cbind(mainDates, the_dow, SA_all)

## ================ ##
##  Create dow_best  ##
## ================ ##

## this model will take either the average SA data or the individual data, whichever performs best

#dow_best <-

## ================================ ##
##  Join all subsets into one list  ##
## ================================ ##

## Create list with correct element names to hold subsets
mod_data_v2 <- sapply(c("dow_only", "dow_trad", "dow_macro", "dow_SA_avg", "dow_SA_all", "dow_best"),
                            function(x) NULL)

mod_data_v2$dow_only <- dow_only
mod_data_v2$dow_trad <- dow_trad
mod_data_v2$dow_macro <- dow_macro
mod_data_v2$dow_SA_avg <- dow_SA_avg
mod_data_v2$dow_SA_all <- dow_SA_all
#modelling_data_v1$dow_best <- dow_best    ## We can remove this element from the list for now then
mod_data_v2 <- mod_data_v2[-length(mod_data_v2)]    ## Here we remove the last element from the list

## Save the data (doesn't contain the empty element for dow_best)
save(mod_data_v2, file = "mod_data_v2.rda")

## Create list to save results
v2_results <- sapply(names(mod_data_v2), function(x) NULL)

## Run glmboost on each subset of data, with CV
for( i in 1:length(mod_data_v2)){

    data_in <- mod_data_v2[[i]]
    data_in <- data_in[, index:=NULL]
    ## Create formula from data

    ## Create the formula
    predictors <- names(data_in)[names(data_in) != "log_ret_DJI"]
    rhs <- paste(predictors, collapse = " + ")
    formula <- as.formula(paste("log_ret_DJI ~", rhs))

    
    rhs <- paste(colnames(data_in)[1:length(colnames(data_in))-1], collapse = " + ")
    formula <- as.formula(paste("log_ret_DJI ~", rhs))

    ## Fit the model using glmboost
    v2_results[[i]][[1]] <- glmboost(formula, data = data_in, center = TRUE,
                                     control = boost_control(mstop = 5000, nu = 0.01))
    ## Perform cross-validation to find optimal 'mstop'
    v2_results[[i]][[2]] <- cvrisk(v2_results[[i]][[1]])

    ## Save the optimal mstop
    v2_results[[i]][[3]] <- mstop(v2_results[[i]][[2]])
}



###################### ====================================================================== ######################
######################  Use above code to create subsets for all lags and imputation methods  ######################
###################### ====================================================================== ######################

## =============== ##
##  Load all data  ## --> same as at beginning of script, so possibly not required
## =============== ##

## Sentiment analysis data - individual (all scaled) and averaged data
#' This also includes the non logged/lagged macro data, which isn't required here
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/all_nominal_data.rda")
## The log returns for the indices
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/indice_lag_log_imputed.rda")
## The log return data for other macro data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/macro_lag_log_imputed.rda")
## Function and data to deal with dummy variables for dates
source("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/add_dummy_vars.R")

## ================ ##
##  create subsets  ##
## ================ ##

#' All steps should be identical once the initial subset is created (i.e. a, b, c are selected)
#' This means creating a loop for the imputation method (caret, locf and spline)
#' and also for the lag, 0:5.

## Create main list to hold all subsets
all_subsets <- sapply(c("caret", "locf", "spline"), function(x) NULL)

all_subsets$caret <- sapply(c("lag_0", "lag_1", "lag_2", "lag_3", "lag_4", "lag_5"), function(x) NULL)
all_subsets$locf <- sapply(c("lag_0", "lag_1", "lag_2", "lag_3", "lag_4", "lag_5"), function(x) NULL)
all_subsets$spline <- sapply(c("lag_0", "lag_1", "lag_2", "lag_3", "lag_4", "lag_5"), function(x) NULL)

## Create a list for each subset i.e. one of these lists for each combination of lag and imputation method
## "dow all" is kept and so must be left out of indexing later on
subsets <- sapply(c("dow_only", "dow_trad", "dow_macro", "dow_SA_avg", "dow_SA_all", "dow_best"),
       function(x) NULL)

## Apply lists throughout to assign all subsets
for(i in 1:3){
    for(j in 1:6){
        all_subsets[[i]][[j]] <- subsets
    }
}

## ## Original subsetting for test subset
## a <- all_nominal_data$locf$locf_L2
## b <- indice_lag_log_imputed$locf$locf_L2
## c <- macro_lag_log_imputed$locf$locf_L2

## Lop through all subsets
for( i in 1:3){                          # i : represents the imputation method
    for( j in 1:6){                      # j : represents the number [j-1] of lags

        a <- all_nominal_data[[i]][[j]]
        b <- indice_lag_log_imputed[[i]][[j]]
        c <- macro_lag_log_imputed[[i]][[j]]

        ## Define a function to merge the data sets
        myMerge <- function(x, y){merge(x, y, by.x = "index", by.y = "index", all = TRUE)}
        ## Merge them
        myData <- Reduce(myMerge, list(a, b, c))

        ## Use the function in 'add_dummy_vars.R' to leave only one set of dummy vars
        myData <- add_dummy_vars(myData)

        ## Re-impute data to keep longest time series possible (see notes above)
        ## The dates column is now called 'index' - result of as.xts()
        if( i == 1){
            ## Do nothin to keep NAs which will be imputed using {caret}
            message("caret subsets not imputed")
        } else if( i == 2){
            myData <- as.data.table(na.locf(as.xts(myData)))
        } else if( i == 3){
            myData <- as.data.table(na.spline(as.xts(myData)))
        }

        ## Required for every data set --> put to one side
        mainDates <- myData[, .(index)]
        the_dow <- subset(myData, select = c(grep("^log_ret_DJI", names(myData))))

        ## ================= ##
        ##  Create dow_only  ##
        ## ================= ##

        dow_only <- cbind(mainDates, subset(myData, select = c(grep("log_ret_DJI", names(myData)))))
        ##names(dow_only) <- c("index", "L1_log_ret_DJI", "log_ret_DJI")

        ## ================= ##
        ##  Create dow_trad  ##
        ## ================= ##

        sp500 <- subset(myData, select = c(grep("log_ret_GSPC", names(myData))))
        oil <- subset(myData, select = c(grep("log_oil_WTI", names(myData))))
        int_rates <- subset(myData, select = c(grep("(log_zc_US_1Y|log_zc_US_10Y)", names(myData))))
        gold <- subset(myData, select = c(grep("log_gold_USD", names(myData))))

        ## Merge all subsets and also the first row,
        dow_trad <- cbind(mainDates, the_dow, sp500, oil, int_rates, gold)

        ## ================== ##
        ##  Create dow_macro  ##
        ## ================== ##

        ## This includes the lags of the Dow Returns itself, which is not the case all the subsets above
        dow_ret <- subset(myData, select = c(grep("log_ret_DJI", names(myData))))
        macro_data <- subset(myData, select = names(myData)[(which(
                                                                grepl("log_ret_DAX",
                                                                      names(myData)))[1]):ncol(myData)])

        ## Combine data
        dow_macro <- cbind(mainDates, dow_ret, macro_data)

        ## =================== ##
        ##  Create dow_SA_avg  ##
        ## =================== ##

        ## Extract required variables
        SA_avg <- subset(myData, select = c(grep("avg_", names(myData))))
        ## Attach to dates and dependent variable
        dow_SA_avg <- cbind(mainDates, the_dow, SA_avg)

        ## =================== ##
        ##  Create dow_SA_all  ##
        ## =================== ##

        SA_all <- subset(myData, select = c(grep("(Sentistrength_pos|Sentistrength_neg|Emolex_raw|Sentiment140|Vader_Afinn|Vader_decimal)", names(myData))))

        dow_SA_all <- cbind(mainDates, the_dow, SA_all)

        ## =========================== ##
        ##  Assign all to all_subsets  ##
        ## =========================== ##

        ## Assign each of the subsets to the relevant place within list: "all_subsets"
        all_subsets[[i]][[j]][[1]] <- dow_only
        all_subsets[[i]][[j]][[2]] <- dow_trad
        all_subsets[[i]][[j]][[3]] <- dow_macro
        all_subsets[[i]][[j]][[4]] <- dow_SA_avg
        all_subsets[[i]][[j]][[5]] <- dow_SA_all
        ##all_subsets[[i]][[j]][[6]] <- dow_best   ## We can use this later once preliminary results are in

    }
}

## Print number of NAs in all subsets
for( i in 1:3){

    message(paste0("----------------- ", names(all_subsets)[[i]]))
    
    for( j in 1:6){

        message(paste0("+++++ ", names(all_subsets[[i]])[[j]]))
        
        for(k in 1:5){
            
            message(paste0(names(all_subsets[[i]][[j]])[[k]], ": ", sum(is.na(all_subsets[[i]][[j]][[k]]))))
        }
    }
}

## ================= ##
##  Create dow_best  ## --> not used! Possibly replaced later by 'cherry picking' models
## ================= ##

## this model will take either the average SA data or the individual data, whichever performs best

#dow_best <-

## ========================= ##
##  Save all_subsets output  ##
## ========================= ##

## Although rather encompassing, there are still other subsets that could be used
## We could also use nominal data as opposed to lagged data

save(all_subsets, file = "all_subsets_v1.rda")


###################### =========================================================== ######################
######################  Create loop to test all subsets - excluding caret for now  ######################
###################### =========================================================== ######################


## Load data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_subsets/all_subsets_v1.rda")

## Create structure to save results - this replicates the input structure that we loop through
#all_results <- rapply(all_subsets, function(q) { q <- NULL; q }, how = "list")

## ================================== ##
##  Create container for all results  ##
## ================================== ##

## Create each layer of results container
my_results <- sapply(c("caret", "locf", "spline"), function(x) NULL)
my_lags <- sapply(c("lag_0", "lag_1", "lag_2", "lag_3", "lag_4", "lag_5"), function(x) NULL)
my_subsets <- sapply(c("dow_only", "dow_trad", "dow_macro", "dow_SA_avg", "dow_SA_all", "dow_best"), function(x) NULL)
## Apply short lists throughout to assign output from glmboost(), cvrisk() and the mstop value
my_results_headers <- sapply(c("glmFit", "cvr", "mstop"), function(x) NULL)

## Create the container
for(i in 1:3){
    my_results[[i]] <- my_lags
    for(j in 1:6){
        my_results[[i]][[j]] <- my_subsets
        for(k in 1:6){
            my_results[[i]][[j]][[k]] <- my_results_headers
        }
    }
}

#' i is imputation --> 2:3 to rule out caret for now
#' j is the lag --> 1:6 for lags zero to five
#' k is the subset --> 1:5 (sixth is dow_best)

## Run glmboost on each subset of data, with CV
for( i in 2:3) {

    for( j in 1:6) {

        for( k in 1:5) {

            if(j==1 & k==1){k=2}               # skip lag = 0 for "dow_only", as it cannot work
            
            data_in <- copy(all_subsets[[i]][[j]][[k]]) # We have to copy a data.table, otherwise it is a reference
            data_in[, index:=NULL]
            
            ## Create the formula
            myPreds <- names(data_in)[names(data_in) != "log_ret_DJI"]
            rhs <- paste(myPreds, collapse = " + ")
            formula <- as.formula(paste("log_ret_DJI ~", rhs))
            
            ## rhs <- paste(colnames(data_in)[1:length(colnames(data_in))-1], collapse = " + ")
            ## formula <- as.formula(paste("log_ret_DJI ~", rhs))

            ## Fit the model using glmboost
            my_results[[i]][[j]][[k]][[1]] <- glmboost(formula, data = data_in, center = TRUE,
                                                       control = boost_control(mstop = 5000, nu = 0.01))
            ## Perform cross-validation to find optimal 'mstop'
            my_results[[i]][[j]][[k]][[2]] <- cvrisk(my_results[[i]][[j]][[k]][[1]])

            ## Save the optimal mstop
            my_results[[i]][[j]][[k]][[3]] <- mstop(my_results[[i]][[j]][[k]][[2]])
        }
    }
}

## Print the mstop values
for(i in 2:3) {
    message(paste("++++++++++ ", names(my_results)[[i]], " ++++++++++"))
    for(j in 1:6) {
        message(paste0("---------- ", names(my_results[[i]])[[j]], " ----------"))
        for(k in 1:5) {
            message(names(my_results[[i]][[j]])[[k]])
            message(my_results[[i]][[j]][[k]][[3]])
        }
    }
}

## ================================= ##
##  Perform cross validation (test)  ##
## ================================= ##

## Choose a model that has a relatively high mstop value
## cv_data <- all_results$locf$lag_3$dow_macro$glmFit
cv_data <- all_results[[2]][[4]][[3]][[1]] #equivalent


# Set glm output to optimal mstop value
cv_data[all_results[[2]][[4]][[3]][[3]]]

## Measure predictive accuracy of model using an anchored prediction window
## Means length of time-series used to make prediction grows for every prediction



