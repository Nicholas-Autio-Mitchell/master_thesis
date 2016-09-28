###################### ============================================================ ######################
######################  Model Dow returns with binomial family for pos/neg returns  ######################
###################### ============================================================ ######################

#' We repeat the work completed in 'predictor_of_values.R', working through all the subsets,
#' however we are predicting only the movement of the market using a binomial model,
#' which means family = Binomial() in mboost.
#' We cannot measure the residuals as such, because we only know if we were correct for not
#' with the binary outcome.
#'
#' The results can be merged with a model that only predicts the magnitude of the returns, but does not
#' concern itself with the sign. Our final results would be x = magnitude * sign. Where the magnitude
#' comes from one model and the sign from the binomial model.
#'
#' NOTE: When predicting the magnitude, we are only looking at continuous positive values and so the family will
#' be not be Gaussian. We can use family = GammaReg() in mboost.

## ===================== ##
##  Setup for AWS usage  ##
## ===================== ##

library(mboost)
library(caret)
library(data.table)
library(doParallel)

install.packages("mboost")
install.packages("caret")
install.packages("data.table")
install.packages("multilevelPSA")
install.packages("doParallel")

## ================================================= ##
##  Source necessary files with functions used here  ##
## ================================================= ##

## If all files are in this folder, all data should be already in same folder as this file
## This R file should open the R instance so the wd() is this filder!
this_folder <- getwd()

## The required data for all modelling
load(paste0(this_folder, "/subsets_lagged.rda"))

## Files that contain functions - they don't perform anything themselves
source(paste0(this_folder, "/corr_pruner.R"))
source(paste0(this_folder, "/results_template.binomial.R"))
source(paste0(this_folder, "/ultra_modeller.binomial.R"))

## =============================== ##
##  Setup for parallel processing  ##
## =============================== ##

## Windows
#install.packages("doParallel")
library(doParallel)
registerDoParallel(8)

## ===================================================== ##
##  Select data and remove highly correlated covariates  ##
## ===================================================== ##

## Regardless of the model being run, this step ois to be performed on all data sets before any transformations
## or anything else. It is to be held consistent across all models for comparison purposes.

## Create lists to store all subsets for input to my_CV()
input_data <- sapply(c("Lag.1", "Lag.2", "Lag.3", "Lag.4", "Lag.5"), function(x) NULL)
input_subsets <- sapply(c("trad_small", "trad_large", "sent_small", "sent_large", "combined"), function(x) NULL)
for(i in 1:length(input_data)){input_data[[i]] <- input_subsets}

## Assistance:
## subsets_lagged[[4]][[3]]   --> first index is the lag value, here Lag.4 [(i-1) = lag]   
##                            --> second index is the subset.  1 = trad_small
##                                                             2 = trad_large
##                                                             3 = sent_small
##                                                             4 = sent_large
##                                                             5 = combined

## Need to run this each time too to take a fresh copy of the data. DOW.DP is removed by reference otherwise
raw_data <- copy(subsets_lagged)
for(i in 1:5) {
    message(paste0("++ ----  ", names(raw_data)[i], "  ---- ++"))
    for(j in 1:5) {
        input_data[[i]][[j]] <- corr_pruner(raw_data[[i]][[j]],
                                            cutoff = 1,
                                            plot = FALSE,
                                            print.removed = FALSE,
                                            s_name = names(input_data[[i]])[j])
    }
}

## =================================================== ##
##  Convert the outcome DOW.DP to a binomial response  ##
## =================================================== ##

## We want to predict the movement of the market, so if the market went down or didn't move, our outcome is 0.
## If the market climbed in value, then our outcome is 1
## There is only one value of zero over the entire time period.

for(i in 1:5) {
    for(j in 1:5) {

        input_data[[i]][[j]][, DOW.bin := DOW.DP]       #create a new column to house the binomial repsonse
        input_data[[i]][[j]][DOW.DP <= 0, DOW.bin := 0] #all returns less than or equal to zero, set to 0
        input_data[[i]][[j]][DOW.DP > 0, DOW.bin := 1]  #all returns greater than zero, set to 1
        input_data[[i]][[j]][, DOW.bin := as.factor(DOW.bin)] #outcome variable must be factor for Binomial()
        
        ## We no longer require the numeric Dow returns, so remove the column
        input_data[[i]][[j]][, DOW.DP := NULL]
        ## Make the new binomial response the first column
        setcolorder(input_data[[i]][[j]], neworder = c(ncol(input_data[[i]][[j]]), 1:(ncol(input_data[[i]][[j]]) - 1)))
    }
}

## ================================================================== ##
##  Create one list containing all data tables to optimise foreach()  ##
## ================================================================== ##

input_data.one_list <- unlist(input_data, recursive = FALSE)

## Create vector holding the names of the input data to use when saving the output
output_names <- names(input_data.one_list)

###################### ======================== ######################
######################  Run the data in a loop  ######################
###################### ======================== ######################

## =================================================== ##
##  Run model on flat list to use all available cores  ##
## =================================================== ##

## Assign results to one parent object on top of saving each result individually
all_results.bin.no_cutoff.frame40 <- foreach(i = 1:length(input_data.one_list),
                                        .combine = append,
                                        .packages = c("mboost", "caret", "data.table")) %dopar%
    {
        tmp_results <- bin_model(input_data.one_list[[i]], nIter = 2000,
                                   shrinkage = 0.05, frame_size = 40)
    
        to_save <- paste0("bin_results.", as.character(names(input_data.one_list)[[i]])) #desired name
        assign(to_save, tmp_results)    #assign data to desired name
        file_name <- paste0(to_save, ".rda") #create desired file name
        save(list = to_save, file = file_name) #save data with desired names
        
        ##Print progress
        print(paste0("Finished chunk: ", as.character(i), "/", as.character(length(input_data.one_list)),
                     " (", output_names[i], ")",
                     " at ", substr(as.character(Sys.time()), 1, 19)))

        ## Create a list of the results which is then appended to in ever foreach() loop
        par_results <- list(tmp_results)
        names(par_results) <- paste0(names(input_data.one_list[i]))

        return(par_results)

    }

## Save large object holding all results
save(all_results.bin.no_cutoff.frame40, file = paste0(this_folder, "/all_results.bin.no_cutoff.frame40.rda"))

## ================================================== ## - Test with nIter = 1000, shrinkage = 0.05
##  Run through all data sets, using parallel chunks  ## --> 4 cores took 17 minutess
## ================================================== ## --> 5 cores took 15.5 minutes

## This works through the subsets of each lag separately.
## This means it always waits until one lag is finished 

for(i in 1:5) {                         #i represents the lag value 
    
    my_results_par <- foreach(j = 1:length(input_data[[i]]), #j represents the subset
                              .combine = append,
                              .packages = c("mboost", "caret", "data.table")) %dopar%
        {
            tmp_results <- ultra_model(input_data[[i]][[j]], nIter = 1000,
                                 shrinkage = 0.05, frame_size = 40)

            to_save <- paste0("bin_results_", as.character(names(input_data)[[j]])) #desired name
            assign(to_save, tmp_results)        #assign data to desired name
            file_name <- paste0("binomial_results.L", as.character(i), ".",
                                as.character(names(input_data[[i]])[[j]]), ".rda") #create desired file name
            save(list = to_save, file = file_name) #save data with desired names
            
            ##Print progress
            print(paste0("Finished chunk: ", as.character(j), "/", as.character(length(input_data[[i]])),
                         " with a lag value of ", as.character(i), "/5"))

            
            par_results <- list(tmp_results)
            names(par_results) <- paste0(names(input_data[[i]])[j])

            return(par_results)
            
        }
}

## ================================ ## - Test with nIter = 1000, shrinkage = 0.05
##  Run each data set for all lags  ## --> testing cv_ 1:12, took 28 mins
## ================================ ##

for(i in 1:5) {
  
  for(j in 1:length(input_data[[i]])) {
    
    data_chunk <- copy(input_data[[i]][[j]])
    
    ## Running 
    tmp_results <- ultra_model(data_chunk, nIter = 1000, shrinkage = 0.05, frame_size = 40)
    
    to_save <- paste0("bin_results_", as.character(names(input_data)[[j]])) #desired name
    assign(to_save, tmp_results)        #assign data to desired name
    file_name <- paste0("bin_results.L", as.character(i), ".",
                        as.character(names(input_data[[i]])[[j]]), ".rda") #create desired file name
    save(list = to_save, file = file_name) #save data with desired names
    
    ##Print progress
    print(paste0("Finished chunk: ", as.character(j), "/", as.character(length(input_data[[i]])),
                 " with a lag value of ", as.character(i), "/5"))
    
    rm(data_chunk)
  }
}

