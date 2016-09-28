###################### =========================================== ######################
######################  Use simple loop to perform time-series CV  ######################
###################### =========================================== ######################

## This script should be left unchanged. It is to be used for predicting the values
## that the Dow will take. This file has been copied to the Dropbox folder 'ultra'
## where it is used for six variants of the glm_boost modelling

#' Separate scripts will be used to indepedently predict the sign and magnitude,
#' which may then be combined.
#' Similarly for stochastic gradient boosting.

## ===================== ##
##  Setup for AWS usage  ##
## ===================== ##

install.packages("mboost")
install.packages("caret")
install.packages("data.table")
install.packages("multilevelPSA")
install.packages("doMC")
install.packages("doParallel")

library(mboost)
library(caret)
library(data.table)
library(multilevelPSA)
library(doParallel)

## ================================================= ##
##  Source necessary files with functions used here  ##
## ================================================= ##

## If all files are in this folder, all data should be already in same folder as this file
## This R file should open the R instance so the wd() is in this filder!
this_folder <- getwd()

## The required data for all modelling
load(paste0(this_folder, "/subsets_lagged.rda"))

## Files that contain functions - they don't perform anything themselves
## These files names may need to be adjusted, each one similar to "X.glm_boost.R"
source(paste0(this_folder, "/corr_pruner.R"))
source(paste0(this_folder, "/error_functions.abs.R"))
source(paste0(this_folder, "/results_template.abs.R"))
source(paste0(this_folder, "/ultra_modeller.abs.R"))

## =============================== ##
##  Setup for parallel processing  ##
## =============================== ##

library(doParallel)
registerDoParallel(8)

## ===================================================== ##
##  Select data and remove highly correlated covariates  ##
## ===================================================== ##

## Create lists to store all subsets for input to my_CV()
input_data <- sapply(c("Lag.1", "Lag.2", "Lag.3", "Lag.4", "Lag.5"), function(x) NULL)
input_subsets <- sapply(c("trad_small", "trad_large", "sent_small", "sent_large", "combined"), function(x) NULL)
for(i in 1:length(input_data)){input_data[[i]] <- input_subsets}

## Need to run this each time too to take a fresh copy of the data. DOW.DP is removed by reference otherwise
raw_data <- copy(subsets_lagged)
for(i in 1:5) {
    message(paste0("++ ----  ", names(raw_data)[i], "  ---- ++"))
    for(j in 1:5) {
        input_data[[i]][[j]] <- corr_pruner(raw_data[[i]][[j]],
                                            cutoff = 0.6,
                                            plot = FALSE,
                                            print.removed = FALSE,
                                            s_name = names(input_data[[i]])[j])
    }
}

## ================================================================== ##
##  Create one list containing all data tables to optimise foreach()  ##
## ================================================================== ##

input_data.one_list <- unlist(input_data, recursive = FALSE)

## Create vector holding the names of the input data to use when saving the output
output_names <- names(input_data.one_list)

## =================================================== ##
##  Run model on flat list to use all available cores  ##
## =================================================== ##

## Assign results to one parent object on top of saving each result individually
all_results.abs.cutoff60.frame40 <- foreach(i = 1:length(input_data.one_list),
                                        .combine = append,
                                        .packages = c("mboost", "caret", "data.table")) %dopar%
    {
        tmp_results <- abs_model(input_data.one_list[[i]], nIter = 1200,
                                   shrinkage = 0.05, frame_size = 40)
    
        to_save <- paste0("abs_results.", as.character(names(input_data.one_list)[[i]])) #desired name
        assign(to_save, tmp_results)    #assign data to desired name
        file_name <- paste0(to_save, ".rda") #create desired file name
        save(list = to_save, file = file_name) #save data with desired names
        
        ## Create a list of the results which is then appended to in ever foreach() loop
        par_results <- list(tmp_results)
        names(par_results) <- paste0(names(input_data.one_list[i]))

        return(par_results)

    }

## Save large object holding all results
save(all_results.abs.cutoff60.frame40, file = paste0(this_folder, "/all_results.abs.cutoff60.frame40.rda"))
