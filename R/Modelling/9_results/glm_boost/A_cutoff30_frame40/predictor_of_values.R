###################### =========================================== ######################
######################  Use simple loop to perform time-series CV  ######################
###################### =========================================== ######################

## This script should be left unchanged. It is to be used for predicting the values
## that the Dow will take. Separate scripts will be used to indepedently
## predict the sign and magnitude, which may then be combined

## ===================== ##
##  Setup for AWS usage  ##
## ===================== ##

library(mboost)
library(caret)
library(data.table)
library(multilevelPSA)
library(doParallel)

install.packages("mboost")
install.packages("caret")
install.packages("data.table")
install.packages("multilevelPSA")
install.packages("doMC")
install.packages("doParallel")

## ================================================= ##
##  Source necessary files with functions used here  ##
## ================================================= ##

## If all files are in this folder, all data should be already in same folder as this file
## This R file should open the R instance so the wd() is in this filder!
this_folder <- getwd()

## The required data for all modelling
load(paste0(this_folder, "/subsets_lagged.rda"))

## Files that contain functions - they don't perform anything themselves
source(paste0(this_folder, "/corr_pruner.R"))
source(paste0(this_folder, "/error_functions.R"))
source(paste0(this_folder, "/results_template.R"))
source(paste0(this_folder, "/ultra_modeller.R"))

## =============================== ##
##  Setup for parallel processing  ##
## =============================== ##

## Mac/Linux
library(doMC)
registerDoMC(5)

## Windows
#install.packages("doParallel")
library(doParallel)
registerDoParallel(4)

## ===================================================== ##
##  Select data and remove highly correlated covariates  ##
## ===================================================== ##

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
    for(j in 1:5) {
        input_data[[i]][[j]] <- corr_pruner(raw_data[[i]][[j]],
                                            cutoff = 0.3,
                                            plot = FALSE,
                                            print.removed = FALSE,
                                            s_name = names(input_data[[i]])[j])
    }
}

## ---------------------------------------------------------- ##
##  Flatten the list so that all available cores can be used  ##
## ---------------------------------------------------------- ##

## Flatten
input_data.one_list <- unlist(input_data, recursive = FALSE)

## Create vector holding the names of the input data to use when saving the output
output_names <- names(input_data.one_list)

###################### ======================== ######################
######################  Run the data in a loop  ######################
###################### ======================== ######################

## ----------------------------------------- ##
##  Use foreach to work through all subsets  ##
## ----------------------------------------- ##

## Assign results to one parent object on top of saving each result individually
all_results.glm.cutoff30.frame40 <- foreach(i = 1:length(input_data.one_list),
                                            .combine = append,
                                            .packages = c("mboost", "caret", "data.table")) %dopar%
    {
        tmp_results <- ultra_model(input_data.one_list[[i]], nIter = 2000,
                                   shrinkage = 0.05, frame_size = 40)

        to_save <- paste0("glm_results.", output_names[i])
        assign(to_save, tmp_results)        #assign data to desired name
        file_name <- paste0(to_save, ".rda") #create desired file name
        save(list = to_save, file = file_name) #save data with desired names
        
        ##Print progress
        print(paste0("Finished chunk: ", as.character(i), "/", as.character(length(input_data.one_list)),
                     " (", output_names[i], ")",
                     " at ", substr(as.character(Sys.time()), 1, 19)))

        par_results <- list(tmp_results)
        names(par_results) <- paste0(names(input_data.one_list[i]))

        return(par_results)
        
    }

save(all_results.glm.cutoff30.frame40, file = "all_results.glm.cutoff30.frame40.rda")

###################### =============================================================== ######################
######################  ¡¡¡ Old loop system with mistake in the naming conventions!!!  ######################
###################### =============================================================== ######################

## ====================================================== ## - Test with nIter = 1000, shrinkage = 0.05
##  Run through all data sets, using parallel processing  ## --> 4 cores took 17 minutess
## ====================================================== ## --> 5 cores took 15.5 minutes

for(i in 1:5) {                         #i represents the lag value 
    
    my_results_par <- foreach(j = 1:length(input_data[[i]]), #j represents the subset
                              .combine = append,
                              .packages = c("mboost", "caret", "data.table")) %dopar%
        {
            tmp_results <- ultra_model(input_data[[i]][[j]], nIter = 2000,
                                 shrinkage = 0.05, frame_size = 40)

            to_save <- paste0("glm_results_", as.character(names(input_data)[[j]])) #desired name
            assign(to_save, tmp_results)        #assign data to desired name
            file_name <- paste0("glm_results.L", as.character(i), ".",
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
    
    to_save <- paste0("glm_results_", as.character(names(input_data)[[j]])) #desired name
    assign(to_save, tmp_results)        #assign data to desired name
    file_name <- paste0("glm_results.L", as.character(i), ".",
                        as.character(names(input_data[[i]])[[j]]), ".rda") #create desired file name
    save(list = to_save, file = file_name) #save data with desired names
    
    ##Print progress
    print(paste0("Finished chunk: ", as.character(j), "/", as.character(length(input_data[[i]])),
                 " with a lag value of ", as.character(i), "/5"))
    
    rm(data_chunk)
  }
}
