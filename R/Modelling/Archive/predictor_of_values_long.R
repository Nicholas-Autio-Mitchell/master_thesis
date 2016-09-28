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
library(doMC)
library(doParallel)

install.packages("mboost")
install.packages("caret")
install.packages("data.table")
install.packages("multilevelPSA")
install.packages("doMC")
install.packages("doParallel")

## ==================== ##
##  Load required data  ##
## ==================== ##

##All data should be already in the session included in the zip file

## ## Raw data and a function to remove highly correlated covariates
## ## Mac
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_subsets/subsets_lagged.rda")
source("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/0_data_management/corr_pruner.R")

## ## Windows
## load("C:/cygwin64/home/dev231/Thesis/R/Modelling/run_modelling/all_subsets_v1.rda")
## source("C:/cygwin64/home/dev231/Thesis/R/Modelling/run_modelling/corr_pruner.R")

## =============================== ##
##  Setup for parallel processing  ##
## =============================== ##

## Mac/Linux
library(doMC)
registerDoMC(5)

## Windows
#install.packages("doParallel")
library(doParallel)
registerDoParallel(5)

## ========================================= ##
##  Create a structure to house the results  ##
## ========================================= ##

#' Split results into three categories:
#' (1) the input parameters used for this CV run
#' (2) stepwise results with individual errors etc., and
#' (3) errors for the entire cross-validation
results_template <- function(num_cv_steps, #to create a correctly sized template
                      frame_size, look_ahead, nIter, shrinkage) # taken from inputs to my_CV
{

    ## The main three sections of our final output object
    my_template <- sapply(c("input_parameters", "results", "errors"), function(x) NULL)

    ## Input parameters used for this model to be saved for reproducibility
    mod_parameters <- sapply(c("frame_size", "look_ahead", "nIter", "shrinkage"), function(x) NULL)
    mod_parameters$frame_size <- frame_size
    mod_parameters$look_ahead <- look_ahead
    mod_parameters$nIter <- nIter
    mod_parameters$shrinkage <- shrinkage
    
    ## We will have (num_cv_steps) prediction rounds, also equal to length(cv_sets$train)
    results <- sapply(c(paste0("cv_", seq(1:num_cv_steps))), function(x) NULL)
    ## Results to be stored during each CV-step, each time glmboost is performed
    step_results <- sapply(c("mstop", "actual", "forecast", "errors"), function(x) NULL)
    ## The individual error measures computed for each CV step
    step_errors <- sapply(c("residuals", "MAE", "MSE", "RMSE", "sign_accuracy"), function(x) NULL)

    ## Apply the lists to create entire structure
    for(i in 1:num_cv_steps){results[[i]] <- step_results}
    for(i in 1:num_cv_steps){results[[i]][[4]] <- step_errors}

    ## The overall (aggregated) errors over all CV-steps
    ## HOW best to aggregate them must still be defined
    final_error <- sapply(c("all_residuals", "all_MAE", "all_MSE", "all_RMSE", "sign_accuracy"), function(x) NULL)

    ## Assign all categories to the final object
    my_template$input_parameters <- mod_parameters
    my_template$results <- results
    my_template$errors <- final_error
    return(my_template)
}

## =========================================== ##
##  Create functions for measuring the errors  ##
## =========================================== ##

## Create functions to calculate various error measurements
## Function that returns Mean Absolute Error
mae <- function(error){mean(abs(error))}
## Function that returns the Mean Squared Error
mse <- function(error){mean(error^2)}
## Function that returns Root Mean Squared Error
rmse <- function(error){sqrt(mean(error^2))}
## Function to find numbers of times the sign was correctly predicted
signs <- function(preds, true_values){sum((preds * true_values) > 0)}

## ===================================================== ##
##  Select data and remove highly correlated covariates  ##
## ===================================================== ##

#' To begin with we choose a data set for one of three groups:
#' 1. Macro Data
#' 2. Sentiment Analysis Data (a) all (b) avg
#' 3. A combination of both 1. and 2. Components
#'
#' Choose a lag value of 3 as a mid-point of the possible choices - 1:5
#'
#' The data must be selected, copied (preventing referencing issues) and
#' lastly pruned for highly correlated covariates, for the function my_CV()

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
                                            cutoff = 0.7,
                                            plot = FALSE,
                                            print.removed = FALSE)
    }
}

###################### ============== ######################
######################  Create loops  ######################
###################### ============== ######################

## ================================== ##
##  Create one function to run loops  ##
## ================================== ##

my_CV <- function(data_in, frame_size = 40, look_ahead = 1, #CV parameters
           nIter = 2000, shrinkage = 0.05,          #glmboost parameters
           RNG.seed = TRUE,                          #set.seed for reproducible results
           updates = TRUE)                           #print loop number and time at intervals
{

    ## =============================================================== ##
    ##  Define parameters for our time-series cross-validation method  ##
    ## =============================================================== ##

    ## This is how many models are fitted. Must be equal to length(cv_sets$train)
    num_shifts <- floor((dim(data_in)[1] - frame_size) - (look_ahead -1))
    ## ## Check that we are using the data efficiently
    ## ifelse((dim(data_in)[1] - (num_shifts) - frame_size) != 0,
    ##        warning("++ Not all time-series utilised \n++ Consider using different frame_size/horizon values", call. = FALSE),
    ##        warning("++ All data used", call. = FALSE))

    ## Create index range (a.k.a. 'time slices') for each train and test sets
    #' To make the CV somewhat sparser in order to optimise the shrinkage,
    #' we could set 'skip = 10', reducing the number of models run by 90%
    cv_sets <- createTimeSlices(data_in$DOW.DP, initialWindow= frame_size,
                                horizon = look_ahead, fixedWindow=TRUE, skip = 0)
    ## Create the object to store all output
    glm_results <- results_template(num_shifts,
                                    frame_size, look_ahead, nIter, shrinkage) #Saved with results
    ## ========================================= ##
    ##  Define model parameters and run CV loop  ##
    ## ========================================= ##

    ## Create the formula
    indep_vars <- names(data_in)[names(data_in) != "DOW.DP"]
    rhs <- paste(indep_vars, collapse = " + ")
    my_formula <- as.formula(paste("DOW.DP ~", rhs))
    ## ## Alternative:
    ## my_formula <- "log_ret_DJI ~ ."

    print(paste0("Cross validation started: ", as.character(Sys.time())))
    
    ## Loop through all data for cross validation
    ## Conscious decision NOT to store all cvrisk() output
    for(i in 1:length(cv_sets$train)) {
    ##for(i in 1:12) {    
    ##foreach(i = 1:8, .packages = c("mboost", "caret", "data.table")) %dopar% {
        
        ## Run model and cross validate
        glmFit <- glmboost(my_formula, data = data_in[cv_sets$train[[i]]], center = TRUE,
                           control = boost_control(mstop = nIter, nu = shrinkage))

        ## Set seed (unique for each loop)
        ## (cvrisk() uses rmultinom() in bootstraps)
        if (RNG.seed == TRUE) {
            #set.seed(i)
            set.seed.alpha(paste0("Nicholas_Mitchell_", i))
        } else {
            message("Seed not set for pseudo-random number generation")
        }

        ## Complete 25-fold stratified bootstrap CV
        cvr <- cvrisk(glmFit)
        
        ## Put model into the position of its optimal mstop value
        glmFit[mstop(cvr), return = FALSE]

        ## Predict the (i+1)th value and measure the error
        preds <- predict(glmFit, newdata = data_in[cv_sets$test[[i]], -c("DOW.DP"), with = FALSE])
        true_values <- data_in[cv_sets$test[[i]], .(DOW.DP)]
        ## Compute the estimation error
        glmErr <- as.matrix(true_values) - preds

        ## Assign results: model output, mstop value, actual and forecasted results
        glm_results$results[[i]][[1]] <- mstop(cvr)
        glm_results$results[[i]][[2]] <- data_in[cv_sets$test[[i]], .(DOW.DP)]
        glm_results$results[[i]][[3]] <- preds
        ## Assign step-wise errors (functions allow to expand prediction horizon)
        glm_results$results[[i]][[4]][[1]] <- glmErr
        glm_results$results[[i]][[4]][[2]] <- mae(glmErr)
        glm_results$results[[i]][[4]][[3]] <- mse(glmErr)
        glm_results$results[[i]][[4]][[4]] <- rmse(glmErr)
        glm_results$results[[i]][[4]][[5]] <- signs(preds, true_values)

        ## Force garbage collection of larger objects
        rm(glmFit, cvr)

        if(updates == TRUE){
            ## print some output to show progress
            ## num_loops <- length(cv_sets$train)
            ## print(Sys.time())               #start time
            ## ifelse(i == 1*length(cv_sets$train)/6, print(Sys.time()), print(i))
            ## ifelse(i == 2*length(cv_sets$train)/6, print(Sys.time()), "")
            ## ifelse(i == 3*length(cv_sets$train)/6, print(Sys.time()), "")
            ## ifelse(i == 4*length(cv_sets$train)/6, print(Sys.time()), "")
            ## ifelse(i == 5*length(cv_sets$train)/6, print(Sys.time()), "")
            ## ifelse(i == 6*length(cv_sets$train)/6, print(Sys.time()), "")
            ## ifelse(i == length(cv_sets$train), print(Sys.time()), "")
            print(i)
            ifelse(i == 10, print(Sys.time()), "")
            ifelse(i == 100, print(Sys.time()), "")
            ifelse(i == 200, print(Sys.time()), "")
            ifelse(i == 300, print(Sys.time()), "")
            ifelse(i == 400, print(Sys.time()), "")
            ifelse(i == 500, print(Sys.time()), "")
            ifelse(i == 600, print(Sys.time()), "")
            
            
        }
    }
    print(paste0("Cross validation completed: ", as.character(Sys.time())))
    
    return(glm_results)
}

###################### ======================== ######################
######################  Run the data in a loop  ######################
###################### ======================== ######################

## ================================ ##
##  Run each data set for all lags  ## --> testing cv_ 1:12, took 28 mins
## ================================ ##

for(i in 1:5) {
  
  for(j in 1:length(input_data[[i]])) {
    
    data_chunk <- copy(input_data[[i]][[j]])
    
    ## Running 
    tmp_results <- my_CV(data_chunk, nIter = 1000, shrinkage = 0.05, frame_size = 40)
    
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

## ====================================================== ##
##  Run through all data sets, using parallel processing  ## --> testing cv_ 1:12, took 17 mins
## ====================================================== ##

for(i in 1:5) {                         #i represents the lag value 
    
    my_results_par <- foreach(j = 1:length(input_data[[i]]), #j represents the subset
                              .combine = append,
                              .packages = c("mboost", "caret", "data.table")) %dopar%
        {
            tmp_results <- my_CV(input_data[[i]][[j]], nIter = 1000,
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


## ============================== ##
##  Run a loop for each data set  ##
## ============================== ##

for(j in 1:length(input_data)) {

    data_chunk <- copy(input_data[[j]])

    ## Running 
    tmp_results <- my_CV(data_chunk)                   #use default parameters
    
    to_save <- paste0("glm_results_", as.character(names(input_data)[[j]])) #desired name
    assign(to_save, tmp_results)        #assign data to desired name
    file_name <- paste0("glm_results_", as.character(names(input_data)[[j]]), ".rda") #create desired file name
    save(list = to_save, file = file_name) #save data with desired names

    ##Print progress
    print(paste0("Finished chunk: ", as.character(j), "/", as.character(length(input_data))))

    rm(data_chunk)
}


###################### ====================== ######################
######################  Run my_CV in batches  ######################
###################### ====================== ######################

## Runs the function in smaller groups and saves the intermediate results to disk

## ======================================== ##
##  Specify how many batches are performed  ##
## ======================================== ##

#' We require that all the data is used, regardless of how many sub-rounds
#' are performed. This means the last data set must be at least as big as
#' window_size, so that the last data point can be predicted and with,
#' that all data is used

chunker <- function(data_in, frame_size = 40, num_chunks = 10) {

    ## Basically num_chunks = full_chunks + remainder
    full_chunks <- num_chunks - 1
    chunk_size <- floor((nrow(data_in)-frame_size)/full_chunks)

    ## Compute starting points/indexes for each batch
    starts <- c(1, cumsum(rep(chunk_size, full_chunks)))
    ends <- starts + chunk_size + frame_size
    closes <- c(chunk_size+frame_size, ends[2:(length(ends)-1)], nrow(data_in))
    
    chunks <- data.table(starts, closes)
    ##num_chunks <- nrow(chunks)

    return(chunks)
}

## ======================== ##
##  Select the data to use  ##
## ======================== ##

## loop_data <- input_data$Macro
## loop_data <- input_data$SA_all
loop_data <- input_data$SA_avg
## loop_data <- input_data$Mix

## ============== ##
##  Run the loop  ##
## ============== ##

## Create chunks to be used
num_batches <- 10
myFrame <- 40
chunks <- chunker(loop_data, frame_size = myFrame, num_chunks = num_batches)

## Run the loop
for(i in 1:num_batches){

    ## ## If chunks need to be used (to prevent crashing due to memory issues)
    ## data_chunk <- copy(loop_data[chunks$starts[i]:chunks$closes[i]])
    ## tmp_results <- my_CV(data_chunk)                   #use default parameters

    ## Running 
    tmp_results <- my_CV(data_chunk)                   #use default parameters
    
    to_save <- paste0("glm_Macro.part_", as.character(i)) #desired name
    assign(to_save, tmp_results)        #assign data to desired name
    file_name <- paste0("glm_Macro.part_", as.character(i), ".rda") #create desired file name
    save(list = to_save, file = file_name) #save data with desired names

    ##Print progress
    print(paste0("Finished chunk: ", as.character(i), "/", as.character(nrow(chunks))))
}

