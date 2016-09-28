###################### =============================================== ######################
######################  Function to perform modelling on one data set  ######################
###################### =============================================== ######################

## This file contains the main function which performs the boosting algorithm over the data
## it also performs the cvrisk() function, makes predictions using the mstop value obtained.
#' This is executed on a rolling basis over the time-series using a fixed-window of 40 days.
#' Errors are measured and recorded. The final output object returned is saved by the parent file,
#' which sources this file: 'predictor_of_values.abs.R'

## ================================== ##
##  Create one function to run loops  ##
## ================================== ##

abs_model <- function(data_in, frame_size = 40, look_ahead = 1, #CV parameters
                      nIter = 2000, shrinkage = 0.05,          #boosting parameters
                      RNG.seed = TRUE,                         #set.seed for reproducible results
                      updates = TRUE)                          #print loop number and time at intervals
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
  abs.model_results <- results_template(num_shifts,
                                        frame_size, look_ahead, nIter, shrinkage) #Saved with results
  ## ========================================= ##
  ##  Define model parameters and run CV loop  ##
  ## ========================================= ##
  
  ## Create the formula
  indep_vars <- names(data_in)[names(data_in) != "DOW.DP"]
  rhs <- paste(indep_vars, collapse = " + ")
  my_formula <- as.formula(paste("DOW.DP ~", rhs))
  
  ## It is necesary to make the DOW.DP column > 0 for GammaReg()
  data_in$DOW.DP <- abs(data_in$DOW.DP)
  
  print(paste0("Cross validation started: ", as.character(Sys.time())))
  
  ## Loop through all data for cross validation
  ## Conscious decision NOT to store all cvrisk() output
  for(i in 1:length(cv_sets$train)) {
  #for(i in 1:1) {    
    ##foreach(i = 1:8, .packages = c("mboost", "caret", "data.table")) %dopar% {
    
    ## Define the upper bound on Gamma distribution for search
    ## 5 times the max seemed a good value from tests
    u_bound <- max(data_in)*2
    ## Run model and cross validate
    abs.modelFit <- glmboost(my_formula, data = data_in[cv_sets$train[[i]]], center = TRUE,
                             family = GammaReg(nuirange = c(0, u_bound)), 
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
    cvr <- cvrisk(abs.modelFit)
    
    ## Put model into the position of its optimal mstop value
    abs.modelFit[mstop(cvr), return = FALSE]
    
    ## Predict the (i+1)th value and measure the error
    preds <- predict(abs.modelFit, type = "response",
                     newdata = data_in[cv_sets$test[[i]], -c("DOW.DP"), with = FALSE])
    true_values <- data_in[cv_sets$test[[i]], .(DOW.DP)]
    ## Compute the estimation error
    abs.modelErr <- as.matrix(true_values) - preds
    
    ## Assign results: model output, mstop value, actual and forecasted results
    abs.model_results$results[[i]][[1]] <- mstop(cvr)
    abs.model_results$results[[i]][[2]] <- data_in[cv_sets$test[[i]], .(DOW.DP)]
    abs.model_results$results[[i]][[3]] <- preds
    ## Assign step-wise errors (functions allow to expand prediction horizon)
    abs.model_results$results[[i]][[4]][[1]] <- abs.modelErr
    abs.model_results$results[[i]][[4]][[2]] <- mae(abs.modelErr)
    abs.model_results$results[[i]][[4]][[3]] <- mse(abs.modelErr)
    abs.model_results$results[[i]][[4]][[4]] <- rmse(abs.modelErr)
    abs.model_results$results[[i]][[4]][[5]] <- signs(preds, true_values)
    ## Save the names of the coefficients that were seleceted
    abs.model_results$results[[i]][[5]] <- coef(abs.modelFit)
    
    
    ## Force garbage collection of larger objects
    rm(abs.modelFit, cvr)
    
  }
  print(paste0("Cross validation completed: ", as.character(Sys.time())))
  
  return(abs.model_results)
}
