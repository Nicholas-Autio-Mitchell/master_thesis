###################### ================================================ ######################
######################  A file to create an object that houses results  ######################
###################### ================================================ ######################

## Take from 'predictor_of_results_long.R'


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
