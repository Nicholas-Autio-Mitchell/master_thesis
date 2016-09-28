###################### ================================================ ######################
######################  A file to create an object that houses results  ######################
###################### ================================================ ######################

#' The script creates a data structure to hold results from glmboost() using the
#' Binomial family, that is for logistic regression.
#' As we only predict a value {0,1} in each iteration across the data, there aren't any
#' errors that can be computed from residuals at each step.
#' The sign_accuracy is aggregated post modelling, where some analysis can then be performed.

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
    my_template <- sapply(c("input_parameters", "results"), function(x) NULL)

    ## Input parameters used for this model to be saved for reproducibility
    mod_parameters <- sapply(c("frame_size", "look_ahead", "nIter", "shrinkage"), function(x) NULL)
    mod_parameters$frame_size <- frame_size
    mod_parameters$look_ahead <- look_ahead
    mod_parameters$nIter <- nIter
    mod_parameters$shrinkage <- shrinkage
    
    ## We will have (num_cv_steps) prediction rounds, also equal to length(cv_sets$train)
    results <- sapply(c(paste0("cv_", seq(1:num_cv_steps))), function(x) NULL)
    ## Results to be stored during each CV-step, each time glmboost is performed
    step_results <- sapply(c("mstop", "actual", "forecast", "correct_sign", "coefs"), function(x) NULL)

    ## Apply the lists to create entire structure
    for(i in 1:num_cv_steps){results[[i]] <- step_results}

    ## Assign all categories to the final object
    my_template$input_parameters <- mod_parameters
    my_template$results <- results
    
    return(my_template)
}
