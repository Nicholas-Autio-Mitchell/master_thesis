###################### =============================================== ######################
######################  Aggregation of results for Binomial modelling  ######################
###################### =============================================== ######################

## ======================= ##
##  Load the results data  ##
## ======================= ##

## Make use of the new aggregated output lists, e.g. "all_results.bin.cutoff80.frame100.rda"
## This holds the results for the named parameters, for all subsets(5) for all lags(5), so 25 lists


## Create necessary paths to load all data
base_path <- "/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/9_results/binomial/"
parm_set <- list.files(path = base_path, pattern = "^[A_|B_|C_|D_|E]")
full_paths <- paste0(base_path, parm_set, "/results/")

to_load <- sapply(parm_set, function(x) NULL)
## The files that we want to load are just those containing 'all results', i.e. not the individual files
for(i in 1:length(full_paths)) {
    
    to_load[i] <- list.files(full_paths[i], "^all_results")
}

## Create list to hold the results of all parameters
results_objects <- sapply(parm_set, function(x) NULL)
## Load the results into the workspace. Loads the all_results* versions
for(i in 1:length(parm_set)) {
    load(file = paste0(full_paths[i], to_load[i]))
}

## Assign the results files to the list --> ~1Gb
for(i in 1:length(results_objects)) {
    results_objects[[i]] <- get(substr(to_load[i], start = 1, stop = (nchar(to_load[i])-4)))
}

## Remove everything else from the workspace
rm(list=setdiff(ls(), c('results_objects', 'binomial_collated.results')))

###################### =============================================================== ######################
######################  Define functions and then compute and assign aggregate errors  ######################
###################### =============================================================== ######################

#' We can calculate the following descriptive statistics for each binomial data set:
#' (1) accuracy of movement prediction, i.e. poitive or negative market movements correctly predicted
#' (2) breakdown of error when sign correctly predicted versus incorrectly
#'     --> This could be like the category plots in the ggplot2 book
#'
#' We can plot the following basic graphs:
#' (1) actual against predicted values with errors (like Bollinger bands?)
#' (2) Box plots for the predictions
#' 

## ============================================================================= ##
##  Create functions to check for NA values and compute the predictive accuracy  ##
## ============================================================================= ##

## ---------------------------------------------------- ##
##  Function to check for NA values within the results  ##
## ---------------------------------------------------- ##

#' Scan through all output for NAs - if there are any on the level of the calculated errors,
#' we can then dive deeper to find the cause
NA_finder <- function(results_object) {

    ## Save number of NAs found in each set of results into a preallocated matrix
    num_NAs <- matrix(data = 0, nrow = length(results_object$results), ncol = 1)

    for(i in 1:length(results_object$results)) {
        
        num_NAs[i] <- sum(is.na(results_object$results[[i]]))
    }

    ## Compute total number of NAs and report warning if any are found
    total_NAs <- sum(num_NAs)

    if(total_NAs != 0) {warning("There are some NA values in the list of errors")}
    ##else {message(paste0("No NAs found in: ", names(results_object$results$cv_1$errors)[[i]]))}
}

## ------------------------------------------------------------ ##
##  Function to aggregate prediction accuracy over all results  ##
## ------------------------------------------------------------ ##

## A function to summarise the predictive accuracy of the sign
sign_pred_accuracy <- function(results_object){

    ## Count the number of elements in the list
    num_slots <- length(results_object$results)

    ## Assign all sign results to preallocation matrix
    sign_preds <- matrix(data = NA, nrow = length(results_object$results), ncol = 1)
    for(i in 1:length(results_object$results)) {
        
        sign_preds[i] <- results_object$results[[i]][[4]]
    }

    ## Compute number of correct predictions and percentage accuracy
    num_correct <- sum(sign_preds)
    sign_accuracy <- num_correct / num_slots

    predictive_power <- list("num_correct" = num_correct, "pct_accuracy" = sign_accuracy)
    return(predictive_power)
}

## --------------------------------------------------------- ##
##  Function to summarise the mstop values over all results  ##
## --------------------------------------------------------- ##

mstop_analyser <- function(results_object) {
    
    ## Create a list to store results
    all_mstop <- sapply(c("avg", "min", "max", "sd"), function(x) NULL)

    ## Define custom functions required to work over lists
    myMean <- function(x){x <- unlist(x) %>% mean(.); return(x)}
    mySd <- function(x){x <- unlist(x) %>% sd(.); return(x)}

    ## Compute and assign the mean, variance, min and max of the mstop values
    all_mstop$avg <- do.call(myMean, list(lapply(results_object[[2]], function(x) x[[1]])))
    all_mstop$min <- sapply(results_object[[2]], function(x){x[[1]]}) %>% min(.)
    all_mstop$max <- sapply(results_object[[2]], function(x){x[[1]]}) %>% max(.)
    all_mstop$sd <- do.call(mySd, list(lapply(results_object[[2]], function(x) x[[1]])))
    
    return(all_mstop)
    
}

## ------------------------------------------------------------------------ ##
##  Function to count number of times each predictor was selected at mstop  ##
## ------------------------------------------------------------------------ ##

## Create a function that takes one results set and returns a frequency count of each variable over all models:
## This function takes in one set of results, so all the cv_steps of one set of parameters for one lagged subset
coef_counter <- function(results_object) {
    
    ## Create a list from the ~650 CV runs on each subset
    cv_runs <- lapply((results_object[[2]]), function(x) x[[5]])

    ## Create object to hold all tables
    coef_freqs <- sapply(names(cv_runs), function(x) NULL)

    for(i in 1:length(cv_runs)) {
        ## Create data tables out of the tables of names
        coef_freqs[[i]] <- as.data.table(table(names(cv_runs[[i]])))
        ## Provide unique names so that merge works afterwards
        colnames(coef_freqs[[i]]) <- c("V1", paste0(colnames(coef_freqs[[i]])[2], i))
    }

    ## Create a merge function with fitting arguments
    my.merge <- function(DT1, DT2) {merge(DT1, DT2, by = "V1", all = TRUE)}
    ## Merge all the results for each CV run
    ## --> this is slow! (~20s for each results set!) it appends with each merge, but pre-allocating would also be an sob
    my.output <- Reduce(f = my.merge, x = coef_freqs)

    ## Supply a more useful name for the variables column
    setnames(my.output, old = "V1", new = "var.name")
    ## Sum up all counts for each variable
    my.output[, total := rowSums(subset(my.output, select = -1), na.rm = TRUE)]
    ## Remove the columns with individual counts
    my.output[, 2:(ncol(my.output)-1) := NULL]
    ## Reorder by decreasing frequency
    setorder(my.output, -total)
}

## ----------------------------------------------------------- ##
##  Code to loop only over the coef_counts for eac result set  ##
## ----------------------------------------------------------- ##
## ## Create container to hold all freq counts if they are to be saved separately
## ## Main group split into parameter classes, cutoff and frame sizes - 20 variations at present
## bin_results.var_counts <- sapply(names(results_objects), function(x) NULL)
## ## Each set of parameter classes has five model subsets and five lags, so 25 names in total for each parameter set
## subsets <- sapply(names(results_objects$A_cutoff30_frame40), function(x) NULL) #use any parameter set to get model names
## ## Apply a subset to each parameter set
## for(i in 1:length(bin_results.var_counts)) {bin_results.var_counts[[i]]  <- subsets}

## ================================ ##
##  A function to call those above  ##
## ================================ ##

## Define one function that aggregates the results for all results in one output_object
results_preprocessor <- function(results_object) {

    ## This will just print out messages, nothing is saved
    NA_finder(results_object)

    ## Define output object
    results_summary <- sapply(c("sign_accuracy", "all_mstop", "coef_counts"), function(x) NULL)

    ## Assign the results to the output container
    results_summary$sign_accuracy <- sign_pred_accuracy(results_object)
    results_summary$all_mstop <- mstop_analyser(results_object)
    results_summary$coef_counts <- coef_counter(results_object)
    
    return(results_summary)

}

## ============================================================= ##
##  Create a container to house all results from preprocessor()  ##
## ============================================================= ##

## Main group split into parameter classes, cutoff and frame sizes - 20 variations at present
bin_results.collated <- sapply(names(results_objects$binomial), function(x) NULL)
## ## Each set of parameter classes has five model subsets and five lags, so 25 names in total for each parameter set
## subsets <- sapply(names(results_objects$A_cutoff30_frame40), function(x) NULL) #use any parameter set to get model names
## ## Apply a subset to each parameter set
## for(i in 1:length(bin_results.closing_prices)) {bin_results.closing_prices[[i]]  <- subsets}


###################### ========================================== ######################
######################  Preprocess all the results and save them  ######################
###################### ========================================== ######################

for(i in 1:length(bin_results.collated)) {

    bin_results.collated[[i]] <- results_preprocessor(results_objects$binomial[[i]])
    
}

## Save as RDS so it can be given a new name in namespace when loading
save(object = bin_results.collated,
        file = "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/binomial/bin_results.collated.rda")
getwd()
