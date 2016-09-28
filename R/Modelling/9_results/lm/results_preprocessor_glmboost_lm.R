###################### =========================================== ######################
######################  Basic analysis of results from glmboost()  ######################
###################### =========================================== ######################

## ======================= ##
##  Load the results data  ##
## ======================= ##
## Make use of the new aggregated output lists, e.g. "all_results.glm.cutoff80.frame100.rda"
## This holds the results for the named parameters, for all subsets(5) for all lags(5), so 25 lists

## Create necessary paths to load all data
base_path <- "/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/9_results/glm_boost/"
parm_set <- list.files(path = base_path, pattern = "^[A_|B_|C_|D_|E]")
full_paths <- paste0(base_path, parm_set, "/results/")

## Matrix to store all individual file path
to_load <- c()
## The files that we want to load are just those containing 'all results', i.e. not the individual files
for(i in 1:length(full_paths)) {
    
    to_load[i] <- list.files(full_paths[i], "^all_results.glm")
}

## Create list to hold the results of all parameters
results_objects <- sapply(parm_set, function(x) NULL)

## Load the results into the container. Loads the all_results* versions
for(i in 1:length(parm_set)) {
    load(file = paste0(full_paths[i], to_load[i]))
}

## Assign the results files to the list --> ~1.1 Gb
for(i in 1:length(results_objects)) {
    results_objects[[i]] <- get(substr(to_load[i], start = 1, stop = (nchar(to_load[i])-4)))
}

## Remove everything else from the workspace
rm(list=setdiff(ls(), c('results_objects')))


###################### =============================================================== ######################
######################  Define functions and then compute and assign aggregate errors  ######################
###################### =============================================================== ######################

#' We can calculate the following descriptive statistics for each data set:
#' (1) accuracy of movement prediction, i.e. poitive or negative market movements correctly predicted
#' (2) the aggregated error in terms of MAE, MSE and RMSE
#' (3) breakdown of error when sign correctly predicted versus incorrectly
#'     --> This could be like the category plots in the ggplot2 book
#'
#' We can plot the following basic graphs:
#' (1) actual against predicted values with errors (like Bollinger bands?)
#' (2) Box plots for the predictions
#' 
## Compute the error in several ways (the 'means' to be calculated at the end)


## ============================================================================= ##
##  Create functions to check for NA values and compute the predictive accuracy  ##
## ============================================================================= ##

#' Scan through all output for NAs - if there are any on the level of the calculated errors,
#' we can then dive deeper to find the cause
NA_finder <- function(results_object) {

    ## Check all error elemts for NAs
    for( i in 1:length(results_object$results$cv_1$errors)) #cv_1 as they are all the same, coming from results_template()
    {
        ## count how long the list could be (changes with the lag used, or batch-size if they are used later on)
        num_slots <- lapply(results_object[[2]], function(x){x[[4]][[i]]}) %>%
            unlist(.) %>%
            as.data.table(.) %>%
            nrow(.)

        ## Count how many elements have non-values
        num_elements <- num_slots - sum(sapply(num_slots, function(x) is.null(x)))

        ## Report error if there are any missing values
        if(num_slots != num_elements) {warning("There are some NA values in the list of errors")}
        #else {message(paste0("No NAs found in: ", names(results_object$results$cv_1$errors)[[i]]))}
    }
}

## A function to summarise the predictive accuracy of the sign
sign_pred_accuracy <- function(results_object){

    ## Count the number of elements in the list
    num_slots <- lapply(results_object[[2]], function(x){x[[4]][[5]]}) %>%
            unlist(.) %>%
            as.data.table(.) %>%
            nrow(.)

    ## Sum up all the 1's i.e. those that are correct
    num_correct <- lapply(results_object[[2]], function(x){x[[4]][[5]]}) %>%
        unlist(.) %>%
        as.data.table(.) %>%
        .[, sum(.)]

    sign_accuracy <- lapply(results_object[[2]], function(x){x[[4]][[5]]}) %>%
        unlist(.) %>%
        as.data.table(.) %>%
        .[, sum(.)/length(.)*100]

    predictive_power <- list("num_correct" = num_correct, "pct_accuracy" = sign_accuracy)
    return(predictive_power)
}
    
## ========================== ##
##  Look at the mstop values  ##
## ========================== ##

mstop_analyser <- function(results_object) {

    ## Create a list to store results
    all_mstop <- sapply(c("avg", "min", "max", "sd"), function(x) NULL)
    ## Define custom functions required to work over lists
    myMean <- function(x){x <- unlist(x) %>% mean(.); return(x)}
    mySd <- function(x){x <- unlist(x) %>% sd(.); return(x)}
    ## Compute and assign the mean, variance, min and max of the mstop values
    all_mstop$avg <- do.call(myMean, list(lapply(results_object[[2]], function(x) x[[1]])))
    all_mstop$sd <- do.call(mySd, list(lapply(results_object[[2]], function(x) x[[1]])))
    all_mstop$min <- as.data.table(sapply(results_object[[2]], function(x){x[[1]]})) %>%
        .[, min(.)] 
    all_mstop$max <- as.data.table(sapply(results_object[[2]], function(x){x[[1]]})) %>%
        .[, max(.)]

    return(all_mstop)

}

## ================================================================== ##
##  Count the number of times each coefficient was selected at mstop  ##
## ================================================================== ##

coef_counter <- function(result_object) {                                                        
    
    ## Create a list from the ~650 CV runs on each subset
    cv_runs <- lapply((result_object[[2]]), function(x) x[[5]])

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
    ## --> this is slow! the results grows with each merge, but pre-allocating would also be an sob
    my.output <- Reduce(f = my.merge, x = coef_freqs)

    ## Supply a more useful name for the variables column
    setnames(my.output, old = "V1", new = "var.name")
    ## Sum up all counts for each variable
    my.output[, total := rowSums(subset(my.output, select = -1), na.rm = TRUE)]
    ## Remove the columns with individual counts
    my.output[, 2:(ncol(my.output)-1) := NULL]
    ## Reorder by decreasing frequency
    setorder(my.output, -total)

    return(my.output)
}

## =============================================== ##
##  collate and average the errors for each model  ##
## =============================================== ##


error_analyser <- function(results_object){

    ## Create container for all error measurements
    error.output <- sapply(c("residuals", "MAE", "MSE", "RMSE", "Theils_U"), function(x) NULL)

    ## Theil's U
    theil_u <- function(results_object) {
        numerator <- do.call(sum, lapply(results_object[[2]], function(x){ (as.numeric(x[[3]] - x[[2]]))^2} ))
        denom <- do.call(sum, lapply(results_object[[2]], function(x) {(as.numeric(x[[2]]))^2}))
        u <- sqrt(numerator) / sqrt(denom)
        return(u) }

    ## Compute and store the errors
    error.output$residuals <- unlist(lapply(results_object[[2]], function(x){x[[4]][[1]]}), use.names = FALSE)
    error.output$MAE <- as.data.table(sapply(results_object[[2]],
                                             function(x){x[[4]][[2]]})) %>% .[, mean(V1)]
    error.output$MSE <-  do.call(sum, lapply(results_object[[2]], function(x){x[[4]][[3]]}))/length(results_object[[2]])
    error.output$RMSE <- as.data.table(sapply(results_object[[2]],
                                              function(x){x[[4]][[1]]})) %>% .[, sqrt(mean(.^2))]
    error.output$Theils_U <- theil_u(results_object)

    return(error.output)

    ## Record all the residuals
    #results_summary$all_residuals <- unlist(lapply(results_object[[2]], function(x){x[[4]][[1]]}), use.names = FALSE)

    ## Average Mean Absolute Error
    ## avg_MAE <- as.data.table(sapply(results_object[[2]],
    ##                                 function(x){x[[4]][[2]]})) %>%
    ##     .[, mean(V1)]
    ## Average Mean Squared Error
    ## avg_MSE <- do.call(sum, lapply(results_object[[2]],
    ##                                function(x){x[[4]][[3]]}))/length(results_object[[2]])
    ## Aggregate Root Mean Squared Error
    ## Use the residuals directly instead of working backwards from the individual RMSEs
    ## avg_RMSE <- as.data.table(sapply(results_object[[2]], function(x){x[[4]][[1]]})) %>%
    ##     .[, sqrt(mean(.^2))] #colName V1 not necessary as ^2 returns a numeric vector    
}

## ========================================================= ##
##  Perform NA check on output and compute aggregate errors  ##
## ========================================================= ##

## Define one function that aggregates the results for all results in one output_object
results_preprocessor <- function(results_object) {

    ## This will just print out messages, nothing is saved
    NA_finder(results_object)
    
    ## Define output object
    results_summary <- sapply(c("errors", "sign_accuracy", "all_mstop", "coef_counts"), function(x) NULL)

    ## Assign the results to the output container
    results_summary$errors <- error_analyser(results_object)
    results_summary$sign_accuracy <- sign_pred_accuracy(results_object)
    results_summary$all_mstop <- mstop_analyser(results_object)
    results_summary$coef_counts <- coef_counter(results_object)
    
    return(results_summary)
}

###################### ================= ######################
######################  Extract results  ######################
###################### ================= ######################

## ============================================================= ##
##  Create a container to house all results from preprocessor()  ##
## ============================================================= ##

## Main group split into parameter classes, cutoff and frame sizes - 20 variations at present
glm_results.closing_prices <- sapply(names(results_objects), function(x) NULL)
## Each set of parameter classes has five model subsets and five lags, so 25 names in total for each parameter set
subsets <- sapply(names(results_objects$A_cutoff30_frame40), function(x) NULL) #use any parameter set to get model names
## Apply a subset to each parameter set
for(i in 1:length(glm_results.closing_prices)) {glm_results.closing_prices[[i]]  <- subsets}

## =========================================== ##
##  Preprocess all the results and store them  ##
## =========================================== ##

for(i in 1:length(results_objects)) {
    for(j in 1:length(results_objects[[i]])) {
        glm_results.closing_prices[[i]][[j]] <- results_preprocessor(results_object = results_objects[[i]][[j]])
    }
}

save(glm_results.closing_prices, file = "glm_collated.results.rda")







###################### =============================== ######################
######################  Old code - not needed anymore  ######################
###################### =============================== ######################

## ================================================================================== ##
##  Count how many times each coefficient was selected at mstop - for cherry picking  ##
## ================================================================================== ##

## Create container to hold all freq counts
## Main group split into parameter classes, cutoff and frame sizes - 20 variations at present
glm_results.var_counts <- sapply(names(results_objects), function(x) NULL)
## Each set of parameter classes has five model subsets and five lags, so 25 names in total for each parameter set
subsets <- sapply(names(results_objects$A_cutoff30_frame40), function(x) NULL) #use any parameter set to get model names
## Apply a subset to each parameter set
for(i in 1:length(glm_results.var_counts)) {glm_results.var_counts[[i]]  <- subsets}


## Test case
results_object <- results_objects$C_cutoff90_frame80$Lag.2.combined #results_objects[[i]][[j]] - i = model parms, j = lagged subsets

## Create a function that takes one results set and returns a frequency count of each variable over all models:
## This function takes in one set of results, so all the cv_steps of one set of parameters for one lagged subset


## ==================================================================== ##
##  Create loop to apply coef_counter() recursivley to results_objects  ##
## ==================================================================== ##

## Loop through all results - aggregate them afterwards
## Save to container created above
for(i in 1:length(results_objects)) {
    for(j in 1:length(results_objects[[i]])) {
        glm_results.var_counts[[i]][[j]] <- coef_counter(results_objects[[i]][[j]])
    }
}

## ================================ ##
##  Aggregate the frequency counts  ##
## ================================ ##



## ---------------------------------------- ##
##  Have a quick look at the sign accuracy  ##
## ---------------------------------------- ##

for(i in 1:length(glm_results.closing_prices)) {
    
    for(j in 1:length(glm_results.closing_prices[[i]])) {
        
        print(glm_results.closing_prices[[i]][[j]][[5]])
    }
}

## Save as RDS so it can be given a new name in namespace when loading
saveRDS(object = glm_results.closing_prices,
        file = "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/glm_results.closing_prices.rda")



## ========================================================================== ##
##  A function to allow the assignment of two output objects from a function  ##
## ========================================================================== ##

multiOut <- structure(NA,class="result")
"[<-.result" <- function(x,...,value) {
    args <- as.list(match.call())
    args <- args[-c(1:2,length(args))]
    length(value) <- length(args)
    for(i in seq(along=args)) {
        a <- args[[i]]
        if(!missing(a)) eval.parent(substitute(a <- v,list(a=a,v=value[[i]])))
    }
    x
}

## Test it out - a & b are now variables in the workspace
myfun <- function() list(a = 1, b = 2)

multiOut[a, b] <- myfun()
a + b

# same
with(myfun(), a + b)
