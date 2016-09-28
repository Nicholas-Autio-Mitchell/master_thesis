
###################### ============================================= ######################
######################  Perform gbm on data using train a test sets  ######################
###################### ============================================= ######################


load("/Volumes/Mac OS Drive/Documents/Dropbox/ultra/stochastic/subsets_lagged.rda")
source("/Volumes/Mac OS Drive/Documents/Dropbox/ultra/stochastic/corr_pruner.R")

input_data <- sapply(c("Lag.1", "Lag.2", "Lag.3", "Lag.4", "Lag.5"), function(x) NULL)
input_subsets <- sapply(c("trad_small", "trad_large", "sent_small", "sent_large", "combined"), function(x) NULL)
for(i in 1:length(input_data)){input_data[[i]] <- input_subsets}

## Need to run this each time too to take a fresh copy of the data. DOW.DP is removed by reference otherwise
raw_data <- copy(subsets_lagged)
for(i in 1:5) {
    message(paste0("++ ----  ", names(raw_data)[i], "  ---- ++"))
    for(j in 1:5) {
        input_data[[i]][[j]] <- corr_pruner(raw_data[[i]][[j]],
                                            cutoff = 0.8,
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


## ---------------------------- ##
##  Create train and test data  ##
## ---------------------------- ##

x <- copy(input_data.one_list$Lag.3.trad_large) #test set

gbm_results <- sapply(names(input_data.one_list), function(x) NULL)

for(i in 1:length(input_data.one_list)) {
    
    x <- input_data.one_list[[i]]



    x$DOW.DP <- ifelse(x$DOW.DP > 0, "up", "down")

    the_split <- createDataPartition(x$DOW.DP, p=0.6, list = FALSE)

    to_train <- x[the_split]
    to_test <- x[-the_split]

    my_control <- trainControl(method = "repeatedcv", repeats = 5,
                               classProbs = TRUE, summaryFunction = twoClassSummary)

    ## Using paramters allready optimised for...
    my_grid <- expand.grid(interaction.depth = c(1, 3, 5, 7),
                           n.trees = c(1500,3000),
                           shrinkage = 0.01)

    this_model <- train(DOW.DP ~., data = to_train,
                        method = "gbm",
                        metric = "ROC",
                        verbose = FALSE,
                        trControl = my_control)

    my_predictions <- predict(this_model, newdata = to_test)

    gbm_results[[i]] <- sum(to_test$DOW.DP == my_predictions)/length(my_predictions)
    
}

save(gbm_results, file = "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/5_stochastic_gbm/gbm_results.rda")

