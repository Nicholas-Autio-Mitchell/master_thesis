###################### ========================================================= ######################
######################  Define several functions that record forecasting errors  ######################
###################### ========================================================= ######################

## Results from the Binomial() family do not require additional error functions.
## Testing whether or not the output was correct or not can be performed within modelling script:
## "ultra_modeller.binomial.R"

## =========================================== ##
##  Create functions for measuring the errors  ##
## =========================================== ##

## ## Create functions to calculate various error measurements
## ## Function that returns Mean Absolute Error
## mae <- function(error){mean(abs(error))}
## ## Function that returns the Mean Squared Error
## mse <- function(error){mean(error^2)}
## ## Function that returns Root Mean Squared Error
## rmse <- function(error){sqrt(mean(error^2))}
## ## Function to find numbers of times the sign was correctly predicted
## signs <- function(preds, true_values){sum((preds * true_values) > 0)}
