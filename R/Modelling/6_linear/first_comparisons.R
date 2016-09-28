###################### ========================================================== ######################
######################  Set up a simple linear model using the boosting data set  ######################
###################### ========================================================== ######################

## Load the data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/6_linear_comparisons/subsets_lagged.rda")

data_in <- subsets_lagged$Lag.3$combined


## Create the formula
indep_vars <- names(data_in)[names(data_in) != "DOW.DP"]
rhs <- paste(indep_vars, collapse = " + ")
my_formula <- as.formula(paste("DOW.DP ~", rhs))


lmFit <- lm(formula = my_formula, data = data_in[1:60], offset = NULL)

my.pred <- predict(lmFit, newdata = data_in[61, -c("DOW.DP"), with = FALSE], type = "response")

true_values <- data_in[61, DOW.DP]
