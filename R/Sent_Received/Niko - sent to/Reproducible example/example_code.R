################# ================================================================ #################
#################  Reproducible example to test lag system & basic glmboost setup  #################
################# ================================================================ #################

## ========================================= ##
##  Following packages required to run code  ##
## ========================================= ##

## Necessary packages. If you don't currently have them, install them with code below
library(data.table)
library(mboost)
library(caret)

## Install as necessary
install.packages("data.table")
install.packages("mboost")
install.packages("caret")

## =============================== ##
##  Load the raw data and view it  ##
## =============================== ##

this_folder <- getwd()
load(paste0(this_folder, "/example_data.rda")) ## one data table

## There are 8 columns: Date, our outcome (DOW.DP) then Dow/Dax data plus one sentiment var.
## DOW.DP = the returns of our dependent variable
## whatever.L1 = first lag
## whatever.L2 = second lag

## Look at the data (it shows head and tail together, as it is a data.table)
my_data
## The values in the columns with .L1 correspond to the Date column!
## Notice now the DOW.DP column (our outcome) has been shifted up by one in comparison to the DOW.L1 column

## Does this all seem correct?


## ====================== ##
##  Basic glmboost model  ##
## ====================== ##

## Remove the first row with NAs, as well as the non-numeric Date column
my_data <- my_data[, Date := NULL][2:nrow(my_data)]

## Create the indices for our moving 40-day frame for training and testing
time_slices <- createTimeSlices(my_data$DOW.DP, initialWindow = 40)

## Have a look at how the frame moves, making one prediction for each time-step.
## 40 rows (days!) in each training set
## 1 row for each test case
## Each test set is a shift of one day forwards
## Note the list length = 654, which is our 694 days MINUS the 40 we lose for the first training set. 
str(time_slices, list.len = 10)


## ====================== ##
##  Run a glmboost model  ## --> ## Do it just for one prediction
## ====================== ##

## Pick a seed
set.seed(1)   # gets one coefficient
set.seed(666) # gets two coefficients

input_data <- my_data[1:41]             #40 rows to train, one to test

## Create formula
my_formula <- as.formula(paste0("DOW.DP ~ ", paste(names(my_data)[2:ncol(my_data)], collapse = " + ")))

## fit the model
glmFit <- glmboost(my_formula,
                   data = input_data[time_slices$train$Training001], #first 40 row
                   control = boost_control(mstop = 2000, nu = 0.05,  #boosting parameters
                                           center = TRUE))            #center the data

## Perform cross validation to get optimal mstop value
cvr <- cvrisk(glmFit)

## Look at the bootstrapping results
plot(cvr)
mstop(cvr)                            # 5 or 14 depending on seed used

## Position model into the optimal mstop position and inspect coefficients
glmFit[mstop(cvr)]

## Make a prediction using the coefficients
my_prediction <- predict(glmFit,
                         newdata = input_data[time_slices$test$Testing001, #row 41
                                              -c("DOW.DP"), with = FALSE]) #removing our outcome var

## The actual value is the value of DOW.DP on the same row as that of the new data used for prediction (which we removed)
true_value <- input_data$DOW.DP[time_slices$test$Testing001]

## Absolute error
abs(true_value - my_prediction)


## =========== ##
##  Questions  ##
## =========== ##

## Find (m)any mistakes??  ;)

## It is possible to put the 'center' argument in the boost_control and also directly in glmboost(center = TRUE)...
## ...I thought where you put it could cause as issue, but trying both seems to make no difference. Thoughts?

## Looking at the plot of cvr, it seems there just insn't any information in the data, which just can't be so noisy
## in this small subset... what else could explain this, if not a coding error on my part?

## Setting the seed to 666 above, and so getting two coefficients, doesn't mean better results.
## But still, how might we tweak the model to reach higher mstop values? (without changing the data)
