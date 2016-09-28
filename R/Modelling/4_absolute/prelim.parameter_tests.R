
## ======= ##
##  Intro  ##
## ======= ##

#' I use one data set (all possible data for Lag = 2) to make a singe prediction
#' using the GammaReg() family in glmboost.
#' 
#' This family takes one parameter (nuirange), I have specified two variants
#' (1) the mboost default, and (2) the limits of my data set
#'
#' Summary: results are terrible

## Where am I going wrong??

## -------------------- ##
##  Required libraries  ##
## -------------------- ##

library(caret)
library(data.table)
library(mboost)

## ---------------------- ##
##  Read in one data set  ##
## ---------------------- ##

## Assuming you opened R with this file!
this_folder = getwd()

## contains the dependent variable (DOW.DP) already in absolute form
load(paste0(this_folder, "/data_in.rda"))
summary(data_in$DOW.DP)                 #min is 0

data_in$DOW.DP <- abs(data_in$DOW.DP)

## Create the formula
indep_vars <- names(data_in)[names(data_in) != "DOW.DP"]
rhs <- paste(indep_vars, collapse = " + ")
my_formula <- as.formula(paste("DOW.DP ~", rhs))

## Input parameters
my_mstop <- 2000
shrinkage <- 0.05
def_family <- GammaReg(nuirange = c(0,100)) #default from mboost package (see below)
## Alternative
my_family1 <- GammaReg(nuirange = c(0, max(data_in)*1.1))   #use range of the data instead - plus 10%

##set the seed
set.seed(789)

## Run model using thw two families
abs.modelFit1 <- glmboost(my_formula, data = data_in[1:100], center = TRUE,
                         family = def_family, 
                         control = boost_control(mstop = my_mstop, nu = shrinkage))

## Only difference ins the family parameters used
abs.modelFit2 <- glmboost(my_formula, data = data_in[1:100], center = TRUE,
                         family = my_family1, 
                         control = boost_control(mstop = my_mstop, nu = shrinkage))

## Cross-validate
cvr1 <- cvrisk(abs.modelFit1)              #cv
mstop(cvr1)                                # (343)
cvr2 <- cvrisk(abs.modelFit2)              
mstop(cvr2)                                # (352)

abs.modelFit1[mstop(cvr1), return = FALSE] #set to mstop value
abs.modelFit2[mstop(cvr2), return = FALSE] #set to mstop value

## Make predictions
pred1 <- predict(abs.modelFit1, type="response",
                 newdata = data_in[101, -c("DOW.DP"), with = FALSE]) # use next period
pred1a <- predict(abs.modelFit1,
                 newdata = data_in[101, -c("DOW.DP"), with = FALSE]) # use next period
pred2 <- predict(abs.modelFit2, type = "response",
                 newdata = data_in[101, -c("DOW.DP"), with = FALSE]) # use next period

## Take value from next time period
true_value <- data_in[101, .(DOW.DP)]

## Compute the estimation error
abs.modelErr1 <- as.matrix(true_value) - pred1 ## squaring these isn't going to help things  ;)
abs.modelErr2 <- as.matrix(true_value) - pred2

## --------------------------------------------------------------- ##
##  Same again, slimmer data set to remove preds with huge values  ##
## --------------------------------------------------------------- ##

## Certain variables removed (number of tweets/favourites) because they're massive 
load(paste0(this_folder, "/slim_data.rda"))
summary(slim_data$DOW.DP)         #same as before

## Redfine new formula with for data
indep_vars1 <- names(slim_data)[names(slim_data) != "DOW.DP"]
rhs1 <- paste(indep_vars1, collapse = " + ")
my_formula_slim <- as.formula(paste("DOW.DP ~", rhs1))

## New family specfic to new data - shouldn't be very different to family2 above
my_family2 <- GammaReg(nuirange = c(0, max(slim_data)*1.1))   #use range of the data instead plus 10%

## set seed
set.seed(789)

## Run model using thw two families - other parameters all same as before
abs.modelFit3 <- glmboost(my_formula_slim, data = slim_data[1:100], center = TRUE,
                         family = def_family, 
                         control = boost_control(mstop = my_mstop, nu = shrinkage))
abs.modelFit4 <- glmboost(my_formula_slim, data = slim_data[1:100], center = TRUE,
                         family = my_family2, 
                         control = boost_control(mstop = my_mstop, nu = shrinkage))

## Cross-validate
cvr3 <- cvrisk(abs.modelFit3)              # cv
mstop(cvr3)                                # (343) - same as with large-scale variables above!
plot(cvr3)                                 # Interesing. Not good, but interesting
cvr4 <- cvrisk(abs.modelFit4)              #
mstop(cvr4)                                # (352) - smaller than above, so better data
plot(cvr4)                                 #

abs.modelFit3[mstop(cvr3), return = FALSE] #set to mstop value
abs.modelFit4[mstop(cvr4), return = FALSE] #set to mstop value

## Make predictions
pred3 <- predict(abs.modelFit3, type = "response",
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame
pred4 <- predict(abs.modelFit4, type = "response",
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame

## Take value from next time period
true_value <- slim_data[101, .(DOW.DP)]

## Errors - много голям!
abs.modelErr3 <- as.matrix(true_value) - pred3
abs.modelErr4 <- as.matrix(true_value) - pred4

## --------------------------------------------------------------------------------- ##
##  Same yet again, but using the max value of the DOW.DP only for GammaReg paramter ##
## --------------------------------------------------------------------------------- ##

## Looks more promising, initially...

## New family specfic to new data - shouldn't be very different to family2 above
my_family5 <- GammaReg(nuirange = c(0, max(slim_data$DOW.DP)*1.1))   #use range of DOW.DP, plus 10%

## set seed
set.seed(789)

##
abs.modelFit5 <- glmboost(my_formula_slim, data = slim_data[1:100], center = TRUE,
                         family = my_family5, 
                         control = boost_control(mstop = my_mstop, nu = shrinkage))
## with larger mstop - is required, see plot(cvr6)
abs.modelFit6 <- glmboost(my_formula_slim, data = slim_data[1:100], center = TRUE,
                         family = my_family5, 
                         control = boost_control(mstop = 5000, nu = shrinkage))
## using mboost's default values again
abs.modelFit7 <- glmboost(my_formula_slim, data = slim_data[1:100], center = TRUE,
                         family = def_family, 
                         control = boost_control(mstop = 4000, nu = shrinkage))


## Cross-validate
cvr5 <- cvrisk(abs.modelFit5)              #cv
mstop(cvr5)                                # (2000)
plot(cvr5)

cvr6 <- cvrisk(abs.modelFit6)              #cv
mstop(cvr6)                                # (2370)
plot(cvr6)                                 # looks a bit more like what one expects

cvr7 <- cvrisk(abs.modelFit7)              #cv
mstop(cvr7)                                # (349) - similar to original results in first round!
plot(cvr7)                                 # funky!

## Put model in mstop positions
abs.modelFit5[mstop(cvr5), return = FALSE] #set to mstop value
abs.modelFit6[mstop(cvr6), return = FALSE] #set to mstop value
abs.modelFit7[mstop(cvr7), return = FALSE] #set to mstop value

## Make predictions
pred5 <- predict(abs.modelFit5, type = "response",
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame
pred6 <- predict(abs.modelFit6, type = "response",
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame
pred7 <- predict(abs.modelFit5, type = "response",
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame

## Take value from next time period
true_value <- slim_data[101, .(DOW.DP)]

## Errors - still много голям!
abs.modelErr5 <- as.matrix(true_value) - pred5 #smaller than the value with a higer mstop (pred6)
abs.modelErr6 <- as.matrix(true_value) - pred6 #very large
abs.modelErr7 <- as.matrix(true_value) - pred7 #identical to pred5!

## ============================ ##
##  Combine results to compare  ##
## ============================ ##

results <- matrix(NA, 7, 1)
results[1] <- abs.modelErr1
results[2] <- abs.modelErr2
results[3] <- abs.modelErr3
results[4] <- abs.modelErr4
results[5] <- abs.modelErr5
results[6] <- abs.modelErr6
results[7] <- abs.modelErr7
results

## ========================================================== ##
##  Take best reulst and test sentitivity against parameters  ##
## ========================================================== ##

#' Best results came equally from models 2 and 4
#' These used nuirange c(0, max(data)*1.1)
#' Seemed independent of the data set used, meaning those variables with 
#' larger magnitudes need not be removed - we use the full data set here
#' 
#' Lets test to see if c(0, max(data)) alone is better

## Load oin larger data set to try out
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/3_binomial/subsets_lagged.rda")

data_in <- copy(subsets_lagged$Lag.4$trad_large)

data_in$DOW.DP <- abs(data_in$DOW.DP)
    
## Create the formula
indep_vars <- names(data_in)[names(data_in) != "DOW.DP"]
rhs <- paste(indep_vars, collapse = " + ")
my_formula <- as.formula(paste("DOW.DP ~", rhs))

## Input parameters
my_mstop <- 2000
shrinkage <- 0.05
test_family <- GammaReg(nuirange = c(0, max(data_in)))

##set the seed
set.seed(789)

## Run model using thw two families
abs.modelFit10 <- glmboost(my_formula, data = data_in[1:100], center = TRUE,
                           family = test_family, 
                           control = boost_control(mstop = 2000, nu = shrinkage))

## Cross-validate
cvr10 <- cvrisk(abs.modelFit10)              #cv
mstop(cvr10)                                # (343)

abs.modelFit10[mstop(cvr10), return = FALSE] #set to mstop value

## Make predictions
the_pred <- predict(abs.modelFit10, type="response",
                 newdata = data_in[101, -c("DOW.DP"), with = FALSE]) # use next period

## Take value from next time period
true_value <- data_in[101, .(DOW.DP)]

## Compute the estimation error
err8 <- as.matrix(true_value) - the_pred

## with c(0, max(data_in)*1.1)
abs.modelErr10                          #0.00495
(abs.modelErr10)^2
## with c(0, max(data_in))
abs.modelErr11                          #0.007113
(abs.modelErr11)^2                      
## with c(0, 100)
abs.modelErr12
(abs.modelErr12)^2                      #0.007113
## with c(0, max(data_in))
abs.modelErr13                          #0.006478
(abs.modelErr13)^2

to_remove <- grep("day.number", names(data_in), value = TRUE)
data_in[, c(to_remove) := NULL]

## Without day.number
##nuirange = c(0, max(data_in))
err1                                    # 0.006478283
err1^2
##nuirange = c(0, max(data_in)*1.1)
err2                                    # 0.007112745
err2^2
##nuirange = c(0, max(data_in$DOW.DP))
err3 # mstop was 2000                   # -0.0117654
err3^2
##nuirange = c(0, max(data_in$DOW.DP))
err4 # mstop limit increased. cvr:3999  # 0.008133477
err4^2
##nuirange = c(0, max(data_in$DOW.DP))
err5 # mstop2000,nu0.1. cvr:1734        # 0.007821013
err5^2
##nuirange = c(0, max(data_in)*2)
err6 # mstop2000,nu0.05. cvr:384        # 0.007327337
err6^2
##nuirange = c(0, max(data_in)*5)
err7 # mstop2000,nu0.1. cvr:374         # 0.006616729
err7^2
##nuirange = c(0, max(data_in))
err8 # mstop2000,nu0.1. cvr: 377        # 0.006478283
err8^2   ## same as err1 , confirms method was stable for all tests

## =============================================== ##
##  Perform same test but for some sentiment data  ##
## =============================================== ##

#' This max values in each data set will vary much more with the number of tweets
#'

## Chose a large set of data
data_in <- subsets_lagged$Lag.4$sent_large
max(data_in)                            #22764 !!

data_in$DOW.DP <- abs(data_in$DOW.DP)
    
## Create the formula
indep_vars <- names(data_in)[names(data_in) != "DOW.DP"]
rhs <- paste(indep_vars, collapse = " + ")
my_formula <- as.formula(paste("DOW.DP ~", rhs))

## Input parameters
my_mstop <- 2000
shrinkage <- 0.05
test_family <- GammaReg(nuirange = c(0, max(data_in)*20))

##set the seed
set.seed(789)

## Run model using thw two families
abs.modelFit10 <- glmboost(my_formula, data = data_in[1:100], center = TRUE,
                           family = test_family, 
                           control = boost_control(mstop = 2000, nu = shrinkage))

## Cross-validate
cvr10 <- cvrisk(abs.modelFit10)              #cv
mstop(cvr10)                                # (343)

abs.modelFit10[mstop(cvr10), return = FALSE] #set to mstop value

## Make predictions
the_pred <- predict(abs.modelFit10, type="response",
                 newdata = data_in[101, -c("DOW.DP"), with = FALSE]) # use next period

## Take value from next time period
true_value <- data_in[101, .(DOW.DP)]

## Compute the estimation error
r5 <- as.matrix(true_value) - the_pred

## --------- ##
##  results  ##
## --------- ##

## With al the large variables still in the data set
r1         #mstop 280                   # -0.03781034

to_remove <- grep(".total_", names(data_in), value = TRUE)
data_in[, c(to_remove) := NULL]
max(data_in)                            #5 (from day numbers)

## same model again, just with large vars removed...
r2                                      # -0.03699995
#' surprisingly little difference to previsou result!

## nuirange = c(0, max(data_in)*2)
r3                                      # -0.03699976
## nuirange = c(0, max(data_in)*5)
r4                                      # -0.03699966
## nuirange = c(0, 100)
r5                                      # -0.03700033

