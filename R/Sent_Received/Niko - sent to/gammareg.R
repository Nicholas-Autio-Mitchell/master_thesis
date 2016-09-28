
## ======= ##
##  Intro  ##
## ======= ##

list.files(getwd())
commandArgs()
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
pred1 <- predict(abs.modelFit1,
                 newdata = data_in[101, -c("DOW.DP"), with = FALSE]) # use next period
pred2 <- predict(abs.modelFit2,
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
pred3 <- predict(abs.modelFit3,
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame
pred4 <- predict(abs.modelFit4,
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
                         control = boost_control(mstop = 5000, nu = shrinkage))


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
pred5 <- predict(abs.modelFit5,
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame
pred6 <- predict(abs.modelFit6,
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame
pred7 <- predict(abs.modelFit5,
                 newdata = slim_data[101, -c("DOW.DP"), with = FALSE]) #use a 100-period frame

## Take value from next time period
true_value <- slim_data[101, .(DOW.DP)]

## Errors - still много голям!
abs.modelErr5 <- as.matrix(true_value) - pred5 #smaller than the value with a higer mstop (pred6)
abs.modelErr6 <- as.matrix(true_value) - pred6 #very large
abs.modelErr7 <- as.matrix(true_value) - pred7 #identical to pred5!


## ================================= ##
##  Example from normal glm package  ##
## ================================= ##

clotting <- data.frame(
    u = c(5,10,15,20,30,40,60,80,100),
    lot1 = c(118,58,42,35,27,25,21,19,18),
    lot2 = c(69,35,26,21,18,16,13,12,12))
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma(link = "inverse")))
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma(link = "log")))
summary(glm(lot1 ~ log(u), data = clotting, family = Gamma(link = "identity")))

## -------------- ##
##  With my data  ##
## -------------- ##

summary(glm(my_formula_slim, data = data_abs, family = Gamma))




data_abs <- abs(slim_data)

data_nozero <- data_abs[, which(as.matrix(data_abs)==0, arr.ind = TRUE) := 0]
data_abs[which(as.matrix(data_abs)==0, arr.ind = TRUE)]  <- 0.0000001
