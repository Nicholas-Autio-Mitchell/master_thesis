###################### ================================================================================ ######################
######################  Create a data set with the dow aligned with other variables in time once again  ######################
###################### ================================================================================ ######################

## Load the data that still has the dates column
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/subsets_lagged_(with_dates).rda")

## Take a test case
t_ <- subsets_lagged$Lag.3$trad_large

names(t_)
t_[, c(1, 2, 3, 6, 7, 8, 18, 19), with = FALSE]

this_folder <- getwd()
source(paste0(this_folder, "/corr_pruner.glmboost.R"))
source(paste0(this_folder, "/results_template.glmboost.R"))
source(paste0(this_folder, "/ultra_modeller.glmboost.R"))

t1 <- copy(t_)
t1[, DOW.DP := NULL]
names(t1)

setnames(t1, old = "DOW.L1", new = "DOW.new")


## ------------------------------------- ##
##  Now we would run the glmboost model  ##
## ------------------------------------- ##

data_in <- copy(t1)
my_formula <- as.formula("DOW.new ~.")


names(data_in)





