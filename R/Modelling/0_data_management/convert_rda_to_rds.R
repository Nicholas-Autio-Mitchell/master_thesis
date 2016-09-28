###################### ======================================== ######################
######################  Files to convert RDA to RDS file types  ######################
###################### ======================================== ######################

## Simple list of commands that load RDA files and save them as RDS files for improved usability of individual objects

## Need to be careful as all data, once loaded, have identical names.
#' E.g. glm_results_Macro -> this could be for any lag, ony way to tell is from lengths
#' length(glm_results_Macro$results) --> shows number of CVs

## ============================ ##
##  Create some lists of names  ##
## ============================ ##

names_ <- sapply(c("Macro", "SA_all", "SA_avg", "Mix"), function(x) NULL)
names_[[1]] <- paste0("glm_results_Macro")
names_[[2]] <- paste0("glm_results_SA_all")
names_[[3]] <- paste0("glm_results_SA_avg")
names_[[4]] <- paste0("glm_results_Mix")

save_lags_ <- sapply(c("lag1", "lag2", "lag3", "lag4", "lag5"), function(x) NULL)
save_names_ <- sapply(c("Macro", "SA_all", "SA_avg", "Mix"), function(x) NULL)
for(i in 1:5) {save_lags_[[i]] <- save_names_}

## list of names to use for saving, lag number appearing in file names
for(i in 1:5) {
    save_lags_[[i]][[1]] <- paste0("glm_results_lag_", as.character(i), "_Macro.rds") 
    save_lags_[[i]][[2]] <- paste0("glm_results_lag_", as.character(i), "_SA_all.rds")
    save_lags_[[i]][[3]] <- paste0("glm_results_lag_", as.character(i), "_SA_avg.rds")
    save_lags_[[i]][[4]] <- paste0("glm_results_lag_", as.character(i), "_Mix.rds")
}

## Destination
save_path <- "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_output/glm_nIter5000_nu0.005_results/"
    
## ========= ##
##  Lag = 1  ##
## ========= ##

## Load from here
lag1_path <- "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_output/glm_lag1/nIter5000_nu0.005_results/"

## Load all lag1 data
for(i in 1:4) {
    load(paste0(lag1_path, names_[[i]], ".rda"))
}

## Save al files as .rds data
saveRDS(glm_results_Macro, file = paste0(save_path, save_lags_[[1]][[1]]))
saveRDS(glm_results_SA_all, file = paste0(save_path, save_lags_[[1]][[2]]))
saveRDS(glm_results_SA_avg, file = paste0(save_path, save_lags_[[1]][[3]]))
saveRDS(glm_results_Mix, file = paste0(save_path, save_lags_[[1]][[4]]))

## glmboost() with nIter=5000 and nu=0.005
## Macro data
load(paste0(lag1_path, "glm_results_Macro.rda"))
## SA_all
load(paste0(lag1_path, "glm_results_SA_all.rda"))
## SA_avg
load(paste0(lag1_path, "glm_results_SA_avg.rda"))
## Mix
load(paste0(lag1_path, "glm_results_Mix.rda"))

## ========= ##
##  Lag = 2  ##
## ========= ##

lag2_path <- "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_output/glm_lag2/nIter5000_nu0.005_results/"

## glmboost() with nIter=5000 and nu=0.005
## Macro data
load(paste0(lag2_path, "glm_results_Macro.rda"))
## SA_all
load(paste0(lag2_path, "glm_results_SA_all.rda"))
## SA_avg
load(paste0(lag2_path, "glm_results_SA_avg.rda"))
## Mix
load(paste0(lag2_path, "glm_results_Mix.rda"))

## Save the loaded data
saveRDS(glm_results_Macro, file = paste0(save_path, save_lags_[[2]][[1]]))
saveRDS(glm_results_SA_all, file = paste0(save_path, save_lags_[[2]][[2]]))
saveRDS(glm_results_SA_avg, file = paste0(save_path, save_lags_[[2]][[3]]))
saveRDS(glm_results_Mix, file = paste0(save_path, save_lags_[[2]][[4]]))


## ========= ##
##  Lag = 3  ##
## ========= ##

lag3_path <- "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_output/glm_lag3/nIter5000_nu0.005_results/"

## glmboost() with nIter=5000 and nu=0.005
## Macro data
load(paste0(lag3_path, "glm_results_Macro.rda"))
## SA_all
load(paste0(lag3_path, "glm_results_SA_all.rda"))
## SA_avg
load(paste0(lag3_path, "glm_results_SA_avg.rda"))
## Mix
load(paste0(lag3_path, "glm_results_Mix.rda"))

## Save the loaded data
saveRDS(glm_results_Macro, file = paste0(save_path, save_lags_[[3]][[1]]))
saveRDS(glm_results_SA_all, file = paste0(save_path, save_lags_[[3]][[2]]))
saveRDS(glm_results_SA_avg, file = paste0(save_path, save_lags_[[3]][[3]]))
saveRDS(glm_results_Mix, file = paste0(save_path, save_lags_[[3]][[4]]))

## ========= ##
##  lag = 4  ##
## ========= ##

lag4_path <- "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_output/glm_lag4/nIter5000_nu0.005_results/"

## glmboost() with nIter=5000 and nu=0.005
## Macro data
load(paste0(lag4_path, "glm_results_Macro.rda"))
## SA_all
load(paste0(lag4_path, "glm_results_SA_all.rda"))
## SA_avg
load(paste0(lag4_path, "glm_results_SA_avg.rda"))
## Mix
load(paste0(lag4_path, "glm_results_Mix.rda"))

## Save the loaded data
saveRDS(glm_results_Macro, file = paste0(save_path, save_lags_[[4]][[1]]))
saveRDS(glm_results_SA_all, file = paste0(save_path, save_lags_[[4]][[2]]))
saveRDS(glm_results_SA_avg, file = paste0(save_path, save_lags_[[4]][[3]]))
saveRDS(glm_results_Mix, file = paste0(save_path, save_lags_[[4]][[4]]))

## ========= ##
##  lag = 5  ##
## ========= ##

lag5_path <- "/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_output/glm_lag5/nIter5000_nu0.005_results/"

## glmboost() with nIter=5000 and nu=0.005
## Macro data
load(paste0(lag5_path, "glm_results_Macro.rda"))
## SA_all
load(paste0(lag5_path, "glm_results_SA_all.rda"))
## SA_avg
load(paste0(lag5_path, "glm_results_SA_avg.rda"))
## Mix
load(paste0(lag5_path, "glm_results_Mix.rda"))

## Save the loaded data
saveRDS(glm_results_Macro, file = paste0(save_path, save_lags_[[5]][[1]]))
saveRDS(glm_results_SA_all, file = paste0(save_path, save_lags_[[5]][[2]]))
saveRDS(glm_results_SA_avg, file = paste0(save_path, save_lags_[[5]][[3]]))
saveRDS(glm_results_Mix, file = paste0(save_path, save_lags_[[5]][[4]]))
