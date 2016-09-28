###################### ============================================== ######################
######################  Create some comparative statistics and plots  ######################
###################### ============================================== ######################

## Compare the accuracy of the various data subsets and the results from the Gaussian and Binomial families

## --------------------------- ##
##  Load the collated results  ##
## --------------------------- ##

## the collated results
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/glm_collated.results.rda")

## ---------------------------------------- ##
##  Extract just the predictive accuracies  ##
## ---------------------------------------- ##

##Create a container for all results
glm_pred_acc <- sapply(names(glm_collated.results), function(x) NULL)

## Container for each data subset (of frame-size and cutoff pairs)
results_frame <- data.table(Lag = 1:5, trad_small = matrix(0, 5, 1), trad_large = matrix(0, 5, 1),
                            sent_small = matrix(0, 5, 1), sent_large = matrix(0, 5, 1), combine = matrix(0, 5, 1))
## Give useful names
names(results_frame) <- c("Lag", "trad_small", "trad_large", "sent_small", "sent_large", "combined")

## Assign to list elements
for(i in 1:length(glm_pred_acc)) {glm_pred_acc[[i]] <- results_frame}

## Extract all predictive accuracy values
for(z in 1:length(glm_collated.results)) {

    x <- glm_collated.results[[z]]

    for(i in 1:5) {                     #assigns the columns one-yby-one
        
        for(j in seq(0, 20, by = 5)) {
            
            k <- i + j
            curr_subset <- (j/5)+1
            glm_pred_acc[[z]][curr_subset, (i+1)] <- x[[k]][[2]][[2]]
        }
    }
}

## -------------------------------------------- ##
##  Transform and combine results for plotting  ##
## -------------------------------------------- ##

## Define values to label results within the melted data-table
s_names <- names(glm_collated.results)
cutoffs <- c(seq(30, 90, 10), replicate(3, c(70, 80, 90)), replicate(4, "no_cutoff"))
frames <- c(replicate(7, 40), 60, 60, 60, 80, 80, 80, replicate(4, 100), 40, 60, 80)

## Create list to hold melted results tables in melted form
mres <- sapply(names(glm_collated.results), function(x) NULL)

## Melt results and assign to new list
for(i in 1:length(mres)) {
    
    mres[[i]] <- melt(glm_pred_acc[[i]], id.vars = "Lag")
}

## Add columns to name results for plotting in facets
for(i in 1:length(mres)) {
    
    mres[[i]]$subset <- s_names[[i]]
    mres[[i]]$cutoff <- cutoffs[[i]]
    mres[[i]]$frame_size <- frames[[i]]
    mres[[i]]$subset <- NULL
    setcolorder(mres[[i]], neworder = c(5, 4, 1, 2, 3))
    
}

## Combine all subsets into one data-table
melted_results <- Reduce(f=rbind2, x = mres)
## Rename some columns for plotting labels
setnames(melted_results, old = c("variable", "value"), new = c("Subset", "Predictive accuracy (%)"))

## Inspect the top 50 results
head(melted_results[order(-`Predictive accuracy (%)`)], 50)

## ---------------- ##
##  Plot in facets  ##
## ---------------- ##

## Create facetted plot of all results
pred_facet <- ggplot(data = melted_results, aes(x = Lag, y = `Predictive accuracy (%)`, colour = Subset)) +
    facet_grid(facets = "frame_size ~ cutoff", scales = "free_y") +
    geom_line() +
    theme_bw() +
    labs(x = "\nNumber of lags", y = "Predictive Accuracy (%)\n")

pred_facet                              #have a look

## Save
ggsave(pred_facet, filename = "glm_pred_acc_all_results1.png")


## ----------------------------------------------- ##
##  Remove the lower levels of correlation cutoff  ##
## ----------------------------------------------- ##

x <- copy(melted_results)

x1 <- x[101:500]

## Create facetted plot of all results
pred_facet1 <- ggplot(data = x1, aes(x = Lag, y = `Predictive accuracy (%)`, colour = Subset)) +
    facet_grid(facets = "frame_size ~ cutoff", scales = "free_y") +
    geom_line() +
    theme_bw() +
    labs(x = "\nNumber of lags", y = "Predictive Accuracy (%)\n")

pred_facet1

ggsave(pred_facet1, filename = "glm_pred_acc_slim1.png")

## --------------------------- ##
##  Remove longer frame-sizes  ## --> main results for thesis
## --------------------------- ##

## Select only the fram-sizes 40 and 60, but keep all cutoff values
x2 <- x1[frame_size < 70]

## Create facetted plot of all results
pred_facet2 <- ggplot(data = x2, aes(x = Lag, y = `Predictive accuracy (%)`, colour = Subset)) +
    facet_grid(facets = "frame_size ~ cutoff", scales = "free_y") +
    geom_line() +
    theme_bw() +
    theme(legend.direction = "horizontal", legend.position = "bottom") + #this differs from what is in the thesis
    labs(x = "\nNumber of lags", y = "Predictive Accuracy (%)\n")

pred_facet2                             #have a look

ggsave(pred_facet2, filename = "glm_pred_acc_best.png")


## ================================================================= ##
##  Plot the dow according to our predicted returns, nominal of 100  ##
## ================================================================= ##

#' We want to see how closely the predictions match the actual movement.
#' Are there any patters here? Visible mean reversion in any way?
#' Can we say that our predictions look truly i.i.d., as the values in reality should be?
#' Can we detect heteroscedasticity in the residuals? How should we interpret this? (shift in current trend?)
#' see notes in book on explaining the 'frame_size' choice, and why it is a moving average.


## The best performing combined data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff80_frame60/results/glm_results.Lag.3.combined.rda")
p <- copy(glm_results.Lag.3.combined)
## The best performing traditional data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff70_frame60/results/glm_results.Lag.1.trad_large.rda")
q <- copy(glm_results.Lag.1.trad_large)
## The traditional data set that had the sam parameters as the best performing combined data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff80_frame60/results/glm_results.Lag.1.trad_large.rda")
r <- copy(glm_results.Lag.1.trad_large)

## Extract the predictions for each day into a matrix
p1 <- matrix(NA, length(p$results), 4)

for(i in 1:nrow(p1)) {

    p1[i, 1] <- as.numeric(p[[2]][[i]][[2]])       #actual market return
    p1[i, 2] <- as.numeric(p[[2]][[i]][[3]])       #our prediction - combine
    p1[i, 3] <- as.numeric(q[[2]][[i]][[3]])       #our prediction - sent_large
    p1[i, 4] <- as.numeric(r[[2]][[i]][[3]])       #our prediction - sent_large

}

price_paths <- matrix(NA, nrow(p1), 4)

## Initialise the values
price_paths[1, 1] <- 100 * (1 + p1[1, 1])       #using nominal value of 100
price_paths[1, 2] <- 100 * (1 + p1[1, 2])
price_paths[1, 3] <- 100 * (1 + p1[1, 3])
price_paths[1, 4] <- 100 * (1 + p1[1, 4])

for(i in 2:nrow(p1)) {

    price_paths[i, 1] <- price_paths[i-1, 1] * (1 + p1[i, 1])
    price_paths[i, 2] <- price_paths[i-1, 2] * (1 + p1[i, 2])
    price_paths[i, 3] <- price_paths[i-1, 3] * (1 + p1[i, 3])
    price_paths[i, 4] <- price_paths[i-1, 4] * (1 + p1[i, 4])    
}

## Create the required dates
library(chron)
days = seq(as.Date("2013-01-14"), as.Date("2015-09-11"), by="day")
my_days = days[!is.weekend(days)]
my_days <- my_days[63:695]

## Combine dates with data, rename and order
p2 <- as.data.table(cbind(my_days, price_paths))
p2$Date <- as.Date(p2$my_days)
p2$my_days <- NULL
setcolorder(p2, c(5, 1, 2, 3, 4))
setnames(p2, names(p2), new = c("Date", "DJIA", "Combined", "Traditional (best)", "Traditional (same)"))
## Melt to create plots
p3 <- melt(p2, id.vars = c("Date"))
setnames(p3, old = c("value", "variable"), new = c("Nominal value", "Price path"))

## Plot all prices paths
paths <- ggplot(data = p3, aes(x = Date, y = `Nominal value`, colour = `Price path`)) +
    geom_line() +
    labs(x = "\nDate", y = "Nominal value\n(normalised to 100)\n") +
    theme_bw() +
    theme(legend.position = c(0.09, 0.85))

paths                                   #have a look

## Save
ggsave(paths, filename = "price_paths.png")



## =============================================== ##
##  Same plot but with only the DJIA and combined  ##
## =============================================== ##


## Load the "combined" subset that had the best predictive accuracy: it had 56.24%. Best (sent_large L4, frame60, cutoff90) had 56.80%
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff80_frame60/results/glm_results.Lag.3.combined.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff70_frame60/results/glm_results.Lag.1.trad_large.rda")

p <- copy(glm_results.Lag.3.combined)

## Extract the predictions for each day into a matrix
p1 <- matrix(NA, length(p$results), 2)

for(i in 1:nrow(p1)) {

    p1[i, 1] <- as.numeric(p[[2]][[i]][[2]])       #actual market return
    p1[i, 2] <- as.numeric(p[[2]][[i]][[3]])       #our prediction - combine
}

price_paths <- matrix(NA, nrow(p1), 2)

## Initialise the values
price_paths[1, 1] <- 100 * (1 + p1[1, 1])       #using nominal value of 100
price_paths[1, 2] <- 100 * (1 + p1[1, 2])

for(i in 2:nrow(p1)) {

    price_paths[i, 1] <- price_paths[i-1, 1] * (1 + p1[i, 1])
    price_paths[i, 2] <- price_paths[i-1, 2] * (1 + p1[i, 2])
}


library(chron)
days = seq(as.Date("2013-01-14"), as.Date("2015-09-11"), by="day")
my_days = days[!is.weekend(days)]
my_days <- my_days[63:695]


p2 <- as.data.table(cbind(my_days, price_paths))
p2$Date <- as.Date(p2$my_days)
p2$my_days <- NULL
setcolorder(p2, c(4, 1, 2, 3))
setnames(p2, names(p2), new = c("Date", "DJIA", "Predicted"))

p3 <- melt(p2, id.vars = c("Date"))
setnames(p3, old = c("value", "variable"), new = c("Nominal value", "Price path"))


paths <- ggplot(data = p3, aes(x = Date, y = `Nominal value`, colour = `Price path`)) +
    geom_line() +
    labs(x = "\nDate") +
    theme_bw() +
    theme(legend.position = c(0.06, 0.9))

paths

ggsave(paths, filename = "price_paths.png")








## match the length of the values to be predicted to our time-scale
sim_ <- data.table(actual = rep(100, length(model_acc$y)-1),
                   macro = rep(100, length(model_acc$y)-1),
                   sa_all = rep(100, length(model_acc$y)-1),
                   sa_avg = rep(100, length(model_acc$y)-1),
                   mix = rep(100, length(model_acc$y)-1))


## Calculate the nominal returns on our notional 10
for(j in 1:5) {
    for(i in 2:(length(model_acc$y)-1)) {

        sim_[[j]][[i]] <- sim_[[j]][[(i-1)]] * (1+model_acc[[j]][[i]])
    }
}



## The individual results objects
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




## ============= ##
##  Import data  ##
## ============= ##
## Until all results are obtained and grouped together, this is the method to use

## Use the lag_3 results 
load(paste0(lag3_path, "glm_results_Macro.rda"))
load(paste0(lag3_path, "glm_results_SA_all.rda"))
load(paste0(lag3_path, "glm_results_SA_avg.rda"))
load(paste0(lag3_path, "glm_results_Mix.rda"))

## Create their future aliases (use copy due to data.table referencing)
macro <- copy(glm_results_Macro)
sa_all <- copy(glm_results_SA_all)
sa_avg <- copy(glm_results_SA_avg)
mix <- copy(glm_results_Mix)

## =========================================== ##
##  Bring all errors together into one object  ##
## =========================================== ##

## Combine the actual results with the predictions for all cvrisk() output
## The actual outcome only needs to be shown once
model_acc <- data.table(y = unlist(sapply(macro$results, function(x){x[[2]]})), #--> should this be a 3?
                        macro_f = sapply(macro$results, function(x){x[[3]]}),
                        sa_all_f = sapply(sa_all$results, function(x){x[[3]]}),
                        sa_avg_f = sapply(sa_avg$results, function(x){x[[3]]}),
                        mix_f = sapply(mix$results, function(x){x[[3]]})
                        )

myData <- as.data.frame(model_acc)      #why must this be a data.frame??

## Plot the results
boxplot(myData, las = 2, at =c(1, 3, 5, 7, 9), par(mar = c(12, 5, 4, 2) + 0.1))
qplot(myData, )
