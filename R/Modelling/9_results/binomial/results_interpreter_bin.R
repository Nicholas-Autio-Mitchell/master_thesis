###################### ============================================== ######################
######################  Create some comparative statistics and plots  ######################
###################### ============================================== ######################

## Compare the accuracy of the various data subsets and the results from the Gaussian and Binomial families

## --------------------------- ##
##  Load the collated results  ##
## --------------------------- ##

##The collated data set
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/binomial/binomial_collated.results.rda")
load("/Volumes/Mac OS Drive/Documents/FIM/Thesis - move to Mac OS/Source Code/R/Modelling/9_results/binomial/binomial_collated.results.rda")

## ---------------------------------------- ##
##  Extract just the predictive accuracies  ##
## ---------------------------------------- ##

##Create a container for all results
bin_pred_acc <- sapply(names(binomial_collated.results), function(x) NULL)

## Container for each data subset (of frame-size and cutoff pairs)
results_frame <- data.table(Lag = 1:5, trad_small = matrix(0, 5, 1),
                            trad_large = matrix(0, 5, 1),
                            sent_small = matrix(0, 5, 1),
                            sent_large = matrix(0, 5, 1),
                            combine = matrix(0, 5, 1))
## Give useful names
names(results_frame) <- c("Lag", "trad_small", "trad_large", "sent_small", "sent_large", "combined")

## Assign to list elements
for(i in 1:length(bin_pred_acc)) {bin_pred_acc[[i]] <- results_frame}

## Extract all predictive accuracy values
for(z in 1:length(binomial_collated.results)) {

    x <- binomial_collated.results[[z]]

    for(i in 1:5) {                     #assigns the columns one-by-one
        
        for(j in seq(0, 20, by = 5)) {
            
            k <- i + j
            curr_subset <- (j/5)+1
            bin_pred_acc[[z]][curr_subset, (i+1)] <- x[[k]][[1]][[2]]
        }
    }
}



## -------------------------------------------- ##
##  Transform and combine results for plotting  ##
## -------------------------------------------- ##

## Define values to label results within the melted data-table
s_names <- names(binomial_collated.results)
cutoffs <- c(seq(30, 90, 10), replicate(3, c(70, 80, 90)), replicate(4, "no_cutoff"))
frames <- c(replicate(7, 40), 60, 60, 60, 80, 80, 80, replicate(4, 100), 40, 60, 80)

## Create list to hold melted results tables in melted form
mres <- sapply(names(binomial_collated.results), function(x) NULL)

## Melt results and assign to new list
for(i in 1:length(mres)) {
    
    mres[[i]] <- melt(bin_pred_acc[[i]], id.vars = "Lag")
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

## Inspect the top 50 predictive accuracies
dfg <- head(melted_results[order(-`Predictive accuracy (%)`)], 50)

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
ggsave(pred_facet, filename = "bin_pred_acc_all_results1.png")


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

ggsave(pred_facet1, filename = "bin_pred_acc_slim1.png")

## --------------------------- ##
##  Remove longer frame-sizes  ##
## --------------------------- ##

## Select only the fram-sizes 40 and 60, but keep all cutoff values
x2 <- x1[frame_size < 70]

## Create facetted plot of all results
pred_facet2 <- ggplot(data = x2, aes(x = Lag, y = `Predictive accuracy (%)`, colour = Subset)) +
    facet_grid(facets = "frame_size ~ cutoff", scales = "free_y") +
    geom_line() +
    theme_bw() +
    labs(x = "\nNumber of lags", y = "Predictive Accuracy (%)\n")

pred_facet2                             #have a look

ggsave(pred_facet2, filename = "bin_pred_acc_best1.png", )

