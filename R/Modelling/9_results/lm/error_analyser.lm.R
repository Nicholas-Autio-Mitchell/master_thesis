
###################### ======================================== ######################
######################  Extract and plot errors across subsets  ######################
###################### ======================================== ######################

#' extract all errors and make a plot of some kind. Perhaps the average predictive accuracy with errors as a box plot.

load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/glm_collated.results.rda")

## -------------------------- ##
##  Extract the error values  ## --> only the MSE ?
## -------------------------- ##

##Create a container for all results
glm_mse <- sapply(names(glm_collated.results), function(x) NULL)

## Container for each data subset (of frame-size and cutoff pairs)
results_frame <- data.table(Lag = 1:5, trad_small = matrix(0, 5, 1), trad_large = matrix(0, 5, 1),
                            sent_small = matrix(0, 5, 1), sent_large = matrix(0, 5, 1), combine = matrix(0, 5, 1))
## Give useful names
names(results_frame) <- c("Lag", "trad_small", "trad_large", "sent_small", "sent_large", "combined")

## Assign to list elements
for(i in 1:length(glm_mse)) {glm_mse[[i]] <- results_frame}

## Extract all predictive accuracy values
for(z in 1:length(glm_collated.results)) {

    x <- glm_collated.results[[z]]

    for(i in 1:5) {                     #assigns the columns one-by-one
        
        for(j in seq(0, 20, by = 5)) {
            
            k <- i + j
            curr_subset <- (j/5)+1
            glm_mse[[z]][curr_subset, (i+1)] <- x[[k]][[1]][[3]]
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
    
    mres[[i]] <- melt(glm_mse[[i]], id.vars = "Lag")
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
setnames(melted_results, old = c("variable", "value"), new = c("Subset", "MSE"))

## Inspect the top 50 results
head(melted_results[order(`MSE`)], 50)

## ---------------- ##
##  Plot in facets  ##
## ---------------- ##

## Create facetted plot of all results
pred_facet <- ggplot(data = melted_results, aes(x = Lag, y = `MSE`, colour = Subset)) +
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
pred_facet1 <- ggplot(data = x1, aes(x = Lag, y = `MSE`, colour = Subset)) +
    facet_grid(facets = "frame_size ~ cutoff", scales = "free_y") +
    geom_line() +
    theme_bw() +
    labs(x = "\nNumber of lags", y = "Predictive Accuracy (%)\n")

pred_facet1

ggsave(pred_facet1, filename = "glm_pred_acc_slim1.png")

## --------------------------- ##
##  Remove longer frame-sizes  ##
## --------------------------- ##

## Select only the fram-sizes 40 and 60, but keep all cutoff values
x2 <- x1[frame_size < 70]


library(latex2exp)

## Make naming of facets nicer
x2[cutoff == "no_cutoff"][, c("cutoff")] <- "None" 

x2$Lag <- factor(x2$Lag)

## Create facetted plot of all results
pred_facet2 <- ggplot(data = x2, aes(x = Subset, y = 100000*`MSE`, colour = Lag)) +
    facet_grid(facets = " frame_size ~ cutoff") +
    geom_point(size = 5) +
    theme_bw(base_size = 24) +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, )) +
    scale_y_continuous(limits = c(5.5, 8)) +
    theme(legend.position = "bottom") +
    guides(fill = guide_legend(title = "Lag value")) +
    labs(x = "\nSubset", y = TeX("Mean sqaured error $\\left( 10^{-5} \\right)$"))

pred_facet2                             #have a look

scale_fill_manual(name = "This is my title", values = c("pink", "green")
                    , labels = c("0" = "Foo", "1" = "Bar")) +
    

ggsave(pred_facet2, filename = "glm_pred_acc_best.png")




