## ===================================================================================== ##
##  Create plots for the binomial results using only large and combined dataset results  ##
## ===================================================================================== ##

## --------------------------- ##
##  Load the collated results  ##
## --------------------------- ##

##The collated data set
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/binomial/binomial_collated.results.rda")
load("/Volumes/Mac OS Drive/Documents/FIM/Thesis - move to Mac OS/Source Code/R/Modelling/9_results/binomial/binomial_collated.results.rda")

## Remove the results for all the small subsets i.e. those containing "small" in the name
to_remove <- grep(pattern = "_small$", names(binomial_collated.results$A_cutoff70_frame40))

x <- copy(binomial_collated.results)

for(i in 1:length(x)) {

    for(j in 1:length(x[[i]])) {

        for(k in 1:length(to_remove)) {
            
            x[[to_remove[[k]]]] <- NULL
            
        }
    }
}
