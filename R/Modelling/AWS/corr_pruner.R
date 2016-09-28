## ============================================================================================== ##
##  Create function to remove covariates pbased on pairwise correlation - visualisation possible  ##
## ============================================================================================== ##

## Get the correlation matrix
corr_pruner <- function(input, cutoff = .85,                           #choose correlation cutoff level
                 plot = FALSE, low_corr = .65, high_corr = .99, #plot with limits low and high
                 print.removed = FALSE) {                        #show names of variables removed
    
    ## Create correlation matrix of input data
    x <- cor(input)

    ## If user wants a plot, create one
    if(plot == TRUE) {
        ## Choose some parameters
        low_corr <- low_corr * 100
        high_corr <- high_corr * 100
        n.points = high_corr - low_corr         # Warning message left in by design - to get nice data.frame
        corr_rem <- as.matrix(cbind(seq(low_corr, high_corr, 1),
                                    rep(0, times = n.points),
                                    rep(0, times = n.points)))

        ## Use the findCorrelation() function from {caret}
        for( i in low_corr:high_corr){
            tmp <- length(findCorrelation(x, cutoff = i/100, names = TRUE, exact = TRUE))
            corr_rem[(i-(low_corr-1)), 2] <- tmp
            corr_rem[(i-(low_corr-1)), 3] <- tmp/dim(x)[[2]]
        }

        ## ## Simple plot
        ## plot(corr_rem, main = "Number of variables removed against cutoff correlation used",
        ##      xlab = "Correlation cutoff (%)",
        ##      ylab = "Percentage of predictors removed",
        ##      type = "l", lwd = 2, col = "red")

        ## Add a variable for the delta of predictors between points
        corr_rem <- as.data.table(corr_rem)
        names(corr_rem) <- c("Cutoff", "Total", "Percent")
        corr_rem$Delta <- diff(corr_rem[,Total])

        ## Plot the percentage of predictors removed on second axis
        par(mar = c(5,5,5,5))
        with(corr_rem, plot(Cutoff, Total, type="l", col="blue",
                            xlab = "Correlation used for cutoff (%)",
                            ylab= "Number of predictors removed",
                            main = "Predictors removed as a function of average pairwise correlation",
                            ylim=range(max(corr_rem[,Total]), min(corr_rem[,Total]))))
        arrows(corr_rem$Cutoff, corr_rem$Total-corr_rem$Delta , corr_rem$Cutoff, corr_rem$Total+corr_rem$Delta, length= .05, angle=90, code = 3)

        par(new = T)
        with(corr_rem, plot(Cutoff, Percent, type = "l", col="blue", pch=16, axes=F,
                            xlab=NA, ylab=NA, cex=1.2),
             ylim=rev(range(min(corr_rem[,Percent]), (max(corr_rem[,Percent])))))
        axis(side = 4)
        mtext(side = 4, line = 3, 'Percentage of total predictors removed')
        with(corr_rem, arrows(Cutoff, Total-Delta , Cutoff, Total+Delta, length= 50, angle=90, code=3))
        legend("topright",pch = NULL,
               legend=c("'Error' bars reflect relative number\n of predictors removed at threshold"),
               lty=c(2,0))
    } else {
        message("++ To visualise the numbers of covariates removed as a function of chosen cutoff,\n++ run the same function again with 'plot = TRUE'")
    }
    ## Pick a percentage and see how many and which predictors will be removed as a result
    corr_cutoff <- cutoff
    to_remove <- findCorrelation(x, cutoff = corr_cutoff, names = TRUE, exact = TRUE)
    if(length(to_remove) == 0) {
        message("No covariates were removed using the supplied correlation cutoff level")
        return(input)
    } else {
        output <- input[, !to_remove, with=FALSE]
        message(paste0("Number of covariates removed with supplied correlation cutoff level: ",
                       as.character(length(to_remove))))
        if(print.removed == TRUE) {
            print(setdiff(names(input), names(output)))
        }
        
        return(output)
    }
}


## ========================================= ##
##  Function to set random seed from string  ##
## ========================================= ##

## Define a function to set random seed for any RNGs
set.seed.alpha <- function(x) {
    library(digest)
    hexval <- paste0("0x",digest(x,"crc32"))
    intval <- type.convert(hexval) %% .Machine$integer.max
    set.seed(intval)
}
