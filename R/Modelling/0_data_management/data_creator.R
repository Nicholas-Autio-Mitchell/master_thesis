###################### ============================================================== ######################
######################  Create all lagged and logged data for indices and macro data  ######################
###################### ============================================================== ######################

#' This file is used to obtain, impute and combine indice data.
#' Further macro data and the sentiment analysis results are combined later in 'data_subsetter.R'

## ======================================= ##
##  Add log returns for other market data  ##
## ======================================= ##

## As a template for other smybols
getSymbols("^DJI")                      #get the data
x <- DJI[, 6]["20130111/20150911"]      #select out time-series plus one day before
##x <- DJI[, 6]["20130111/20130111"]    #select just the preceding Friday 11th Jan 2013

DJI.Adjusted["20130111/20130111"]  

## As a template for other smybols
getSymbols(c("^GDAX", "^FTSE", "GSPC", "^N225", "000001.SS", ))                      #get the data
x <- DJI[, 6]["20130111/20150911"]      #select out time-series plus one day before
##x <- DJI[, 6]["20130111/20130111"]      #select just the preceding Friday 11th Jan 2013

DJI.Adjusted["20130111/20130111"]  

## ============================================================================ ##
##  Taken and adapted from data_enhancer.R - there was used for Dow Jones only  ##
## ============================================================================ ##

## Create base data set, removing weekends and col: day_names
ds <- data_to_impute[is.weekend == 0] %>%
    subset(., select = c(1, seq(3, 164)))

## Add a column for the log returns of the DJI
## filling in from the week before to keep length
getSymbols(c('^GDAXI', "^FTSE", "GSPC", "^N225", "EFA"))
## Once the variable for the SSE is auto assigned, its name begin with digits, which means SSE can't read it. Need to auto.assign with a name beginning with characters
SSE = getSymbols(Symbols =  c("000001.SS"), auto.assign = FALSE)
myDAX = getSymbols(Symbols =  c("^GDAXI"), auto.assign = FALSE)
myFTSE = getSymbols(Symbols = c("^FTSE"), auto.assign = FALSE)

## Could do all of them individually for consistency, but not necessary
## dax <- myDAX["20130111/20150911"]
## dax <- dax[,6]
## log_ret_DAX <- diff(log(dax))
## log_ret_DAX <- log_ret_DAX[2:672,]
## ftse <- myFTSE["20130111/20150911"]
## ftse <- ftse[,6]
## log_ret_FTSE <- diff(log(ftse))
## log_ret_FTSE <- log_ret_FTSE[2:672,]

## This works, assigning only SSE manually as above
dax <- GDAXI["20130111/20150911"]
dax <- dax[,6]
log_ret_DAX <- diff(log(dax))
ftse <- FTSE["20130111/20150911"]
ftse <- ftse[,6]
log_ret_FTSE <- diff(log(ftse))
gspc <- GSPC["20130111/20150911"]
gspc <- gspc[,6]
log_ret_GSPC <- diff(log(gspc))
n225 <- N225["20130110/20150911"]
n225 <- n225[,6]
log_ret_nikkei225 <- diff(log(n225))
SSE_comp <- SSE["20130111/20150911"]
SSE_comp <- SSE_comp[,6]
log_ret_SSE_comp <- diff(log(SSE_comp))
EFA <- EFA["20130111/20150911"]
EFA <- EFA[,6] # only one column
log_ret_EFA <- diff(log(EFA))

## Merging works better as data.tables
dtDAX <- as.data.table(log_ret_DAX)
dtFTSE <- as.data.table(log_ret_FTSE)
dtGSPC <- as.data.table(log_ret_GSPC)
dtnikkei225 <- as.data.table(log_ret_nikkei225)
dtSSE_comp <- as.data.table(log_ret_SSE_comp)
dtEFA <- as.data.table(log_ret_EFA)

## Create a list with data to merge using Reduce()
lind <- sapply(c("dtDAX", "dtFTSE", "dtGSPC", "dtnikkei225", "dtSSE_comp", "dtEFA"), function(x) NULL) #logIndex
lind$dtDAX <- dtDAX
lind$dtFTSE <- dtFTSE
lind$dtGSPC <- dtGSPC
lind$dtnikkei225 <- dtnikkei225
lind$dtSSE_comp <- dtSSE_comp
lind$dtEFA <- dtEFA

## Define function to merge data by
lind_merge <- function(x, y){merge(x, y, by.x = "index", by.y = "index",
                            all = TRUE)}

x <- as.data.table(Reduce(lind_merge, lind))
names(x) <- c("dates", "log_ret_DAX", "log_ret_FTSE","log_ret_GSPC",
              "log_ret_nikkei225", "log_ret_SSE_comp", "log_ret_EFA")

## Save the names to reapply after imputating
lind_names <- names(x)

## Trim the basic set leaving NAs for {caret} package
indices_caret <- x[3:nrow(x1)]  
## Impute and trim NA row --> LOCF
x1 <- as.data.table(na.locf(as.xts(x)))
names(x1) <- lind_names
indices_locf <- x1[3:nrow(x1)]          ##Has to be two as we start on 10th Jan to account for Nikkei missing a day!
## Impute and trim NA row --> spline
x2 <- as.data.table(na.spline(as.xts(x)))
names(x2) <- lind_names
indices_spline <- x2[3:nrow(x1)]        ##Has to be two as we start on 10th Jan to account for Nikkei missing a day!

## Bundle together and save the indice data
indice_log_ret <- sapply(c("caret", "locf", "spline"), function(x) NULL)
indice_log_ret$caret <- indices_caret
indice_log_ret$locf <- indices_locf
indice_log_ret$spline <- indices_spline

save(indices_log_ret, file = "indice_log_ret.rda")
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/indice_log_ret.rda") #called indiceS_log_ret in workspace

## ============================================== ##
##  Compute and save the lagged data for indices  ##
## ============================================== ##

## Define a list to house all data at end
indice_lag_log_imputed <- sapply(c("caret", "locf", "spline"), function(x) NULL)

## Names of data sets ('caret' - means not imputed here, to be done in {caret})
caret_base_names <- paste0("caret_L", seq(0, 5))
locf_base_names <- paste0("locf_L", seq(0, 5))
spline_base_names  <- paste0("spline_L", seq(0, 5))
## Create separate lists for each imputation method
caret_lags <- sapply(caret_base_names, function(x) NULL)
locf_lags <- sapply(locf_base_names, function(x) NULL)
spline_lags <- sapply(spline_base_names, function(x) NULL)

## Assign the non-lagged values to the lists unaltered
caret_lags[[1]] <- as.data.table(indice_log_ret$caret)
locf_lags[[1]] <- as.data.table(indice_log_ret$locf)
spline_lags[[1]] <- as.data.table(indice_log_ret$spline)

indice_lag_log_imputed$caret <- caret_lags
indice_lag_log_imputed$locf <- locf_lags
indice_lag_log_imputed$spline <- spline_lags

## Adapted from the code below creating lags for macro data!

## save the dates to reattach later
mainDates <- indice_lag_log_imputed$caret$caret_L0[, .(dates)] #is a data table
## Save the names of the variables to renames (_embed_ removes column names)
mainNames <- names(indice_lag_log_imputed$caret$caret_L0)[2:7] # excludes the dates col: 'dates'

for(i in 1:length(indice_lag_log_imputed)) {
    
    for(j in 1:(length(indice_lag_log_imputed$caret)-1)) {  # j is equivalent to the lag we're calculating
        
        tmp_list <- indice_lag_log_imputed[[i]] #the imputed list that we are lagging
        
        tmp <- as.data.table(embed(as.xts(tmp_list[[1]]), dimension = (j+1)))
        tmp1 <- cbind(mainDates[(j+1):nrow(mainDates)], tmp)
        indice_lag_log_imputed[[i]][[j+1]] <- tmp1
        
    }

    ## Name the variables in each data table
    for(p in 2:length(tmp_list)) {
        names(indice_lag_log_imputed[[i]][[p]]) <- c("dates", paste0("L", as.character((p-1)), "_", mainNames),
                                      names(tmp_list[[p-1]])[2:length(names(tmp_list[[p-1]]))])
    }
}

## Save the data as its own file
save(indice_lag_log_imputed, file = "indice_lag_log_imputed.rda")
## In an environment as backup
indice_data_lag_log_imputed <- new.env()         
indice_data_lag_log_imputed$data <- indice_lag_log_imputed
save(indice_data_lag_log_imputed, file = "indice_data_lag_log_imputed_ENV.rda")


###################### =============================================================== ######################
######################  Create one data table to create lagged data and merge with SA  ######################
###################### =============================================================== ######################

## Load the macro log data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/macro_log_ret.rda")
## Define function to merge all log macro data
lmacro_merge <- function(x, y){merge(x, y, by.x = "index", by.y = "index",
                            all = TRUE)}

## Put all log macro data together into one data table, altering names
raw_macro <- as.data.table(Reduce(lmacro_merge, macro_log_ret))
x <- raw_macro[, 2:23, with=FALSE]
myNames <- names(x)
names(x) <- paste0("log_", myNames)
raw_macro <- cbind(raw_macro[, .(index)], x)

## Define a list to house all data at end
macro_lag_log_imputed <- sapply(c("caret", "locf", "spline"), function(x) NULL)

## Names of data sets ('caret' - means not imputed here, to be done in {caret})
caret_base_names <- paste0("caret_L", seq(0, 5))
locf_base_names <- paste0("locf_L", seq(0, 5))
spline_base_names  <- paste0("spline_L", seq(0, 5))
## Create separate lists for each imputation method
caret_lags <- sapply(caret_base_names, function(x) NULL)
locf_lags <- sapply(locf_base_names, function(x) NULL)
spline_lags <- sapply(spline_base_names, function(x) NULL)

## Assign the non-lagged values to the lists unaltered
caret_lags[[1]] <- as.data.table(raw_macro)
locf_lags[[1]] <- as.data.table(na.locf(as.xts(raw_macro)))
spline_lags[[1]] <- as.data.table(na.spline(as.xts(raw_macro)))

macro_lag_log_imputed$caret <- caret_lags
macro_lag_log_imputed$locf <- locf_lags
macro_lag_log_imputed$spline <- spline_lags


## ======================================================== ##
##  Create the lagged data sets for caret, locf and spline  ##
## ======================================================== ##

## save the dates to reattach later
mainDates <- macro_lag_log_imputed$caret$caret_L0[, .(index)]
## <- macro_caret_lags$macro_caret_L0[, .(index)] # the older version? Delete?

## Save the names of the variables to renames (_embed_ removes column names)
mainNames <- paste0(names(macro_lag_log_imputed$caret$caret_L0)[2:23]) # excludes the dates col: 'index'

for(i in 1:length(macro_lag_log_imputed)) {
    
    for(j in 1:(length(macro_lag_log_imputed$caret)-1)) {  # j is equivalent to the lag we're calculating
        
        imputed_list <- macro_lag_log_imputed[[i]] #the imputed list that we are lagging
        
        tmp <- as.data.table(embed(as.xts(imputed_list[[1]])[,1:22], dimension = (j+1)))
        tmp1 <- cbind(mainDates[(j+1):nrow(mainDates)], tmp)
        macro_lag_log_imputed[[i]][[j+1]]  <- tmp1
        
    }

    ## Name the variables in each data table
    for(p in 2:length(imputed_list)) {
        names(macro_lag_log_imputed[[i]][[p]]) <- c("dates", paste0("L", as.character((p-1)), "_", mainNames),
                                      names(imputed_list[[p-1]])[2:length(names(imputed_list[[p-1]]))])
    }
}

## Save the data as its own file
save(macro_lag_log_imputed, file = "macro_lag_log_imputed.rda")
## In an environment as backup
macro_data_lag_log_imputed <- new.env()         
macro_data_lag_log_imputed$data <- macro_lag_log_imputed
save(macro_data_lag_log_imputed, file = "macro_data_lag_log_imputed_ENV.rda")

