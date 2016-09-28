###################### ============================================================== ######################
######################  Take "data_dirty", complete imputation and feature selection  ######################
###################### ============================================================== ######################

#' Here we create several variations of the data set.
#' The idea is to see which imputations methods are the most effective in producing strong results.
#' One basic test case for boosting can then be carried out on all test sets,
#' before going more deeply into the data set which has shown the most significant results.

## ================================== ##
##  Conclusions from data inspection  ##
## ================================== ##

#' The weekends are inherently the cause of the majority of NAs
#' These must be removed, as they are not random;
#' [Missing And Random, Missing And Completely Random -> COULD be replaced]

#' We can keep some form of information from the weekend data from SA:
#' Create new dummy variables: was.sat.pos, was.sun.pos, was.weekend.pos
#' -> These will be the average of all models in each case
#' Use file "sentiment_analysis_daily_averages.rda"

#' Additionally we can plot (according to my notes in book):
#' 1) Whether the weekend sentiment results are correlated with Monday-movements
#' 2) Is there are general pattern? [See the four example cases in notebook]

## Load the most advanced data set: "data_to_impute"
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_to_impute.rda")
## Load the merged data: "data_dirty"
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/data_dirty.rda")
## Load the bank holidays; they are saves as a vector of strings and must be converted to dates
#load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/holidays.rda")

###################### ==================== ######################
######################  Imputation Methods  ######################
###################### ==================== ######################

## ## Load the bank holidays; they are saved as a vector of character dates
## load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/holidays.rda")
## holidays <- as.Date(holidays)

## ## Get a first day for Nikkei225 so that LOCF works!
## getSymbols("^N225")
## nikkei_Jan13 <- N225["201301/201302"]
## nikkei_Jan13 <- N225["20130111/20130112"]
## ## Insert 'LOCF' for Nikkei225
## data_to_impute$nikkei225[1] <- nikkei_Jan13[1,6]

## ## ======================================== ##
## ##  Create columns to help with imputation  ##
## ## ======================================== ##
## #######
## ## This was done earlier before scaled average SA scores were calculated and appened
## ## Add dummy variables: the weekdays (char), plus DVs for Mondays, weekends and bank holidays
## data_dirty$days_names <- weekdays(data_dirty$dates)
## data_dirty$is.monday <- as.numeric(weekdays(data_dirty$dates) %in% c('Monday'))
## data_dirty$is.weekend <- as.numeric(weekdays(data_dirty$dates) %in% c('Sunday','Saturday'))
## ##data_dirty$is.weekend <- format(as.Date(data_dirty$dates),"%w")   ## get numbers for days: 0-6 -> Sun-Sat
## #######

## ## add holidays to data_to_compute
##data_to_impute$is.holiday <- as.numeric(data_to_impute$dates %in% holidays)

## Define function to check if sentiment results were positive or negative
is.naturalnumber <- function(x, tol = .Machine$double.eps^0.5)  x > tol & abs(x - round(x)) < tol
data_to_impute$is.sent.pos <- ifelse(is.naturalnumber())


## is.bank.holiday

library(lubridate)

## was.weekend.pos

## Applying a function to each column of a data.table
## Source: http://stackoverflow.com/questions/16846380/how-to-apply-same-function-to-every-specified-column-in-a-data-table
## Here the function just makes the column a negative value
for (j in flipcols) set(dt,j=j,value=-dt[[j]])

## ======================================================= ##
##  Create data sets (see table in 'TO_DO_modelling.org')  ##
## ======================================================= ##

## Create base data set, removing weekends and col: day_names
ds <- data_to_impute[is.weekend == 0] %>% 
    subset(., select = c(1, seq(3, 164))) #Weekends are removed, but the is.weekend variable is still in the data set

ds1 <- subset(data_to_impute, select = -4)

## Add a column for the log returns of the DJI
## filling in from the week before to keep length
getSymbols("^DJI")
dji <- DJI["20130111/20150911"]
dji <- dji[,6]
dd <- diff(log(dji))
dd <- dd[2:672,]
names(dd) <- c("log_ret_DJI")


## Append to data
ds <- as.xts(merge(ds, as.data.table(dd), by.x = "dates", by.y = "index", all = TRUE)) %>%
    as.data.table(.)
## Save the column names and dates
mainNames1 <- c(names(ds)[2:ncol(ds)])
mainDates <- ds[, .(index)]

## ============================================================================== ##
##  the 'ds' will be taken forward for imputaing within {caret} - kNN, bagImpute  ##
## ============================================================================== ##

## non-imputed data set to be imputed in {caret} - PreProcess()
ds_caret_L0 <- as.xts(ds)

## LOCF method for replacing NAs
ds_locf_L0 <- na.locf(as.xts(ds))
names(ds_locf_L0) <- mainNames
#ds_locf_L0 <- as.data.table(ds_locf_L0)

## Spline method for replacing NAs
ds_spline_L0 <- na.spline(as.xts(ds))
names(ds_spline_L0) <- mainNames
#ds_spline_L0 <- as.data.table(ds_spline_L0)

## Show that there are zero NAs and the methods don't yield identical results
sum(is.na(ds_locf_L0))
sum(is.na(ds_spline_L0))
identical(ds_locf_L0, ds_spline_L0)
identical(dim(ds_locf_L0), dim(ds_spline_L0))

## ======================================================== ##
##  Create the lagged data sets and save all to envrinment  ##
## ======================================================== ##
#' To use embed() correctly, we need an xts/zoo object
#' We also lose the dates column and the names
#' These must be reinsterted below for each lagged data set

## Names of data sets ('caret' signifies not imputed here, to be done using {caret} package)
caret_base_names <- paste0("ds_caret_L", seq(1, 5))
locf_base_names <- paste0("ds_locf_L", seq(1, 5))
spline_base_names  <- paste0("ds_spline_L", seq(1, 5))
## Create list to store the data
ds_caret_lags <- sapply(caret_base_names, function(x) NULL)
ds_locf_lags <- sapply(locf_base_names, function(x) NULL)
ds_spline_lags <- sapply(spline_base_names, function(x) NULL)


## =============================================== ##
##  Change the index here and regenerate the data  ##    !!!!!!!!!!!!!!!!!!!!!!
## =============================================== ##


## Create all lagged data sets for {caret} data
for(i in 1:length(ds_caret_lags)) {       # i basically represents the lag
    
    tmp <- embed(ds_caret_L0, (i+1))
    tmp <- cbind(mainDates[(i+1):(dim(ds_caret_L0)[1])], tmp)
    #names(tmp) <- c("dates", paste0("1lag_", mainNames), mainNames)
    ds_caret_lags[[i]] <- tmp
}

## Create all lagged data sets for LOCF data
for(i in 1:length(ds_locf_lags)) {       # i basically represents the lag
    
    tmp <- embed(ds_locf_L0, (i+1))
    tmp <- cbind(mainDates[(i+1):(dim(ds_locf_L0)[1])], tmp)
    #names(tmp) <- c("dates", paste0("1lag_", mainNames), mainNames)
    ds_locf_lags[[i]] <- tmp
}

## Create all lagged data sets for spline data
for(i in 1:length(ds_spline_lags)) {       # i basically represents the lag
    
    tmp <- embed(ds_spline_L0, (i+1))
    tmp <- cbind(mainDates[(i+1):(dim(ds_spline_L0)[1])], tmp)
    #names(tmp) <- c("dates", paste0("1lag_", mainNames), mainNames)
    ds_spline_lags[[i]] <- tmp
}

## ========================================================= ##
##  Apply names to all of the variables in all of the lists  ##
## ========================================================= ##

## For the CARET data set
names(ds_caret_lags[[1]]) <- c("dates", paste0("L1_", mainNames), names(ds_caret_L0))

for(i in 2:length(ds_caret_lags)){

    names(ds_caret_lags[[i]]) <- c("dates", paste0("L", as.character(i), "_", mainNames),
                                  names(ds_caret_lags[[i-1]])[2:length(names(ds_caret_lags[[i-1]]))])
}

## For the LOCF data set
names(ds_locf_lags[[1]]) <- c("dates", paste0("L1_", mainNames), names(ds_locf_L0))

for(i in 2:length(ds_locf_lags)){

    names(ds_locf_lags[[i]]) <- c("dates", paste0("L", as.character(i), "_", mainNames),
                                  names(ds_locf_lags[[i-1]])[2:length(names(ds_locf_lags[[i-1]]))])
}

## for the spline data set
names(ds_spline_lags[[1]]) <- c("dates", paste0("L1_", mainNames), names(ds_spline_L0))

for(i in 2:length(ds_spline_lags)){

    names(ds_spline_lags[[i]]) <- c("dates", paste0("L", as.character(i), "_", mainNames),
                                  names(ds_spline_lags[[i-1]])[2:length(names(ds_spline_lags[[i-1]]))])
}

## Data set: 'ready_to_boost.rda' saved into data archive, new CORRECT version saved with more meaningful name 'all_data_lagged'
#' The error was that the date column was made shorter from the end of the column,
#' not the beginning as should be the case when embedding (creating lags)
#' The new data structure (list) also uses a more compact form, matching that
#' used by 'macro_lag_log_imputed.rda' - all data logged and lagged

## ## Save all data to environment to load into boosting session
## ready_to_boost <- new.env()
## ready_to_boost$ds_caret_L0 <- ds_caret_L0
## ready_to_boost$ds_locf_L0 <- ds_locf_L0
## ready_to_boost$ds_spline_L0 <- ds_spline_L0
## ready_to_boost$ds_caret_lags <- ds_caret_lags
## ready_to_boost$ds_locf_lags <- ds_locf_lags
## ready_to_boost$ds_spline_lags <- ds_spline_lags
## save(ready_to_boost, file = "ready_to_boost.rda")

## Create environment in which to save all data
all_data_lagged <- new.env()

## Create sub-lists for data
caret <- sapply(c("caret_L0", "caret_L1", "caret_L2", "caret_L3", "caret_L4", "caret_L5"), function(x) NULL)
locf <- sapply(c("locf_L0", "locf_L1", "locf_L2", "locf_L3", "locf_L4", "locf_L5"), function(x) NULL)
spline <- sapply(c("spline_L0", "spline_L1", "spline_L2", "spline_L3", "spline_L4", "spline_L5"), function(x) NULL)

## Assign lists to environment
all_data_lagged$caret <- caret
all_data_lagged$locf <- locf
all_data_lagged$spline <- spline

## caret
all_data_lagged$caret[[1]] <- as.data.table(ds_caret_L0)
all_data_lagged$caret[[2]] <- ds_caret_lags$ds_caret_L1
all_data_lagged$caret[[3]] <- ds_caret_lags$ds_caret_L2
all_data_lagged$caret[[4]] <- ds_caret_lags$ds_caret_L3
all_data_lagged$caret[[5]] <- ds_caret_lags$ds_caret_L4
all_data_lagged$caret[[6]] <- ds_caret_lags$ds_caret_L5
## locf
all_data_lagged$locf[[1]] <- as.data.table(ds_locf_L0)
all_data_lagged$locf[[2]] <- ds_locf_lags$ds_locf_L1
all_data_lagged$locf[[3]] <- ds_locf_lags$ds_locf_L2
all_data_lagged$locf[[4]] <- ds_locf_lags$ds_locf_L3
all_data_lagged$locf[[5]] <- ds_locf_lags$ds_locf_L4
all_data_lagged$locf[[6]] <- ds_locf_lags$ds_locf_L5
## spline
all_data_lagged$spline[[1]] <- as.data.table(ds_spline_L0)
all_data_lagged$spline[[2]] <- ds_spline_lags$ds_spline_L1
all_data_lagged$spline[[3]] <- ds_spline_lags$ds_spline_L2
all_data_lagged$spline[[4]] <- ds_spline_lags$ds_spline_L3
all_data_lagged$spline[[5]] <- ds_spline_lags$ds_spline_L4
all_data_lagged$spline[[6]] <- ds_spline_lags$ds_spline_L5

save(all_data_lagged, file = "all_data_lagged.rda")


###################### ========================================= ######################
######################  Create lagged log return for macro data  ######################
###################### ========================================= ######################

## ======================================= ##
##  Add log returns for other market data  ##
## ======================================= ##

## As a template for other smybols
getSymbols("^DJI")                      #get the data
x <- DJI[, 6]["20130111/20150911"]      #select out time-series plus one day before
##x <- DJI[, 6]["20130111/20130111"]      #select just the preceding Friday 11th Jan 2013

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
getSymbols(c('^GDAXI', "^FTSE", "GSPC", "^N225", "000001.SS", "EFA"))
## Once the variable for the SSE is auto assigned, its name begin with digits, which means ESS can't read it. Need to auto.assign with a name beginning with characters
SSE = getSymbols(Symbols =  c("000001.SS"), auto.assign = FALSE)
myDAX = getSymbols(Symbols =  c("^GDAXI"), auto.assign = FALSE)
myFTSE = getSymbols(Symbols = c("^FTSE"), auto.assign = FALSE)

## Not really necessary to manually assign tickers that have regular names
#' myDAX and myFTSE are examples, but really only for consistency's sake.

## dax <- myDAX["20130111/20150911"]
## dax <- dax[,6]
## log_ret_DAX <- diff(log(dax))
## log_ret_DAX <- log_ret_DAX[2:672,]
## ftse <- myFTSE["20130111/20150911"]
## ftse <- ftse[,6]
## log_ret_FTSE <- diff(log(ftse))
## log_ret_FTSE <- log_ret_FTSE[2:672,]

dax <- GDAXI["20130111/20150911"]
dax <- dax[,6]
log_ret_DAX <- diff(log(dax))
ftse <- FTSE["20130111/20150911"]
ftse <- ftse[,6]
log_ret_FTSE <- diff(log(ftse))
gspc <- GSPC["20130111/20150911"]
gspc <- gspc[,6]
log_ret_GSPC <- diff(log(gspc))
n225 <- N225["20130111/20150911"]
n225 <- n225[,6]
log_ret_nikkei225 <- diff(log(n225))
SSE_comp <- SSE["20130111/20150911"]
SSE_comp <- SSE_comp[,6]
log_ret_SSE_comp <- diff(log(SSE_comp))
EFA <- EFA["20130111/20150911"]
## EFA <- EFA[,6] # only one column is returned!
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
lind_merge <- function(x, y){merge(x, y,
                            by.x = "index",
                            by.y = "index",
                            all = TRUE)}

x <- as.data.table(Reduce(lind_merge, lind))
names(x) <- c("dates", "log_ret_DAX", "log_ret_FTSE","log_ret_GSPC",
              "log_ret_nikkei225", "log_ret_SSE_comp", "log_ret_EFA")
## Get rid of the first row of NAs producd by taking log returnsimput
x1 <-  x[2:695]
## Save the names to reapply after imputating
lind_names <- names(x1)
## Impute (only locf is )
x2 <- as.data.table(na.locf(as.xts(x1)))
names(x2) <- lind_names


## "macro_log_ret.rda" was saved at this point


###################### ============================================= ######################
######################  Create the lagged verison of all macro data  ######################
###################### ============================================= ######################

## Load the macro log data
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/raw_data/macro_log_ret.rda")

## Define function to merge all log macro data
lmacro_merge <- function(x, y){merge(x, y,
                              by.x = "index",
                              by.y = "index",
                              all = TRUE)}

## Put all log macro data together into one data table
raw_macro <- as.data.table(Reduce(lmacro_merge, macro_log_ret))

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
mainDates <- macro_caret_lags$macro_caret_L0[, .(index)] #is a data table
## Save the names of the variables to renames (_embed_ removes column names)
mainNames <- names(macro_caret_lags$macro_caret_L0)[2:23] # excludes the dates col: 'index'

for(i in 1:length(macro_lag_log_imputed)) {
    
    for(j in 1:(length(imputed_list)-1)) {  # j is equivalent to the lag we're calculating
        
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



## ================================================= ##
##  Testing out predictive modelling for Imputation  ##
## ================================================= ##

## Test replacing NA values with output of predictive model
## simple linear model used to test
x <- data_dirty
mod <- lm(DJI ~ SSE_comp, data = x, na.action = "na.exclude")

## Replace NA values with values from model predictions
which(is.na(data_dirty$DJI), arr.ind = TRUE) <- predict(mod) #Not complete

pred <- rep(NA, nrow(xn));              # Create a vector of NAs to overwrite with model-simulated values
names(pred) <- row.names(xn)            # Names the vector columns as per the input 
xnn <- na.omit(xn)                      # Takes data set without the NAs to use as new data for predicting
pred[-attr(xnn, "na.action")] <- predict(mod, xnn) #predicts replacement values based on model
pred


###################### =================== ######################
######################  Feature Selection  ######################
###################### =================== ######################








###################### ==================================== ######################
######################  Viewing the Data differently - PCA  ######################
###################### ==================================== ######################










###################### ========================================= ######################
######################  ¡¡The following code is test material!!  ######################
###################### ========================================= ######################

## ==================================================== ##
##  Some work on trying to impute NAs more effectively  ##
## ==================================================== ##

#' Loop designed to select each model output within each search term
#' This is to allow more tailored NA replacements or to manipulate
#' results further on a model specific basis

cols_per_search <- 11

    ## Begin at 2 to avoid the Dates column
for( j in 2:13){
    
    for( i in 1:13) {

        y <- j + ((i-1)*11)
        print(y)
    }
}

## Example col_numbers for one specific variable (here num_tweets)
## 2
## 13
## 24
## 35
## 46
## 57
## 68
## 79
## 90
## 101
## 112
## 123
## 134

## Example function to apply different functions to each column in data.table
## Source: http://stats.stackexchange.com/questions/28576/filling-nas-in-a-dataset-with-column-medians-in-r
f=function(x){
   x<-as.numeric(as.character(x)) #first convert each column into numeric if it is from factor
   x[is.na(x)] =median(x, na.rm=TRUE) #convert the item with NA to median value from the column
   x #display the column
}
ss=data.frame(apply(df,2,f))


## ----------------------------------------------------------------

## =============================== ##
##  Test functions for imputation  ##
## =============================== ##

zoo.to.data.frame <- function(x, index.name="Date") {
  stopifnot(is.zoo(x))
  xn <- if(is.null(dim(x))) deparse(substitute(x)) else colnames(x)
  setNames(data.frame(index(x), x, row.names=NULL), c(index.name,xn))
}

out1 <- zoo.to.data.frame(out1)
out1 <- as.data.table(out1)


## After
dim(e)
length(which(is.na(e)))
which(is.na(out1))

## Where are the NA values?

na.test <-  function (x) {
  w <- sapply(x, function(x) (is.na(x)))
 
return(w)}

na.test(data)


colnames <- names(data)

for (i in 1:length(data)) {
    
    
}

na_replacer <- function(dat) {
    N <- length(dat)
    na.pos <- which(is.na(dat))
    if (length(na.pos) %in% c(0, N)) {
        return(dat)
    }
    non.na.pos <- which(!is.na(dat))
    intervals  <- findInterval(na.pos, non.na.pos,
                               all.inside = TRUE)
    left.pos   <- non.na.pos[pmax(1, intervals)]
    right.pos  <- non.na.pos[pmin(N, intervals+1)]
    left.dist  <- na.pos - left.pos
    right.dist <- right.pos - na.pos

    dat[na.pos] <- ifelse(left.dist <= right.dist,
                          dat[left.pos], dat[right.pos])
    return(dat)
}
testy <- numcolwise(na_replacer)
test <- testy(data)


t <- c(1, 2, 3, 4, 5, NA, 2)
na_replacer(t)

data[ , .I[is.na(data$1)] ]

n <- names(data)

for ( i in 1:length(names)) {

    data[, n[i] := na_replacer(n[i])]
    
}

na_replacer(data$DJI)

for (i in which(sapply(data, is.numeric))) {
    data[is.na(data[, i]), i] <- mean(data[, i],  na.rm = TRUE)
}
