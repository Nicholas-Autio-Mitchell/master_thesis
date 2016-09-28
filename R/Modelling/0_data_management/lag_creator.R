###################### ================================================ ######################
######################  Create the lagged versions of all base subsets  ######################
###################### ================================================ ######################

"""
Here we shift the DOW and the dummy date variables by one so that each row represents a regression equation
in terms of time. These can then be used with the timeSlices created later.

Afterwards we create the lagged versions of each data subset.

Extra care must be taken not to violate any information flow, that is we must not use information from
tomorrow when predicting tomorrow. Only the dates columns like 'is.monday' may be shifted along with
the DOW returns to be predicted, as they are deterministic and so known in advance.

The variables that can be kept in sync with DOW are 'day.number', 'is.monday' and 'is.holiday'

We must also add the DOW as a variable itself back into the subsets after it has been shifted.
This shall be done at the point when the names are adjusted for all variables to reflect their lag value.
Otherwise there may be confusion with the naming.
The same thing applies to the deterministic dummy variables, i.e. the date relates predictors.
We shall keep a copy of them back, unshifted, to reapply to the data with the lag1 tag.

**NOTE**
The 'DOW' column appears twice (identically named!) in the base_subsets$combined data set, [col 2] and [col 112]. Neither is shifted!!

It is important to note that the 'Date' column in all subsets corresponds to the lag1 data.
The DOW and dummy variables are actually one day ahead, depite what the 'Date' column suggests
Example:

      Date          DOW day.number is.monday is.holiday       DAX.L1
2013-01-14  0.002038986          2         0          0  0.001811615 --> this is the DAX log return for the Jan 14th

^^Columns 2-5 are actually showing the value for Jan 15th!
This is set up so intentionally so modelling is possible on one row of the data.

"""

## ======================= ##
##  Load the base subsets  ##
## ======================= ##

load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/base_subsets.rda")

## Take a copy of the DOW variables, as we will require them late on, unshifted
DOW_unshifted <- base_subsets$trad_small[, .(DOW)] #from any subsets, they're all the same
dummy_unshifted <- base_subsets$combined[, .(day.number, is.monday, is.holiday)]
## Change the day.number to numeric - wh the hell is it a string anyway??
dummy_unshifted[, day.number := as.numeric(day.number)]

## ================================================ ##
##  Perform time shifts for DOW and date variables  ##
## ================================================ ##

## Here we simply shift our dependent variable (DOW) by one, so we are left with a regression eqn setup:
## y(t) ~ y(t-1) + y(t-2) + ...  (simplified)
## Say, t = "today". We predict today based on data from yesterday and further into the past

## We know that that DOW column is always the second column (Date being the first)

## Get Dow returns for 2015-09-14 in order to maintain a variable 695 periods long
dow <- getSymbols("^DJI", auto.assign = FALSE)
dow1 <- dow["20150911/20150914"][, 6] #extract adjusted prices for the two required days
dow_day <- as.data.table(diff(log(dow1)))[2] #compute and extract the log return
names(dow_day) <- c("Date", "DOW.DP")           #rename DP = Dependen Variable

new_row <- copy(dow_day)

## Create a row to be added including dow_day and other shifted volumns (dummy var. columns)
## these will be added to the ends of the relevant columns, the first element will be removed and then
## these new columns will replace the old ones. Essentially shifting the columns 'up' one period and filling in
## the ensuing NAs with new data.

## Expand dow_day created above --> the order is important!!
## Must match the order the columns appear in the base subsets
new_row$day.number <- 1                 #monday is day number one on our scale
new_row$is.monday <- 1                  #it is a monday (otherwise would = 0)
new_row$is.holiday <- 0                 #it is not a bank holiday

## ---------------------------------------------------- ##
##  Create a version of the dummy vars to match DOW.DP  ##
## ---------------------------------------------------- ##


## --------------------------------------------------------------------- ##
##  Create list to hold new subsets with shifts DOW and dummy variables  ##
## --------------------------------------------------------------------- ##

base_subsets_shifted <- sapply(names(base_subsets), function(x) NULL)

## ----------------------------------------------------------- ##
##  Shift the trad_small subset - only required new DOW value  ##
## ----------------------------------------------------------- ##

## The traditional subset shall not contain the lags of the dependent variables (DOW) itself

## Take a copy of the trad_small subset
x <- base_subsets$trad_small
## Give new name to the shifted columns,  "DOW Dependent Variable"
setnames(x, "DOW", "DOW.DP")            #Must be performed here, rbind() required identical col names
## Create new DOW column to replace old one
to_shift <- x[, .(Date, DOW.DP)]
shifted <- rbind(to_shift, dow_day) %>% #attach new day 
    .[2:nrow(.)]                        #remove first element
## Inspecting the Date column in 'shifted' shows that we have lost our first day 2013-01-14
## We have gained another day. This DOW column can now replace the old one

## Replace old DOW
x$DOW.DP <- shifted$DOW.DP

## Add to final list, ready to create lags
base_subsets_shifted$trad_small <- x

## ----------------------------------------------------------------------- ##
##  Create function to shifts all remaining subsets - all require new_row  ##
## ----------------------------------------------------------------------- ##

## This function inserts the four shifted columns DOW plus dummy vars into the 2nd - 5th columns, WITHOUT removing original cols

## Test subset
## input_subset <- copy(base_subsets$trad_large)

shifter <- function(input_subset, new_data = new_row) {

    to_shift <- input_subset[, .(Date, DOW, day.number, is.monday, is.holiday)]
    setnames(to_shift, names(to_shift), c("Date.DP", "DOW.DP", "day.number.L0", "is.monday.L0", "is.holiday.L0")) #change name to reflect the shift
    names(new_data) <- names(to_shift)
    shifted <- rbind(to_shift, new_data) %>%
        .[2:nrow(.)]

    print(shifted)                      #have a look - does it look ok??

    ## Attach to original subset and reorder the cols
    output_subset <- cbind(shifted[, .(DOW.DP, day.number.L0, is.monday.L0, is.holiday.L0)], input_subset)
    setcolorder(output_subset, c(5, 1:4, 6:ncol(output_subset)))

    ## Convert day.number into numeric from char - for both .L0 and non.shifted
    output_subset[, day.number := as.numeric(day.number)]
    output_subset[, day.number.L0 := as.numeric(day.number.L0)]
    
    ## Look at the first seven columns as sanity check
    print(output_subset[, c(1:7), with = FALSE])

    return(output_subset)
    
}

## Test the above function
input_subset <- base_subsets$sent_small       #use any base subset
shifted_subset <- shifter(input_subset, new_data = new_row)

## --------------------------------------------- ##
##  Apply function to the relevant base subsets  ##
## --------------------------------------------- ##

base_subsets_shifted$trad_large <- shifter(base_subsets$trad_large)
base_subsets_shifted$sent_small <- shifter(base_subsets$sent_small)
base_subsets_shifted$sent_large <- shifter(base_subsets$sent_large)
base_subsets_shifted$combined <- shifter(base_subsets$combined)

## Save the shifted subsets
save(base_subsets_shifted, file = "base_subsets_shifted.rda")


###################### ============================= ######################
######################  Create the lagged variables  ######################
###################### ============================= ######################

## Clear workspace, leaving required objects from above
rm(list=setdiff(ls(), c("DOW_unshifted", "dummy_unshifted", "DOW_dummy.L0"))) #need to keep this to add it as a lagged variable with name DOW.L1
## Give the name'DOW' to the unshifted DOW column

## Load the shifted base subsets into a clean workspace
load("/Volumes/Mac\ OS\ Drive/Thesis/Source\ Code/R/Modelling/raw_data/base_subsets_shifted.rda")

## ----------------------------------------- ##
##  Create some usefúl subsets to use later  ##
## ----------------------------------------- ##

main_dates <- base_subsets_shifted$combined[, .(Date)] #Just the entire date range
DOW.DP <- base_subsets_shifted$combined[, .(DOW.DP)]   #the shifted DOW, entire date range
## This is what we prepend the four larger data sets with later once they are lagged
DOW_dummy.L0 <- base_subsets_shifted$trad_large[, c(1:5), with = FALSE]

## ============= ##
##  Information  ##
## ============= ##
"""
The first data set is actually the 'lag1' set, so we can simply rename the variables to reflect that.
subsequent lags are appended using the shift() function within data tables.

We take the subset of columns that are already lag1, rename them, then use them to create the further lags
up to 5, after which we reattach the Date, DOW.DP and dummy variable(.L0) columns as necessary.

**NOTE** --> this has been fixed now!!!
The 'DOW' column appears twice (identically named!) in the base_subsets_shifted$combined data set. One is shifted [col 2], one is not [col 112]

Again it will likely be easiest to create the lags for the 'trad_small' subset separately from the other subsets
because it isn't easily manipulated in a generic way, not containing the dummy variables.

The values of the columns with names ending .L1, are lagged once w.r.t. DOW.DP (which has been shifted backwards one time period)
however the value of the .L1 columns are the correct ones for the corresponding value in the Date column!
"""

## =================================================================== ##
##  Extract the data and variable names to create the lagged versions  ##
## =================================================================== ##

## Take a copy of the names without "Date" and "DOW" for all subsets,
## Additionally remove the dummy variables columns for the four larger subsets.
names_ts <- names(base_subsets_shifted[[1]])[3:ncol(base_subsets_shifted[[1]])] #trad_small
names_tl <- names(base_subsets_shifted[[2]])[6:ncol(base_subsets_shifted[[2]])] #trad_large
names_ss <- names(base_subsets_shifted[[3]])[6:ncol(base_subsets_shifted[[3]])] #sent_small
names_sl <- names(base_subsets_shifted[[4]])[6:ncol(base_subsets_shifted[[4]])] #sent_large
names_c <- names(base_subsets_shifted[[5]])[6:ncol(base_subsets_shifted[[5]])]  #combined

## Create subsets containing only the data to be lagged (these are actually already lag1)
sub_ts <- copy(base_subsets_shifted$trad_small[, names_ts, with = FALSE]) #with=FALSE required as the names are quoted char strings
sub_tl <- copy(base_subsets_shifted$trad_large[, names_tl, with = FALSE])
sub_ss <- copy(base_subsets_shifted$sent_small[, names_ss, with = FALSE])
sub_sl <- copy(base_subsets_shifted$sent_large[, names_sl, with = FALSE])
sub_c <- copy(base_subsets_shifted$combined[, names_c, with = FALSE])

## Create the names that are use to create the lags
new_names_ts <- names(sub_ts)
new_names_tl <- names(sub_tl)
new_names_ss <- names(sub_ss)
new_names_sl <- names(sub_sl)
new_names_c <- names(sub_c)


## -------------------------------------------- ##
##  Create an object to hold all laged subsets  ##
## -------------------------------------------- ##

subsets_lagged <- sapply(c("Lag.1", "Lag.2", "Lag.3", "Lag.4", "Lag.5"), function(x) NULL)
## Create a list for each lag containing all subsets at that lag level
the_models <- sapply(names(base_subsets_shifted), function(x) NULL)
## Apply the names to the main object
for(i in 1:5){subsets_lagged[[i]] <- the_models}

## ---------------------------- ##
##  Create lags for trad_small  ##
## ---------------------------- ##

##issue/feature request on data table package:
## https://github.com/Rdatatable/data.table/issues/1539

## Create the lag5 subset, from which we can subtract all other lagged subsets
x <- sub_ts[,
               shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE),
               #setnames(setDT(x), sub("_lag_", ".L", names(x)))},
    .SDcols = new_names_ts]
## ## as we shift (i.e. lag) ALL cols, we don't need to use the .SDcols argument. Just leave it in for now.
## y <- copy(sub_ts[, shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE)])
## identical(x, y)

names(x) <- gsub("_lag_", ".L", names(x)) #Change the appended text "_lag_"
names(x) <- gsub(".L4", ".L5", names(x))
names(x) <- gsub(".L3", ".L4", names(x))
names(x) <- gsub(".L2", ".L3", names(x))
names(x) <- gsub(".L1", ".L2", names(x))
names(x) <- gsub(".L0", ".L1", names(x))
## Save this to then create the smaller lagged subsets from it
ts_L5 <- x

## Have a look at the lags -> the SP500 column is now renamed to SP500.L1, as it is one day behind the (shifted) dow: DOW.DP
tester <- cbind(base_subsets_shifted$trad_small[, c(1, 2, 3), with = FALSE], x[, c(1:8), with =FALSE])

## Create the first four lag data tables and remove the rows with NAs resulting from lagging
ts_L1 <- ts_L5[, grep(".L1$", names(x), value = TRUE), with=FALSE]             #with=FALSE required as the names are quoted char strings
ts_L2 <- ts_L5[, grep(".L(1|2)$", names(x), value = TRUE), with=FALSE][2:695]
ts_L3 <- ts_L5[, grep(".L(1|2|3)$", names(x), value = TRUE), with=FALSE][3:695]
ts_L4 <- ts_L5[, grep(".L(1|2|3|4)$", names(x), value = TRUE), with=FALSE][4:695]
ts_L5 <- copy(ts_L5)[5:695]

## We now must reattach the Date and DOW.DP columns and assign the result to our main object
subsets_lagged$Lag.1$trad_small <- cbind(main_dates[1:695], DOW.DP[1:695], ts_L1)
subsets_lagged$Lag.2$trad_small <- cbind(main_dates[2:695], DOW.DP[2:695], ts_L2)
subsets_lagged$Lag.3$trad_small <- cbind(main_dates[3:695], DOW.DP[3:695], ts_L3)
subsets_lagged$Lag.4$trad_small <- cbind(main_dates[4:695], DOW.DP[4:695], ts_L4)
subsets_lagged$Lag.5$trad_small <- cbind(main_dates[5:695], DOW.DP[5:695], ts_L5)

## Check everything is fine
str(subsets_lagged$Lag.1$trad_small)
str(subsets_lagged$Lag.2$trad_small)
str(subsets_lagged$Lag.3$trad_small)
str(subsets_lagged$Lag.4$trad_small)
str(subsets_lagged$Lag.5$trad_small)

## ---------------------------- ##
##  Create lags for trad_large  ##
## ---------------------------- ##

## Create the lag5 subset, from which we can subtract all other lagged subsets
x1 <- sub_tl[,
               shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE),
               #setnames(setDT(x1), sub("_lag_", ".L", names(x1)))},
    .SDcols = new_names_tl]
## ## as we shift (i.e. lag) ALL cols, we don't need to use the .SDcols argument. Just leave it in for now.
## y <- copy(sub_tl[, shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE)])
## identical(x1, y)

names(x1) <- gsub("_lag_", ".L", names(x1)) #Change the appended text "_lag_"
names(x1) <- gsub(".L4", ".L5", names(x1))
names(x1) <- gsub(".L3", ".L4", names(x1))
names(x1) <- gsub(".L2", ".L3", names(x1))
names(x1) <- gsub(".L1", ".L2", names(x1))
names(x1) <- gsub(".L0", ".L1", names(x1))
## Save this to then create the smaller lagged subsets from it
tl_L5 <- x1

## Have a look at the lags -> the SP500 column is now renamed to SP500.L1, as it is one day behind the (shifted) dow: DOW.DP
tester <- cbind(base_subsets_shifted$trad_large[, c(1:5), with = FALSE], x1[, c(1:8), with =FALSE])

## Create the first four lag data tables and remove the rows with NAs resulting from lagging
tl_L1 <- tl_L5[, grep(".L1$", names(x1), value = TRUE), with=FALSE]             #with=FALSE required as the names are quoted char strings
tl_L2 <- tl_L5[, grep(".L(1|2)$", names(x1), value = TRUE), with=FALSE][2:695]
tl_L3 <- tl_L5[, grep(".L(1|2|3)$", names(x1), value = TRUE), with=FALSE][3:695]
tl_L4 <- tl_L5[, grep(".L(1|2|3|4)$", names(x1), value = TRUE), with=FALSE][4:695]
tl_L5 <- copy(tl_L5)[5:695]

## We now must reattach the Date and DOW.DP columns and assign the result to our main object
## Extra to the trad_small model, we also include the shifted_verisons of the dummy variables
subsets_lagged$Lag.1$trad_large <- cbind(DOW_dummy.L0[1:695], tl_L1)
subsets_lagged$Lag.2$trad_large <- cbind(DOW_dummy.L0[2:695], tl_L2)
subsets_lagged$Lag.3$trad_large <- cbind(DOW_dummy.L0[3:695], tl_L3)
subsets_lagged$Lag.4$trad_large <- cbind(DOW_dummy.L0[4:695], tl_L4)
subsets_lagged$Lag.5$trad_large <- cbind(DOW_dummy.L0[5:695], tl_L5)

## Check everything is fine
str(subsets_lagged$Lag.1$trad_large)
str(subsets_lagged$Lag.2$trad_large)
str(subsets_lagged$Lag.3$trad_large)
str(subsets_lagged$Lag.4$trad_large)
str(subsets_lagged$Lag.5$trad_large)

## ---------------------------- ##
##  Create lags for sent_small  ##
## ---------------------------- ##

## Create the lag5 subset, from which we can subtract all other lagged subsets
x2 <- sub_ss[,
               shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE),
               #setnames(setDT(x2), sub("_lag_", ".L", names(x2)))},
    .SDcols = new_names_ss]
## ## as we shift (i.e. lag) ALL cols, we don't need to use the .SDcols argument. Just leave it in for now.
## y <- copy(sub_ss[, shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE)])
## identical(x2, y)

names(x2) <- gsub("_lag_", ".L", names(x2)) #Change the appended text "_lag_"
names(x2) <- gsub(".L4", ".L5", names(x2))
names(x2) <- gsub(".L3", ".L4", names(x2))
names(x2) <- gsub(".L2", ".L3", names(x2))
names(x2) <- gsub(".L1", ".L2", names(x2))
names(x2) <- gsub(".L0", ".L1", names(x2))
## Save this to then create the smaller lagged subsets from it
ss_L5 <- x2

## Have a look at the lags -> the SP500 column is now renamed to SP500.L1, as it is one day behind the (shifted) dow: DOW.DP
tester <- cbind(base_subsets_shifted$sent_small[, c(1:5), with = FALSE], x2[, c(1:8), with =FALSE])

## Create the first four lag data tables and remove the rows with NAs resulting from lagging
ss_L1 <- ss_L5[, grep(".L1$", names(x2), value = TRUE), with=FALSE]             #with=FALSE required as the names are quoted char strings
ss_L2 <- ss_L5[, grep(".L(1|2)$", names(x2), value = TRUE), with=FALSE][2:695]
ss_L3 <- ss_L5[, grep(".L(1|2|3)$", names(x2), value = TRUE), with=FALSE][3:695]
ss_L4 <- ss_L5[, grep(".L(1|2|3|4)$", names(x2), value = TRUE), with=FALSE][4:695]
ss_L5 <- copy(ss_L5)[5:695]

## We now must reattach the Date and DOW.DP columns and assign the result to our main object
## Extra to the trad_small model, we also include the shifted_verisons of the dummy variables
subsets_lagged$Lag.1$sent_small <- cbind(DOW_dummy.L0[1:695], ss_L1)
subsets_lagged$Lag.2$sent_small <- cbind(DOW_dummy.L0[2:695], ss_L2)
subsets_lagged$Lag.3$sent_small <- cbind(DOW_dummy.L0[3:695], ss_L3)
subsets_lagged$Lag.4$sent_small <- cbind(DOW_dummy.L0[4:695], ss_L4)
subsets_lagged$Lag.5$sent_small <- cbind(DOW_dummy.L0[5:695], ss_L5)

## Check everything is fine
str(subsets_lagged$Lag.1$sent_small)
str(subsets_lagged$Lag.2$sent_small)
str(subsets_lagged$Lag.3$sent_small)
str(subsets_lagged$Lag.4$sent_small)
str(subsets_lagged$Lag.5$sent_small)

## ---------------------------- ##
##  Create lags for sent_large  ##
## ---------------------------- ##

## Create the lag5 subset, from which we can subtract all other lagged subsets
x3 <- sub_sl[,
               shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE),
               #setnames(setDT(x3), sub("_lag_", ".L", names(x3)))},
    .SDcols = new_names_sl]
## ## as we shift (i.e. lag) ALL cols, we don't need to use the .SDcols argument. Just leave it in for now.
## y <- copy(sub_sl[, shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE)])
## identical(x3, y)

names(x3) <- gsub("_lag_", ".L", names(x3)) #Change the appended text "_lag_"
names(x3) <- gsub(".L4", ".L5", names(x3))
names(x3) <- gsub(".L3", ".L4", names(x3))
names(x3) <- gsub(".L2", ".L3", names(x3))
names(x3) <- gsub(".L1", ".L2", names(x3))
names(x3) <- gsub(".L0", ".L1", names(x3))
## Save this to then create the smaller lagged subsets from it
sl_L5 <- x3

## Have a look at the lags -> the SP500 column is now renamed to SP500.L1, as it is one day behind the (shifted) dow: DOW.DP
tester <- cbind(base_subsets_shifted$sent_large[, c(1:5), with = FALSE], x3[, c(1:8), with =FALSE])

## Create the first four lag data tables and remove the rows with NAs resulting from lagging
sl_L1 <- sl_L5[, grep(".L1$", names(x3), value = TRUE), with=FALSE]             #with=FALSE required as the names are quoted char strings
sl_L2 <- sl_L5[, grep(".L(1|2)$", names(x3), value = TRUE), with=FALSE][2:695]
sl_L3 <- sl_L5[, grep(".L(1|2|3)$", names(x3), value = TRUE), with=FALSE][3:695]
sl_L4 <- sl_L5[, grep(".L(1|2|3|4)$", names(x3), value = TRUE), with=FALSE][4:695]
sl_L5 <- copy(sl_L5)[5:695]

## We now must reattach the Date and DOW.DP columns and asssign the result to our main object
## Extra to the trad_small model, we also include the shifted_verisons of the dummy variables
subsets_lagged$Lag.1$sent_large <- cbind(DOW_dummy.L0[1:695], sl_L1)
subsets_lagged$Lag.2$sent_large <- cbind(DOW_dummy.L0[2:695], sl_L2)
subsets_lagged$Lag.3$sent_large <- cbind(DOW_dummy.L0[3:695], sl_L3)
subsets_lagged$Lag.4$sent_large <- cbind(DOW_dummy.L0[4:695], sl_L4)
subsets_lagged$Lag.5$sent_large <- cbind(DOW_dummy.L0[5:695], sl_L5)

## Check everything is fine
str(subsets_lagged$Lag.1$sent_large)
str(subsets_lagged$Lag.2$sent_large)
str(subsets_lagged$Lag.3$sent_large)
str(subsets_lagged$Lag.4$sent_large)
str(subsets_lagged$Lag.5$sent_large)

## -------------------------- ##
##  Create lags for combined  ##
## -------------------------- ##

## Create the lag5 subset, from which we can subtract all other lagged subsets
x4 <- sub_c[,
               shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE),
               #setnames(setDT(x4), sub("_lag_", ".L", names(x4)))},
    .SDcols = new_names_c]
## ## as we shift (i.e. lag) ALL cols, we don't need to use the .SDcols argument. Just leave it in for now.
## y <- copy(sub_c[, shift(.SD, n = 0:4, fill = NA, type = "lag", give.names = TRUE)])
## identical(x4, y)

names(x4) <- gsub("_lag_", ".L", names(x4)) #Change the appended text "_lag_"
names(x4) <- gsub(".L4", ".L5", names(x4))
names(x4) <- gsub(".L3", ".L4", names(x4))
names(x4) <- gsub(".L2", ".L3", names(x4))
names(x4) <- gsub(".L1", ".L2", names(x4))
names(x4) <- gsub(".L0", ".L1", names(x4))
## Save this to then create the smaller lagged subsets from it
c_L5 <- x4

## Have a look at the lags -> the SP500 column is now renamed to SP500.L1, as it is one day behind the (shifted) dow: DOW.DP
tester <- cbind(base_subsets_shifted$combined[, c(1:5), with = FALSE], x4[, c(1:8), with =FALSE])

## Create the first four lag data tables and remove the rows with NAs resulting from lagging
c_L1 <- c_L5[, grep(".L1$", names(x4), value = TRUE), with=FALSE]             #with=FALSE required as the names are quoted char strings
c_L2 <- c_L5[, grep(".L(1|2)$", names(x4), value = TRUE), with=FALSE][2:695]
c_L3 <- c_L5[, grep(".L(1|2|3)$", names(x4), value = TRUE), with=FALSE][3:695]
c_L4 <- c_L5[, grep(".L(1|2|3|4)$", names(x4), value = TRUE), with=FALSE][4:695]
c_L5 <- copy(c_L5)[5:695]

## We now must reattach the Date and DOW.DP columns and assign the result to our main object
## Extra to the trad_small model, we also include the shifted_verisons of the dummy variables
subsets_lagged$Lag.1$combined <- cbind(DOW_dummy.L0[1:695], c_L1)
subsets_lagged$Lag.2$combined <- cbind(DOW_dummy.L0[2:695], c_L2)
subsets_lagged$Lag.3$combined <- cbind(DOW_dummy.L0[3:695], c_L3)
subsets_lagged$Lag.4$combined <- cbind(DOW_dummy.L0[4:695], c_L4)
subsets_lagged$Lag.5$combined <- cbind(DOW_dummy.L0[5:695], c_L5)

## Check everything is fine
str(subsets_lagged$Lag.1$combined, list)
str(subsets_lagged$Lag.2$combined)
str(subsets_lagged$Lag.3$combined)
str(subsets_lagged$Lag.4$combined)
str(subsets_lagged$Lag.5$combined)

###################### =========================================== ######################
######################  Perform some simple check for consistency  ######################
###################### =========================================== ######################

## =================================== ##
##  Inspect structure of final object  ##
## =================================== ##

str(subsets_lagged, max.level = 2, give.attr = FALSE, list.len = 5)

## =============== ##
##  Check for NAs  ##
## =============== ##

for(i in 1:5) {                         #the lag
    
    for(j in 1:5) {                     #the subset
        
        print(sum(is.na(subsets_lagged[[i]][[j]])))
    }
}

## --> all zero

## ========================= ##
##  Save the lagged subsets  ##
## ========================= ##

## This saved verison contains all the Date columns, and so must still be edited before modelling can commence
save(subsets_lagged, file = "subsets_lagged_(with_dates).rda")


## =================================================================== ##
##  Remove Date columns so subsets are ready to be used for modelling  ##
## =================================================================== ##

for(i in 1:5) {                         #the lag
    
    for(j in 1:5) {                     #the subset
        
        subsets_lagged[[i]][[j]][, Date := NULL]
    }
}

## Save this versions of the subsets
save(subsets_lagged, file = "subsets_lagged.rda")

###################### ============ ######################
######################  Next Steps  ######################
###################### ============ ######################

"""
The last save file 'subsets_lagged.rda' can be loaded into the modelling files to be looped through

The file 'corr_pruner.R' may need to be amended to work with the new data sets.

We could also imprive it with use of nearZeroVar() from the caret package. would help remove dow_SPDR when it isn't useful
"""















###################### ============================ ######################
######################  Data table feature request  ######################
###################### ============================ ######################

## Create dummy data
y <- data.table(A = rnorm(1:10), B = rgamma(10, shape = 1))
## Create copy so we can compare
y1 <- copy(y)
## Create two new cols, lag=1 of original two cols
y1[, paste0(names(y), ".L1") := shift(.SD, n = 1, give.names = TRUE), .SDcols = names(y)]

## do the same but for n>1
y2 <- copy(y)
## The names I would like to end up with
c(paste0(names(y), ".L1"), paste0(names(y), ".L2")) #new names I want for the four new columns

## Eventually got it to work with this:
## Create he names fitting to what is returned... very verbose
new_names <- c(c(paste0(names(y)[1], c(".L1", ".L2")), c(paste0(names(y)[2], c(".L1", ".L2")))))
## The columns I am defining here on the LHS of := must be in that vector c(), otherwise things gets weird
y2 <- y2[, c(new_names) := shift(.SD, n = 1:2, give.names = TRUE), .SDcols = names(y)]
