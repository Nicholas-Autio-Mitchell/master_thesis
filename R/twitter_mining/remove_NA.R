l <- length(dates)

for (i in 1:length(dates)) {
   if (is.na(dates[[i]]))
      dates[[i]] <- NULL
}
dates_l <- lapply(dates, if(i in 1:))