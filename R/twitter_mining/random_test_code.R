# create a function named 'pause' to introduce a CPU-pause within a process or loop
pause <- function(x) {
p1 <- proc.time()
Sys.sleep(x)
# The cpu usage should be negligible
proc.time() - p1
}

# Test the pause function
pause(4.2)


# get dates from data table
dates <- as.vector(dow_list$Date)
# get the actual days that are in the data
day_values <- as.vector(lapply(dates, function(x) substr(x, start = 9, stop = 10)))

#define first and last day in date range
first_day <- day_values[length(day_values)]
last_day <- day_values[1]

## Find out how many tweets are present on each of the days
# initialise bins for days in data range
freq_days <- data.table(unique(day_values), 0)

for (i in 12:20){
freq_days[i-11,"V2"] <- length(grep(pattern = i, x = day_values[]))
}

head(freq_days[,Dow:=freq_days$r1])
