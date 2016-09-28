### A script to pull quote data from yahoo

## Initialise the desired parameters and pull the data ####

# Create a vector with the symbols (tickers) of all assets that data is wanted for
# index_tickers_18082015.RData contains ticker lists for four indices
tickers <- c("^DJI")

# Define start and end dates for the time series that is to be created, format: YYYY-MM-DD
start_date <- "2013-01-01"
end_date <- "2015-08-01"

# Specify which data is to be collected for each time interval/point in the series and become the table headers
table_headers <- c("Date", "Open", "High", "Low", "Close", "Volume", "AdjClose")

# Choose a source for the data (either 'yahoo' and/or 'oanda')
stock_source <- "yahoo"

# Specify the frequency of the data to be extracted (daily, weekly or monthly --> "d" "w" "m")
data_frequency <- "d"

# Provide the format that the results should take --> "zoo" returns ordered observations in a vector or matrix, here ordered by date for us
data_format <- "zoo"

# Extract the data from Yahoo
dow_jones <- get.hist.quote(instrument = tickers, start = start_date, end = end_date, quote = table_headers, provider = stock_source, compression = data_frequency, retclass = data_format)