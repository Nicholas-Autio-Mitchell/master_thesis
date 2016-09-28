The data in this folder should now be used statically.

Included is:

- the output of the sentiment analysis: "sentiment_data.rda"
- market data for six indices and an ETF representing emerging markets
- macroeconomic data: fixed income, commodities, FX and market indicators

Sentiment Analysis data is imported and sorted in: "sentiment_importer.R"

The market and macro data in collected in the file: "get_market_data.R"

This data is then combined with sentiment analysis data in: "data_merger.R"

Although cleaning of data has been performed along the way, the final 
merged data table is further enhanced through imputation, removing any 
clear sources of multicollinearity and the addition of lagged predictors 
in the file: "data_enhancer.R"

After this file, the data is ready to be used for boosting.
