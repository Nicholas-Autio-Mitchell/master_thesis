###################### ============================================ ######################
######################  Look at Google Trend data for correlations  ######################
###################### ============================================ ######################

#' Here we simply look at our search terms using a different social media platform - Google.
#' The idea is to simply show that there are further dimensions that could potentially be
#' used when attempting to assess public opinion or quantify public interest in a topic.

## --------------------------------------------- ##
##  This data was taken from 'data_inspector.R'  ##
## --------------------------------------------- ##
## ======================================================== ##
##  Another perspective on correlation using Google Trends  ##
## ======================================================== ##

#' These simple plots help to visualise the amount of correlation that can be measured
#' within the realm of public interest, perhaps comparable to the frequency plots for tweets.
#' We see the relative number of Google searches that contained our terms or similar.
#' It helps to show in which time frames, which search terms appeared to be linked in the minds of the public.

## The search terms must be in groups of five or smaller
## The groups loosely follow a theme, but also are chosen to return relatively similar scaling
library(RColorBrewer)
library(gtrendsR)
user <- "n1k31t4"                              #google account username
password <- "Sh1nyH4ppyPeeh0le5"                                       #the password
## Connect to the Google API
gconnect(user, password)

## First a group closely related to stock markets ('hard' traditional factors)
trends1 <- gtrends(c("dow jones", "oil prices", "interest rates", "USD EUR", "gold price"),
                   res = "week", start_date = as.Date("2013-01-14"), end_date = as.Date("2015-09-11"))

## Second a group with more general terms (macro economic)
trends2 <- gtrends(c("dow jones", "federal reserve", "financial crisis", "stock prices", "obama economy"),
                   res = "week", start_date = as.Date("2013-01-14"), end_date = as.Date("2015-09-11"))

## Thirdly the terms that are market specific but not traditional factors ('soft' traditional factors)
trends3 <- gtrends(c("dow jones", "bull market", "bear market", "market volatility", "goldman sachs"),
                   res = "week", start_date = as.Date("2013-01-14"), end_date = as.Date("2015-09-11"))
## Compares to the plot using tweet counts versus the dow in Empirical Studies chapter
trends4 <- gtrends(c("dow jones", "bear market", "oil prices"),
                   res = "week", start_date = as.Date("2013-01-14"), end_date = as.Date("2015-09-11"))
g_plot <- my.plot.gtrends(trends4)
g_plot
g_data <- trends1$trend$dow.jones

## Get dow data
dow <- getSymbols("^DJI")
## date range
dow1 <- DJI["20130114/20150911"]
dow2 <- as.data.table(dow1) %>% .[,  c(1,7), with = FALSE]

## Scale dow from 0 to 100
rescale <- function(x) (x-min(x))/(max(x) - min(x)) * 100
dow2$DJI.Adjusted <- rescale(dow2$DJI.Adjusted)

dow3 <- dow2[ceiling(seq(1, 671, length.out = 139))]

my_data <- trends4$trend

data1 <- as.data.table(cbind(my_data, dow3$DJI.Adjusted))
names(data1) <- c("start", "end", "Dow Jones (search term)", "bear market", "oil prices", "DJIA (value)")

data1$start <- NULL

melted_data <- as.data.table(melt(data1, id.vars = "end"))
setnames(melted_data, old = "variable", new = "Legend")

my_trend <- ggplot(data = melted_data, aes(x = end, y = value, colour = Legend)) +
    geom_line() +
    theme_bw(base_size = 24) +
    theme(legend.position = "bottom") +
    labs(x = "\nDate\n", y = "Relative value\n")

my_trend


## Define custom version of plotting function
my.plot.gtrends <- function(x,
                     type = c("trend", "regions", "topmetros", "cities"),
                     region = "world",
                     resolution = c("countries", "provinces", "metros"),
                     displaymode = c("auto", "regions", "markers"),
                     ind = 1L,
                     main = "",
                     ...) {
    type <- match.arg(type)
    
    resolution <- match.arg(resolution)
    
    gvisopt <- list(region = region,
                    displayMode = "markers",
                    resolution = resolution)
    
    if (type == "trend") {
        
        df <- x$trend
        
        df <- reshape(df,
                      varying = names(df)[mapply(is.numeric, x$trend)],
                      v.names = "hit",
                      idvar = names(df)[!mapply(is.numeric, x$trend)],
                      direction = "long",
                      times = names(df)[mapply(is.numeric, x$trend)],
                      timevar = "keyword")


        ##Create a custom color scale
        myColors <- brewer.pal(5,"Set1")
        names(myColors) <- unique(df$keyword)
        colScale <- scale_colour_manual(name = "Search terms:\n", values = myColors)
        
        df$start <- as.POSIXct(df$start)
        df$hit <- log10(df$hit)

        p <- ggplot(df, aes_string(x = "start", y = "hit", color = "keyword")) +
            geom_line() +
            xlab("\nDate") +
            ylab("Relative number of searches\n") +
            ggtitle(main) +
            theme_bw(base_size = 24) +
            colScale
            ## scale_fill_discrete(labels = c())

        return(p)
        print(p)
        
    } else if (type == "regions") {
        
        x <- x[["regions"]][[ind]]
        
        if(all(is.na(x))) stop("Not enough search volume to show results.", 
                               call. = FALSE)
        
        df <- data.frame(loc = x[, 1], hits = x[, 2])
        
        plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
        
    } else if (type == "topmetros") {
        
        x <- x[["topmetros"]][[ind]]
        
        if(all(is.na(x))) stop("Not enough search volume to show results.", 
                               call. = FALSE)
        
        df <- data.frame(loc = x[, 1], hits = x[, 2])
        
        plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
        
    } else if (type == "cities") {
        
        x <- x[["cities"]][[ind]]
        
        if(all(is.na(x))) stop("Not enough search volume to show results.", 
                               call. = FALSE)
        
        df <- data.frame(loc = x[, 1], hits = x[, 2])
        
        plot(gvisGeoChart(df, 'loc', 'hits', options = gvisopt))
    }
    
    invisible(NULL)
}

## Display the trends
my_plot1 <- my.plot.gtrends(trends1, main = "Dow Jones search frequency vs. traditional market factors")

my_plot2 <- my.plot.gtrends(trends2, main = "Dow Jones search frequency vs. macro-economic factors")

my_plot3 <- my.plot.gtrends(trends3, main = "Dow Jones search frequency vs. related market terms")

## ============================================================ ##
##  First steps in adding the Dow returns to the same chart...  ##
## ============================================================ ##

## ## Plot the second plot and put axis scale on right
## plot(data_dirty$DJI, pch=15,  xlab="", ylab="", ylim=c(13500,18320), 
##     axes=FALSE, type="l", lwd = 2, col="red")
## ## a little farther out (line=4) to make room for labels
## mtext("Cell Density",side=4,col="red",line=4) 
## axis(4, ylim=c(0,7000), col="red",col.axis="red",las=1)

## ------------------------------------------ ##
##  Plot several gTrends results on one grid  ##
## ------------------------------------------ ##

## Using the function defined below
multiplot(my_plot1, my_plot2, my_plot3, cols = 1)

## Multplot function from R cookbook
## http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_%28ggplot2%29/
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)

  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)

  numPlots = length(plots)

  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                    ncol = cols, nrow = ceiling(numPlots/cols))
  }

 if (numPlots==1) {
    print(plots[[1]])

  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))

    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))

      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
