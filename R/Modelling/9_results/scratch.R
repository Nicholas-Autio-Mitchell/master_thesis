
## The best performing combined data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff80_frame60/results/glm_results.Lag.3.combined.rda")
p <- copy(glm_results.Lag.3.combined)
## The best performing traditional data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff70_frame60/results/glm_results.Lag.1.trad_large.rda")
q <- copy(glm_results.Lag.1.trad_large)
## The traditional data set that had the sam parameters as the best performing combined data set
load("/Volumes/Mac OS Drive/Thesis/Source Code/R/Modelling/9_results/glm_boost/B_cutoff80_frame60/results/glm_results.Lag.1.trad_large.rda")
r <- copy(glm_results.Lag.1.trad_large)

## Extract the predictions for each day into a matrix
p1 <- matrix(NA, length(p$results), 4)

for(i in 1:nrow(p1)) {

    p1[i, 1] <- as.numeric(p[[2]][[i]][[2]])       #actual market return
    p1[i, 2] <- as.numeric(p[[2]][[i]][[3]])       #our prediction - combine
    p1[i, 3] <- as.numeric(q[[2]][[i]][[3]])       #our prediction - sent_large
    p1[i, 4] <- as.numeric(r[[2]][[i]][[3]])       #our prediction - sent_large

}

price_paths <- matrix(NA, nrow(p1), 4)

## Initialise the values
price_paths[1, 1] <- 100 * (1 + p1[1, 1])       #using nominal value of 100
price_paths[1, 2] <- 100 * (1 + p1[1, 2])
price_paths[1, 3] <- 100 * (1 + p1[1, 3])
price_paths[1, 4] <- 100 * (1 + p1[1, 4])

for(i in 2:nrow(p1)) {

    price_paths[i, 1] <- price_paths[i-1, 1] * (1 + p1[i, 1])
    price_paths[i, 2] <- price_paths[i-1, 2] * (1 + p1[i, 2])
    price_paths[i, 3] <- price_paths[i-1, 3] * (1 + p1[i, 3])
    price_paths[i, 4] <- price_paths[i-1, 4] * (1 + p1[i, 4])    
}


library(chron)
days = seq(as.Date("2013-01-14"), as.Date("2015-09-11"), by="day")
my_days = days[!is.weekend(days)]
my_days <- my_days[63:695]



p2 <- as.data.table(cbind(my_days, price_paths))
p2$Date <- as.Date(p2$my_days)
p2$my_days <- NULL
setcolorder(p2, c(5, 1, 2, 3, 4))
setnames(p2, names(p2), new = c("Date", "DJIA", "Combined", "Traditional (best)", "Traditional (same)"))

p3 <- melt(p2, id.vars = c("Date"))
setnames(p3, old = c("value", "variable"), new = c("Nominal value", "Price path"))


paths <- ggplot(data = p3, aes(x = Date, y = `Nominal value`, colour = `Price path`)) +
    geom_line() +
    labs(x = "\nDate") +
    theme_bw() +
    theme(legend.position = c(0.09, 0.85))

paths
