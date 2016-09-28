###################### =================================== ######################
######################  A worked example from the website  ######################
###################### =================================== ######################
## http://topepo.github.io/caret/training.html

library(mlbench)

set.seed(998)
data(Sonar)

str(Sonar[, 1:10])
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)

testing <- Sonar[ -inTraining,] 	training <- Sonar[ inTraining,]
inTraining <- createDataPartition(Sonar$Class, p = .75, list = FALSE)

set.seed(825)
fitControl <- trainControl(## 10-fold CV
                           method = "repeatedcv",
                           number = 10,
                           ## repeated ten times
                           repeats = 10)


gbmFit1 <- train(Class ~ ., data = training, method = "gbm", trControl = fitControl, verbose = FALSE)
gbmFit1


## ========================= ##
##  Alternative Tuning Grid  ##
## ========================= ##

gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                        n.trees = (1:30)*50,
                        shrinkage = 0.1,
                        n.minobsinnode = 20)

nrow(gbmGrid)

set.seed(825)
gbmFit2 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 ## Now specify the exact models 
                 ## to evaluate:
                 tuneGrid = gbmGrid)
gbmFit2


## Plot with caret theme, the metric can also be specified
trellis.par.set(caretTheme())
plot(gbmFit1)
plot(gbmFit2)


plot(gbmFit1, metric = "Kappa")
plot(gbmFit2, metric = "Kappa")

?plot.train

## Plot with ggplot
ggplot(gbmFit2) + theme_bw()

## Heatmap
plot(gbmFit2, metric = "Accuracy", plotType = "level",
     scales = list(x = list(rot = 90)))
plot(gbmFit2, metric = "Kappa", plotType = "level",
     scales = list(x = list(rot = 90)))


## ================================= ##
##  Alternate performance summaries  ##
## ================================= ##

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           ## Estimate class probabilities
                           classProbs = TRUE,
                           ## Evaluate performance using 
                           ## the following function
                           summaryFunction = twoClassSummary)

set.seed(825)
gbmFit3 <- train(Class ~ ., data = training,
                 method = "gbm",
                 trControl = fitControl,
                 verbose = FALSE,
                 tuneGrid = gbmGrid,
                 ## Specify which metric to optimize
                 metric = "ROC")
gbmFit3

## Inspect results
ggplot(gbmFit3)



## ======================== ##
##  Compare several models  ##
## ======================== ##

## SVM model
set.seed(825)
svmFit <- train(Class ~ ., data = training,
                 method = "svmRadial",
                 trControl = fitControl,
                 preProc = c("center", "scale"),
                 tuneLength = 8,
                 metric = "ROC")
svmFit

## Regularizes Discriminant Analysis model
set.seed(825)
rdaFit <- train(Class ~ ., data = training,
                 method = "rda",
                 trControl = fitControl,
                 tuneLength = 4,
                 metric = "ROC")
rdaFit

## Compare them with resampling
resamps <- resamples(list(GBM = gbmFit3,
                          SVM = svmFit,
                          RDA = rdaFit))
resamps

summary(resamps)

## Quite helpful, perhaps
trellis.par.set(theme1)
bwplot(resamps, layout = c(3, 1))
dotplot(resamps, metric = "ROC")
xyplot(resamps, what = "BlandAltman")
splom(resamps)
densityplot(resamps, metric = "ROC",auto.key = list(columns = 3), pch = "|")

## Not very helpful
parallelplot(resamps, metric = "ROC")
parallelplot(resamps, metric = "Sens")
parallelplot(resamps, metric = "Spec")
