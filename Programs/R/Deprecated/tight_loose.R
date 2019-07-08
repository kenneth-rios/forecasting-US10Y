##### For tight block, take the final tightening regime from 2015-12-15 to 2019-01-04 as the test set.
#####
##### For loose block, take the first three years of the final loosening regime as the test set since it 
##### captures the Great Recession (??). The years are 2007-09-16 to 2010-09-16.
#####

rm(list=ls())

##### Import libraries #####
library(dplyr)
library(tibble)
library(randomForest)
library(xgboost)

##### Split macroeconomic data by Fed policy regimes #####

# Import regime data (change to your local directory with RDS files)
tight <- readRDS("C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\tight.rds")
loose <- readRDS("C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\loose.rds")

set.seed(4)

# Split data from tightening block into training and test sets
train_tight <- as.data.frame(tight["/2015-12-14"])
test_tight <- as.data.frame(tight["2015-12-15/"])
# tight <- rownames_to_column(as.data.frame(tight), 'date')
# train_tight <- column_to_rownames(sample_frac(as.data.frame(tight), .7), 'date')  # Use 70/30 or 50/50 split?
# test_tight <- column_to_rownames(anti_join(as.data.frame(tight), train_tight), 'date')

# Split data from loosening block into training and test sets
train_loose <- as.data.frame(loose[c("/2007-09-15", "2010-09-17/")])
test_loose <- as.data.frame(loose["2007-09-16/2010-09-16"])
# loose <- rownames_to_column(as.data.frame(loose), 'date')
# train_loose <- column_to_rownames(sample_frac(as.data.frame(loose), .7), 'date')  # Use 70/30 or 50/50 split?
# test_loose <- column_to_rownames(anti_join(as.data.frame(loose), train_loose), 'date')

# Calculate RMSE for market baselines
RMSE.baseline.tight <- (mean(test_tight$DGS10 - test_tight$THREEFF3)^2)^.5
RMSE.baseline.loose <- (mean(test_loose$DGS10 - test_loose$THREEFF3)^2)^.5



##### MACHINE LEARNING MODELS #####

baggedTrees <- function(training_data, testing_data){
  
  ### BAGGED TREES ###
  # Train the bagged model
  bag <- randomForest(DGS10 ~ ., data = subset(training_data, select = -c(THREEFF3)),
                      mtry = ncol(training_data)-2, importance = TRUE)
  
  importance(bag)
  
  # Testing the fitted model
  yhat.bag <- predict(bag, newdata = subset(testing_data, select = -c(THREEFF3)))
  
  # Calculate RMSE
  RMSE.bag <- (mean(testing_data$DGS10 - yhat.bag)^2)^.5
  
  
  return(list(bag, yhat.bag, RMSE.bag))
  
}



randomForests <- function(training_data, testing_data){
  
  ### RANDOM FOREST ###
  # Train the bagged model
  rf <- randomForest(DGS10 ~ ., data = subset(training_data, select = -c(THREEFF3)),
                     mtry = round(sqrt(ncol(training_data)-2)), importance = TRUE)
  
  importance(rf)
  
  # Testing the fitted model
  yhat.rf <- predict(rf, newdata = subset(testing_data, select = -c(THREEFF3)))
  
  # Calculate RMSE
  RMSE.rf <- (mean(testing_data$DGS10 - yhat.rf)^2)^.5
  
  
  return(list(rf, yhat.rf, RMSE.rf))
  
}



boostedTrees <- function(training_data, testing_data){

  ### BOOSTED TREES ####
  # Create training and test matrices
  train <- xgb.DMatrix(as.matrix(subset(training_data, select = -c(THREEFF3, DGS10))), label = training_data$DGS10)
  test <- xgb.DMatrix(as.matrix(subset(testing_data, select = -c(THREEFF3, DGS10))), label = testing_data$DGS10)
  
  # Establish default parameters
  params <- list(booster = "gbtree", objective = "reg:linear", eta=0.3, gamma=0, max_depth=10, 
                 min_child_weight=1, subsample=1, colsample_bytree=1)
  
  
  # Cross-validate for optimal number of rounds
  xgbcv <- xgb.cv(params = params, data = train, nrounds = 150, nfold = 10, showsd = T, 
                  stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)
  
  best_nrounds <- xgbcv[8]$best_iteration
  
  
  # Fit the boosted model using gradient descent
  boost <- xgb.train(params = params, data = train, nrounds = best_nrounds, 
                     watchlist = list(val = test,train = train), print_every_n = 10, 
                     early_stopping_rounds = 20, maximize = F, eval_metric = "rmse")
  
  
  # Generate predictions from boosted model
  yhat.boost <- predict(boost, test)
  
  # Calculate RMSE
  RMSE.boost <- (mean(testing_data$DGS10 - yhat.boost)^2)^.5
  
  # Generate feature importance matrix
  mat.boost <- xgb.importance(model = boost, 
                              feature_names = colnames(as.matrix(subset(training_data, select = -c(THREEFF3, DGS10)))))
  
  
  return(list(boost, yhat.boost, RMSE.boost, mat.boost))
  
}



##### RUNNING THE MACHINE LEARNING MODELS #####

set.seed(3)

### Fit bagged, random forest, and boosted models on data within the Great tight regime ###
# Bagged Model
bagModel.tight <- baggedTrees(train_tight, test_tight)
RMSE.bag.tight <- bagModel.tight[[3]]

cat(as.character(bagModel.tight[[3]]), "is the RMSE for the bagged model over the Great tight.")

# Random Forest Model
rfModel.tight <- randomForests(train_tight, test_tight)
RMSE.rf.tight <- rfModel.tight[[3]]

cat(as.character(rfModel.tight[[3]]), "is the RMSE for the random forest model over the Great tight.")

# Boosted Model
boostModel.tight <- boostedTrees(train_tight, test_tight)
RMSE.boost.tight <- boostModel.tight[[3]]

cat(as.character(boostModel.tight[[3]]), "is the RMSE for the boosted model over the Great tight.")


### Fit bagged, random forest, and boosted models on data within the Zero Interest Rate Policy regime ###
# Bagged Model
bagModel.loose <- baggedTrees(train_loose, test_loose)
RMSE.bag.loose <- bagModel.loose[[3]]

cat(as.character(bagModel.loose[[3]]), "is the RMSE for the bagged model over the Zero Interest Rate Policy regime.")

# Random Forest Model
rfModel.loose <- randomForests(train_loose, test_loose)
RMSE.rf.loose <- rfModel.loose[[3]]

cat(as.character(rfModel.loose[[3]]), "is the RMSE for the random forest model over the Zero Interest Rate Policy regime.")

# Boosted Model
boostModel.loose <- boostedTrees(train_loose, test_loose)
RMSE.boost.loose <- boostModel.loose[[3]]

cat(as.character(boostModel.loose[[3]]), "is the RMSE for the boosted model over the Zero Interest Rate Policy regime.")



##### Plot forecast curves #####

forecastCurve <- function(testing_data, model){
  
  plot(as.Date(rownames(testing_data)), testing_data$DGS10, xaxt = 'n', type = 'l')
  
  #lines(as.Date(rownames(testing_data)), testing_data$THREEFF3, xaxt = 'n', col = 'brown')  # Uncomment to see forward rates
  lines(as.Date(rownames(testing_data)), model[[2]], xaxt = 'n', col = 'orange')
  
  axis(1, as.Date(rownames(testing_data)), format(as.Date(rownames(testing_data)), "%b %Y"), cex.axis = .8)
  
}

### Plot forecast curves for the Great tight ###
#forecastCurve(test_tight, bagModel.tight)
forecastCurve(test_tight, rfModel.tight)
#forecastCurve(test_tight, boostModel.tight)

### Plot forecast curves for the Zero Interest Rate Policy Regime ###
#forecastCurve(test_loose, bagModel.loose)
#forecastCurve(test_loose, rfModel.loose)
forecastCurve(test_loose, boostModel.loose)



##### Plot relative importance plots for the best model in each regime #####
#varImpPlot(bagModel.tight[[1]])
varImpPlot(rfModel.tight[[1]])
#xgb.plot.importance(boostModel.tight[[4]])

#varImpPlot(bagModel.loose[[1]])
#varImpPlot(rfModel.loose[[1]])
xgb.plot.importance(boostModel.loose[[4]])

