##### For moderation regime, take first 12 years as training and last 5 years as test => 
##### cutoff year is 2004 (~71% share in training set).
#####
##### For zirp regume, take first 5 years as training and last 2 years as test => 
##### cutoff year is 2013 (~71% share in training set).

rm(list=ls())

##### Import ML libraries #####
library(randomForest)
library(xgboost)

##### Split macroeconomic data by Fed policy regimes #####

# Import regime data (Kenneth only)
moderation <- readRDS("C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\moderation.rds")
zirp <- readRDS("C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\zirp.rds")

# Split data from Great Moderation into training and test sets
train_moderation <- as.data.frame(moderation["/2003"])
test_moderation <- as.data.frame(moderation["2004/"])
# train_moderation <- as.data.frame(data["/2007"])  
# test_moderation <- as.data.frame(data["2008/"])

# Split data from Zero Interest Rate Policy regime into training and test sets
train_zirp <- as.data.frame(zirp["/2013"])
test_zirp <- as.data.frame(zirp["2014/"])

# Calculate RMSE for market baselines
RMSE.baseline.moderation <- (mean(test_moderation$DGS10 - test_moderation$THREEFF3)^2)^.5
RMSE.baseline.zirp <- (mean(test_zirp$DGS10 - test_zirp$THREEFF3)^2)^.5



##### MACHINE LEARNING MODELS #####

baggedTrees <- function(training_data, testing_data){
  
  #set.seed(1)
  
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
  
  #set.seed(1)

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
  
  #set.seed(1)

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

set.seed(1)

### Fit bagged, random forest, and boosted models on data within the Great Moderation regime ###
# Bagged Model
bagModel.moderation <- baggedTrees(train_moderation, test_moderation)
RMSE.bag.moderation <- bagModel.moderation[[3]]

cat(as.character(bagModel.moderation[[3]]), "is the RMSE for the bagged model over the Great Moderation.")

# Random Forest Model
rfModel.moderation <- randomForests(train_moderation, test_moderation)
RMSE.rf.moderation <- rfModel.moderation[[3]]

cat(as.character(rfModel.moderation[[3]]), "is the RMSE for the random forest model over the Great Moderation.")

# Boosted Model
boostModel.moderation <- boostedTrees(train_moderation, test_moderation)
RMSE.boost.moderation <- boostModel.moderation[[3]]

cat(as.character(boostModel.moderation[[3]]), "is the RMSE for the boosted model over the Great Moderation.")


### Fit bagged, random forest, and boosted models on data within the Zero Interest Rate Policy regime ###
# Bagged Model
bagModel.zirp <- baggedTrees(train_zirp, test_zirp)
RMSE.bag.zirp <- bagModel.zirp[[3]]

cat(as.character(bagModel.zirp[[3]]), "is the RMSE for the bagged model over the Zero Interest Rate Policy regime.")

# Random Forest Model
rfModel.zirp <- randomForests(train_zirp, test_zirp)
RMSE.rf.zirp <- rfModel.zirp[[3]]

cat(as.character(rfModel.zirp[[3]]), "is the RMSE for the random forest model over the Zero Interest Rate Policy regime.")

# Boosted Model
boostModel.zirp <- boostedTrees(train_zirp, test_zirp)
RMSE.boost.zirp <- boostModel.zirp[[3]]

cat(as.character(boostModel.zirp[[3]]), "is the RMSE for the boosted model over the Zero Interest Rate Policy regime.")



##### Plot forecast curves #####

forecastCurve <- function(testing_data, model){
  
  plot(as.Date(rownames(testing_data)), testing_data$DGS10, xaxt = 'n', type = 'l')
  
  #lines(as.Date(rownames(testing_data)), testing_data$THREEFF3, xaxt = 'n', col = 'brown')  # Uncomment to see forward rates
  lines(as.Date(rownames(testing_data)), model[[2]], xaxt = 'n', col = 'orange')
  
  axis(1, as.Date(rownames(testing_data)), format(as.Date(rownames(testing_data)), "%b %Y"), cex.axis = .8)
  
}

### Plot forecast curves for the Great Moderation ###
forecastCurve(test_moderation, bagModel.moderation)
forecastCurve(test_moderation, rfModel.moderation)
forecastCurve(test_moderation, boostModel.moderation)

### Plot forecast curves for the Zero Interest Rate Policy Regime ###
forecastCurve(test_zirp, bagModel.zirp)
forecastCurve(test_zirp, rfModel.zirp)
forecastCurve(test_zirp, boostModel.zirp)



##### Plot relative importance plots for the best model in each regime #####
varImpPlot(bagModel.moderation[[1]])
xgb.plot.importance(boostModel.moderation[[4]])

varImpPlot(bagModel.zirp[[1]])
xgb.plot.importance(boostModel.zirp[[4]]) 

