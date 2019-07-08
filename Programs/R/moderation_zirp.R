##### For moderation regime, take first 13 years as training and last 5 years as test => 
##### cutoff year is 2004 (~72% share in training set).
#####
##### For zirp regume, take first 5 years as training and last 2 years as test => 
##### cutoff year is 2013 (~71% share in training set).

# Import regime data (change to your local directory with RDS files)
moderation <- readRDS("C:/Users/DATA/moderation.rds")
zirp <- readRDS("C:/Users/DATA/zirp.rds")

# Import libraries
library(xgboost)
library(randomForest)
library(glmnet)

##### Split macroeconomic data by Fed policy regimes #####

# Split data from Great Moderation into training and test sets
train_moderation <- as.data.frame(moderation["/2003"])
test_moderation <- as.data.frame(moderation["2004/"])
# train_moderation <- as.data.frame(data["/2007"])  
# test_moderation <- as.data.frame(data["2008/"])

# Split data from Zero Interest Rate Policy regime into training and test sets
train_zirp <- as.data.frame(zirp["/2013"])
test_zirp <- as.data.frame(zirp["2014/"])

# Calculate RMSE for market baselines
RMSE.baseline.moderation <- (mean(test_moderation$UST10Y - test_moderation$FF3Y)^2)^.5
RMSE.baseline.zirp <- (mean(test_zirp$UST10Y - test_zirp$FF3Y)^2)^.5



##### MACHINE LEARNING MODELS #####

baggedTrees <- function(training_data, testing_data){
  
  ### BAGGED TREES ###
  # Train the bagged model
  bag <- randomForest(UST10Y ~ ., data = subset(training_data, select = -c(FF3Y)),
                                 mtry = ncol(training_data)-2, importance = TRUE)
  
  importance(bag)
  
  # Testing the fitted model
  yhat.bag <- predict(bag, newdata = subset(testing_data, select = -c(FF3Y)))
  
  # Calculate RMSE
  RMSE.bag <- (mean(testing_data$UST10Y - yhat.bag)^2)^.5
  
  
  return(list(bag, yhat.bag, RMSE.bag))

}



randomForests <- function(training_data, testing_data){

  ### RANDOM FOREST ###
  # Train the bagged model
  rf <- randomForest(UST10Y ~ ., data = subset(training_data, select = -c(FF3Y)),
                                 mtry = round(sqrt(ncol(training_data)-2)), importance = TRUE)
  
  importance(rf)
  
  # Testing the fitted model
  yhat.rf <- predict(rf, newdata = subset(testing_data, select = -c(FF3Y)))
  
  # Calculate RMSE
  RMSE.rf <- (mean(testing_data$UST10Y - yhat.rf)^2)^.5
  
  
  return(list(rf, yhat.rf, RMSE.rf))

}



boostedTrees <- function(training_data, testing_data){

  ### BOOSTED TREES ####
  # Create training and test matrices
  train <- xgb.DMatrix(as.matrix(subset(training_data, select = -c(FF3Y, UST10Y))), label = training_data$UST10Y)
  test <- xgb.DMatrix(as.matrix(subset(testing_data, select = -c(FF3Y, UST10Y))), label = testing_data$UST10Y)
  
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
  RMSE.boost <- (mean(testing_data$UST10Y - yhat.boost)^2)^.5
  
  # Generate feature importance matrix
  mat.boost <- xgb.importance(model = boost, 
                              feature_names = colnames(as.matrix(subset(training_data, select = -c(FF3Y, UST10Y)))))

  
  return(list(boost, yhat.boost, RMSE.boost, mat.boost))
  
}



fitLASSO <- function(training_data, testing_data){
  
  # Convert training and test sets
  x.train <- model.matrix(UST10Y ~. - FF3Y, data = training_data)
  y.train <- training_data[,"UST10Y"]
  x.test <- model.matrix(UST10Y ~. - FF3Y, data = testing_data)
  y.test <- testing_data[,'UST10Y']
  
  # Define grid of lambdas to retrieve optimal lambda
  grid <- 10^(seq(10, -10, length = 100))
  
  # Fit model, cross-validate for optimal lambda
  lasso.mod <- glmnet(x.train, y.train, alpha = 1, lambda = grid)
  cv.out <- cv.glmnet(x.train, y.train , alpha = 1)
  bestlam <- cv.out$lambda.min
  
  # Predict and record RMSE
  lasso.pred <- predict(lasso.mod, s = bestlam , newx = x.test)
  RMSE.lasso.mod <- (mean(lasso.pred - y.test)^2)^.5
  
  # Obtain summary of coefficients and clean up
  lasso.mod.coef <- predict(lasso.mod ,type = "coefficients", s = bestlam)
  lasso.coefs <- data.frame(rownames(as.matrix(lasso.mod.coef))[-2], 
                           as.vector(lasso.mod.coef)[-2])
  colnames(lasso.coefs) <- c("variables", "coefficients")
  lasso.coefs <- lasso.coefs[order(-abs(lasso.coefs$coefficients)),]
  
  
  return(list(lasso.mod, lasso.pred, RMSE.lasso.mod, lasso.coefs))
  
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

# Lasso
lassoModel.moderation <- fitLASSO(train_moderation, test_moderation)
RMSE.lasso.moderation <- lassoModel.moderation[[3]]

cat(as.character(lassoModel.moderation[[3]]), "is the RMSE for the boosted model over the Great Moderation.")



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

# Lasso
lassoModel.zirp <- fitLASSO(train_zirp, test_zirp)
RMSE.lasso.zirp <- lassoModel.zirp[[3]]

cat(as.character(lassoModel.zirp[[3]]), "is the RMSE for the boosted model over the Zero Interest Rate Policy regime.")


##### Plot forecast curves #####

forecastCurve <- function(testing_data, model){
  
  plot(as.Date(rownames(testing_data)), testing_data$UST10Y, 
       xaxt = 'n', type = 'l', xlab = "", ylab = "", lwd = 2,
       main = "Actual vs. Baseline vs. Forecast")
  
  lines(as.Date(rownames(testing_data)), testing_data$FF3Y, xaxt = 'n', lwd = 2, col = 'brown')  # Uncomment to see forward rates
  lines(as.Date(rownames(testing_data)), model[[2]], xaxt = 'n', lwd = 2, col = 'orange')
  
  axis(1, as.Date(rownames(testing_data)), format(as.Date(rownames(testing_data)), "%b %Y"), cex.axis = .8)
  
}

### Plot forecast curves for the Great Moderation ###
forecastCurve(test_moderation, bagModel.moderation)
{legend("bottomleft", c("UST10Y", "Baseline", "Bagged Trees"), 
       col = c("black", "brown", "orange"), lty=1, cex = .75)}
forecastCurve(test_moderation, rfModel.moderation)  # Lowest RMSE: 0.14
{legend("bottomleft", c("UST10Y", "Baseline", "Random Forest"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)}
forecastCurve(test_moderation, boostModel.moderation)
{legend("bottomleft", c("UST10Y", "Baseline", "Boosted Trees"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)}
forecastCurve(test_moderation, lassoModel.moderation)
{legend("bottomleft", c("UST10Y", "Baseline", "LASSO"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)}

### Plot forecast curves for the Zero Interest Rate Policy Regime ###
forecastCurve(test_zirp, bagModel.zirp)  # Lowest RMSE: 0.1
{legend("topright", c("UST10Y", "Baseline", "Bagged Trees"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_zirp, rfModel.zirp)
{legend("topright", c("UST10Y", "Baseline", "Random Forest"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_zirp, boostModel.zirp)
{legend("topright", c("UST10Y", "Baseline", "Boosted Trees"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_zirp, lassoModel.zirp)
{legend("topright", c("UST10Y", "Baseline", "LASSO"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}

##### Plot relative importance plots for the best model in each regime #####
varImpPlot(bagModel.moderation[[1]], main = "Relative Importance: Bagged Trees")
varImpPlot(rfModel.moderation[[1]], main = "Relative Importance: Random Forest")
xgb.plot.importance(boostModel.moderation[[4]])
title(main = "Relative Importance: Boosted Trees")
lassoModel.moderation[[4]]

varImpPlot(bagModel.zirp[[1]], main = "Relative Importance: Bagged Trees")
varImpPlot(rfModel.zirp[[1]], main = "Relative Importance: Random Forest")
xgb.plot.importance(boostModel.zirp[[4]])
title(main = "Relative Importance: Boosted Trees")
lassoModel.zirp[[4]]

