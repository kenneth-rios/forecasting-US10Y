##### For tight block, take the final tightening regime from 2015-12-15 to 2019-01-04 as the test set.
#####
##### For loose block, take the first three years of the final loosening regime as the test set since it 
##### captures the Great Recession (??). The years are 2007-09-16 to 2010-09-16.
#####

##### Import libraries #####
library(dplyr)
library(tibble)
library(randomForest)
library(xgboost)


##### Split macroeconomic data by Fed policy regimes #####
# Import regime data (change to your local directory with RDS files)
tight <- readRDS("C:/Users/DATA/tight.rds")
loose <- readRDS("C:/Users/DATA/loose.rds")

# Split data from tightening block into training and test sets
tight <- as.xts(tight)
train_tight <- as.data.frame(tight["1997-01-02/2005-06-24"])  # Last five years before crisis test set
test_tight <- as.data.frame(tight["2005-06-25/2007-09-17"])
tight <- rownames_to_column(as.data.frame(tight), 'date')
# train_tight <- column_to_rownames(sample_frac(as.data.frame(tight), .7), 'date')  # Use 70/30 or 50/50 split?
# test_tight <- column_to_rownames(anti_join(as.data.frame(tight), train_tight), 'date')

# Split data from loosening block into training and test sets
loose <- as.xts(loose)
train_loose <- as.data.frame(loose[c("2008-12-14/2013-12-14")])
test_loose <- as.data.frame(loose["2013-12-15/"])
# loose <- rownames_to_column(as.data.frame(loose), 'date')
# train_loose <- column_to_rownames(sample_frac(as.data.frame(loose), .7), 'date')  # Use 70/30 or 50/50 split?
# test_loose <- column_to_rownames(anti_join(as.data.frame(loose), train_loose), 'date')

# Calculate RMSE for market baselines
RMSE.baseline.tight <- (mean(test_tight$UST10Y - test_tight$FF3Y)^2)^.5
RMSE.baseline.loose <- (mean(test_loose$UST10Y - test_loose$FF3Y)^2)^.5



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
  params <- list(booster = "gbtree", objective = "reg:linear", eta=0.2, gamma=0, max_depth=10, 
                 min_child_weight=1, subsample=1, colsample_bytree=1, lambda=1)
  
  
  # Cross-validate for optimal number of rounds
  xgbcv <- xgb.cv(params = params, data = train, nrounds = 150, nfold = 10, showsd = T, 
                  stratified = T, print_every_n = 10, early_stopping_rounds = 20, maximize = F)
  
  best_nrounds <- xgbcv[8]$best_iteration
  
  
  # Fit the boosted model using gradient descent
  boost <- xgb.train(params = params, data = train, nrounds = best_nrounds, 
                     watchlist = list(val = test,train = train), print_every_n = 10, 
                     early_stopping_rounds = 30, maximize = F, eval_metric = "rmse")
  
  
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

# Lasso
lassoModel.tight <- fitLASSO(train_tight, test_tight)
RMSE.lasso.tight <- lassoModel.tight[[3]]

cat(as.character(lassoModel.tight[[3]]), "is the RMSE for the Lasso model over the Great tight.")


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

# Lasso
lassoModel.loose <- fitLASSO(train_loose, test_loose)
RMSE.lasso.loose <- lassoModel.loose[[3]]

cat(as.character(lassoModel.loose[[3]]), "is the RMSE for the Lasso model over the Great tight.")


##### Plot forecast curves #####

forecastCurve <- function(testing_data, model){
  
  plot(as.Date(rownames(testing_data)), testing_data$UST10Y,
       xaxt = 'n', type = 'l', xlab = "", ylab = "", lwd = 2,
       main = "Actual vs. Baseline vs. Forecast")
  
  lines(as.Date(rownames(testing_data)), testing_data$FF3Y, xaxt = 'n', lwd = 2, col = 'brown')  # Uncomment to see forward rates
  lines(as.Date(rownames(testing_data)), model[[2]], xaxt = 'n', lwd = 2, col = 'orange')
  
  axis(1, as.Date(rownames(testing_data)), format(as.Date(rownames(testing_data)), "%b %Y"), cex.axis = .8)
  
}

### Plot forecast curves for the Great tight ###
forecastCurve(test_tight, bagModel.tight)  # Decent as well: 0.103
{legend("topleft", c("UST10Y", "Baseline", "Bagged Trees"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_tight, rfModel.tight)  # Lowest RMSE: 0.095
{legend("topleft", c("UST10Y", "Baseline", "Random Forest"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_tight, boostModel.tight)  # Low RMSE and best-tracking: 0.100 
{legend("topleft", c("UST10Y", "Baseline", "Boosted Trees"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_tight, lassoModel.tight)
{legend("topleft", c("UST10Y", "Baseline", "LASSO"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}

### Plot forecast curves for the Zero Interest Rate Policy Regime ###
forecastCurve(test_loose, bagModel.loose)  # Lowest RMSE and best-tracking: 0.103
{legend("topright", c("UST10Y", "Baseline", "Bagged Trees"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_loose, rfModel.loose)
{legend("topright", c("UST10Y", "Baseline", "Random Forest"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_loose, boostModel.loose)  
{legend("topright", c("UST10Y", "Baseline", "Boosted Trees"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}
forecastCurve(test_loose, lassoModel.loose)
{legend("topright", c("UST10Y", "Baseline", "LASSO"), 
        col = c("black", "brown", "orange"), lty=1, cex = .75)
}

##### Plot relative importance plots for the best model in each regime #####
varImpPlot(bagModel.tight[[1]], main = "Relative Importance: Bagged Trees")
varImpPlot(rfModel.tight[[1]],  main = "Relative Importance: Random Forest")
xgb.plot.importance(boostModel.tight[[4]])
title(main = "Relative Importance: Boosted Trees")
lassoModel.tight[[4]]

varImpPlot(bagModel.loose[[1]], main = "Relative Importance: Bagged Trees")
varImpPlot(rfModel.loose[[1]],  main = "Relative Importance: Random Forest")
xgb.plot.importance(boostModel.loose[[4]])
title(main = "Relative Importance: Boosted Trees")
lassoModel.loose[[4]]
