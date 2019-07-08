
library(quantmod)
library(imputeTS)

# Initialize empty dataframe 'Days' containing all days in our time horizon. 
# The INITIAL DATE should be based on the constraining temporal series, which is FF3Y, and it is "1990-06-30".
days <- data.frame(day = rep(NA, length(seq(as.Date("1990-06-30"), as.Date("2018-12-31"), by='days'))), 
                   row.names = seq(as.Date("1990-06-30"), as.Date("2018-12-31"), by='days'))


# Import DAILY 10 year UST yield and 10 year, 3 year hence forward rate data starting from the initial date.
CMR_10Y <- as.data.frame(getSymbols("DGS10", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"])
FF5Y <- as.data.frame(getSymbols("THREEFF5", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"])
FF4Y <- as.data.frame(getSymbols("THREEFF4", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"])
FF3Y <- as.data.frame(getSymbols("THREEFF3", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"])
FF2Y <- as.data.frame(getSymbols("THREEFF2", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"])
FF1Y <- as.data.frame(getSymbols("THREEFF1", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"])


?na.approx
# Merge interest rates and chosen macroeconomic predictors with Days to fill in all days in our time horizon
mergeDf <- function(x, y){
  Df <- merge(x, y, by = 0, all = TRUE)
  row.names(Df) <- Df$Row.names
  Df$Row.names <- NULL
  return(Df)
}


data <- Reduce(mergeDf, list(days, CMR_10Y, FF5Y, FF4Y, FF3Y, FF2Y, FF1Y)) 

data$day <- NULL


# Aggregate daily data to weekly data using the mean
data <- aggregate(data, by=list((seq(nrow(data))-1) %/% 7), FUN=mean, na.rm = TRUE)[-1]

# Date observations by week such that each week ends on a FRIDAY
rownames(data) <- seq(as.Date("1990-07-06"), as.Date("2019-01-04"), by="weeks")

# Delete rows for which there is missing rate data
data <- data[!(is.na(data$DGS10)), ]


# Convert data to time series
#data <- ts(data)

# Impute time series using imputeTS spline interpolation
data <- na.interpolation(data, option = 'spline')  # Various different options/models to choose from!

data
dates = rownames(data)
dates = as.Date(dates)

Actual = xts(data[,1], order.by = dates)
FF5Y = as.xts(data[,2], order.by = dates)
FF4Y = as.xts(data[,3], order.by = dates)
FF3Y = as.xts(data[,4], order.by = dates)
FF2Y = as.xts(data[,5], order.by = dates)
FF1Y = as.xts(data[,6], order.by = dates)

head(Actual)
tail(Actual)

## Align forward rates with corresponding spot rates

align_fwd_rates <- function(x, n.years = 0, from = "1991-07-20" , to = "2019-01-04"){
  
  dates = index(x)
  tmp_dates = as.POSIXlt(dates)
  tmp_dates$year = tmp_dates$year + n.years
  
  aligned_set = xts(as.numeric(x), order.by = tmp_dates)[paste0(from,"/",to)]
  return(aligned_set)
  
}
# x is an xts object, n.years is the number of years to lead the fwd rates to
# the 'from' and 'to' should be the final aligned dates. 
# e.g. from 1990 + 3 (n.years) => from = '1993' 

head(FF1Y); head(FF3Y);head(FF5Y)

# Create aligned set
A_FF5Y = align_fwd_rates(FF5Y, n.years = 5, from = '1995-07-20', to = '2019-01-04')
A_FF4Y = align_fwd_rates(FF4Y, n.years = 4, from = '1994-07-20', to = '2019-01-04')
A_FF3Y = align_fwd_rates(FF3Y, n.years = 3, from = '1993-07-20', to = '2019-01-04')
A_FF2Y = align_fwd_rates(FF2Y, n.years = 2, from = '1992-07-20', to = '2019-01-04')
A_FF1Y = align_fwd_rates(FF1Y, n.years = 1)


## BREAK



# Merge Individual series with actual
A = merge(A_FF1Y, Actual)['1991-07-20/2019-01-04'] # from beginning obs. of aligned FF1Y to the last record of Actual
                                                   # to check, try 'head(A, 60);tail(A)' after erasing index
B = merge(A_FF2Y, Actual)['1992-07-20/2019-01-04'] # "
C = merge(A_FF3Y, Actual)['1993-07-20/2019-01-04'] # "
D = merge(A_FF4Y, Actual)['1994-07-20/2019-01-04'] # "
E = merge(A_FF5Y, Actual)['1995-07-20/2019-01-04'] # "

?merge

# The 1-Year ahead forward rates and actual CMR 10Y rates are not perfectly aligned, but observations
# for each series are 7 days apart. Thus, we use the recorded forward rate in each week to compare
# with the Actual rate the same week. For A_FF1Y, we carry each observation forward to align it with 
# the Actual CMR dates and then subset for those dates:
?na.locf
A$A_FF1Y = na.locf(A$A_FF1Y)
A = A[index(Actual),]

# Do the same for B, C, D and E 
B$A_FF2Y = na.locf(B$A_FF2Y)
B = B[index(Actual),]

C$A_FF3Y = na.locf(C$A_FF3Y)
C = C[index(Actual),]

D$A_FF4Y = na.locf(D$A_FF4Y)
D = D[index(Actual),]

E$A_FF5Y = na.locf(E$A_FF5Y)
E = E[index(Actual),]


# NoW, Calculate the spread for each set (prediction error of our benchmark), as well as squared errors
A$error = (A$Actual - A$A_FF1Y); A$sq.error = (A$error)^2 
B$error = (B$Actual - B$A_FF2Y); B$sq.error = (B$error)^2
C$error = (C$Actual - C$A_FF3Y); C$sq.error = (C$error)^2
D$error = (D$Actual - D$A_FF4Y); D$sq.error = (D$error)^2
E$error = (E$Actual - E$A_FF5Y); E$sq.error = (E$error)^2

# Calcualte RMSE for each

Benchmark_RMSE = data.frame(FF1Y_RMSE = (mean(A$sq.error))^0.5,
                            FF2Y_RMSE = (mean(B$sq.error))^0.5,
                            FF3Y_RMSE = (mean(C$sq.error))^0.5,
                            FF4Y_RMSE = (mean(D$sq.error))^0.5,
                            FF5Y_RMSE = (mean(E$sq.error))^0.5)
Benchmark_RMSE
# Clearly, the 3-year forward rates are the most accurate.

## Now, to plot each Series with the actual. 
library(xts)
library(timeSeries)
library(PerformanceAnalytics)

# First, change index format to month year


# FF1Y
colnames(A)[1:2] = c( "10-Year Treasury", "1-Year Hence Rates, rolled forward")

A1 = A[,1:2]
plot(A1, bg = 0, type = 'l', lty = 1, lwd = c(2,2), grid.ticks.on = 'auto', 
         minor.ticks = NULL,
         grid.col = 0, legend.loc = "topright")


# FF3y
colnames(C)[1:2] = c( "10-Year Treasury", "3-Years Hence Rates, rolled forward")
C1 = C[,1:2]

plot.xts(C1, bg = 0, type = 'l', lty = 1, lwd = c(2,2),
          minor.ticks = NULL,
         grid.col = 0, legend.loc = "topright")

# FF5Y
colnames(E)[1:2] = c( "10-Year Treasury", "5-Years Hence Rates, rolled forward")
E1 = E[,1:2]

plot.xts(E1, bg = 0, type = 'l', lty = 1, lwd = c(2,2),
         minor.ticks = NULL,
         grid.col = 0, legend.loc = "topright")

# Plot the squared errors:
errors = merge(A$sq.error, B$sq.error, C$sq.error, D$sq.error, E$sq.error)
colnames(errors) = c("1-Year Hence", "2-Years Hence", "3-Years Hence", 
                     "4-Years Hence", "5-Years Hence")


errors1 = errors[,c(1,3,5)]
plot.xts(errors1, bg = 0, col = c('brown', 'blue', 'orange'), type = 'l', lty = 1, lwd = c(1.5,1.5,1.5),
         minor.ticks = NULL,
         grid.col = 0, legend.loc = "topright")
?plot.xts
