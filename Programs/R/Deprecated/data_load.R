
#rm(list = ls())

library(imputeTS)
library(quantmod)

## Load Data

###---### Dependent Variables ###---###

# Daily 10-year CMR Treasuries
CMR_10Y <- na.approx(getSymbols("GS10", src = "FRED", auto.assign = F)["1990-08/2018"])
apply.monthly(CMR_10Y, mean)

# Daily Fitted Instantaneous Forward Rate 1 Years Hence (1990 - 2018)
FF1Y <- na.approx(getSymbols("THREEFF1", src = "FRED", auto.assign = F))["1990-08-01/2018-12-31"]
FF1Y <- apply.monthly(FF1Y, mean)
#time(FF1Y) <- seq.Date(from = start(CMR_10Y), to = end(CMR_10Y), by = "months")

# Daily Fitted Instantaneous Forward Rate 2 Years Hence (1990 - 2018)
FF2Y <- na.approx(getSymbols("THREEFF2", src = "FRED", auto.assign = F))["1990-08-01/2018-12-31"]
FF2Y <- apply.monthly(FF2Y, mean)
#time(FF2Y) <- seq.Date(from = start(CMR_10Y), to = end(CMR_10Y), by = "months")

# Daily Fitted Instantaneous Forward Rate 3 Years Hence (1990 - 2018)
FF3Y <- na.approx(getSymbols("THREEFF3", src = "FRED", auto.assign = F))["1990-08-01/2018-12-31"]
FF3Y <- apply.monthly(FF3Y, mean)
#time(FF3Y) <- seq.Date(from = start(CMR_10Y), to = end(CMR_10Y), by = "months")



###---### Independent Variables ###---###

## Inflation ##

# Monthly CPI for all urban Consumers 
cpi_LFE <- getSymbols("CPILFESL", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"] # Less Food & Energy Items

# Monthly PCE less Food and Energy
pce_LFE <- getSymbols("PCEPILFE", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# 12-month Inflation Expectations (consumer Survey by UMich)
IE_1Y <- getSymbols("MICH", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

## Output ##

# Monthly Total Capacity Utilization 
TCU <- getSymbols("TCU", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# Monthly 12-month ahead Consumer sentiment survey on business conditions (UMich)
CS_1Y <- getSymbols("UMCSENT", src = "FRED", auto.assign = F)["1987-08/2018"]

# GDP disaggregated to monthly frequency later...

## Employment ## 

# 12-month ahead Consumer expectations on unemployment unavailable on FRED

# Monthly Civilian unemployment Rate (NSA)
UR <- getSymbols("UNRATENSA", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

## Other Activity Measures ##

# Composite Index for Leading Indicators (NBER) available upto 2012 in FRED

# Monthly Federal Surplus or Deficit (in levels i.e. millions of dollars)
fiscal_balance <- getSymbols("MTSDS133FMS", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# Monetary Aggregates
M2 <- getSymbols("M2NS", src = "FRED", auto.assign = F)["1987-08/2018"] # M2 (n.s.a)
M1 <- getSymbols("M1NS", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"] # M1 (n.s.a.)
M3 <- getSymbols("MABMM301USM189S", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"] / 10^10 # M3 (s.a.)

# Federal Funds Rate
fed_funds <- getSymbols("FEDFUNDS", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# 6-month Treasury Bill Yields (for spread between fed funds)
CMR_6M <- getSymbols("GS6M", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# Yield Spread between 6-month T-Bill and Fed Funds Rate
spread_6M = (fed_funds - CMR_6M)




### Riley's indicator series ###
MTS <- getSymbols("CMRMTSPL", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]
INDPRO <- getSymbols("INDPRO", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]
INCOME <- getSymbols("W875RX1", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]
LOANS <- getSymbols("BUSLOANS", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]
PRIME <- getSymbols("MPRIME", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]
PERMITS <- getSymbols("HOUST", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]
CS <- getSymbols("UMCSENT", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]
CREDIT <- Delt(getSymbols("TOTALSL", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"] / INCOME) * 100



###---### Temporal Disaggregation (by Kenneth) ###---###
# Initialize empty dataframe 'Days' containing all days in our time horizon. 
# The INITIAL DATE should be based on the constraining temporal series, which is FF3Y, and it is "1990-06-30".
days <- data.frame(day = rep(NA, length(seq(as.Date("1990-06-30"), as.Date("2018-12-31"), by='days'))), 
                   row.names = seq(as.Date("1990-06-30"), as.Date("2018-12-31"), by='days'))


# Import DAILY 10 year UST yield and 10 year, 3 year hence forward rate data starting from the initial date.
CMR_10Y <- as.data.frame(na.approx(getSymbols("DGS10", src = "FRED", auto.assign = F))["1990-06-30/2018-12-31"])
FF3Y <- as.data.frame(na.approx(getSymbols("THREEFF3", src = "FRED", auto.assign = F))["1990-06-30/2018-12-31"])
INV <- as.data.frame(na.approx(getSymbols("T10YFFM", src = "FRED", auto.assign = F))["1990-06-30/2018-12-31"])

# Merge interest rates and chosen macroeconomic predictors with Days to fill in all days in our time horizon
mergeDf <- function(x, y){
  Df <- merge(x, y, by = 0, all = TRUE)
  row.names(Df) <- Df$Row.names
  Df$Row.names <- NULL
  return(Df)
}

data <- Reduce(mergeDf, list(days, CMR_10Y, FF3Y, INV, 
                             as.data.frame(pce_LFE), as.data.frame(TCU), 
                                                  as.data.frame(UR), as.data.frame(M1), as.data.frame(CREDIT),
                                                  as.data.frame(M3), as.data.frame(fiscal_balance),
                                                  as.data.frame(MTS), as.data.frame(INDPRO), as.data.frame(INCOME),
                                                  as.data.frame(LOANS), as.data.frame(PRIME), as.data.frame(PERMITS),
                                                  as.data.frame(CS), as.data.frame(spread_6M)
                             ))

# Replace spread_6M with just the effective fed funds rate, since we already have the spread in INV.

data$day <- NULL


# Aggregate daily data to weekly data using the mean
data <- aggregate(data, by=list((seq(nrow(data))-1) %/% 7), FUN=mean, na.rm = TRUE)[-1]

# Date observations by week such that each week ends on a FRIDAY
rownames(data) <- seq(as.Date("1990-07-06"), as.Date("2019-01-04"), by="weeks")

# Delete rows for which there is missing rate data
data <- data[!(is.na(data$DGS10)) & !(is.na(data$THREEFF3)), ]


# Impute time series using imputeTS spline interpolation
data <- na.interpolation(data, option = 'linear')  # Various different options/models to choose from!
#data <- na.kalman(data)  # Use Kalman smoothing on structural time series models fitted via MLE

# Lag macroeconomic indicators by 1 year
data <- cbind(data[, 1:2], apply(data[, 3:ncol(data)], 2, function(x) lag(x, k = -52)))   # 1 year only?
#data$T10Y2Y <- Lag(data$T10Y2Y, shift = 52)  # Lag the 10-year treasury minus 2-year treasury by another year if needed
#data$FEDFUNDS <- Lag(data$FEDFUNDS, shift = 52)  # Lag the Fed funds rate minus 6-month treasury by another year if needed

# Roll forward THREEFF3 by 3 years
data$THREEFF3 <- lag(data$THREEFF3, k = 52*3)

# Drop resulting NAs
data <- na.omit(data)



### REGIME SPLITS ###
# Split according to regimes pursuant to Gavin (2018)
data <- as.xts(data)

moderation <- data["1991/2008-12"]
zirp <- data["2009/2015-12"]

# Split according to Fed tightening/loosening regimes (see FRED)
tight <- data[c("1994-02-04/2001-01-02", "2004-06-25/2007-09-17", "2015-12-15/")]
loose <- data[c("/1994-02-03", "2001-01-03/2004-06-24", "2007-09-16/2015-12-14")]


# Export regime data (change to your local directory)
saveRDS(moderation, "C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\moderation.rds")
saveRDS(zirp, "C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\zirp.rds")

saveRDS(tight, "C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\tight.rds")
saveRDS(loose, "C:\\Users\\kenri\\OneDrive\\Thesis\\Data\\loose.rds")


# Plot some basic time series
plot(as.Date(index(data)), data$DGS10, xaxt = 'n', type = 'l')
lines(as.Date(index(data)), data$THREEFF3, xaxt = 'n', col = 'brown')
axis(1, as.Date(index(data)), format(as.Date(index(data)), "%b %Y"), cex.axis = .8)

plot(as.Date(index(data)), data$UNRATENSA, xaxt = 'n', type = 'l', col = 'red')
axis(1, as.Date(index(data)), format(as.Date(index(data)), "%b %Y"), cex.axis = .8)

