
rm(list = ls())

dev.new()

library(imputeTS)
library(quantmod)

## Load Data

###---### Dependent Variables and Baseline ###---###

# Import DAILY 10 year UST yield and 10 year, 3 year hence forward rate data starting from the initial date.
CMR_10Y <- as.data.frame(na.approx(getSymbols("DGS10", src = "FRED", auto.assign = F))["1990-06-30/2018-12-31"])
FF3Y <- as.data.frame(na.approx(getSymbols("THREEFF3", src = "FRED", auto.assign = F))["1990-06-30/2018-12-31"])


###---### Independent Variables ###---###

## Inflation ##

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

# Monthly Civilian unemployment Rate (NSA)
UR <- getSymbols("UNRATENSA", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# Monthly Federal Surplus or Deficit (in levels i.e. millions of dollars)
fiscal_balance <- getSymbols("MTSDS133FMS", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# Monetary Aggregates
M1 <- getSymbols("M1NS", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"] # M1 (n.s.a.)
M3 <- getSymbols("MABMM301USM189S", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"] / 10^10 # M3 (s.a.)

# Federal Funds Rate
fed_funds <- getSymbols("FEDFUNDS", src = "FRED", auto.assign = F)["1990-06-30/2018-12-31"]

# 10-Year Treasury Constant Maturity Minus Federal Funds Rate
INV <- as.data.frame(na.approx(getSymbols("T10YFFM", src = "FRED", auto.assign = F))["1990-06-30/2018-12-31"])

# S&P500
setwd("C:/Users/shukr/Desktop/NYU_Classes/SEM 4/Project_R_files/April")
SP500 <- as.xts(read.csv('SP500.csv', header = T, row.names = 1))


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


# Merge interest rates and chosen macroeconomic predictors with Days to fill in all days in our time horizon
mergeDf <- function(x, y){
  Df <- merge(x, y, by = 0, all = TRUE)
  row.names(Df) <- Df$Row.names
  Df$Row.names <- NULL
  return(Df)
}

data <- Reduce(mergeDf, list(days, CMR_10Y, 
                             FF3Y, 
                             INV, as.data.frame(fed_funds),
                             as.data.frame(pce_LFE), as.data.frame(TCU), as.data.frame(UR), 
                             as.data.frame(M1), as.data.frame(M3), 
                             as.data.frame(CREDIT), as.data.frame(fiscal_balance), as.data.frame(INCOME),
                             as.data.frame(MTS), as.data.frame(INDPRO),
                             as.data.frame(LOANS), as.data.frame(PRIME), as.data.frame(PERMITS),
                             as.data.frame(CS),
                             as.data.frame(SP500)))

data$day <- NULL

# Aggregate daily data to weekly data using the mean
data <- aggregate(data, by=list((seq(nrow(data))-1) %/% 7), FUN=mean, na.rm = TRUE)[-1]

# Date observations by week such that each week ends on a FRIDAY
rownames(data) <- seq(as.Date("1990-07-06"), as.Date("2019-01-04"), by="weeks")

# Delete rows for which there is missing rate data
data <- data[!(is.na(data$DGS10)) & !(is.na(data$THREEFF3)), ]


# Impute time series using imputeTS spline interpolation
data <- na.interpolation(data, option = 'linear')  # Various different options/models to choose from!

# Lag macroeconomic indicators by 1 year
data <- cbind(data[, 1:2], apply(data[, 3:ncol(data)], 2, function(x) lag(x, k = -52)))   # 1 year only?

# Roll forward THREEFF3 by 3 years
data$THREEFF3 <- lag(data$THREEFF3, k = 52*3)

# Drop resulting NAs
data <- na.omit(data)



# Rename columns for clarity:
colnames(data) <- c("UST10Y", 
                    "FF3Y", 
                    "SPREAD", "FED_FUNDS", 
                    "PCE", "TCU", "UNEMP", 
                    "M1",  "M3",
                    "CTI", "FISC_BAL", "INCOME",
                    "MTS", "IND_PROD", 
                    "LOANS", "PRIME_RATE", "PERMITS",
                    "CONS_SENT",
                    "SP500")


### TIGHT/LOOSE AND REGIME SPLITS ###

data <- as.xts(data)

# Include indicator for tightening periods in both moderation and zirp
data$TIGHT <- NA
data$TIGHT[c("1994-02-04/2001-01-02", "2004-06-25/2007-09-17", "2015-12-15/")] <- 1
data$TIGHT[c("/1994-02-03", "2001-01-03/2004-06-24", "2007-09-18/2015-12-14")] <- 0

# Split according to regimes pursuant to Gavin (2018)
moderation <- data["1991/2008-12"]
zirp <- data["2009/2015-12"]

# Split according to Fed tightening/loosening regimes (see FRED)
tight <- data[c("1994-02-04/2001-01-02", "2004-06-25/2007-09-17", "2015-12-15/")]
loose <- data[c("/1994-02-03", "2001-01-03/2004-06-24", "2007-09-18/2015-12-14")]


# Export regime data (change to your local directory)
saveRDS(moderation, "C:/Users/shukr/Desktop/NYU_Classes/SEM 4/Project_R_files/April/RDS_files/moderation.rds")
saveRDS(zirp, "C:/Users/shukr/Desktop/NYU_Classes/SEM 4/Project_R_files/April/RDS_files/zirp.rds")

saveRDS(tight, "C:/Users/shukr/Desktop/NYU_Classes/SEM 4/Project_R_files/April/RDS_files/tight.rds")
saveRDS(loose, "C:/Users/shukr/Desktop/NYU_Classes/SEM 4/Project_R_files/April/RDS_files/loose.rds")


# Plot some basic time series
plot(as.Date(index(data)), data$UST10Y, xaxt = 'n', type = 'l', lwd = 2)
lines(as.Date(index(data)), data$FF3Y, xaxt = 'n', col = 'brown', lwd = 2)
axis(1, as.Date(index(data)), format(as.Date(index(data)), "%b %Y"), cex.axis = .8)

plot(as.Date(index(data)), data$UNRATENSA, xaxt = 'n', type = 'l', col = 'red')
axis(1, as.Date(index(data)), format(as.Date(index(data)), "%b %Y"), cex.axis = .8)

