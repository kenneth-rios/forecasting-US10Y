# forecasting-US10Y
Forecasting the 10 year US Treasury using forward rates and machine learning alternatives

We use market expectations of the 10 year US Treasury and machine learning alternatives such as LASSO, random forests, bagged trees, and boosted trees to forecast the US10Y. We included regime switching in our training and test sets to account for influences from monetary policy and periods of Fed tightening/loosening on interest rates. In addition to including macroeconomic predictors used by the Fed in their determination of target rates, we also include stock market data to account for influences from bull and bear markets. Model performance is assessed using RMSEs and year-forward validation across test sets corresponding to various regimes.

Worked with Shukrit Guha (NYU) and Riley Conlon (NYU)
