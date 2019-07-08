
### Set working directory with master file
setwd('C:/Users/DATA')

### Source analysis files
source('data_load.R')
source('moderation_zirp.R')
source('tight_loose.R')
source('tree_plots.R')

### Print upon successful completion
cat("CODE SUCCESSFULLY COMPLETED")

# write.csv(lassoModel.moderation[[4]], "lasso_moderation_coef.csv")
# write.csv(lassoModel.zirp[[4]], "lasso_ZIRP_coef.csv")
# write.csv(lassoModel.tight[[4]], "lasso_Tight_coef.csv")
# write.csv(lassoModel.loose[[4]], "lasso_Loose_coef.csv")
