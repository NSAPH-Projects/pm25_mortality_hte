
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Group logit LASSO
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)

load("data/intermediate/rolling_cohort_1000.RData")


# change _ever columns so they are either TRUE or FALSE (see script 3)
conditions <- c("hypert_ever", "ischmcht_ever", "diabetes_ever", "chrnkidn_ever")
dt[, (conditions) := lapply(.SD, function(x) !is.na(x)), .SDcols = (conditions)]

#---------- Race indicator variables

race_indicators <- dcast(dt, qid ~ race, fun.aggregate = length)

# Change race indicator column names
setnames(race_indicators,
         old = colnames(race_indicators)[2:7],
         new = c("white", "black", "other", "asian", "hispanic", "native"))

# Bind indicators to original data.table
dt <- merge(dt, race_indicators, by = "qid")
rm(race_indicators); gc()


#---------- Sex indicator variables

# should I do sex and dual? or this is the same as binary?

# sex_indicators <- dcast(dt, qid ~ sex, fun.aggregate = length)
# 
# # Change race indicator column names
# setnames(sex_indicators,
#          old = colnames(sex_indicators)[2:3],
#          new = c("male", "female"))
# 
# # Bind indicators to original data.table
# dt <- merge(dt, sex_indicators, by = "qid")
# rm(sex_indicators); gc()

# make sex binary
dt[,sex := sex - 1]


#---------- Age indicator variables

age_grp_indicators <- dcast(dt, qid ~ age_grp, fun.aggregate = length)

# Bind indicators to original data.table
dt <- merge(dt, age_grp_indicators, by = "qid")
rm(age_grp_indicators); gc()



########

## Group LASSO
# requires all vars in a group (e.g., race, age) to be selected together

library(gglasso)

# X is covariate matrix
X <- dt[, c(
  
  # Sex, age, dual eligibility, RTI race code
  "sex",
  "(64,74]", "(74,84]", "(84,Inf]",
  "dual", 
  "white", "black", "other", "asian", "native", "hispanic",  
  
  # # area-level potential confounders
  # "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
  # "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
  # "pct_blk", "pct_hispanic",
  # "summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax",
  
  # some prevalent chronic conditions
  "hypert_ever", "ischmcht_ever", "diabetes_ever", "chrnkidn_ever"
  
)] |> as.matrix()

# y is outcome
y <- ifelse(dt[,dead_lead], 1, 0)

# var_group tells us the grouping of the variables (which ones must be selected together)
colnames(X)
var_group <- c(1, 2, 2, 2, 3, 
               4, 4, 4, 4, 4, 4,
               5, 5, 5, 5)

# group lasso
group_lasso <- gglasso(X, y, lambda = 0.2,
              group = var_group, 
              loss="ls",
              intercept = F)

# results
summary(group_lasso)
group_lasso$beta



