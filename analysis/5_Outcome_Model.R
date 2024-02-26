
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Discrete time logistic regression
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)

# # Load data
load("data/intermediate/rolling_cohort.RData")
# # Work with a random sample for now
# set.seed(17)
# keep_idx <- sample(unique(dt[,qid]), 1000, replace = FALSE)
# dt <- dt[qid %in% keep_idx,]
# save(dt, file = "data/intermediate/rolling_cohort_1000.RData")

#load("data/intermediate/rolling_cohort_1000.RData")



#########################

# time discrete logistic regression model

# then also make all of these interactions with PM2.5 and do all the zip-level vars (no interaction)

outcome_fit <- glm(dead_lead ~ 
                     
                     # exposure
                     pm25 +
                     
                     # calendar year and year of follow-up
                     as.factor(year) + as.factor(year_follow) + 
                     
                     # sex, age, race, Medicaid eligibility
                     sex + age + as.factor(race) + dual +
                     
                     # area-level covariates
                     poverty + popdensity + medianhousevalue + medhouseholdincome + 
                     pct_owner_occ + education + smoke_rate + mean_bmi +
                     summer_tmmx + summer_rmax + winter_tmmx + winter_rmax +
                     
                     # some one-way interactions
                     #pm25:sex + 
                     pm25:age + 
                     #pm25:as.factor(race) + 
                     pm25:dual, 
                   
                   family = binomial(), 
                   data = dt)

# save(outcome_fit, file = "results/outcome_model.RData")
# 
load("results/outcome_model.RData")

summary(outcome_fit)


# now make a quick little visualization to look at interaction terms



