
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Discrete time logistic regression
########################################################

library(data.table)
library(fst)

# # Load data
load("data/intermediate/rolling_cohort.RData")

# # Work with a random sample for now
# set.seed(17)
# keep_idx <- sample(unique(dt[,qid]), 1000, replace = FALSE)
# dt <- dt[qid %in% keep_idx,]
# save(dt, file = "data/intermediate/rolling_cohort_1000.RData")

# load("data/intermediate/rolling_cohort_1000.RData")


#########################

# time discrete logistic regression model

# outcome_fit <- glm(dead_lead ~ 
#                      
#                      # exposure
#                      pm25 +
#                      
#                      # calendar year and year of follow-up
#                      as.factor(year) + as.factor(year_follow) + 
#                      
#                      # # census division
#                      as.factor(census_div) + 
#                      
#                      # sex, age, race, Medicaid eligibility
#                      sex + age + as.factor(race) + dual +
#                      
#                      # chronic conditions
#                      hypert_ever + ischmcht_ever + diabetes_ever + chrnkidn_ever +
#                      
#                      # area-level covariates
#                      poverty + popdensity + medianhousevalue + medhouseholdincome + 
#                      pct_owner_occ + education + smoke_rate + mean_bmi +
#                      summer_tmmx + summer_rmax + winter_tmmx + winter_rmax +
#                      
#                      # all individual-level one-way interactions
#                      pm25:age + pm25:sex + pm25:dual + pm25:as.factor(race) +
#                      pm25:hypert_ever + pm25:ischmcht_ever + 
#                      pm25:diabetes_ever + pm25:chrnkidn_ever, 
#                    
#                    family = binomial(), 
#                    data = dt)
# 
# save(outcome_fit, file = "results/outcome_model/model_all.RData")
# load("results/outcome_model/model_lasso_vars.RData")


outcome_fit <- glm(dead_lead ~ 
                     
                     # exposure
                     pm25 +
                     
                     # # calendar year and year of follow-up
                     # as.factor(year) + as.factor(year_follow) + 
                     
                     # # # census division
                     # as.factor(census_div) + 
                     
                     # sex, age, race, Medicaid eligibility
                     sex + age + 
                     #as.factor(race) + 
                     asian + hispanic + 
                     dual +
                     
                     # chronic conditions
                     hypert_ever + ischmcht_ever + diabetes_ever + chrnkidn_ever +
                     
                     # # area-level covariates
                     # poverty + popdensity + medianhousevalue + medhouseholdincome + 
                     # pct_owner_occ + education + smoke_rate + mean_bmi +
                     # summer_tmmx + summer_rmax + winter_tmmx + winter_rmax +
                     
                     # all individual-level one-way interactions
                     #pm25:age + 
                     pm25:sex + 
                     #pm25:dual + 
                     #pm25:as.factor(race) +
                     pm25:asian +
                     pm25:hispanic + 
                     pm25:hypert_ever + pm25:ischmcht_ever + 
                     pm25:diabetes_ever + pm25:chrnkidn_ever, 
                   
                   family = binomial(), 
                   data = dt)


#save(outcome_fit, file = "results/outcome_model/model_lasso_vars.RData")
load("results/outcome_model/model_lasso_vars.RData")



summary(outcome_fit)


# now make a quick visualization to look at interaction terms





