
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Discrete time logistic regression
########################################################

library(data.table)
library(fst)
library(readr)

# # Load data
#dt <- read_rds("data/intermediate/rolling_cohort.rds")
load("data/intermediate/rolling_cohort.RData")


# # Work with a random sample for now
# set.seed(17)
# keep_idx <- sample(unique(dt[,qid]), 30000000, replace = FALSE)
# dt <- dt[qid %in% keep_idx,]
# gc()

#save(dt, file = "data/intermediate/rolling_cohort_1000.RData")

# load("data/intermediate/rolling_cohort_1000.RData")


# create new columns for grouped conditions
dt[, c(
  "group_cancer_ever", "group_metabolic_ever", "group_blood_ever", "group_mental_ever",
  "group_circulatory_ever", "group_respiratory_ever", 
  "group_urinary_ever", "group_skeletal_ever"
) := list(
  as.integer(breastCancer_ever == 1 | colorectalCancer_ever == 1 | endometrialCancer_ever == 1,
             lungCancer_ever == 1 | prostateCancer_ever == 1),
  as.integer(hypoth_ever == 1 | diabetes_ever == 1),
  as.integer(anemia_ever == 1),
  as.integer(alzh_ever == 1 | alzhdmta_ever == 1, depressn_ever == 1),
  as.integer(ami_ever == 1 | chf_ever == 1 | atrialfb_ever == 1 | hypert_ever == 1 | 
               ischmcht_ever == 1 | stroke_ever == 1 | hyperl_ever == 1),
  as.integer(asthma_ever == 1 | copd_ever == 1),
  as.integer(chrnkidn_ever == 1 | hyperp_ever == 1),
  as.integer(osteoprs_ever == 1 | ra_oa_ever == 1, hipfrac_ever == 1)
)]

#########################

# time discrete logistic regression model

outcome_fit <- glm(dead_lead ~ 
                     
                     # exposure
                     pm25 +
                     
                     # # calendar year
                     # as.factor(year) + 
                     # 
                     # # year of follow-up
                     # year_follow + 

                     # census division
                     #as.factor(census_region) +
                     
                     # sex, age, race, Medicaid eligibility
                     sex + age + #dual + 
                     race_black + race_other + race_asian + race_hispanic + race_native +

                     # # chronic conditions (all start with "group_")
                     # group_cancer_ever + group_metabolic_ever + group_blood_ever +
                     # group_mental_ever + group_circulatory_ever + group_respiratory_ever +
                     # group_urinary_ever + group_skeletal_ever +
                     # 
                     # # area-level covariates
                     # poverty + popdensity + medianhousevalue + medhouseholdincome +
                     # pct_owner_occ + education + smoke_rate + mean_bmi +
                     # pct_blk + pct_hispanic +
                     # summer_tmmx + summer_rmax + winter_tmmx + winter_rmax #+
                     
                     # interactions with race only
                    pm25:race_black + pm25:race_other + pm25:race_asian +
                     pm25:race_hispanic + pm25:race_native
                   
                     
                     # # individual-level one-way interactions (selected by lasso)
                     # pm25:group_cancer_ever +
                     # pm25:group_metabolic_ever +
                     # pm25:group_blood_ever +
                     # pm25:group_mental_ever +
                     # pm25:group_circulatory_ever +
                     # pm25:group_respiratory_ever +
                     # pm25:group_urinary_ever +
                     # pm25:group_skeletal_ever
                   , 
                   
                   family = binomial(), 
                   data = dt)
#save(outcome_fit, file = "results/outcome_model/all_no_interactions.RData")
#save(outcome_fit, file = "results/outcome_model/all_ccs_interactions.RData")

#save(outcome_fit, file = "results/outcome_model/ccw_cat_follow.RData") # already done
#save(outcome_fit, file = "results/outcome_model/ccw_no_follow.RData")
#save(outcome_fit, file = "results/outcome_model/ccw_cont_follow.RData")

#save(outcome_fit, file = "results/outcome_model/no_ccw_cat_follow.RData")
#save(outcome_fit, file = "results/outcome_model/no_ccw_no_follow.RData")
#save(outcome_fit, file = "results/outcome_model/no_ccw_cont_follow.RData")

#save(outcome_fit, file = "results/outcome_model/indiv_only.RData")
write_rds(outcome_fit, file = "results/outcome_model/indiv_race_interact_no_dual.rds")
#save(outcome_fit, file = "results/outcome_model/indiv_area_only.RData")

summary(outcome_fit)



