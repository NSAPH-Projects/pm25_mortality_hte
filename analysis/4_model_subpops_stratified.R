
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Discrete time logistic regression
########################################################

library(data.table)
library(fst)
library(stringr)

# # Load data
# dt <- readRDS("data/intermediate/rolling_cohort.rds")

# SAMPLE FOR NOW TO TEST ARRAY
load("data/intermediate/rolling_cohort_1000.RData")


#######################################################################

# get task ID from the sbatch array
args <- commandArgs(trailingOnly = TRUE)
ccw_idx <- as.numeric(args[1])
# ccw_idx = 1

# CCW indicator columns
ccw_names <- names(dt)[str_detect(names(dt), "ever")]

# get two new columns

# new column that = 1 for individuals with no previous hosp (all other indicators = 0)
dt[, nohosp := as.integer(rowSums(.SD) == 0), .SDcols = ccw_names]

# new column that = 1 for all individuals
dt[, fullpop := 1]


# List of binary indicator column names
subpops <- c(ccw_names, "nohosp", "fullpop")

# get column name for current subpop 
ccw_current <- subpops[ccw_idx]

# filter the data to the subpopulation where the selected indicator equals 1
subpop <- dt[dt[[ccw_current]] == 1, ]

# print subpop for this job
str_remove(ccw_current, "_ever")


#######################################################################

# time discrete logistic regression model

outcome_fit <- glm(dead_lead ~ 
                     
                     # exposure
                     pm25 +
                     
                     # calendar year
                     as.factor(year) +
                     
                     # year of follow-up
                     as.factor(year_follow) +
                     
                     # census division
                     as.factor(census_region) +
                     
                     # sex, age, race, Medicaid eligibility
                     sex + age + dual + 
                     race_black + race_other + race_asian + race_hispanic + race_native +
                     
                     # area-level covariates
                     poverty + popdensity + medianhousevalue + medhouseholdincome +
                     pct_owner_occ + education + smoke_rate + mean_bmi +
                     pct_blk + pct_hispanic +
                     summer_tmmx + summer_rmax + winter_tmmx + winter_rmax , 
                   
                   family = binomial(), 
                   data = subpop)

# print summary with coefficients
summary(outcome_fit)$coefficients

# save model coefficients
# not using scratch space because I'm only saving coefficients
saveRDS(summary(outcome_fit)$coefficients, 
        file = paste0("results/subpop/coeff_", str_remove(ccw_current, "_ever"), ".rds"))

