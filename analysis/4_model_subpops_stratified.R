
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Discrete time logistic regression, stratified
########################################################

library(data.table)
library(fst)
library(stringr)
library(dplyr)

# in the command line, run something like:
# sbatch analysis/4_model_subpops_stratified_small.sbatch race_white
# to specify the variable to stratify on

# Load data
dt <- readRDS("data/intermediate/rolling_cohort.rds")
# dt <- readRDS("data/intermediate/rolling_cohort_1000.rds")


#######################################################################

# get task ID from the sbatch array
args <- commandArgs(trailingOnly = TRUE)
ccw_idx <- as.numeric(args[1])
# ccw_idx = 1

# get the name of the variable to stratify by
stratify_by <- args[2]
# stratify_by = "race"
# stratify_by = "dual"
# stratify_by = "old"
# stratify_by = "urban"


if(is.na(stratify_by)){
  message("Must specify stratifying variable, e.g.,
sbatch analysis/4_model_subpops_stratified.sbatch race_white")
}

# print slurm arguments
paste0("Subpop index: ", ccw_idx)
paste0("Stratifying variable: ", stratify_by)

#######################################################################

# new columns

# above/below median age
if(stratify_by == "old"){
  median_age <- dt[, median(age)]
  dt[, old := as.integer(age > median_age)]
}

# # above/below median urbanicity
# if(stratify_by == "urban"){
#   median_popdens <- dt[, median(popdensity)]
#   dt[, urban := as.integer(popdensity > median_popdens)]
# }

#######################################################################



# CCW indicator columns
ccw_names <- names(dt)[str_detect(names(dt), "ever")]

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

# get levels of the stratifying variable
strata <- unique(dt[, ..stratify_by])[[1]]


#######################################################################

# time discrete logistic regression model

# write out model formula
model_form <- 
  "dead_lead ~ 
  pm25 + as.factor(year) + as.factor(year_follow) + census_region +
  sex + age + dual + race_black + race_other + race_asian + race_hispanic + race_native +
  poverty + popdensity + medianhousevalue + medhouseholdincome +
  pct_owner_occ + education + smoke_rate + mean_bmi +
  pct_blk + pct_hispanic + summer_tmmx + summer_rmax + winter_tmmx + winter_rmax"

# remove the stratifying variable from the model
# this is necessary for character/factor variables--will throw an error
model_form <- str_remove(model_form, pattern = paste0(stratify_by, " \\+"))

# if stratifying by race, remove other race variable indicators
if(stratify_by == "race"){
  model_form <- "dead_lead ~ 
  pm25 + as.factor(year) + as.factor(year_follow) + census_region +
  sex + age + dual +
  poverty + popdensity + medianhousevalue + medhouseholdincome +
  pct_owner_occ + education + smoke_rate + mean_bmi +
  pct_blk + pct_hispanic + summer_tmmx + summer_rmax + winter_tmmx + winter_rmax"
}


# fit the model, looping through levels of the stratifying variable
for(i in 1:length(strata)){
  
  stratum <- strata[i]
  
  # print stratum
  paste0("Stratum: ", stratify_by, " = ", stratum)
  
  # filter to current stratum
  subpop_stratum <- subpop |>
    filter(!!sym(stratify_by) == stratum)
  
  outcome_fit <- glm(as.formula(model_form), family = binomial(), data = subpop_stratum)
  
  # print summary with coefficients
  summary(outcome_fit)$coefficients
 
  # save model coefficients
  # not using scratch space because I'm only saving coefficients
  saveRDS(summary(outcome_fit)$coefficients, 
          file = paste0("results/models/stratified/coeff_", str_remove(ccw_current, "_ever"), 
                        "_", "strat_", stratify_by, "_", stratum, ".rds"))
  
  rm(outcome_fit); gc()
  
}

