
########################################################
### Mortality CRE project
### Author: Lauren Mock
### Get proportion for each subpopulation
########################################################

library(data.table)
library(fst)
library(stringr)
library(tidyr)
library(dplyr)

# Load data
dt <- readRDS("data/intermediate/rolling_cohort.rds")
# dt <- readRDS("data/intermediate/rolling_cohort_10000.rds")


#---- prepare data

# CCW indicator columns
ccw_names <- names(dt)[str_detect(names(dt), "ever")]

# # new column that = 1 for individuals with no previous hosp (all other indicators = 0)
# dt[, nohosp := as.integer(rowSums(.SD) == 0), .SDcols = ccw_names]

# new column that = 1 for all individuals
dt[, fullpop := 1]

# List of binary indicator column names
#subpops <- c(ccw_names, "nohosp", "fullpop")
subpops <- c(ccw_names, "fullpop")

#---- calculate prevalence

# what % of people in the cohort contributed to each subgroup?

prev_dt <- dt[year == last_year_ffs - 1, lapply(.SD, function(x) sum(x) / .N * 100),
              .SDcols = subpops] |>
  gather(key = "cond_abbr", value = "prevalence") |>
  mutate(cond_abbr = str_remove(cond_abbr, "_ever"))


saveRDS(prev_dt, 
        file = paste0("results/subgroup_prevalence.rds"))

