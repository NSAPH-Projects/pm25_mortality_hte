
########################################################
### Heterogeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Merge dataset
########################################################

#---------- Data References ----------#

# 27 chronic conditions determined by previous hospitalizations in reference period
#  See Chronic Conditions Warehouse (CCW) algorithms
#  References: https://github.com/NSAPH-Data-Processing/ccw_proxy
#  https://www2.ccwdata.org/documents/10280/19139421/ccw-chronic-condition-algorithms.pdf

# Area-level confounders (from the American Community Survey)
#  https://github.com/NSAPH/National-Causal-Analysis/tree/master/Confounders

# PM2.5 exposure data: 
#  https://pubmed.ncbi.nlm.nih.gov/31272018/

# Medicare denominator
#  FFS only

# Urbanization levels
#  NCHS Urban-Rural Classification Scheme for Counties
#  https://www.cdc.gov/nchs/data-analysis-tools/urban-rural.html

# Note: rolling windows here are referred to as three years (hosp, exposure, outcome)
# instead of two years (exposure, outcome)


#---------- Load Libraries ----------#

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(tidyr)
library(readxl)
library(dplyr)


# # Setup ---------------------
# 
# # snippets of code below taken from Daniel Mork
# # /n/dominici_nsaph_l3/Lab/projects/dmork_dataverse_adrd_first_hosp_denom/1. Medicare FFS enrollment.R
# 
# options(stringsAsFactors = FALSE)
# 
# # read denominator files
# # this is the full denominator (not just FFS)
# # /n/dominici_nsaph_l3/Lab/projects/analytic/
# # I need area-level confounders eventually, but this is too much data to load all at once
# f <- list.files("data/raw/denom_by_year/", pattern = "\\.fst", full.names = TRUE)
# vars <- c("qid", "year", "hmo_mo", "zip", "age", "race", "sex", "dual", "dead", "statecode"#,
#           # "poverty", "popdensity", "medianhousevalue", "pct_blk", "medhouseholdincome",
#           # "pct_owner_occ", "hispanic", "education", "smoke_rate", "mean_bmi",
#           #"summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax"
#           )
# dt <- rbindlist(lapply(f[2:18], read_fst,
#                        columns = vars,
#                        as.data.table = TRUE))
# 
# gc()
# 
# setkey(dt, qid, year)
# 
# gc()
# 
# # require individuals to be in FFS the whole time
# # nrow(dt[hmo_mo != 0]) # known race and sex
# dt <- dt[hmo_mo == 0 & # retain FFS only (zero HMO months)
#            race != 0 & sex != 0] # known race and sex
# dt <- dt[, hmo_mo := NULL]
# dt <- unique(dt, by = c("qid", "year")) # remove any duplicates
# 
# # define entry year (first year in FFS)
# dt[, first_year_ffs := first(year), by = qid]
# # function to get last year of continuous enrollment in Medicare FFS
# last_yr_cont <- function(year) {
#   year[first(which(!(first(year):2017 %in% year))) - 1] }
# # define last year continuously enrolled
# dt[, last_year_ffs := last_yr_cont(year), by = qid]
# # remove records for any years after first departure from FFS
# dt <- dt[year <= last_year_ffs]


#----- get FFS denominator -----#

# using old path because new path doesn't have county
dir_denom <- "/n/dominici_nsaph_l3/Lab/projects/analytic/mbsf_medpar_denom/"
#dir_denom <- "/n/dominici_nsaph_l3/Lab/lego/medicare/mbsf_medpar_denom/"
cols <- c("bene_id", "year", "zip", "zcta", "county", "sex", "race", "age_dob", "state", "dual", "yod")

denom <- list()

# needs to be 2000-2018 so I can get first and last year in FFS
cat("Loading denominator files \n")
for (y in as.character(2000:2018)) {
  cat(y, " ")
  f <- paste0(dir_denom, "mbsf_medpar_denom_", y, ".parquet")
  #f <- paste0(dir_denom, "denom_", y, ".parquet")
  
  # confirmed that this is FFS only
  denom[[y]] <- as.data.table(read_parquet(f, col_select = all_of(cols)))
  #denom[[y]] <- as.data.table(read_parquet(f))
  
  # set column types as integer
  denom[[y]][, zip := as.integer(zip)]
  denom[[y]][, zcta := as.integer(zcta)]
  denom[[y]][, county := as.integer(county)]

}

# get into a single data.table with a column for year
dt <- rbindlist(denom)

#ids <- sample(unique(dt$bene_id), 10000)
#m <- dt[bene_id %in% ids]
#dt <- m

# remove years that are greater than year of death 
# there are ~10,000 rows like this (issue with raw files)
dt <- dt[(dt$year <= dt$yod) | is.na(yod)]

# remove rows where individuals are 100+
dt <- dt[age_dob <= 100]

# remove people with unknown sex or race
dt <- dt[sex != 0]
dt <- dt[race != 0]

# define age groups
dt[, age_grp := cut(age_dob, breaks = c(64, 74, 84, Inf))]

# # first year in FFS
# dt[, first_year_ffs := first(year), by = bene_id]
# #dt[, last_year_ffs := last(year), by = bene_id]

# function to get last year of continuous enrollment in Medicare FFS
last_yr_cont <- function(year) {
  year[first(which(!(first(year):2019 %in% year))) - 1] }
# define last year continuously enrolled
dt[, last_year_ffs := last_yr_cont(year), by = bene_id]
# remove records for any years after first departure from FFS
dt <- dt[year <= last_year_ffs]



#----- get appropriate lag/lead columns for 2-year rolling windows -----#

# I will keep the exposure column (year 1) since we want year 1 covariates

# # get outcome (death) for the following year
# dt[, dead_lead := shift(dead, n = 1, fill = NA, type = "lead"), by = bene_id]
# ^ doesn't work--there is no "dead" column!
# remove rows where we don't have data for the following year 
# # (since the study design has year 1 = exposure, year 2 = outcome) 
# dt <- dt[!is.na(dead_lead),]
# # now remove dead column, since I only care about dead_lead
# dt <- dt[, c("dead") := NULL]


### I don't need this! if they weren't in Medicare prev year, they won't be in hosp data 
# # indicator for indiv being in medicare in the previous year (meaning CCW is available for that year)
# dt[, prev_year := shift(year, n = 1, fill = NA, type = "lag"), by = bene_id]
# # remove rows where this is NA (must have been in Medicare the previous year)
# dt <- dt[!is.na(prev_year)]
# # and remove this column, because the values aren't meaningful anymore
# dt <- dt[, prev_year := NULL]

# # Make a new variable for year of death
# death_year <- dt[(dead_lead == 1), .(death_year = year + 1), by = qid]
# dt <- merge(dt, death_year, by = "qid", all.x = TRUE)
# rm(death_year); gc()


#---------- Previous Hospitalizations (based on 27 CCW algorithms) ----------#

# Directory with hospitalization data
# dir_hosp <- "/n/dominici_nsaph_l3/Lab/projects/analytic/prev_hosp/"
dir_hosp <- "data/raw/prev_hosp/"

gc()

# conditions of interest
conditions <- paste0(c("hypoth", "ami",
                       "alzh", "alzhdmta",
                       "anemia", "asthma", "atrialfb", "hyperp", "breastCancer",
                       "colorectalCancer", "endometrialCancer", "lungCancer", "prostateCancer",
                       "cataract", "chrnkidn", "copd", "depressn", "diabetes", "glaucoma", "chf",
                       "hipfrac", "hyperl", "hypert", "ischmcht", "osteoprs", "ra_oa", "stroke"),
                     "_ever")

# Load each file and combine
prev_hosp <- rbindlist(lapply(2000:2016, function(y) {
  file_path <- paste0(dir_hosp, "prev_hosp_", y, ".parquet")
  read_parquet(file_path, col_select = c("bene_id", "rfrnc_yr", all_of(conditions)))
}))
  
# # doing this because hopefully it will use less memory
# prev_hosp <- data.table()
# hosp_years <- 2000:2016
# cat("Loading hospitalization files \n")
# # loop through years
# for (i in 1:length(hosp_years)) {
# 
#   cat(hosp_years[i], " ")
#   
#   # get file path
#   file_path <- paste0(dir_hosp, "prev_hosp_", hosp_years[i], ".parquet")
# 
#   # load that year into the list
#   one_year <- read_parquet(file_path,
#                            col_select = c("bene_id", "rfrnc_yr", all_of(conditions)))
#   gc()
#   setDT(one_year)
#   gc()
# 
#   # save into list
#   prev_hosp <- rbindlist(list(prev_hosp, one_year), fill = TRUE)
#   rm(one_year); gc()
# 
# }

# change _ever columns so they are 1/0 instead of FALSE/TRUE
prev_hosp[, (conditions) := lapply(.SD, function(x) ifelse(is.na(x), 0, 1)), .SDcols = (conditions)]


#---------- Merge with Previous Hospitalizations ----------#

# take the year column and add 1 
# (since we're interested in hospitalizations that occurred prior to exposure)
prev_hosp[, year := rfrnc_yr + 1]
prev_hosp[, rfrnc_yr := NULL]

# set keys prior to merge
setkey(dt, bene_id, year)
setkey(prev_hosp, bene_id, year)

# Merge by beneficiary ID
gc()
cat("Merging denominator and hospitalization files \n")
all_hosp <- merge(dt, prev_hosp, 
                  by = c("bene_id", "year"))

rm(dt, prev_hosp); gc()

# first_year_ffs
# may be a partial year that isn't captured in prev_hosp --> 
# then we will want to start in the following year
all_hosp[, first_year_ffs := first(year) - 1, by = bene_id]


#---------- Load confounders ----------#

# load saved data from other script
covar <- readRDS("data/intermediate/covariates.rds")


#---------- Merge Hospitalizations with ZCTA-level covariates ----------#

# set keys prior to merge
setkey(all_hosp, zcta, year)
setkey(covar, zcta, year)

# Merge hospitalizations with confounders
cat("Merging health and covariate files \n")
# dt <- merge(all_hosp, conf, 
#             by = c("zcta", "year"), 
#             all.x = TRUE)
dt <- all_hosp[covar, on = .(zcta, year), nomatch = 0, allow.cartesian = TRUE]
rm(all_hosp, covar); gc()

gc()


#---------- Get US Census Bureau Divisions ----------#

state_to_div <- data.table(
  state = c(
    "ME", "VT", "NH", "MA", "CT", "RI",
    "NY", "NJ", "PA", 
    "WI", "IL", "MI", "IN", "OH",
    "ND", "SD", "NE", "KS", "MO", "IA", "MN",
    "WV", "VA", "NC", "SC", "GA", "DE", "DC", "FL", "MD",
    "KY", "TN", "AL", "MS",
    "AR", "LA", "OK", "TX",
    "MT", "WY", "CO", "NM", "ID", "UT", "NV", "AZ",
    "WA", "OR", "CA", "AK", "HI"
  ),
  census_region = c(
    rep("Northeast", 9),
    rep("Midwest", 12),
    rep("South", 17),
    rep("West", 13)
  ),
  census_div = c(
    rep("New England", 6),
    rep("Middle Atlantic", 3),
    rep("East North Central", 5),
    rep("West North Central", 7),
    rep("South Atlantic", 9),
    rep("East South Central", 4),
    rep("West South Central", 4),
    rep("Mountain", 8),
    rep("Pacific", 5)
  )
)

# set keys prior to merge
setkey(dt, state)
setkey(state_to_div, state)

# Merge with all data
cat("Merging data with region and division indicators \n")
dt <- merge(dt, state_to_div, by = "state")

rm(state_to_div); gc()

# # Convert categorical division to indicator variables
# div_indicators <- dcast(dt, qid ~ census_div, fun.aggregate = length)
# dt <- merge(dt, div_indicators, by = "qid", all.x = TRUE)
# rm(div_indicators); gc()
# 
# # Convert categorical region to indicator variables
# region_indicators <- dcast(dt, qid ~ census_region, fun.aggregate = length)
# dt <- merge(dt, region_indicators, by = "qid", all.x = TRUE)
# rm(region_indicators); gc()


#---------- Make all variables binary or continuous (not categorical) ----------#

#--- Health conditions

# only need this if using reference periods instead of _ever

# conditions <- c("alzhdmta", "alzh",
#                 "hypoth", "ami", "anemia", "asthma", "atrialfb", "hyperp", "breastCancer", 
#                 "colorectalCancer", "endometrialCancer", "lungCancer", "prostateCancer", 
#                 "cataract", "chrnkidn", "copd", "depressn", "diabetes", "glaucoma", "chf", 
#                 "hipfrac", "hyperl", "hypert", "ischmcht", "osteoprs", "ra_oa", "stroke")
# 
# # Chronic condition present if code is:
# #    1 (beneficiary met claims criteria but did not have sufficient FFS coverage) and 
# #    3 (beneficiary met claims criteria and had sufficient FFS coverage) 
# # Must be 0/1 to work with CRE
# dt[, (conditions) := lapply(.SD, function(x) ifelse(x == 1 | x == 3, 1, 0)), 
#         .SDcols = conditions]


#--- Race

# indicator variables are helpful here so I can make white the reference

# # Create indicator variables
# race_indicators <- dcast(dt, qid ~ race, fun.aggregate = length)

# make race a factor
dt[, race := factor(race)]

# Create a list of dummy columns using lapply
race_indicators <- sapply(levels(dt$race), function(level) as.integer(dt$race == level)) |>
  as.data.frame()

# Change race indicator column names
names(race_indicators) <- c(paste0("race_", c("white", "black", "other", "asian", "hispanic", "native")))
# setnames(race_indicators,
#          old = colnames(race_indicators),
#          new = paste0("race_", c("white", "black", "other", "asian", "hispanic", "native")))

# Bind indicators to original data.table
dt <- cbind(dt, race_indicators)
rm(race_indicators); gc()


#--- Sex

# # Make sex 0/1 (1 is male)
# dt[,sex := ifelse(sex == 2, 0, sex)]


# #---------- Age indicator variables
# 
# age_grp_indicators <- dcast(dt, qid ~ age_grp, fun.aggregate = length)
# 
# # Bind indicators to original data.table
# dt <- merge(dt, age_grp_indicators, by = "qid")
# rm(age_grp_indicators); gc()

# I don't need group indicators (using continuous age)
# if I decide to do indicators, see new race indicators

#---------- Dual indicator 

# # make sure it's an integer 
# dt[, dual := as.integer(dual)]


#---------- Binary indicator for death

# remove rows where the current year is equal to the last year in FFS
# because last year in FFS can't be year 1 in two-year study period
dt <- dt[(year != last_year_ffs)]

# if year + 1 = yod: died_next_year = 1, otherwise 0
dt[, died_next_year := as.integer(year + 1 == yod)]

# if died_next_year is missing (time of death is unknown), but we know they were in Medicare 
# the following year, we know they didn't die in the following year
dt[, died_next_year := fifelse(is.na(yod) & (year + 1 <= last_year_ffs), 0, died_next_year)]




# # get binary indicator for death in a given year
# dt[, died_year1 := as.integer(year == yod)]
# 
# # shift to following year to get death in year 2
# dt[, dead_year2 := shift(died_year1, n = 1, fill = NA, type = "lead"), by = bene_id]

# # if yod is NA, died will also be NA
# # but we know that a beneficiary is still alive if they aren't in their last year of FFS
# # but if the beneficiary was in Medicare the next year, we know they didn't die (keep these rows)
# dt[, died := ifelse(is.na(yod) & year < last_year_ffs, 0, NA)]

# # remove rows where death in the following year is unknown 
# dt <- dt[!is.na(died_next_year),]


#---------- Urban/rural indicator 

# read in raw data
ur <- read_excel("data/raw/urban_rural/NCHSURCodes2013.xlsx")

# columns of interest
ur <- ur %>%
  select(c("FIPS code", "2013 code")) %>%
  rename(county = `FIPS code`,
         ur_class = `2013 code`)

# make county integer (previously numeric)
ur$county <- as.integer(ur$county)

# get binary classification
ur <- ur %>%
  mutate(urban = ifelse(ur_class %in% c(1,2,3,4), TRUE, FALSE))

# set keys prior to merge
setDT(ur)
setkey(dt, county)
setkey(ur, county)

# merge with data
cat("Merging urban/rural indicators with data \n")
dt <- merge(dt, ur, by = c("county"))


#---------- Other variables ----------#

# count number of times each individual appears in the dataset
dt[, n_windows := .N, by = bene_id]

# get year of follow-up for each year within each individual
dt[, year_follow := year - first_year_ffs]


#---------- Missing Data ----------#

# Check all columns except those with _ever (NA means never hospitalized)
cols_to_check <- names(dt)[grep("_ever", names(dt), invert = TRUE)]
cols_to_check <- cols_to_check[!cols_to_check %in% c("yod")]

# What % of rows are complete?
cc <- 100 * nrow(dt[complete.cases(dt[, ..cols_to_check])]) / nrow(dt) # 97%
cat(cc, "% of rows complete")

# what % are missing from each column?
names(dt)
dt[, lapply(.SD, function(x) 100 * sum(is.na(x)) / length(x)), .SDcols = cols_to_check]

# Remove rows with missing data
dt <- dt[complete.cases(dt[, ..cols_to_check])]


#---------- Visual check ----------#

head(dt, 100)

#---------- Save Data ----------#

# # Order by region
# setorder(dt, cols = census_region)

# Save
saveRDS(dt, file = "data/intermediate/rolling_cohort.rds")

# also save a small random sample of data
id_mini <- sample(unique(dt[,bene_id]), 10000, replace = FALSE)
dt_mini <- dt[bene_id %in% id_mini,]
saveRDS(dt_mini, file = "data/intermediate/rolling_cohort_10000.rds")

