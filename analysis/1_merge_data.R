
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Merge dataset
########################################################

#---------- Data References ----------#

# 27 chronic conditions determined by previous hospitalizations in reference period
#  See Chronic Conditions Warehouse (CCW) algorithms
#  References: https://github.com/NSAPH-Data-Processing/ccw_proxy
#  https://www2.ccwdata.org/documents/10280/19139421/ccw-chronic-condition-algorithms.pdf

# Area-level confounders
#  https://github.com/NSAPH/National-Causal-Analysis/tree/master/Confounders

# PM2.5 exposure data: 
#  https://pubmed.ncbi.nlm.nih.gov/31272018/

# Denominator
#  includes FFS & Medicare advantage, but I restricted to FFS here (hmo = 0)

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
library(arrow)
library(readxl)
library(dplyr)


# Setup ---------------------

# snippets of code below taken from Daniel Mork
# /n/dominici_nsaph_l3/Lab/projects/dmork_dataverse_adrd_first_hosp_denom/1. Medicare FFS enrollment.R

options(stringsAsFactors = FALSE)

# read denominator files
# this is the full denominator (not just FFS)
# /n/dominici_nsaph_l3/Lab/projects/analytic/
# I need area-level confounders eventually, but this is too much data to load all at once
f <- list.files("data/raw/denom_by_year/", pattern = "\\.fst", full.names = TRUE)
vars <- c("qid", "year", "hmo_mo", "zip", "age", "race", "sex", "dual", "dead", "statecode"#,
          # "poverty", "popdensity", "medianhousevalue", "pct_blk", "medhouseholdincome",
          # "pct_owner_occ", "hispanic", "education", "smoke_rate", "mean_bmi",
          #"summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax"
          )
dt <- rbindlist(lapply(f[2:18], read_fst,
                       columns = vars,
                       as.data.table = TRUE))

gc()

setkey(dt, qid, year)

gc()

# require individuals to be in FFS the whole time
# nrow(dt[hmo_mo != 0]) # known race and sex
dt <- dt[hmo_mo == 0 & # retain FFS only (zero HMO months)
           race != 0 & sex != 0] # known race and sex
dt <- dt[, hmo_mo := NULL]
dt <- unique(dt, by = c("qid", "year")) # remove any duplicates

# get first and last year in Medicare
# not necessarily continuously enrolled or in FFS
#dt[, first_year_medicare := first(year), by = qid]
#dt[, last_year_medicare := last(year), by = qid]

# define entry year (first year in FFS)
dt[, first_year_ffs := first(year), by = qid]
# function to get last year of continuous enrollment in Medicare FFS
last_yr_cont <- function(year) {
  year[first(which(!(first(year):2017 %in% year))) - 1] }
# define last year continuously enrolled
dt[, last_year_ffs := last_yr_cont(year), by = qid]
# remove records for any years after first departure from FFS
dt <- dt[year <= last_year_ffs]


#----- get appropriate lag/lead columns -----#

# I will keep the exposure column (year 2) since we want year 2 covariates

# get outcome (death) for the following year
dt[, dead_lead := shift(dead, n = 1, fill = NA, type = "lead"), by = qid]
# make death 0/1 instead of TRUE/FALSE
dt[, dead_lead := ifelse(dead_lead, 1, 0)]
# remove rows where we don't have data for the following year 
dt <- dt[!is.na(dead_lead),]

# indicator for indiv being in medicare in the previous year (meaning CCW is available for that year)
dt[, prev_year := shift(year, n = 1, fill = NA, type = "lag"), by = qid]
# remove rows where this is NA (must have been in Medicare the previous year)
dt <- dt[!is.na(prev_year)]
# and remove this column, because the values aren't meaningful
dt <- dt[, prev_year := NULL]

# corrected age (in case of inconsistencies)
dt[, age := first(age) + year - first(year), by = qid]


#---------- Death (Outcome) and Denominator ----------#

# Make a new variable for year of death
death_year <- dt[(dead_lead == 1), .(death_year = year + 1), by = qid]
dt <- merge(dt, death_year, by = "qid", all.x = TRUE)
rm(death_year); gc()

# Remove dead column, since I only care about dead_lead
dt <- dt[, c("dead") := NULL]

# Remove rows where individuals are 100+
dt <- dt[age <= 100]

# Define age groups
dt[, age_grp := cut(age, breaks = c(64, 74, 84, Inf))]

# Merge RTI race code ---------------------
# we could still do this only in years for which its available

# # available 2009-2014 and 2016 in current data
# rti <- rbindlist(lapply(c(2010), function(y) {
#   d <- fread(paste0("data/raw/auxiliary_medicare_cols/rti_race_", y, ".csv"))
#   d[, year := y]
# }), fill = TRUE)
# setkey(rti, qid, year)
# rti <- rti[!is.na(rti_race_cd) & rti_race_cd != 0 & rti_race_cd != "X"]
# dt <- merge(dt, rti, by = c("qid"), all.x = TRUE)
# dt[is.na(rti_race_cd) | rti_race_cd == "", rti_race_cd := race]
# rm(rti); gc()


#---------- Previous Hospitalizations (based on 27 CCW algorithms) ----------#

# Directory with hospitalization data
#dir_hosp <- "/n/dominici_nsaph_l3/Lab/projects/analytic/prev_hosp/"

gc()

# # Load each file and combine
# prev_hosp <- rbindlist(lapply(2000:2014, function(y) {
#   #read_parquet(paste0("data/raw/prev_hosp/prev_hosp_", y, ".parquet"))
#   file_path <- paste0("data/raw/prev_hosp/prev_hosp_", y, ".parquet")
#   read_parquet(file_path, col_select = c("bene_id", "rfrnc_yr", conditions))
# }))

# conditions of interest
conditions <- paste0(c("hypoth", "ami",
                       "alzh", "alzhdmta",
                       "anemia", "asthma", "atrialfb", "hyperp", "breastCancer",
                       "colorectalCancer", "endometrialCancer", "lungCancer", "prostateCancer",
                       "cataract", "chrnkidn", "copd", "depressn", "diabetes", "glaucoma", "chf",
                       "hipfrac", "hyperl", "hypert", "ischmcht", "osteoprs", "ra_oa", "stroke"),
                     "_ever")
  

# doing this because hopefully it will use less memory
prev_hosp <- data.table()
hosp_years <- 2000:2014
# loop through years
for (i in 1:length(hosp_years)) {
  
  # get file path
  file_path <- paste0("data/raw/prev_hosp/prev_hosp_", hosp_years[i], ".parquet")
  
  # load that year into the list
  one_year <- read_parquet(file_path,
                           col_select = c("bene_id", "rfrnc_yr", all_of(conditions)))
  gc()
  setDT(one_year)
  gc()
  
  # save into list
  prev_hosp <- rbindlist(list(prev_hosp, one_year), fill = TRUE)
  rm(one_year); gc()
  
}

# change _ever columns so they are 0/1
prev_hosp[, (conditions) := lapply(.SD, function(x) ifelse(is.na(x), 0, 1)), .SDcols = (conditions)]


#---------- Merge with Previous Hospitalizations ----------#

# take the year column and add 1 
# (since we're interested in hospitalizations that occurred prior to exposure)
prev_hosp[, rfrnc_yr := rfrnc_yr + 1]

# Merge by beneficiary ID
all_hosp <- merge(dt, prev_hosp, by.x = c("qid", "year"), 
                  by.y = c("bene_id", "rfrnc_yr"), all.x = TRUE)

rm(dt, prev_hosp); gc()


#---------- ZIP Exposures and Confounders ----------#

# Data directories
# dir_exposures <- "/n/dominici_lab_ro/lab/data/exposures/exposure/"
# dir_confounders <- "/n/dominici_nsaph_l3/Lab/projects/analytic/confounders/"
# dir_temp <- "/n/dominici_nsaph_l3/Lab/data/gridmet_flat/"

# pm25_df <- rbindlist(lapply(2001:2015, function(y) {
#   dat <- readRDS(paste0("data/raw/pm25/PM25_v2/annual/", y, ".rds"))
#   dat <- dat[,c("ZIP", "pm25")]
#   dat$year <- y
#   dat
# }))
# 
# setDT(pm25_df)
# 
# # Make ZIP integers
# pm25_df[, ZIP := as.integer(ZIP)]

# Load zip code covars
zip_yr_df <-
  rbindlist(lapply(2001:2015, function(y) {
    readRDS(paste0("data/raw/confounders/aggregate_data_", y, ".rds"))
  }))

# Select the columns I need
zip_yr_df <- zip_yr_df[, c("zip", "year", "pm25", "mean_bmi", "smoke_rate",
                           "hispanic", "pct_blk", "medhouseholdincome", "medianhousevalue",
                           "poverty", "education", "popdensity", "pct_owner_occ",
                           "summer_tmmx", "winter_tmmx", "summer_rmax", "winter_rmax")]

# make zip an integer
zip_yr_df[, zip := as.integer(zip)]

gc()


#---------- Merge Hospitalizations with ZIP Confounders ----------#

# make sure zip is integer 
all_hosp[, zip := as.integer(zip)]

# Merge hospitalizations with ZIP confounders
dt <- merge(all_hosp, zip_yr_df, by = c("zip", "year"), all.x = TRUE)
rm(all_hosp, zip_yr_df); gc()

gc()


#---------- Get US Census Bureau Divisions ----------#

state_to_div <- data.table(
  statecode = c(
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

# Merge with all data
dt <- merge(dt, state_to_div, by = "statecode")

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

# change name of pct hispanic column
setnames(dt, "hispanic", "pct_hispanic")

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

# Make sex 0/1 (1 is male)
dt[,sex := ifelse(sex == 2, 0, sex)]


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

# make sure it's an integer 
dt[,dual := as.integer(dual)]


#---------- Other variables ----------#

# count number of times each individual appears in the dataset
dt[, n_windows := .N, by = qid]

# get year of follow-up for each year within each individual
dt[, year_follow := year - first_year_ffs]


#---------- Urban/rural indicator ----------#

#----- get urban/rural indicator

ur <- read_excel("data/raw/urban_rural/NCHSURCodes2013.xlsx")

# columns of interest
ur <- ur %>%
  select(c("FIPS code", "2013 code")) %>%
  rename(county = `FIPS code`, 
         ur_class = `2013 code`)

# get binary classification
ur <- ur %>%
  mutate(urban = ifelse(ur_class %in% c(1,2,3), TRUE, FALSE))


#----- get counties in dataset

# load zip to county crosswalk
zip2county <- read.csv("/n/dominici_lab/lab/data/exposures/exposure/zip2county_master_xwalk/zip2county_master_xwalk_2010_2023_tot_ratio_one2one.csv")

# get columns of interest
zip2county <-zip2county |>
  filter(year == "2015") |>
  select(zip, county)

# make sure zip is integer for both
zip2county$zip <- as.integer(zip2county$zip)
dt[, zip := as.integer(zip)]

# merge with dt
dt <- merge(dt, zip2county, by = "zip", all.x = TRUE)


#----- merge dataset with urban/rural indicators

dt <- merge(dt, ur, by = "county", all.x = TRUE)


#---------- Missing Data ----------#

# Check all columns except those with _ever (NA means never hospitalized)
cols_to_check <- names(dt)[grep("_ever", names(dt), invert = TRUE)]
cols_to_check <- cols_to_check[!cols_to_check %in% c("death_year")]
#cols_to_check <- cols_to_check[!cols_to_check %in% conditions]

# What % of rows are complete?
nrow(dt[complete.cases(dt[, ..cols_to_check])]) / nrow(dt) # 97%

# what % are missing from each column?
#dt[, lapply(.SD, function(x) sum(is.na(x)) / length(x)), .SDcols = cols_to_check]

# Remove rows with missing data
dt <- dt[complete.cases(dt[, ..cols_to_check])]


cat("Column names:")
names(dt)


#---------- Save Data ----------#

# # Order by region
# setorder(dt, cols = census_region)

# Save
saveRDS(dt, file = "data/intermediate/rolling_cohort.rds")

# also save a small sample of data
id_mini <- sample(dt[,qid], 1000, replace = FALSE)
dt_mini <- dt[qid %in% id_mini,]
saveRDS(dt_mini, file = "data/intermediate/rolling_cohort_1000.rds")

