
########################################################
### Heterogeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Merge ZCTA-level covariates
########################################################

library(data.table)
library(fst)
library(arrow)
library(lubridate)
library(tidyr)
library(arrow)
library(readxl)
library(dplyr)


#---------- Confounders: 2001-2016 ----------#

# full path for 2001-2016 confounders:
# /n/dominici_nsaph_l3/Lab/projects/analytic/confounders/

# Load zip code covars
covar_2001_2016 <- rbindlist(lapply(2001:2016, function(y) {
    readRDS(paste0("data/raw/confounders/aggregate_data_", y, ".rds"))
  }))

# Select the columns I need
covar_2001_2016 <- covar_2001_2016[, c("zip", "year", "hispanic", "pct_blk", "medhouseholdincome", 
                                     "medianhousevalue", "poverty", "education", "pct_owner_occ",
                                     "mean_bmi", "smoke_rate", "popdensity")]

# excluding mean BMI, smoking rate, and popdensity for now (but I can use the urban/rural levels)

# make zip an integer
covar_2001_2016[, zip := as.integer(zip)]

# merge with zip2zcta crosswalk, then remove zip column (zcta only)
dir_xwalk <- "/n/dominici_nsaph_l3/Lab/exposure/zip2zcta_master_xwalk/"
zip2zcta <- read.csv(paste0(dir_xwalk, "zip2zcta_master_xwalk.csv"))[,1:2]
covar_2001_2016 <- right_join(zip2zcta, covar_2001_2016)
setDT(covar_2001_2016)

# make ZIP/ZCTA integer
covar_2001_2016[, zip := NULL]
covar_2001_2016[, zcta := as.integer(zcta)]

# get mean value across zip codes in each zcta
covar_2001_2016 <- covar_2001_2016[, lapply(.SD, mean, na.rm = TRUE), by = .(zcta, year)]

# change name of pct hispanic column
setnames(covar_2001_2016, "hispanic", "pct_hispanic")


#---------- Confounders: 2017 ----------#

# only available starting in 2009 (when source data is available)

# path to census variables for 2017
dir_covar_2017 <- "/n/dominici_nsaph_l3/Lab/lego/social/demographics__census/"

# core variables that apply to the full population
covar_2017 <- as.data.table(read_parquet(paste0(dir_covar_2017, 
                                               "core/zcta_yearly/acs5_2017.parquet")))
# variables for 65+ individuals only
covar_2017_65_plus <- as.data.table(read_parquet(paste0(dir_covar_2017, 
                                                     "65_plus/zcta_yearly/acs5_2017.parquet")))

# merge core and 65+ variables
covar_2017 <- left_join(covar_2017, covar_2017_65_plus)

# set zcta column types as integer
covar_2017[, zcta := as.integer(zcta)]

# get the variables I need as percentages
covar_2017 <- copy(covar_2017) # run this to avoid data.table warning below
covar_2017[, `:=` (
  pct_hispanic = pop_hispanic/population,
  pct_blk = pop_black/population,
  poverty = pop_65_plus_below_poverty_level/pop_over_65_years,
  education = pop_65_plus_high_school_not_attained/pop_over_65_years,
  pct_owner_occ = housing_owner_occupied / housing_units
)]

# # load PM2.5 data separately for 2017 (this variable isn't in the 2017 dataset)
# pm25_path <- "/n/dominici_nsaph_l3/Lab/exposure/satellite_pm25_raster2polygon/annual/"
# pm25_2017 <- as.data.table(read_parquet(paste0(pm25_path, "satellite_pm25_zcta_2017.parquet")))
# pm25_2017$zcta <- as.integer(pm25_2017$zcta)
# # join PM2.5 with other confounders for 2017
# conf_2017 <- left_join(conf_2017, pm25_2017, by = c("year", "zcta"))

# change some column names to match the 2001-2016 data
setnames(covar_2017, 
         old = c("median_household_income", "median_home_value"), 
         new = c("medhouseholdincome", "medianhousevalue"))

# just keep the variables I need
covar_2017 <- covar_2017[, .(zcta, year, pct_hispanic, pct_blk, medhouseholdincome, 
                           medianhousevalue, poverty, education, pct_owner_occ)]

# now create blank columns for mean BMI, smoking rate, and population density (in 2000-2016 data)
covar_2017[, mean_bmi := NA]
covar_2017[, smoke_rate := NA]
covar_2017[, popdensity := NA]


#---------- Bind 2001-2016 with 2017 ----------#

# bind this data to the 2000-2016 data
# just use rbind (once the columns all match)

covar <- rbind(covar_2001_2016, covar_2017)


#---------- For variables missing 2017, use 2016 ----------#

# mean BMI, smoking rate, population density

cols_missing_2017 <- c("mean_bmi", "smoke_rate", "popdensity")
covar[, (cols_missing_2017) := lapply(.SD, nafill, type = "locf"), 
     by = zcta, 
     .SDcols = cols_missing_2017]


#---------- Merge in PM2.5 (2001-2017) ----------#

pm25_path <- "/n/dominici_nsaph_l3/Lab/exposure/satellite_pm25_raster2polygon/annual/"
pm25 <- rbindlist(lapply(2001:2017, function(y) {
  file_path <- paste0(pm25_path, "satellite_pm25_zcta_", y, ".parquet")
  read_parquet(file_path)
}))
pm25$zcta <- as.integer(pm25$zcta)

# join PM2.5 with other confounders
covar <- left_join(covar, pm25, by = c("year", "zcta"))


#---------- Merge in temperature and humidity (2001-2017) ----------#

# load data (processed in analysis/get_gridmet_vars.R)
met <- read.csv("data/intermediate/temp_humidity_seasonal_zcta.csv")

# merge with zip_yr_df
covar <- merge(covar, met, by = c("zcta", "year"))


#---------- Save covariates ----------#

saveRDS(covar, "data/intermediate/covariates.rds")

