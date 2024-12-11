
# script to make Table 1

library(data.table)
library(fst)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(table1)
library(magrittr)
library(dplyr)
library(arrow)
library(xtable)

#####################################################

# Load data
dt <- readRDS("data/intermediate/rolling_cohort.rds")
# dt <- readRDS("data/intermediate/rolling_cohort_1000.rds")

# clean up data values
dt[ , `:=` (
  
  # use "--" to indicate where to indent later
  sex = factor(sex, levels = c(1, 0), 
               labels = c("--Male", "--Female")),
  dual = factor(dual, levels = c(0, 1), 
                labels = c("--Ineligible", "--Eligible")),
  age_grp = factor(age_grp, levels = c("(64,74]", "(74,84]", "(84,Inf]"), 
                    labels = c("--65-74", "--75-84", "--85+")),
  race = factor(race, levels = c(1,2,5,4,6,3),
                 labels = c("--White", 
                            "--Black",
                            "--Hispanic", 
                            "--Asian",
                            "--Native American",
                            "--Other")),
  census_region = factor(census_region, levels = c("Northeast", "Midwest", "South", "West"),
                          labels = c("--Northeast", "--Midwest", "--South", "--West")),
  urban = factor(urban, levels = c(TRUE, FALSE),
                  labels = c("--Urban", "--Rural")),
  
  # dead_lead = factor(dead_lead, levels = c(0, 1), 
  #                    labels = c("Survived", "Died")),
  
  # multiply % by 100
  poverty = poverty * 100,
  pct_owner_occ = pct_owner_occ * 100,
  education = education * 100,
  pct_blk = pct_blk * 100,
  pct_hispanic = pct_hispanic * 100,
  smoke_rate = smoke_rate * 100,
  
  # convert temp to F
  summer_tmmx = ((summer_tmmx - 273.15) * 1.8) + 32,
  winter_tmmx = ((winter_tmmx - 273.15) * 1.8) + 32,
  
  # divide house value and income by 10,000
  medianhousevalue = medianhousevalue / 10000,
  medhouseholdincome = medhouseholdincome / 10000,
  
  # divide pop density by 1,000
  popdensity = popdensity / 1000
  
)]


#---------- use table1 function ----------#

# get table 1 for covariates where I'll use only the year of entry
tab1_entry <- table1(~sex + age_grp + race + dual + census_region + urban,
               data = dt[, .SD[1], by = qid], # year of entry only!
               big.mark = ",", 
               render.continuous = c(.="Mean (SD)"))

# custom render function for mean(SD) with rounding
# for some reason, using this means that there aren't commas in N
custom_render <- function(x) {
  mean_val <- sprintf("%.1f", mean(x, na.rm = TRUE))
  sd_val <- sprintf("%.1f", sd(x, na.rm = TRUE))
  paste0(format(as.numeric(mean_val), big.mark = ","), " (", sd_val, ")")
}

# get table 1 for covariates where I want to average across all years
tab1_all_years <- table1(~poverty + popdensity + medianhousevalue + medhouseholdincome + 
                 pct_owner_occ + education + pct_blk + pct_hispanic + smoke_rate + mean_bmi +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax,
               data = dt, 
               #big.mark = ",", 
               #render.continuous = c(.="Mean (SD)"),
               render.continuous = custom_render)


#---------- get all values in a single data.frame ----------#

# get table1s as a dfs (need both of these lines! for some reason)
tab1_entry <- as.data.frame(tab1_entry)
tab1_entry <- as.data.frame(lapply(tab1_entry, as.character), 
                            stringsAsFactors = FALSE) # get rid of noquote
tab1_all_years <- as.data.frame(tab1_all_years)
tab1_all_years <- as.data.frame(lapply(tab1_all_years, as.character), 
                            stringsAsFactors = FALSE) # get rid of noquote

# rename columns so they're easier to work with
names(tab1_entry) <- c("variable", "value")
names(tab1_all_years) <- c("variable", "value")

# labels for # of people vs # of observations
tab1_entry[1,1] <- "Number of individuals"
tab1_all_years[1,1] <- "Number of observations"

# also get the total number of deaths
row_deaths <- data.frame(variable = "Number of deaths", 
                         value = format(dt[,sum(dead_lead)], big.mark = ","))

# bind rows (keeping overall counts at the top)
tab1 <- rbind(tab1_entry[1,], tab1_all_years[1,], row_deaths,
              tab1_entry[2:nrow(tab1_entry),], tab1_all_years[2:nrow(tab1_all_years),])
rm(tab1_entry, tab1_all_years); gc()

# trim white space around variable name (for joining later)
tab1$variable <- trimws(tab1$variable)


#---------- get PM2.5 values for categorical variables ----------#

# new column for total (to calculate PM2.5 overall in the loop)
dt[, all := "Number of observations"]

# subgroups for PM2.5 estimates
group_vars <- c("all", "sex", "age_grp", "race", 
                "dual", "census_region", "urban")

# initialize df
pm25_all_vars <- data.frame()

for(i in 1:length(group_vars)){
  
  # get PM2.5 for one group variable
  one_var <- dt[, 
                   .(pm25 = 
                       paste0(
                         sprintf("%.1f", mean(pm25, na.rm = TRUE)),
                         " (",
                         sprintf("%.1f", sd(pm25, na.rm = TRUE)),
                         ")"
                       )),
                   by = eval(group_vars[i])]
  
  names(one_var)[1] <- "variable"
  
  # bind to table with all variables
  pm25_all_vars <- rbind(pm25_all_vars, one_var)
}


# join PM2.5 column to table 1
tab1 <- left_join(tab1, pm25_all_vars, by = "variable")


#---------- clean up full table ----------#

# neaten up the variable names (now in rows)
tab1 <- tab1 %>%
  mutate(variable = recode(variable,
                           "age_grp" = "Age at entry",
                           "race" = "Race/ethnicity",
                           "sex" = "Sex",
                           "census_region" = "Region",
                           "dual" = "Medicaid",
                           "urban" = "Urbanicity",
                           "pm25" = "Mean annual PM\\(_{2.5}\\) (\\(\\mu g/m^3 \\))",
                           "poverty" = "% of 65+ population below poverty line",
                           "popdensity" = "Pop. density per 1,000 square miles",
                           "medianhousevalue" = "Median value of owner occupied properties (\\$10,000)",
                           "medhouseholdincome" = "Median household income (\\$10,000)",
                           "pct_owner_occ" = "% of housing units occupied by owner",
                           "education" = "% of 65+ population not graduating from high school",
                           "pct_blk" = "% of 65+ population Black",
                           "pct_hispanic" = "% of 65+ population Hispanic",
                           "smoke_rate" = "% of respondents in county who have ever smoked", 
                           "mean_bmi" = "Mean BMI of respondents in county",
                           "summer_tmmx" = "Summer average maximum temperature (°F)",
                           "winter_tmmx" = "Winter average maximum temperature (°F)",
                           "summer_rmax" = "Summer average maximum relative humidity (%)",
                           "winter_rmax" = "Winter average maximum relative humidity (%)"
  ))

# replace NA with " "
tab1$pm25 <- ifelse(is.na(tab1$pm25), " ", tab1$pm25)

# escape % for latex
tab1[] <- lapply(tab1, function(x) gsub("%", "\\\\%", x))

# indent (insert horizontal space) 
tab1[] <- lapply(tab1, function(x) gsub("--", "\\\\hspace{10pt}", x))

# clean up Ns
tab1$value[1:2] <- str_remove_all(tab1$value[1:2], pattern = "[()N=]")

# get commas (missing with continuous render function)
tab1[2,2] <- format(as.numeric(tab1[2,2]), big.mark = ",")

# rename columns (first row)
names(tab1) <- c(" ", "N (\\%)", "PM2.5 mean (SD)")

# print in latex
print(xtable(tab1,
             type = "latex"),
      file = "results/tables/table1.tex", 
      sanitize.text.function = identity,
      include.rownames = FALSE)


##############################################

# get info on age

paste0("mean age in exposure window: ", dt[,mean(age)])
paste0("SD age in exposure window: ", dt[,sd(age)])

# restrict to first year in dt
dt <- dt[, .SD[1], by = qid]

paste0("mean age at entry: ", dt[,mean(age)])
paste0("SD age at entry: ", dt[,sd(age)])


