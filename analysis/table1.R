
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


#--- user-specified

# 2010 only or all person-time?
#obs_include <- "2010_only"
obs_include <- "all_pt"

#####################################################

# Load data
dt <- readRDS("data/intermediate/rolling_cohort.rds")

# # SAMPLE FOR NOW
# load("data/intermediate/rolling_cohort_1000.RData")


# take 2010 only (2010-2011 window)
if(obs_include == "2010_only"){
  dt <- dt[year == 2010,]
}


# change individual-level values
dt[, sex := factor(sex, levels = c(1, 0), 
                   labels = c("sub Male", "sub Female"))]
dt[, dual := factor(dual, levels = c(0, 1), 
                    labels = c("sub Ineligible", "sub Eligible"))]
dt[, age_grp := factor(age_grp, levels = c("(64,74]", "(74,84]", "(84,Inf]"), 
                       labels = c("sub 65-74", "sub 75-84", "sub 85+"))]
dt[, race := factor(race, levels = c(1,2,5,4,6,3),
                    labels = c("sub White", 
                               "sub Black/African-American",
                               "sub Hispanic", 
                               "sub Asian/Pacific Islander",
                               "sub American Indian/Alaska Native",
                               "sub Other"))]
dt[, dead_lead := factor(dead_lead, levels = c(0, 1), 
                         labels = c("Survived", "Died"))]
dt[, census_region := factor(census_region, levels = c("Northeast", "Midwest", "South", "West"),
                             labels = c("sub Northeast", "sub Midwest", "sub South", "sub West"))]

# make some changes to area-level variables to make the columns more interpretable
dt[ , `:=` (
  
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
  medhouseholdincome = medhouseholdincome / 10000
  
)]



### change table column names

# individial-level
dt <- dt %>%
  setnames(old = c("age_grp", "race", "sex", "dual", "census_region"),
           new = c("Age group", "Race", "Sex", "Medicaid eligibility", "Census region"))


# # # ZIP-level
# setnames(dt, "pm25", "Mean annual PM2.5 (ug/m3)")
# setnames(dt, "poverty", "% of 65+ population below poverty line")
# setnames(dt, "popdensity", "Pop. density per square mile")
# setnames(dt, "medianhousevalue", "Median value of owner occupied properties")
# setnames(dt, "medhouseholdincome", "Median household income")
# setnames(dt, "pct_owner_occ", "% of housing units occupied by owner")
# setnames(dt, "education", "% of 65+ population not graduating from high school")
# setnames(dt, "pct_blk", "% of 65+ population Black") # or full population?
# setnames(dt, "pct_hispanic", "% of 65+ population Hispanic") # or full population?
# setnames(dt, "summer_tmmx", "Summer average maximum temperature")
# setnames(dt, "winter_tmmx", "Winter average maximum temperature")
# setnames(dt, "summer_rmax", "Summer average maximum relative humidity")
# setnames(dt, "winter_rmax", "Winter average maximum relative humidity")
# 
# # county-level
# setnames(dt, "smoke_rate", "% of respondents in county who have ever smoked")
# setnames(dt, "mean_bmi", "Mean BMI of respondents in county")



# my.render.cont <- function(x) {
#   with(stats.apply.rounding(stats.default(x), digits=2), 
#        sprintf("%s (± %s)", MEAN, SD))
# }

# get table 1
tab1 <- table1(~Sex + `Age group` + Race + `Medicaid eligibility` + `Census region` +
                 pm25 + poverty + popdensity + medianhousevalue + medhouseholdincome + 
                 pct_owner_occ + education + pct_blk + pct_hispanic + smoke_rate + mean_bmi +
                 summer_tmmx + winter_tmmx + summer_rmax + winter_rmax
               #| dead_lead
               ,
               data = dt, big.mark = ",", 
               render.continuous = c(.="Mean (SD)"))


# # get table 1
# tab1 <- table1(~Sex + `Age group` + Race + `Medicaid eligibility` + 
#                  #`Mean annual PM2.5 (ug/m3)` #+
#                  `Census region` #+
#                # `% of 65+ population below poverty line` + `Pop. density per square mile` +
#                # `Median value of owner occupied properties` + `Median household income` +
#                # `% of housing units occupied by owner` + `% of 65+ population not graduating from high school` +
#                # `% of 65+ population Black` + `% of 65+ population Hispanic` +
#                # `% of respondents in county who have ever smoked` + `Mean BMI of respondents in county` +
#                # `Summer average maximum temperature` + `Winter average maximum temperature` +
#                # `Summer average maximum relative humidity` + `Winter average maximum relative humidity`
#                #| dead_lead
#                ,
#                data = dt, big.mark = ",", 
#                render.continuous = c(.="Mean (SD)"))




#write_rds(tab1, file = "results/table1.rds")

# # get table with just regions
# table1(~`Census region` | dead_12_16, 
#        data = dt, big.mark = ",", render.continuous = c(.="Mean (SD)"))


#tab1

# get table1 as a data.frame (need both of these lines!)
tab1 <- as.data.frame(tab1)
tab1 <- as.data.frame(lapply(tab1, as.character), stringsAsFactors = FALSE) # get rid of noquote

# rename columns so they're easier to work with
names(tab1) <- c("variable", "value")

# do some reformatting to make the area-level covariates cleaner

# initalize vector to store row numbers to remove
row_remove <- c()

# loop through rows
for(i in 1:nrow(tab1)){
  
  # if current row variable is "  Mean (SD)":
  if(tab1$variable[i] == "  Mean (SD)"){
    
    # make note of row number
    row_remove <- c(row_remove, i-1)
    
    # assign current row the value in the previous row
    tab1$variable[i] <- tab1$variable[i-1]
  }
}

# remove rows
tab1 <- tab1[-c(row_remove),]

# now neaten up the area-level values (previously variable names)
tab1 <- tab1 %>%
  mutate(variable = recode(variable,
                           "pm25" = "Mean annual PM\\(_{2.5}\\) (\\(\\mu g/m^3 \\))",
                           "poverty" = "% of 65+ population below poverty line",
                           "popdensity" = "Pop. density per square mile",
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



# library(kableExtra)
# b = kable(as.data.frame(tab1), booktabs=TRUE)
# 
# c = t1kable(tab1)

# escape % 
tab1[] <- lapply(tab1, function(x) gsub("%", "\\\\%", x))


# get horizontal space
tab1[] <- lapply(tab1, function(x) gsub("sub ", "\\\\hspace{10pt}", x))


# use disparities code to get subscript, mu, etc.
# wait I made the cohort table manually with \hspace{20pt}

# library(xtable)
# print(xtable(a, 
#              type = "latex",
#              label = "tab:table1",
#              caption = "Summary of individual-level characteristics for individuals in
#              the 2010-2012 window. Hospitalizations were assessed at the end of 2010, exposure was 
#              assessed in 2011, and mortality was assessed in 2012."), 
#       sanitize.text.function = identity,
#       include.rownames = FALSE)



######################################################################


#------------ get area-level variables manually ------------#

# this is so I have more control over formatting

# same thing but try to do neighborhood level vars separately

# # SAMPLE FOR NOW
# load("data/intermediate/rolling_cohort_1000.RData")

# # Specify the columns for which you want to calculate the mean
# cols_tab1 <- c("pm25", "poverty", "popdensity", "medianhousevalue", "medhouseholdincome", "pct_owner_occ",
#           "education", "pct_blk", "pct_hispanic", "summer_tmmx", "winter_tmmx", "summer_rmax",
#           "winter_rmax", "smoke_rate", "mean_bmi")
# 
# cols_good_names <- c("Mean annual PM2.5 (ug/m3)",
#                      "\\% of 65+ population below poverty line",
#                      "Pop. density per square mile",
#                      "Median value of owner occupied properties (\\$10,000)",
#                      "Median household income (\\$10,000)",
#                      "\\% of housing units occupied by owner",
#                      "\\% of 65+ population not graduating from high school",
#                      "\\% of 65+ population Black",
#                      "\\% of 65+ population Hispanic",
#                      "\\% of respondents in county who have ever smoked",
#                      "Mean BMI of respondents in county",
#                      "Summer average maximum temperature (F)",
#                      "Winter average maximum temperature (F)",
#                      "Summer average maximum relative humidity (\\%)",
#                      "Winter average maximum relative humidity (\\%)")
# 
# # Calculate the means and SDs
# col_means <- dt[, lapply(.SD, mean), .SDcols = cols_tab1] |> t()
# col_sds <- dt[, lapply(.SD, sd), .SDcols = cols_tab1] |> t()
# 
# # put together and format
# col_means_sds <- cbind.data.frame(col_means, col_sds)
# 
# # new column with correct formatting
# col_means_sds <- col_means_sds %>%
#   mutate(mean_sd = paste0(round(col_means, 1), " (", round(col_sds, 1), ")"))
# 
# # reformat
# neigh_lev <- data.frame(#variable = paste0("  \\hspace{10pt}", cols_good_names),
#                         variable = cols_good_names,
#                         value = col_means_sds$mean_sd)
# 
# # make names match a
# names(neigh_lev) <- c(" ", "Overall")
# 
# 
# a = rbind.data.frame(a, 
#                      #c("Neighborhood-level", " "),
#                      neigh_lev)
# 
# colnames(a) <- a[1,]
# a <- a[-1,]

#
# # county-level
# setnames(dt, "smoke_rate", "% of respondents in county who have ever smoked")
# setnames(dt, "mean_bmi", "Mean BMI of respondents in county")

# rename columns again (first row)
names(tab1) <- str_remove_all(tab1[1,], pattern = "[()]")
tab1 <- tab1[-1,]

# get caption, label, and file name
if(obs_include == "2010_only"){
  
  cap <- "Summary of characteristics for individuals in
        the 2010-2011 window. The table shows N (\\%) for variables displayed
        as categorical and mean (SD) for continuous variables.
        Exposure was assessed in 2010, and mortality was assessed in 2011."
  table_label <- "tab:table1_2010"
  file_name <- "results/tables/table1_2010.tex"
  
} else if(obs_include == "all_pt"){
  
  cap <- "Summary of characteristics across all rolling windows. 
        The table shows N (\\%) for variables displayed
        as categorical and mean (SD) for continuous variables."
  table_label <- "tab:table1_allpt"
  file_name <- "results/tables/table1_allpt.tex"
}

# print in latex
print(xtable(tab1,
             type = "latex",
             label = table_label,
             caption = cap),
      file = file_name, 
      sanitize.text.function = identity,
      include.rownames = FALSE)

