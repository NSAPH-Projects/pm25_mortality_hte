
########################################################
### Mortality CRE project
### Author: Lauren Mock
### Exploratory data analysis
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(scales)
library(tidyverse)
library(table1)

load("data/intermediate/rolling_cohort.RData")


#---------- Basic EDA ----------#

# how many individuals?
n_indiv <- length(unique(dt[,qid]))

# how many rows of data?
nrow(dt)

# How many person-years?
# number of rows + (2 * number of people)
# This is because each person contributes two years (years 1 and 3) that aren't 
# captured as exposure years
nrow(dt) + 2*n_indiv

# how many times does each individual appear?
mean(dt[,n_windows])
# plot
dt |>
  # restrict to last year so each person appears only once
  filter(year == last_year_ffs - 1) |>
  ggplot() +
  geom_histogram(aes(n_windows), binwidth = 1, col = "white", fill = "skyblue2") +
  labs(x = "Number of rolling windows per person",
       y = "Count") +
  scale_y_continuous(labels = label_comma()) +
  theme_bw()



# in which years are individuals contributing?
dt |>
  ggplot(aes(year)) +
  geom_bar(col = "white", fill = "skyblue2") +
  labs(x = "Year",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# # What % of indivs in the data have exposure = TRUE?
# sum(dt[,pm25 > 10], na.rm = TRUE)/nrow(dt) * 100 # 43% in ric, 44% in lauren
# expo_12 <- sum(dt[,pm25 > 12], na.rm = TRUE)/nrow(dt) * 100 # 10% in ric, 10% in lauren

# What % of indivs in the data have outcome = TRUE?
#sum(dt[,dead_12_16], na.rm = TRUE)/nrow(dt) * 100 # 28% in ric, 29% in lauren

# what % of individuals died in their last year of followup?
nrow(dt[(year == last_year_ffs - 1) & (dead_lead == 1)]) / length(unique(dt[,qid]))

# what % of rows have outcome = 1 (death)?
mean(dt[,dead_lead]) # 5.4%
# by region?
dt[, .(mean = mean(dead_lead)), by = census_div]


# # get % exposed to 12 by region
# expo_12_region <- dt[, .(exposed_12 = mean(pm25_binary)), by = .(census_region)]
# expo_12_region$exposed_12 <- paste0(round(expo_12_region$exposed_12*100, 0), "%")

# plot dist. of PM2.5 exposure in this cohort
dt |>
  #slice_sample(prop = 0.01) |> # random sample of data for plotting
  ggplot(aes(pm25)) +
  geom_histogram(col = "white", fill = "skyblue2") +
  labs(x = expression(PM[2.5]),
       y = "Count") +
  scale_y_continuous(labels = label_comma()) +
  #geom_vline(xintercept = 10, col = "black", linetype = 2) + 
  #geom_vline(xintercept = 12, col = "black", linetype = 2) + 
  # geom_text(x = 15, y = 40000,
  #           label = paste0(round(expo_12*100, 0), "%"),
  #           hjust   = 1,
  #           vjust   = 1) +
  # annotate("text", 
  #          label = paste0(round(expo_12, 0), "%"), 
  #          x = Inf, y = Inf, 
  #          hjust = 1.2, vjust = 1.5) +
  theme_light()

# PM2.5 by region
dt |>
  ggplot(aes(pm25)) +
  geom_histogram(fill = "skyblue2") +
  labs(x = expression(PM[2.5]),
       y = "Count") +
  scale_y_continuous(labels = label_comma()) +
  #geom_vline(xintercept = 10, col = "black", linetype = 2) + 
  #geom_vline(xintercept = 12, col = "black", linetype = 2) + 
  facet_wrap(~census_region, nrow = 3) +
  # geom_text(data = expo_12_region,
  #           mapping = aes(x = Inf, y = Inf, label = exposed_12),
  #           hjust   = 1.2,
  #           vjust   = 1.5
  # ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

# # get % exposed to 12 by region
# expo_12_division <- dt[, .(exposed_12 = mean(pm25_binary)), by = .(census_div)]
# expo_12_division$exposed_12 <- paste0(round(expo_12_division$exposed_12*100, 2), "%")

# PM2.5 by division
dt |>
  ggplot(aes(pm25)) +
  geom_histogram(fill = "skyblue2") +
  labs(x = expression(PM[2.5]),
       y = "Count") +
  scale_y_continuous(labels = label_comma()) +
  #geom_vline(xintercept = 10, col = "black", linetype = 2) + 
  #geom_vline(xintercept = 12, col = "black", linetype = 2) + 
  facet_wrap(~census_div, nrow = 3) +
  # geom_text(data = expo_12_division,
  #           mapping = aes(x = Inf, y = Inf, label = exposed_12),
  #           hjust   = 1.2,
  #           vjust   = 1.5
  # ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

# # see PM2.5 medians by exposure group in each region
# dt[, group_mean_pm25 := mean(pm25), by = .(pm25_binary, census_region)]
# dt |>
#   ggplot() +
#   geom_density(aes(pm25), bw = 0.12) +
#   labs(title = paste0("PM2.5 cutoff at ", pm25_cutoff, " ug/m3")) +
#   geom_vline(aes(xintercept = pm25_cutoff), col = "black", lty = 2) +
#   geom_vline(aes(xintercept = group_mean_pm25), col = "red", lty = 2) +
#   facet_wrap(~census_region, nrow = 4) +
#   theme_bw() +
#   theme(strip.background = element_rect(fill = "white"))

# plot years of death (filter to year of death so no repeated indivs)
dt[dead_lead == 1,] |>
  ggplot(aes(death_year)) +
  geom_bar(col = "white", fill = "skyblue2") +
  labs(x = "Year of death",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# plot age of death in this cohort (each indiv can only die once so this works)
dt[dead_lead == 1,] |>
  ggplot() +
  geom_bar(aes(age + 1),
           col = "white", fill = "skyblue2") +
  labs(x = "Age at death",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# death in which year of follow-up?
dt[dead_lead == 1,] |>
  ggplot() +
  geom_bar(aes(year_follow),
           col = "white", fill = "skyblue2") +
  labs(x = "Year of follow-up at death",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# what percentage of individuals died in each rolling window?
pct_dead <- dt[, .(pct_dead = mean(dead_lead)*100), by = year]
pct_dead |>
  ggplot() +
  geom_line(aes(x = year, y = pct_dead)) + 
  labs(x = "Exposure year", y = "% dead in following year") +
  ylim(c(0, 7)) + 
  theme_light()

# trying to figure this out --> plot mean age by year
age_dist <- dt[, .(mean_age = mean(age)), by = year]
age_dist |>
  ggplot() +
  geom_line(aes(x = year, y = mean_age)) +
  labs(x = "Year", 
       y = "Mean age") +
  ylim(c(0, 105)) + 
  theme_light()

# mean number of rolling windows individuals appear in per year
dt[, .(mean_n_windows = mean(n_windows)), by = year]

# it's weird to plot age where I count some individuals more times than others
# # plot age by race
# dt |>
#   mutate(rti_race_cd = case_when(
#     rti_race_cd == 1 ~ "white",
#     rti_race_cd == 2 ~ "black",
#     rti_race_cd == 3 ~ "other",
#     rti_race_cd == 4 ~ "asian",
#     rti_race_cd == 5 ~ "hispanic",
#     rti_race_cd == 6 ~ "native"
#   )) |>
#   # REMOVE TWO RACIAL GROUPS
#   filter(rti_race_cd != "other" & rti_race_cd != "native") |>
#   ggplot() +
#   geom_density(aes(age, col = rti_race_cd), bw = 0.8) +
#   #geom_histogram(aes(age, fill = rti_race_cd), binwidth = 1) +
#   labs(x = "Age in 2010",
#        y = "Density",
#        col = "") +
#   scale_y_continuous(labels = label_comma()) +
#   theme_light()

# it's weird to plot age where I count some individuals more times than others
# plot age in 2010 by region
# dt |>
#   ggplot() +
#   geom_density(aes(age, col = census_region), bw = 0.8) +
#   #geom_histogram(aes(age, fill = rti_race_cd), binwidth = 1) +
#   labs(x = "Age in 2010",
#        y = "Density",
#        col = "") +
#   scale_y_continuous(labels = label_comma()) +
#   theme_light()


# #---------- Censoring ----------#

# # What % of people were censored (left FFS) before death?
cens <- dt[year == last_year_ffs - 1 & !(dead_lead == 1),]
nrow(cens) / n_indiv # 67%

# # # years at risk in full cohort
# # dt |>
# #   ggplot() +
# #   geom_bar(aes(years_at_risk), col = "white", fill = "skyblue2") +
# #   labs(x = "Years at risk",
# #        y = "Number of individuals") +
# #   scale_y_continuous(labels = label_comma()) +
# #   theme_light()
# 
# # age distribution at time of censoring
# dt[(cens)] |>
#   ggplot() +
#   geom_histogram(aes(age + (last_year_ffs - 2010)), col = "white", fill = "skyblue2", binwidth = 1) +
#   labs(x = "Age at censoring",
#        y = "Number of individuals") +
#   scale_y_continuous(labels = label_comma()) +
#   theme_bw()
# 
# # year of censoring
# dt[(cens)] |>
#   ggplot(aes(last_year_ffs)) +
#   geom_bar(col = "white", fill = "skyblue2") +
#   labs(x = "Year of censoring",
#        y = "Number of Individuals") +
#   scale_y_continuous(labels = label_comma()) +
#   theme_bw()
# 
# 
# # separate dt for censored people
# dt.cens <- dt[cens == 1]
# # now remove censored people
# dt <- dt[cens == 0]


# ---------- Chronic conditions ----------#

cond_abbr <- paste0(c("hypoth", "ami",
               "alzh", "alzhdmta",
               "anemia", "asthma", "atrialfb", "hyperp", "breastCancer",
               "colorectalCancer", "endometrialCancer", "lungCancer", "prostateCancer",
               "cataract", "chrnkidn", "copd", "depressn", "diabetes", "glaucoma", "chf",
               "hipfrac", "hyperl", "hypert", "ischmcht", "osteoprs", "ra_oa", "stroke"),
               "_ever")

cond_name <- c("Acquired Hypothyroidism", "Acute Myocardial Infarction",
               "Alzheimer's Disease", "ADRD or Senile Dementia",
               "Anemia",
               "Asthma", "Atrial Fibrillation", "Benign Prostatic Hyperplasia",
               "Cancer, Female/Male Breast", "Cancer, Colorectal", "Cancer, Endometrial",
               "Cancer, Lung", "Cancer, Prostate", "Cataract", "Chronic Kidney Disease",
               "COPD and Bronchiectasis", "Depression", "Diabetes", "Glaucoma",
               "Heart Failure", "Hip/Pelvic Fracture", "Hyperlipidemia", "Hypertension",
               "Ischemic Heart Disease", "Osteoporosis",
               "Rheumatoid Arthritis/Osteoarthritis", "Stroke/Transient Ischemic Attack")

conditions <- data.frame("cond_abbr" = cond_abbr, "cond_name" = cond_name)


### MOVE TO SCRIPT 1 ###

#--------
# cond_abbr <- c("hypert_ever", 
#                "ischmcht_ever", 
#                "diabetes_ever", 
#                "chrnkidn_ever")
# 
# cond_name <- c("Hypertension",
#                "Ischemic Heart Disease",
#                "Diabetes",
#                "Chronic Kidney Disease")
# 
# conditions <- data.frame("cond_abbr" = cond_abbr,
#                          "cond_name" = cond_name)

# How common is each chronic condition? BEFORE EXPOSURE IN 2010
prev_overall <- dt[year == 2010, lapply(.SD, function(x) sum(x) / .N * 100),
                   .SDcols = cond_abbr] |>
  gather(key = "cond_abbr", value = "prev_overall") |>
  left_join(conditions, by = "cond_abbr")

# make conditions factors with levels sorted by by overall prevalence (for plotting)
prev_overall$cond_name <- factor(prev_overall$cond_name, 
                                 levels = cond_name[order(prev_overall$prev_overall)])

# plot
prev_overall |>
  arrange(prev_overall) |>
  ggplot(aes(x = prev_overall, y = cond_name)) +
  geom_point(col = "skyblue3", size = 3, alpha = 0.9) +
  labs(x = "% ever hosp before 2010",
       y = "",
       col = "") +
  theme_bw()


#----------------------------------------------------------- by region

# How common is each chronic condition by region?
prev_region <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
                  by = census_region,
                  .SDcols = paste0(cond_abbr, "_ever")] |>
  gather(key = "cond_abbr", value = "prev_region", -census_region)

# merge with full names for plotting
prev_df <- left_join(conditions, prev_region, by = "cond_abbr") |>
  left_join(prev_overall, by = "cond_abbr")

# make conditions factors with levels sorted by by overall prevalence (for plotting)
prev_df$cond_name <- factor(prev_df$cond_name, levels = cond_name[order(prev_overall$prev_overall)])

# get min and max prevalence values across regions
prev_min_max <- prev_df %>%
  group_by(cond_name) %>%
  summarize(min_prev = min(prev_region),
            max_prev = max(prev_region))

# plot
prev_df |>
  ggplot(aes(x = prev_region, y = cond_name)) +
  geom_segment(data = prev_min_max, aes(y = cond_name, yend = cond_name,
                                        x = min_prev, xend = max_prev)) +
  geom_point(aes(col = census_region), size = 3, alpha = 0.9) +
  labs(x = "% ever hosp before 2010",
       y = "",
       col = "") +
  theme_bw()


#----------------------------------------------------------- by race

# How common is each chronic condition by race?
prev_race <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
                by = race,
                .SDcols = cond_abbr] |>
  gather(key = "cond_abbr", value = "prev_race", -race) |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  # REMOVE TWO GROUPS!!!
  filter(race != "other" & race != "native")


# merge with full names for plotting
prev_df <- left_join(conditions, prev_race, by = "cond_abbr") |>
  left_join(prev_overall, by = "cond_abbr")

# make conditions factors with levels sorted by by overall prevalence (for plotting)
prev_df$cond_name <- factor(prev_df$cond_name, levels = cond_name[order(prev_overall$prev_overall)])

# get min and max prevalence values across regions
prev_min_max <- prev_df %>%
  group_by(cond_name) %>%
  summarize(min_prev = min(prev_race),
            max_prev = max(prev_race))

# plot
prev_df |>
  ggplot(aes(x = prev_race, y = cond_name)) +
  geom_segment(data = prev_min_max, aes(y = cond_name, yend = cond_name,
                                        x = min_prev, xend = max_prev)) +
  geom_point(aes(col = race), size = 3, alpha = 0.7) +
  #scale_color_brewer(palette = "Set2") +
  labs(x = "Prevalence (%)",
       y = "",
       col = "") +
  theme_bw()


#----------------------------------------------------------- by race AND region

top_10_cond <- arrange(prev_overall, desc(prev_overall))$cond_abbr[1:10]

# How common is each chronic condition by race?
prev_region_race <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
                       by = .(census_region, race),
                       .SDcols = cond_abbr] |>
  gather(key = "cond_abbr", value = "prev_region_race", -c(race, census_region)) |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  # REMOVE TWO GROUPS!!!
  filter(race != "other" & race != "native") |>
  # keep only top 10 conditions
  filter(cond_abbr %in% top_10_cond)

# merge with full names for plotting
prev_df <- left_join(conditions, prev_region_race, by = "cond_abbr") |>
  left_join(prev_overall, by = "cond_abbr") |>
  filter(!is.na(prev_region_race))

# make conditions factors with levels sorted by by overall prevalence (for plotting)
prev_df$cond_name <- factor(prev_df$cond_name, levels = cond_name[order(prev_overall$prev_overall)])

# get min and max prevalence values across regions
prev_min_max <- prev_df %>%
  group_by(cond_name, census_region) %>%
  summarize(min_prev = min(prev_region_race),
            max_prev = max(prev_region_race))

# plot
prev_df |>
  ggplot(aes(x = prev_region_race, y = cond_name)) +
  geom_segment(data = prev_min_max, aes(y = cond_name, yend = cond_name,
                                        x = min_prev, xend = max_prev)) +
  geom_point(aes(col = race), size = 3, alpha = 0.7) +
  #scale_color_brewer(palette = "Set2") +
  labs(x = "Prevalence (%)",
       y = "",
       col = "") +
  theme_bw() +
  facet_wrap(~census_region) +
  theme(strip.background = element_rect(fill = "white"))




# # How common is each chronic condition?
# cond_prev <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
#                 .SDcols = cond_abbr] |> unlist()
# 
# # among people who survived?
# cond_prev_y0 <- dt[!(dead_12_16), lapply(.SD, function(x) sum(x) / .N * 100),
#                    .SDcols = cond_abbr] |> unlist()
# 
# # among people who died?
# cond_prev_y1 <- dt[(dead_12_16), lapply(.SD, function(x) sum(x) / .N * 100),
#                    .SDcols = cond_abbr] |> unlist()
# 
# # among people who were censored?
# cond_prev_cens <- dt.cens[, lapply(.SD, function(x) sum(x) / .N * 100),
#                    .SDcols = cond_abbr] |> unlist()


# use this to make a nice table instead
# dt[, lapply(.SD, function(x) paste0(format(round(sum(x) / .N * 100, 4), scientific = FALSE), ' %')),
#                 .SDcols = cond_abbr] |> unlist()
# Least common = cataract: 0.0002%
# Most common = hypertension: 8.4%

# # get this into a df (with levels in prevalence order)
# cond_prev_tab <- data.frame(cond = cond_name, prev = cond_prev)
# cond_prev_tab$cond <- factor(cond_prev_tab$cond, levels = cond_name[order(cond_prev)])
# 
# # plot
# cond_prev_tab |>
#   ggplot(aes(x = prev, y = cond)) +
#   geom_point(col = "#4472C4", size = 2) +
#   labs(x = "Prevalence (%)",
#        y = "") +
#   theme_bw()


# # get this into a df (with levels in prevalence order)
# cond_prev_tab_big <- data.frame(cond = cond_name, prev_y0 = cond_prev_y0,
#                                 prev_y1 = cond_prev_y1, prev_cens = cond_prev_cens)
# cond_prev_tab_big$cond <- factor(cond_prev_tab_big$cond, levels = cond_name[order(cond_prev_y0)])
# 
# # convert to long format
# cond_prev_tab_big <- gather(cond_prev_tab_big, key = "data", value = "prevalence", -cond)
# 
# 
# # plot
# cond_prev_tab_big |>
#   ggplot(aes(x = prevalence, y = cond, group = data)) +
#   geom_point(aes(col = data), size = 2) +
#   labs(x = "Prevalence (%)",
#        y = "",
#        col = "") +
#   scale_color_discrete(breaks = c("prev_y0", "prev_y1", "prev_cens"),
#                        labels = c("Survived", "Died", "Censored")) +
#   theme_bw()


#---------- Propensity scores ----------#

# # work with a subset for now
# set.seed(17)
# idx <- sample(1:nrow(dt), 1000, replace = FALSE)
# dt <- dt[idx,]



#--- model propensity scores by census region

# # now make the data a list split by region
# regions.list <- split(dt, by = "census_region")
# rm(dt)
# gc()


#---------- Subset ----------#

# Remove from X, Y, Z
set.seed(17)
idx <- sample(1:nrow(dt), 1000000, replace = FALSE)

# y <- y[idx]
# z <- z[idx]
# X <- X[idx,]
# ps <- ps[idx]
dt <- dt[idx,]

# Order by region (gets messed up when I subset above)
setorder(dt, cols = census_region)

# if no file exists, fit a propensity score model for each region
if (!file.exists(ps_file)) {
  
  # initialize list for ps
  ps <- c()
  #ps <- list()
  
  # loop through 4 regions
  for(i in 1:4){
    
    region <- unique(dt[, census_region])[i]
    
    one_region <- dt[census_region == region]
    #one_region <- regions.list[[i]]
    
    #---------- Set up data ----------#
    
    #--- y (outcome vector)
    y <- ifelse(one_region[, dead_12_16], 1, 0)
    
    
    #--- z (exposure vector)
    z <- one_region[, pm25_binary]
    
    
    
    #--- X (covariate matrix)
    X <- one_region[, c(
      
      # Sex, age, dual eligibility, RTI race code
      "sex", "age", "dual", "white", "black", "hispanic", 
      
      # CHANGE FOR RIC
      #"other", "asian", "native", 
      
      # area-level potential confounders
      "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
      "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
      "pct_blk", "pct_hispanic",
      "summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax"#,
      
      # # US Census Bureau division indicators
      # "East North Central", "East South Central", "Middle Atlantic",
      # "Mountain", "New England", "Pacific", "South Atlantic",
      # "West North Central", "West South Central"#,
      
      # # 27 chronic conditions
      # "alzhdmta", "alzh",
      # 
      # "hypoth", "ami", "anemia", "asthma", "atrialfb", "hyperp", "breastCancer", 
      # "colorectalCancer", "endometrialCancer", "lungCancer", "prostateCancer", 
      # "cataract", "chrnkidn", "copd", "depressn", "diabetes", "glaucoma", "chf", 
      # "hipfrac", "hyperl", "hypert", "ischmcht", "osteoprs", "ra_oa", "stroke"
      
    )]
    
    # Predict exposure from covariates
    propensity_models <- glm(z ~ ., family = "binomial", data = cbind.data.frame(X,z))
    
    # Get probability of exposure
    one_region_ps <- predict(propensity_models, type = "response")
    
    # Concatenate into vector with ps for all regions
    ps <- c(ps, one_region_ps)
    #ps[[i]] <- one_region_ps
    
    rm(y, z, X, propensity_models, one_region_ps, one_region)
    gc()
  }
  
  # Save
  save(ps, file = ps_file)
  
} else {
  load(ps_file)
}

# make ps a column in dt
dt[,ps := ps]


# plot distribution of propensity scores by region
dt |>
  #slice_sample(prop = 0.1) |> # random sample of data for plotting
  ggplot() +
  geom_density(aes(x = ps, col = as.factor(pm25_binary)), bw = 0.03) +
  labs(x = "Propensity score",
       y = "Density",
       col = "",
       title = paste0(ifelse(dataset == "methods", "Methods cohort", "CCW cohort")))  +
  scale_color_manual(values = c("0" = "dodgerblue2", "1" = "red2"),
                     labels = c("0" = "Unexposed", "1" = "Exposed")) +
  xlim(c(0,1)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  facet_wrap(~census_region, scales = "free")


# plot distribution of propensity scores by division
dt |>
  #slice_sample(prop = 0.1) |> # random sample of data for plotting
  ggplot() +
  geom_density(aes(x = ps, col = as.factor(pm25_binary), group = as.factor(pm25_binary)), bw = 0.03) +
  labs(x = "Propensity score",
       y = "Density",
       col = "")  +
  scale_color_manual(values = c("0" = "dodgerblue2", "1" = "red2"),
                     labels = c("0" = "Unexposed", "1" = "Exposed")) +
  xlim(c(0,1)) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white")) +
  facet_wrap(~census_div, scales = "free")


#####################################################
#### focus on New England

# unique number of indivs
nrow(dt[census_div == "New England",]) # 1,645,999
# unique number of zip codes
length(unique(dt[census_div == "New England", zip])) # 2091

# get number of indivs that were exposed
nrow(dt[census_div == "New England" & pm25_binary == 1,])
# get number of ZIP codes that were exposed
length(unique(dt[census_div == "New England" & pm25_binary == 1, zip]))
#####################################################




#----- understand characteristics of people at the extremes

# get data tables with low and high extremes
low.ps <- dt[(ps < quantile(ps, 0.01)),]
high.ps <- dt[(ps > quantile(ps, 0.99)),]

low.ps[, sex := factor(sex, levels = c(1, 2), labels = c("Male", "Female"))]
low.ps[, dual := factor(dual, levels = c(0, 1), labels = c("Ineligible", "Eligible"))]
low.ps[, rti_race_cd := factor(race, levels = c(1:6), 
                               labels = c("Non-Hispanic White", "Black (or African-American)", 
                                          "Other", "Asian/Pacific Islander", 
                                          "Hispanic", "American Indian/Alaska Native"))]

high.ps[, sex := factor(sex, levels = c(1, 2), labels = c("Male", "Female"))]
high.ps[, dual := factor(dual, levels = c(0, 1), labels = c("Ineligible", "Eligible"))]
high.ps[, rti_race_cd := factor(race, levels = c(1:6), 
                                labels = c("Non-Hispanic White", "Black (or African-American)", 
                                           "Other", "Asian/Pacific Islander", 
                                           "Hispanic", "American Indian/Alaska Native"))]

# create basic table 1s
table1(~sex + age_grp + rti_race_cd + dual + pm25 + census_region, 
       data = low.ps, big.mark = ",", render.continuous = c(.="Mean (SD)"))

table1(~sex + age_grp + rti_race_cd + dual + pm25 + census_region, 
       data = high.ps, big.mark = ",", render.continuous = c(.="Mean (SD)"))


#----- quick map of where people are getting trimmed

# green for lowest PM2.5, red for highest PM2.5
# map the zip codes


extreme.ps <- rbind(data.frame(state = unique(low.ps[,statecode]), prop_score = "low"),
                    data.frame(state = unique(high.ps[,statecode]), prop_score = "high"))

extreme.ps[which(duplicated(extreme.ps$state)),]

# check out this dataframe before I decide about plotting --> zips would be cooler

# library(sf)
# library(ggplot2)
# 
# # Sample data (replace this with your own dataset)
# state_data <- data.frame(
#   state_code = state.abb,
#   state_name = tolower(state.name),
#   trim = 
# )
# 
# # Get the US map data
# us_map <- st_as_sf(map("state", plot = FALSE, fill = TRUE))
# 
# # Merge state data with map data
# merged_data <- merge(us_map, state_data, by.x = "ID", by.y = "state", all.x = TRUE)
# 
# # Plot the map
# ggplot() +
#   geom_sf(data = merged_data, aes(fill = category), color = "white", size = 0.2) +
#   scale_fill_manual(values = c("A" = "blue", "B" = "green", "C" = "red")) +  # Adjust colors as needed
#   theme_void()


# ---------- covariate balance ----------#

##### indiv-level

# look at covariate balance in exposed vs. unexposed groups

# convert dual to integer (not sure why the 0/1s are characters)
dt[,dual := as.integer(dual)]

# create a standardized difference in means function (stdif)
stdif_fun <- function(X, W){
  mean_diff = mean(X[W == TRUE], na.rm = TRUE) - mean(X[W == FALSE], na.rm = TRUE)
  SE = sqrt(0.5 * (var(X[W == TRUE], na.rm = TRUE) + var(X[W == FALSE], na.rm = TRUE)))
  return(mean_diff/SE)
}

covs_indiv <- c("sex", "age", "dual", 
                "white", "black", "hispanic", 
                #"other_race"#, 
                "asian", "native", "other" #, cond_abbr
)

# # standardized difference in means
# stdif_indiv <- sapply(dt[, .SD, .SDcols = covs_indiv],
#                 function(x) stdif_fun(x, ifelse(dt[,pm25] > pm25_cutoff, 1, 0)))
# 
# # set up data for ggplot
# love_indiv <- data.frame(covs_indiv, stdif_indiv)
# love_indiv$covs_indiv <- factor(love_indiv$covs_indiv, 
#                                 levels = rev(c("age", "sex", "dual",
#                                            "white", "black", "other", "asian", "hispanic", "native"
#                                            #, cond_abbr
#                                            )))
# 
# # love plot
# love_indiv |>
#   ggplot(aes(x = stdif_indiv, y = covs_indiv)) +
#   geom_vline(xintercept = c(0)) +
#   geom_vline(xintercept = c(-0.1, 0.1), linetype = 2) +
#   geom_point(col = "#4472C4", size = 2) +
#   labs(x = "Standardized difference in covariate means",
#        y = "") +
#   xlim(c(-0.22, 0.22)) +
#   theme_bw()


##### now indiv-level, facetted by region

# look at covariate balance in exposed vs. unexposed groups

# loop through divisions and get stdif
love_indiv_list <- list()

for(i in 1:4){
  one_region <- dt[census_region == unique(dt[, census_region])[i]]
  
  # standardized difference in means
  stdif_indiv_one_region <- sapply(one_region[, .SD, .SDcols = covs_indiv],
                                   function(x) stdif_fun(x, ifelse(dt[,pm25] > pm25_cutoff, 1, 0)))
  
  # set up data for ggplot
  love_indiv_list[[i]] <- data.frame(covs_indiv, stdif_indiv_one_region)
  love_indiv_list[[i]]$covs_indiv <- factor(love_indiv_list[[i]]$covs_indiv, 
                                            levels = rev(c("age", "sex", "dual",
                                                           "white", "black", 
                                                           #"other_race",
                                                           "other", "asian", 
                                                           "hispanic", 
                                                           "native"#, cond_abbr
                                            )))
  love_indiv_list[[i]]$census_region <- unique(dt[, census_region])[i]
  rm(one_region, stdif_indiv_one_region)
  gc()
}


# bind dfs in list
love_indiv_df <- bind_rows(love_indiv_list)

# love plot
love_indiv_df |>
  ggplot(aes(x = stdif_indiv_one_region, y = covs_indiv)) +
  geom_vline(xintercept = c(0)) +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = 2) +
  geom_point(col = "#4472C4", size = 2) +
  labs(x = "Standardized difference in covariate means",
       y = "",
       title = paste0(ifelse(dataset == "methods", 
                             paste0("Methods cohort"), 
                             paste0("CCW cohort")))) +
  xlim(c(-0.3, 0.3)) +
  facet_wrap(~census_region, nrow = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))



##### area level

covs_zip <- c("poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
              "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
              #"summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax"
              "pct_blk", "pct_hispanic")

# # standardized difference in means
# stdif_zip <- sapply(dt[, .SD, .SDcols = covs_zip],
#                       function(x) stdif_fun(x, ifelse(dt[,pm25] > pm25_cutoff, 1, 0)))
# 
# # love plot
# 
# # set up data for ggplot
# love_zip <- data.frame(covs_zip, stdif_zip)
# 
# love_zip |>
#   ggplot(aes(x = stdif_zip, y = covs_zip)) +
#   geom_vline(xintercept = c(0)) +
#   geom_vline(xintercept = c(-0.1, 0.1), linetype = 2) +
#   geom_point(col = "#4472C4", size = 2) +
#   labs(x = "Standardized difference in covariate means",
#        y = "") +
#   xlim(c(-0.6, 0.6)) +
#   theme_bw()


##### now area-level, facetted by division

# look at covariate balance in exposed vs. unexposed groups

# loop through regions and get stdif
love_zip_list <- list()

for(i in 1:4){
  one_region <- dt[census_region == unique(dt[, census_region])[i]]
  
  # standardized difference in means
  stdif_zip_one_region <- sapply(one_region[, .SD, .SDcols = covs_zip],
                                 function(x) stdif_fun(x, ifelse(dt[,pm25] > pm25_cutoff, 1, 0)))
  
  # set up data for ggplot
  love_zip_list[[i]] <- data.frame(covs_zip, stdif_zip_one_region)
  love_zip_list[[i]]$covs_zip <- factor(love_zip_list[[i]]$covs_zip)
  love_zip_list[[i]]$census_region <- unique(dt[, census_region])[i]
  rm(one_region, stdif_zip_one_region)
  gc()
}


# bind dfs in list
love_zip_df <- bind_rows(love_zip_list)

# love plot
love_zip_df |>
  ggplot(aes(x = stdif_zip_one_region, y = covs_zip)) +
  geom_vline(xintercept = c(0)) +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = 2) +
  geom_point(col = "#4472C4", size = 2) +
  labs(x = "Standardized difference in covariate means",
       y = "",
       title = paste0(ifelse(dataset == "methods", 
                             paste0("Methods cohort"), 
                             paste0("CCW cohort")))) +
  xlim(c(-0.45, 0.45)) +
  facet_wrap(~census_region, nrow = 2) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))



##################################

# ### explore PM2.5 dist
# 
# dt[, pm25_avg_above_12 := ifelse(pm25_avg > 12, 1, 0)]
# 
# dt[, group_mean_pm25_12 := mean(pm25_avg), by = pm25_avg_above_12]
# 
# 
# dt |>
#   ggplot() +
#   geom_density(aes(pm25_avg)) +
#   geom_vline(aes(xintercept = group_mean_pm25_12), col = "red", lty = 2) +
#   facet_wrap(~pm25_avg_above_12, nrow = 2) +
#   theme_bw()
# 
# 
# ### 10

##################################



# ---------- Table 1 ----------#

# RUN THIS CODE IN FILE 1 AND DELETE HERE (adrd_denom instead of dt)
# actually, not sure how to handle this
# dt[cens == 1, ADRD_hosp_11_16 := "censored"]

# now remove censored people from dt
# dt <- dt[cens == 0]


# need to make a race indicator for Riccardo's data
# if(dataset == "methods"){
#   dt[, race := ifelse(white == 1, "white", NA)]
#   dt[, race := ifelse(black == 1, "black", race)]
#   dt[, race := ifelse(hispanic == 1, "hispanic", race)]
#   dt[, race := ifelse(other_race == 1, "other_race", race)]
#   
#   #dt[, sex := ifelse(sex == 0, 2, sex)]
#   dt[, sex := sex + 1]
# }



###### RESTRICT TO 2010 FOR NOW ###### 

dt <- dt[year == 2010,]

# Note: rewriting values in dt to make this table

# change table values
dt[, sex := factor(sex, levels = c(1, 0), labels = c("Male", "Female"))]
dt[, dual := factor(dual, levels = c(0, 1), labels = c("Ineligible", "Eligible"))]
dt[, race := factor(race, levels = c(1:6),
                    labels = c("Non-Hispanic White", "Black (or African-American)",
                               "Other", "Asian/Pacific Islander",
                               "Hispanic", "American Indian/Alaska Native"))]
dt[, dead_lead := factor(dead_lead, levels = c(0, 1), 
                          labels = c("Survived", "Died"))]
dt[, census_region := factor(census_region)]

### change table column names

# individial-level
setnames(dt, "age_grp", "Age group")
setnames(dt, "race", "Race")
setnames(dt, "sex", "Sex")
setnames(dt, "dual", "Medicaid eligibility")

# # ZIP-level
setnames(dt, "pm25", "Mean annual PM2.5 (ug/m3)")
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

# # county-level
# setnames(dt, "smoke_rate", "% of respondents in county who have ever smoked")
# setnames(dt, "mean_bmi", "Mean BMI of respondents in county")

# region
setnames(dt, "census_region", "Census region")


# get table 1
table1(~Sex + `Age group` + Race + `Medicaid eligibility` + `Mean annual PM2.5 (ug/m3)` +
         `Census region` #+
       # `% of 65+ population below poverty line` + `Pop. density per square mile` + 
       # `Median value of owner occupied properties` + `Median household income` + 
       # `% of housing units occupied by owner` + `% of 65+ population not graduating from high school` + 
       # `% of 65+ population Black` + `% of 65+ population Hispanic` + 
       # `% of respondents in county who have ever smoked` + `Mean BMI of respondents in county` + 
       # `Summer average maximum temperature` + `Winter average maximum temperature` +
       # `Summer average maximum relative humidity` + `Winter average maximum relative humidity` 
       | dead_lead, 
       data = dt, big.mark = ",", render.continuous = c(.="Mean (SD)"))

# # get table with just regions
# table1(~`Census region` | dead_12_16, 
#        data = dt, big.mark = ",", render.continuous = c(.="Mean (SD)"))


# ##########
# # now get a table with censored people (single column)
# 
# # change table values
# dt.cens[, sex := factor(sex, levels = c(1, 2), labels = c("Male", "Female"))]
# dt.cens[, dual := factor(dual, levels = c(0, 1), labels = c("Ineligible", "Eligible"))]
# dt.cens[, rti_race_cd := factor(race, levels = c(1:6), 
#                            labels = c("Non-Hispanic White", "Black (or African-American)", 
#                                       "Other", "Asian/Pacific Islander", 
#                                       "Hispanic", "American Indian/Alaska Native"))]
# dt.cens[, dead_12_16 := factor(dead_12_16, levels = c(FALSE, TRUE), 
#                           labels = c("Survived", "Died"))]
# 
# ### change table column names
# 
# # individial-level
# setnames(dt.cens, "age_grp", "Age")
# setnames(dt.cens, "rti_race_cd", "Race")
# setnames(dt.cens, "sex", "Sex")
# setnames(dt.cens, "dual", "Medicaid eligibility")
# 
# # ZIP-level
# setnames(dt.cens, "pm25", "Mean annual PM2.5 (ug/m3)")
# setnames(dt.cens, "poverty", "% of 65+ population below poverty line")
# setnames(dt.cens, "popdensity", "Pop. density per square mile")
# setnames(dt.cens, "medianhousevalue", "Median value of owner occupied properties")
# setnames(dt.cens, "medhouseholdincome", "Median household income")
# setnames(dt.cens, "pct_owner_occ", "% of housing units occupied by owner")
# setnames(dt.cens, "education", "% of 65+ population not graduating from high school")
# 
# # can't find appropriate page on GitHub
# setnames(dt.cens, "max_temp", "max temp (check documentation)")
# setnames(dt.cens, "min_humid", "min humid (check documentation)")
# 
# # county-level
# setnames(dt.cens, "smoke_rate", "% of respondents in county who have ever smoked")
# setnames(dt.cens, "mean_bmi", "Mean BMI of respondents in county")
# 
# # get table 1
# table1(~Sex + Age + Race + `Medicaid eligibility` + `Mean annual PM2.5 (ug/m3)` + 
#          `% of 65+ population below poverty line` + `Pop. density per square mile` + 
#          `Median value of owner occupied properties` + `Median household income` + 
#          `% of housing units occupied by owner` + `% of 65+ population not graduating from high school` + 
#          `% of respondents in county who have ever smoked` + `Mean BMI of respondents in county` + 
#          `max temp (check documentation)` + `min humid (check documentation)`, 
#        data = dt.cens, big.mark = ",", render.continuous = c(.="Mean (SD)"))
