
########################################################
### Mortality CRE project
### Author: Lauren Mock
### Exploratory data analysis
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(table1)
library(magrittr)
library(readr)

dt <- read_rds("data/intermediate/rolling_cohort.rds")
#load("data/intermediate/rolling_cohort_1000.RData")

# make a new variable for race + dual
dt[, race_dual := paste0(race, "_", dual)]


#---------- Basic EDA ----------#

pdf("results/EDA.pdf")

# how many individuals?
n_indiv <- length(unique(dt[,qid]))
paste0(n_indiv, " individuals")

# how many rows of data?
n_rows <- nrow(dt)
paste0(n_rows, " rows of data (exposure years)")

# How many person-years?
# number of rows + (2 * number of people)
# This is because each person contributes two years (first and last) that aren't 
# captured as exposure years
n_py <- n_rows + 2*n_indiv
paste0(n_py, " total person-years")

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

# now by race
dt |>
  # restrict to last year so each person appears only once
  filter(year == last_year_ffs - 1) |>
  ggplot() +
  geom_density(aes(n_windows, col = race), bw = 0.9) +
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

# in which follow-up years are individuals contributing?
dt |>
  ggplot(aes(year_follow)) +
  geom_bar(col = "white", fill = "skyblue2") +
  labs(x = "Follow-up year",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# what is the distribution of each race in each year?
race_year <- dt[, .(count_race = .N), by = .(year, race)]
race_year |>
  group_by(year) %>%
  mutate(race_pct = 100*count_race/sum(count_race)) %>%
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  #geom_bar(aes(x = year, y = count_race, fill=race), stat = "identity") +
  geom_bar(aes(x = year, y = race_pct, fill=race), stat = "identity") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()


# what is the distribution of each race in each follow-up year?
race_year_follow <- dt[, .(count_race = .N), by = .(year_follow, race)]
race_year_follow |>
  group_by(year_follow) %>%
  mutate(race_pct = 100*count_race/sum(count_race)) %>%
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  #geom_bar(aes(x = year_follow, y = count_race, fill=race), stat = "identity") +
  geom_bar(aes(x = year_follow, y = race_pct, fill=race), stat = "identity") +
  labs(x = "Follow-up year",
       y = "% of sample") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()


# # What % of indivs in the data have exposure = TRUE?
# sum(dt[,pm25 > 10], na.rm = TRUE)/nrow(dt) * 100 # 43% in ric, 44% in lauren
# expo_12 <- sum(dt[,pm25 > 12], na.rm = TRUE)/nrow(dt) * 100 # 10% in ric, 10% in lauren

# What % of indivs in the data have outcome = TRUE?
#sum(dt[,dead_12_16], na.rm = TRUE)/nrow(dt) * 100 # 28% in ric, 29% in lauren

# what % of individuals died in their last year of followup?
pct_dead_last <- nrow(dt[(year == last_year_ffs - 1) & (dead_lead == 1)]) / 
  length(unique(dt[,qid]))
paste0(pct_dead_last * 100, " % of individuals died in last year of follow-up")

# what % of black individuals died in their last year of followup?
n_black <- length(unique(dt[race == 2, qid]))
pct_dead_last_black <- nrow(dt[(year == last_year_ffs - 1) & (dead_lead == 1) & (race == 2)]) / 
  n_black
paste0(pct_dead_last_black * 100, " % of black individuals died in last year of follow-up")

# asian
n_asian <- length(unique(dt[race == 4, qid]))
pct_dead_last_asian <- nrow(dt[(year == last_year_ffs - 1) & (dead_lead == 1) & (race == 4)]) / 
  n_asian
paste0(pct_dead_last_asian * 100, " % of asian individuals died in last year of follow-up")

# try to get one table for everyone


# what % of white and black are Medicaid eligible?
dt[, .(pct_dual = mean(dual)), by = race]

# what % of rows have outcome = 1 (death)?
pct_outcome <- mean(dt[,dead_lead]) # 5.4%
paste0(round(pct_outcome*100, 1), "% of rows have death in the following year")
# by division?
dt[, .(pct_dead = mean(dead_lead)), by = census_div]
# by race?
dt[, .(pct_dead = mean(dead_lead)), by = race]
# by race and dual eligibilty?
race_dual_tab <- dt[race_dual %in% c("1_0", "1_1", "2_0", "2_1"), 
   .(pct_dead = mean(dead_lead)), by = race_dual]

race_dual_tab %<>%
  as.data.frame() %>%
  mutate(race_dual = case_when(
    race_dual == "1_0" ~ "white, not eligible",
    race_dual == "1_1" ~ "white, eligible",
    race_dual == "2_0" ~ "black, not eligible",
    race_dual == "2_1" ~ "black, eligible"
  ),
  pct_dead = pct_dead * 100
  )

race_dual_tab$race_dual <- factor(race_dual_tab$race_dual, 
                                  levels = c("white, not eligible", "black, not eligible",
                                             "white, eligible", "black, eligible"))

race_dual_tab %<>%
  arrange(race_dual)

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

# PM2.5 by race
dt |>
  ggplot(aes(pm25)) +
  geom_density(aes(col = race)) +
  labs(x = expression(PM[2.5]),
       y = "Count") +
  scale_y_continuous(labels = label_comma()) +
  #geom_vline(xintercept = 10, col = "black", linetype = 2) + 
  #geom_vline(xintercept = 12, col = "black", linetype = 2) + 
  # facet_wrap(~census_div, nrow = 3) +
  # geom_text(data = expo_12_division,
  #           mapping = aes(x = Inf, y = Inf, label = exposed_12),
  #           hjust   = 1.2,
  #           vjust   = 1.5
  # ) +
  theme_bw() +
  theme(strip.background = element_rect(fill = "white"))

# PM2.5 by dual eligibility
dt |>
  ggplot(aes(pm25)) +
  geom_density(aes(col = as.factor(dual))) +
  labs(x = expression(PM[2.5]),
       y = "Count") +
  scale_y_continuous(labels = label_comma()) +
  #geom_vline(xintercept = 10, col = "black", linetype = 2) + 
  #geom_vline(xintercept = 12, col = "black", linetype = 2) + 
  # facet_wrap(~census_div, nrow = 3) +
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
  facet_wrap(~race, nrow = 2) +
  theme_light()

# distribution of age at death
dt[dead_lead == 1,] |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  geom_density(aes(age + 1, col = race)) +
  labs(x = "Age at death",
       y = "Density") +
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

# death in which year of follow-up? by race
dt[dead_lead == 1,] |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  geom_density(aes(year_follow, col = race), bw = 0.9) +
  labs(x = "Year of follow-up at death",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# death in which year of follow-up? by dual
dt[dead_lead == 1,] |>
  ggplot() +
  geom_density(aes(year_follow, col = as.factor(dual)), bw = 0.9) +
  labs(x = "Year of follow-up at death",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# death in which year of follow-up? by race + dual
dt[dead_lead == 1,] |>
  filter(race_dual %in% c("1_0", "1_1", "2_0", "2_1")) |>
  ggplot() +
  geom_density(aes(year_follow, col = race_dual), bw = 0.9) +
  labs(x = "Year of follow-up at death",
       y = "Number of individuals") +
  scale_y_continuous(labels = label_comma()) +
  theme_light()

# what percentage of individuals died in each rolling window?
pct_dead <- dt[, .(pct_dead = mean(dead_lead)*100), by = .(year, race)]
pct_dead |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  geom_line(aes(x = year, y = pct_dead, col = race)) + 
  labs(x = "Exposure year", y = "% dead in following year") +
  ylim(c(0, 8)) + 
  theme_light()

# same for follow-up year
pct_dead <- dt[, .(pct_dead = mean(dead_lead)*100), by = .(year_follow, race)]
pct_dead |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  geom_line(aes(x = year_follow, y = pct_dead, col = race)) + 
  labs(x = "Year of follow-up", y = "% dead in following year") +
  #ylim(c(0, 13)) + 
  theme_light()

# mean age by year
age_dist <- dt[, .(mean_age = mean(age)), by = .(race, year)]
age_dist |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  geom_line(aes(x = year, y = mean_age, col = race)) +
  labs(x = "Year", 
       y = "Mean age") +
  #ylim(c(0, 105)) + 
  theme_light()

# mean age by follow-up year
age_dist <- dt[, .(mean_age = mean(age)), by = .(year_follow, race)]
age_dist |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  geom_line(aes(x = year_follow, y = mean_age, col = race)) +
  labs(x = "Follow-up year", 
       y = "Mean age") +
  #ylim(c(0, 105)) + 
  theme_light()

# among 80 year olds, what % died in each follow-up year?
pct_dead_80 <- dt[age == 80, .(pct_dead = mean(dead_lead)), by = .(year_follow, race)]
pct_dead_80 |>
  mutate(race = case_when(
    race == 1 ~ "white",
    race == 2 ~ "black",
    race == 3 ~ "other",
    race == 4 ~ "asian",
    race == 5 ~ "hispanic",
    race == 6 ~ "native"
  )) |>
  ggplot() +
  geom_line(aes(x = year_follow, y = pct_dead, col = race)) +
  labs(title = "only 80 year olds",
       x = "Follow-up year", 
       y = "% died in the next year") +
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

# # # What % of people were censored (left FFS) before death?
# cens <- dt[year == last_year_ffs - 1 & !(dead_lead == 1),]
# nrow(cens) / n_indiv # 67%

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


#____________________________________________________________________________#


# ---------- Chronic conditions ----------#

cond_abbr <- paste0(c("hypoth", "ami", "alzh", "alzhdmta",
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
  xlim(c(0, 40)) +
  geom_point(col = "skyblue3", size = 3, alpha = 0.9) +
  labs(x = "% ever hosp before 2010",
       y = "",
       col = "") +
  theme_bw()


# how many chronic conditions did each person have?
num_ccw <- rowSums(dt[year == 2010, ..cond_abbr])

# plot
ggplot() +
  geom_histogram(aes(num_ccw), binwidth = 1, col = "gray40", fill = "skyblue2") +
  scale_y_continuous(labels = label_comma()) +
  labs(x = "Number of chronic conditions per person",
       y = "Count of individuals") +
  theme_light()

num_ccw_freq <- table(num_ccw)
num_ccw_freq
# about 24% have never been hospitalized with one of these conditions

#----------------------------------------------------------- by region

# # How common is each chronic condition by region?
# prev_region <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
#                   by = census_region,
#                   .SDcols = cond_abbr] |>
#   gather(key = "cond_abbr", value = "prev_region", -census_region)
# 
# # merge with full names for plotting
# prev_df <- left_join(conditions, prev_region, by = "cond_abbr") |>
#   left_join(prev_overall, by = "cond_abbr")
# 
# # make conditions factors with levels sorted by by overall prevalence (for plotting)
# prev_df$cond_name <- factor(prev_df$cond_name, levels = cond_name[order(prev_overall$prev_overall)])
# 
# # get min and max prevalence values across regions
# prev_min_max <- prev_df %>%
#   group_by(cond_name) %>%
#   summarize(min_prev = min(prev_region),
#             max_prev = max(prev_region))
# 
# # plot
# prev_df |>
#   ggplot(aes(x = prev_region, y = cond_name)) +
#   geom_segment(data = prev_min_max, aes(y = cond_name, yend = cond_name,
#                                         x = min_prev, xend = max_prev)) +
#   geom_point(aes(col = census_region), size = 3, alpha = 0.9) +
#   labs(x = "% ever hosp before 2010",
#        y = "",
#        col = "") +
#   theme_bw()
# 
# 
# #----------------------------------------------------------- by race
# 
# # How common is each chronic condition by race?
# prev_race <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
#                 by = race,
#                 .SDcols = cond_abbr] |>
#   gather(key = "cond_abbr", value = "prev_race", -race) |>
#   mutate(race = case_when(
#     race == 1 ~ "white",
#     race == 2 ~ "black",
#     race == 3 ~ "other",
#     race == 4 ~ "asian",
#     race == 5 ~ "hispanic",
#     race == 6 ~ "native"
#   )) |>
#   # REMOVE TWO GROUPS!!!
#   filter(race != "other" & race != "native")
# 
# 
# # merge with full names for plotting
# prev_df <- left_join(conditions, prev_race, by = "cond_abbr") |>
#   left_join(prev_overall, by = "cond_abbr")
# 
# # make conditions factors with levels sorted by by overall prevalence (for plotting)
# prev_df$cond_name <- factor(prev_df$cond_name, levels = cond_name[order(prev_overall$prev_overall)])
# 
# # get min and max prevalence values across regions
# prev_min_max <- prev_df %>%
#   group_by(cond_name) %>%
#   summarize(min_prev = min(prev_race),
#             max_prev = max(prev_race))
# 
# # plot
# prev_df |>
#   ggplot(aes(x = prev_race, y = cond_name)) +
#   geom_segment(data = prev_min_max, aes(y = cond_name, yend = cond_name,
#                                         x = min_prev, xend = max_prev)) +
#   geom_point(aes(col = race), size = 3, alpha = 0.7) +
#   #scale_color_brewer(palette = "Set2") +
#   labs(x = "Prevalence (%)",
#        y = "",
#        col = "") +
#   theme_bw()
# 
# 
# #----------------------------------------------------------- by race AND region
# 
# top_10_cond <- arrange(prev_overall, desc(prev_overall))$cond_abbr[1:10]
# 
# # How common is each chronic condition by race?
# prev_region_race <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
#                        by = .(census_region, race),
#                        .SDcols = cond_abbr] |>
#   gather(key = "cond_abbr", value = "prev_region_race", -c(race, census_region)) |>
#   mutate(race = case_when(
#     race == 1 ~ "white",
#     race == 2 ~ "black",
#     race == 3 ~ "other",
#     race == 4 ~ "asian",
#     race == 5 ~ "hispanic",
#     race == 6 ~ "native"
#   )) |>
#   # REMOVE TWO GROUPS!!!
#   filter(race != "other" & race != "native") |>
#   # keep only top 10 conditions
#   filter(cond_abbr %in% top_10_cond)
# 
# # merge with full names for plotting
# prev_df <- left_join(conditions, prev_region_race, by = "cond_abbr") |>
#   left_join(prev_overall, by = "cond_abbr") |>
#   filter(!is.na(prev_region_race))
# 
# # make conditions factors with levels sorted by by overall prevalence (for plotting)
# prev_df$cond_name <- factor(prev_df$cond_name, levels = cond_name[order(prev_overall$prev_overall)])
# 
# # get min and max prevalence values across regions
# prev_min_max <- prev_df %>%
#   group_by(cond_name, census_region) %>%
#   summarize(min_prev = min(prev_region_race),
#             max_prev = max(prev_region_race))
# 
# # plot
# prev_df |>
#   ggplot(aes(x = prev_region_race, y = cond_name)) +
#   geom_segment(data = prev_min_max, aes(y = cond_name, yend = cond_name,
#                                         x = min_prev, xend = max_prev)) +
#   geom_point(aes(col = race), size = 3, alpha = 0.7) +
#   #scale_color_brewer(palette = "Set2") +
#   labs(x = "Prevalence (%)",
#        y = "",
#        col = "") +
#   theme_bw() +
#   facet_wrap(~census_region) +
#   theme(strip.background = element_rect(fill = "white"))




#____________________________________________________________________________#


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



# ---------- Table 1 ----------#


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
tab1 <- table1(~Sex + `Age group` + Race + `Medicaid eligibility` + `Mean annual PM2.5 (ug/m3)` +
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

write_rds(tab1, file = "results/table1.rds")

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

dev.off()
