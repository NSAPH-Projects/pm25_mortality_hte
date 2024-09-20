
# script to visualize chronic conditions

library(data.table)
library(fst)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(stringr)
library(table1)
library(magrittr)
library(arrow)
library(proxy) # for heat map

# # Load data
# dt <- readRDS("data/intermediate/rolling_cohort.rds")

# SAMPLE FOR NOW
load("data/intermediate/rolling_cohort_1000.RData")

# path to save figures
fig_path <- "results/figures/"


#################################

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


#################################

# How common is each chronic condition before exposure in 2010?
prev_overall <- dt[year == 2010, lapply(.SD, function(x) sum(x) / .N * 100),
                   .SDcols = cond_abbr] |>
  gather(key = "cond_abbr", value = "prev_overall") |>
  left_join(conditions, by = "cond_abbr")

# new column with condition name and prevalence
prev_overall <- prev_overall |>
  mutate(cond_name_prev = paste0(cond_name, " (", 
                                 formatC(round(prev_overall, 1), format = "f", digits = 1), 
                                 "%)"))

# make conditions factors with levels sorted by by overall prevalence (for plotting)
prev_overall$cond_name_prev <- factor(prev_overall$cond_name_prev,
                                 levels = prev_overall$cond_name_prev[order(prev_overall$prev_overall)])

# plot
pdf(paste0(fig_path, "ccw_prevalence.pdf"), width = 5, height = 5)
prev_overall |>
  arrange(prev_overall) |>
  ggplot(aes(x = prev_overall, y = cond_name_prev)) +
  xlim(c(0, 40)) +
  geom_vline(xintercept = 0, col = "gray70") +
  geom_point(col = "orange2", size = 3) +
  labs(x = "% of individuals",
       y = "",
       col = "") +
  theme_minimal() +
  theme(plot.background = element_rect(fill = "white", color = NA),
        text = element_text(family = "Helvetica"))
dev.off()


#################################

# how many chronic conditions did each person have?
num_ccw <- rowSums(dt[year == 2010, ..cond_abbr])
num_ccw_freq <- table(num_ccw)
num_ccw_freq

#################################

# density of prevalence over the years? (color by condition)

prev_year <- dt[, lapply(.SD, function(x) sum(x) / .N * 100),
                   .SDcols = cond_abbr, by = year]

prev_year <- pivot_longer(prev_year,
                     cols = cond_abbr,
               names_to = "cond_abbr",
               values_to = "prevalence") |>
  left_join(conditions, by = "cond_abbr")


# get 2016 prev
prev_2015 <- prev_year |>
  filter(year == 2015) |>
  select(prevalence, cond_abbr) |>
  rename("prev_2015" = "prevalence")

prev_year <- left_join(prev_year, prev_2015, by = "cond_abbr")


# plot indicators over time
prev_year |>
  ggplot() +
  geom_line(aes(x = year, 
                y = prevalence, 
                group = cond_name),
            col = "gray50") +
  geom_text(aes(x = 2016, y = prev_2015, label = cond_name)) +
  labs(x = "Year", 
       y = "% of individuals") +
  xlim(2000, 2018) +
  theme_minimal()
  
  

#################################

# correlation between conditions (heat map)

# 27 chronic conditions
condition_cols <- dt %>%
  filter(year == "2010") %>%
  #select(names(dt)[grep("^((?!pm25).)*_ever$", names(dt), perl = TRUE)])
  select(names(dt)[grep("_ever", names(dt))])
#colnames(condition_cols) <- sub("_ever$", "", colnames(condition_cols))
colnames(condition_cols) <- cond_name # only works if dfs are in the same order

# Pearson's correlation (works for binary data)
cor_df <- cor(condition_cols)

# # Custom color scale
# custom_colors <- colorRampPalette(c("blue", "white", "red"))(100)

# make heat map
# this should have a legend, but base R doesn't have an option for that
pdf(paste0(fig_path, "ccw_heatmap.pdf"), width = 6, height = 6)
cor_df %>%
  heatmap(Rowv = NA, 
          Colv = NA,
          margins = c(13, 13),
          #col = custom_colors,
          symm = TRUE, # determines if it's symmetrical or not
          revC = TRUE)
dev.off()


# max correlation aside from 1
max(cor(condition_cols)[cor(condition_cols) != 1])




