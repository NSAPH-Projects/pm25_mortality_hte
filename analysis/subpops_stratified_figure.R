
### Get model results and create a figure for the effect of PM2.5 by subpop, stratified

# need at least 24 GB to run this script!

library(tidyverse)
library(ggplot2)
library(xtable)
library(cowplot)


#----- load model results

# path to results
path_res <- "results/models/stratified/"

# get all file names in directory
file_names <- list.files(path_res)

# restrict to the ones that are stratified
file_names <- file_names[str_detect(file_names, "strat")]

# # restrict to the ones stratified by race_white
# file_names <- file_names[str_detect(file_names, "race_white")]

# load into a list
coeff <- lapply(paste0(path_res, file_names), readRDS)

# name list elements with conditions and race
names(coeff) <- str_remove_all(file_names, pattern = "coeff_|.rds|_strat")


#----- get effect of PM2.5 and 95% CI

get_pm25_effect <- function(x){
  
  # get row for PM2.5
  pm25_row <- x["pm25", ]
  
  # get OR and CIs
  pm25_OR <- pm25_row["Estimate"] |> exp()
  pm25_OR_low <- (pm25_row["Estimate"] - 1.96 * pm25_row["Std. Error"]) |> exp()
  pm25_OR_high <- (pm25_row["Estimate"] + 1.96 * pm25_row["Std. Error"]) |> exp()
  
  # get p-value
  pm25_pval <- pm25_row["Pr(>|z|)"]
  
  names(pm25_OR) <- names(pm25_OR_low) <- names(pm25_OR_high) <- names(pm25_pval) <- NULL
  
  return(c(OR = pm25_OR, low = pm25_OR_low, high = pm25_OR_high, pval = pm25_pval))
}

# apply function and get all results in df
pm25_df <- sapply(coeff, get_pm25_effect) |> 
  t() |> 
  as.data.frame()

# adjust p-values with Benjamini-Hochberg
pm25_df <- pm25_df %>%
  mutate(pval_adj = p.adjust(pval, method = "BY"))

# stars for significance
pm25_df <- pm25_df %>% 
  mutate(sig = ifelse(pval_adj < 0.001, "***", 
                      ifelse(pval_adj < 0.01 & pval_adj >= 0.001, "**", 
                             ifelse(pval_adj < 0.05 & pval_adj >= 0.01, "*", ""))))


#---- prepare data for plotting

# add column for rownames (with condition and stratum)
pm25_df$cond_abbr_strat <- rownames(pm25_df)

# split into more columns
pm25_df <- pm25_df %>%
  mutate(cond_abbr = str_extract(cond_abbr_strat, "^[^_]+"), # condition abbreviation
         cond_abbr = ifelse(cond_abbr == "ra", "ra_oa", cond_abbr), # fix ra_oa naming
         strat_var = ifelse(str_detect(cond_abbr_strat, "race_white"), "race_white", NA), # stratify by
         strat_var = ifelse(str_detect(cond_abbr_strat, "sex"), "sex", strat_var),
         strat_var = ifelse(str_detect(cond_abbr_strat, "census_region"), "census_region", strat_var),
         strat_var = ifelse(str_detect(cond_abbr_strat, "dual"), "dual", strat_var),
         strat_var = ifelse(str_detect(cond_abbr_strat, "race(?!_white)"), "race", strat_var),
         strat_var = ifelse(str_detect(cond_abbr_strat, "old"), "old", strat_var),
         strat_var = ifelse(str_detect(cond_abbr_strat, "urban"), "urban", strat_var),
         strat = str_extract(cond_abbr_strat, "[^_]+$")) # stratum



#----- get condition full names too

cond_abbr <- c("hypoth", "ami", "alzh", "alzhdmta",
               "anemia", "asthma", "atrialfb", "hyperp", "breastCancer",
               "colorectalCancer", "endometrialCancer", "lungCancer", "prostateCancer",
               "cataract", "chrnkidn", "copd", "depressn", "diabetes", "glaucoma", "chf",
               "hipfrac", "hyperl", "hypert", "ischmcht", "osteoprs", "ra_oa", "stroke",
               "nohosp", "fullpop")

cond_name <- c("Acquired Hypothyroidism", "Acute Myocardial Infarction",
               "Alzheimer's Disease", "ADRD or Senile Dementia",
               "Anemia",
               "Asthma", "Atrial Fibrillation", "Benign Prostatic Hyperplasia",
               "Cancer, Female/Male Breast", "Cancer, Colorectal", "Cancer, Endometrial",
               "Cancer, Lung", "Cancer, Prostate", "Cataract", "Chronic Kidney Disease",
               "COPD and Bronchiectasis", "Depression", "Diabetes", "Glaucoma",
               "Heart Failure", "Hip/Pelvic Fracture", "Hyperlipidemia", "Hypertension",
               "Ischemic Heart Disease", "Osteoporosis",
               "Rheumatoid Arthritis/Osteoarthritis", "Stroke/Transient Ischemic Attack",
               "None of these hospitalizations", "Full population")

cond_df <- data.frame("cond_abbr" = cond_abbr, "cond_name" = cond_name)

# join with full names
pm25_df <- left_join(pm25_df, cond_df, by = "cond_abbr")



# make strata more clear for plotting
pm25_df <- pm25_df %>%
  mutate(strat = case_when(
    strat_var == "race_white" & strat == 1 ~ "Non-Hispanic white",
    strat_var == "race_white" & strat == 0 ~ "Non-white",
    
    strat_var == "sex" & strat == 1 ~ "Male",
    strat_var == "sex" & strat == 0 ~ "Female",
    
    strat_var == "race" & strat == 1 ~ "White",
    strat_var == "race" & strat == 2 ~ "Black",
    strat_var == "race" & strat == 3 ~ "Other",
    strat_var == "race" & strat == 4 ~ "Asian",
    strat_var == "race" & strat == 5 ~ "Hispanic",
    strat_var == "race" & strat == 6 ~ "Native",
    
    strat_var == "dual" & strat == 1 ~ "Medicaid eligible",
    strat_var == "dual" & strat == 0 ~ "Medicaid ineligible",
    
    strat_var == "old" & strat == 1 ~ "Above median age",
    strat_var == "old" & strat == 0 ~ "Below median age",
    
    strat_var == "urban" & strat == TRUE ~ "Urban",
    strat_var == "urban" & strat == FALSE ~ "Rural",
    
    TRUE ~ strat
  ))

# set factor to choose order of strata in plotting (legend right to left)
# order only matters within a stratum
pm25_df$strat <- factor(pm25_df$strat,
                       levels = c("Black", "White", "Hispanic", "Asian", "Other", "Native",
                                  "Female", "Male",
                                  "Midwest", "South", "Northeast", "West",
                                  "Medicaid eligible", "Medicaid ineligible",
                                  "Rural", "Urban"))

# plot conditions in same order used for overall OR 
cond_abbr_order <- readRDS("data/intermediate/cond_abbr_OR_order.rds")

pm25_df$cond_abbr <- factor(pm25_df$cond_abbr, 
                            levels = cond_abbr_order)
pm25_df <- pm25_df %>%
  arrange(cond_abbr)
pm25_df$cond_name <- factor(pm25_df$cond_name, 
                            levels = unique(pm25_df$cond_name))


############################################## 

# remove nohosp
pm25_df <- pm25_df %>%
  filter(cond_abbr != "nohosp")


# get elements of all plots
gg_strat <- list(
  geom_point(position = position_dodge(width = 0.4)),
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.4)),
  # geom_text(aes(x = 0.99, y = cond_name, label = sig),
  #           position = position_dodge(width = 0.5),
  #           show.legend = FALSE),
  geom_vline(xintercept = 1, lty = 2, col = "gray50"),
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = ""), 
  theme_classic(base_size = 12),
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)),
  guides(color = guide_legend(reverse = TRUE))
)


#----- plot for sex

plot_sex <- pm25_df |>
  filter(strat_var == "sex") %>%
  # remove sex-specific conditions
  filter(!cond_abbr %in% c("prostateCancer", "hyperp", "endometrialCancer", "breastCancer")) %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  scale_color_manual(values = c(`Female` = "#6F40A8", `Male` = "#6CAE75")) +
  gg_strat


#----- plot for census region

plot_census_region <- pm25_df |>
  filter(strat_var == "census_region") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  scale_color_manual(values = c("Midwest" = "#414A90", 
                                "South" = "#69c0dd",
                                "Northeast" = "#36a118", 
                                "West" = "#d8c94c")) +
  gg_strat


#----- plot for race (4 groups)

plot_race <- pm25_df |>
  filter(strat_var == "race",
         !strat %in% c("Native", "Other")) %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  scale_color_manual(values = c(`White` = "#eec200",
                                `Black` = "#0b6d5b",
                                `Hispanic` = "#4dbdbf",
                                `Asian` = "#dd5e09")) +
  gg_strat


#----- plot for age

# may not be valid since this is discrete time format
# individuals age into the older stratum

# plot_old <- pm25_df |>
#   filter(strat_var == "old") %>%
#   ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
#   gg_strat


#----- plot for dual

plot_dual <- pm25_df |>
  filter(strat_var == "dual") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  scale_color_manual(values = c(`Medicaid eligible` = "orange2",
                                `Medicaid ineligible` = "skyblue3")) +
  gg_strat


#----- plot for urban

plot_urban <- pm25_df |>
  filter(strat_var == "urban") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  scale_color_manual(values = c(Urban = "gray40", 
                                Rural = "#00cc00")) +
  gg_strat



############################################## 

# combine all of these into one plot

# could do facet_wrap but I don't want them all in one legend

# pm25_df |>
#   filter(strat_var != "race_white") |>
#   ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
#   geom_point(position = position_dodge(width = 0.4)) +
#   geom_errorbar(aes(xmin = low, xmax = high), 
#                 width = 0, position = position_dodge(width = 0.4)) +
#   geom_vline(xintercept = 1, lty = 2, col = "gray50") +
#   labs(x = expression("OR for mortality with 1 \u03bcg/" * m^3 * " increase in " * PM[2.5]), 
#        y = "",
#        col = "") +
#   #scale_color_manual(values = c(`Non-Hispanic white` = "skyblue3", `Non-white` = "orange2")) +
#   geom_text(aes(x = 0.995, y = cond_name, label = sig),
#             position = position_dodge(width = 0.5),
#             show.legend = FALSE) +
#   facet_wrap(~strat_var, ncol = 2) +
#   theme_classic(base_size = 12) +
#   theme(legend.position = "bottom",
#         axis.line.y = element_blank(),
#         axis.ticks.y = element_blank(),
#         panel.grid.major.y = element_line(),
#         legend.text = element_text(size = 12)) +
#   guides(color = guide_legend(reverse = TRUE))


all_plots <- align_plots(plot_sex, plot_race, plot_dual, 
                         plot_census_region, plot_urban,
                         align = "hv")

pdf("results/figures/six_plots_stratified.pdf", height = 15, width = 13)
plot_grid(all_plots[[1]], all_plots[[2]], all_plots[[3]], 
          all_plots[[4]], all_plots[[5]],
          ncol = 2)
dev.off()


###################################################################################

#--- get table with ORs from all models

# do a table with headers that go across multiple columns
# the column "names" can be 
# 27 rows (+nohosp) for each disease

n_digits <- 4

# new column with proper formatting
pm25_df <- pm25_df %>%
  mutate(or_ci = paste0(
    formatC(round(OR, n_digits), format = "f", digits = n_digits), 
    " (", 
    formatC(round(low, n_digits), format = "f", digits = n_digits), 
    ", ", 
    formatC(round(high, n_digits), format = "f", digits = n_digits), 
    ")"
  ))

# ID column for this purpose
pm25_df <- pm25_df %>%
  mutate(strat_var_x_strat = paste0(strat_var, "_x_", strat))

# select columns of interest
or_tab <- pm25_df %>%
  select(cond_name, strat_var_x_strat, or_ci)

# wide format (rows are conditions, columns are strat variables/strata)
or_tab <- or_tab %>%
  pivot_wider(names_from = strat_var_x_strat,
              values_from = or_ci)

# same order as other tables (by OR)
or_tab <- or_tab %>%
  arrange(match(cond_name, rev(levels(or_tab$cond_name))))

# split up this table into multiple!!
# print in latex
print(xtable(or_tab |> select(cond_name, 
                              names(or_tab)[str_detect(names(or_tab), "sex|dual")]),
             type = "latex",
             label = "tab:stratified_OR_table",
             caption = "sex and dual results"),
      file = "results/tables/stratified_OR_table_sex_dual.tex", 
      sanitize.text.function = identity,
      include.rownames = FALSE)

print(xtable(or_tab |> select(cond_name, 
                              race_x_White, race_x_Black, race_x_Hispanic, race_x_Asian),
             type = "latex",
             label = "tab:stratified_OR_table",
             caption = "race results"),
      file = "results/tables/stratified_OR_table_race.tex", 
      sanitize.text.function = identity,
      include.rownames = FALSE)

print(xtable(or_tab |> select(cond_name, 
                              names(or_tab)[str_detect(names(or_tab), "census_region")]),
             type = "latex",
             label = "tab:stratified_OR_table_census_region",
             caption = "census region results"),
      file = "results/tables/stratified_OR_table_census_region.tex", 
      sanitize.text.function = identity,
      include.rownames = FALSE)

print(xtable(or_tab |> select(cond_name, 
                              names(or_tab)[str_detect(names(or_tab), "urban")]),
             type = "latex",
             label = "tab:stratified_OR_table_urban",
             caption = "urban results"),
      file = "results/tables/stratified_OR_table_urban.tex", 
      sanitize.text.function = identity,
      include.rownames = FALSE)
