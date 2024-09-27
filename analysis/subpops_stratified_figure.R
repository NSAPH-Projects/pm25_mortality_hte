
### Get model results and create a figure for the effect of PM2.5 by subpop, stratified

# need at least 24 GB to run this script!

library(tidyverse)
library(ggplot2)
library(xtable)

# path to save figures
fig_path <- "results/figures/"

# haven't saved any figures yet


#----- load model results

# path to results
path_res <- "results/subpop/stratified/"

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
    
    strat_var == "race" & strat == 1 ~ "white",
    strat_var == "race" & strat == 2 ~ "black",
    strat_var == "race" & strat == 3 ~ "other",
    strat_var == "race" & strat == 4 ~ "asian",
    strat_var == "race" & strat == 5 ~ "hispanic",
    strat_var == "race" & strat == 6 ~ "native",
    
    strat_var == "dual" & strat == 1 ~ "Medicaid eligible",
    strat_var == "dual" & strat == 0 ~ "Medicaid ineligible",
    
    strat_var == "old" & strat == 1 ~ "Above median age",
    strat_var == "old" & strat == 0 ~ "Below median age",
    
    strat_var == "urban" & strat == 1 ~ "above median popdensity",
    strat_var == "urban" & strat == 0 ~ "below median popdensity",
    
    TRUE ~ strat
  ))


# commenting out for now so I don't have to fill these in with the new variables yet
# # set factor to choose order of strata in plotting
# # order only matters within a stratum
# pm25_df$strat <- factor(pm25_df$strat,
#                        levels = c("Non-Hispanic white", "Non-white", 
#                                   "Female", "Male",
#                                   "Midwest", "South", "Northeast", "West"))

# plot in same order used for overall OR 
cond_abbr_order <- readRDS("data/intermediate/cond_abbr_OR_order.rds")

# # sort by OR
# pm25_df <- pm25_df %>%
#   arrange(cond_abbr_order)
# 
# pm25_df <- pm25_df %>% filter(race == "Non-Hispanic white")
# pm25_df$cond_name <- factor(pm25_df$cond_name, 
#                             levels = pm25_df$cond_name[order(cond_abbr_order)])

pm25_df$cond_abbr <- factor(pm25_df$cond_abbr, 
                            levels = cond_abbr_order)
pm25_df <- pm25_df %>%
  arrange(cond_abbr)
pm25_df$cond_name <- factor(pm25_df$cond_name, 
                            levels = unique(pm25_df$cond_name))


############################################## 

# remove full population
pm25_df <- pm25_df %>%
  filter(cond_abbr != "fullpop")



#----- plot for race_white

plot_race_white <- pm25_df |>
  filter(strat_var == "race_white") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = "") +
  scale_color_manual(values = c(`Non-Hispanic white` = "skyblue3", `Non-white` = "orange2")) +
  geom_text(aes(x = 0.99, y = cond_name, label = sig),
            position = position_dodge(width = 0.5),
            show.legend = FALSE) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(reverse = TRUE))


#----- plot for sex

plot_sex <- pm25_df |>
  filter(strat_var == "sex") %>%
  # remove sex-specific conditions
  filter(!cond_abbr %in% c("prostateCancer", "hyperp", "endometrialCancer", "breastCancer")) %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = "") +
  scale_color_manual(values = c(`Male` = "#6CAE75", `Female` = "#6F40A8")) +
  geom_text(aes(x = 0.998, y = cond_name, label = sig),
            position = position_dodge(width = 0.5),
            show.legend = FALSE) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(reverse = TRUE))


#----- plot for census region

plot_census_region <- pm25_df |>
  filter(strat_var == "census_region") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = "") +
  scale_color_manual(values = c("Midwest" = "#414A90", 
                                "Northeast" = "#3FAFD4",
                                "South" = "#3DB71B", 
                                "West" = "#C9D337")) +
  geom_text(aes(x = 0.987, y = cond_name, label = sig),
            position = position_dodge(width = 0.8),
            show.legend = FALSE) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(reverse = TRUE))


#----- plot for race (4 groups)

plot_race <- pm25_df |>
  filter(strat_var == "race",
         !strat %in% c("native", "other")) %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  geom_point(position = position_dodge(width = 0.7)) +
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.7)) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = "") +
  #scale_color_manual(values = c(`Non-Hispanic white` = "skyblue3", `Non-white` = "orange2")) +
  geom_text(aes(x = 0.95, y = cond_name, label = sig),
            position = position_dodge(width = 0.6),
            show.legend = FALSE) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(reverse = TRUE))


#----- plot for age

plot_old <- pm25_df |>
  filter(strat_var == "old") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = "") +
  #scale_color_manual(values = c(`Non-Hispanic white` = "skyblue3", `Non-white` = "orange2")) +
  geom_text(aes(x = 0.995, y = cond_name, label = sig),
            position = position_dodge(width = 0.5),
            show.legend = FALSE) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(reverse = TRUE))


#----- plot for dual

plot_dual <- pm25_df |>
  filter(strat_var == "dual") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = "") +
  #scale_color_manual(values = c(`Non-Hispanic white` = "skyblue3", `Non-white` = "orange2")) +
  geom_text(aes(x = 0.995, y = cond_name, label = sig),
            position = position_dodge(width = 0.5),
            show.legend = FALSE) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(reverse = TRUE))


#----- plot for urban

plot_urban <- pm25_df |>
  filter(strat_var == "urban") %>%
  ggplot(aes(x = OR, y = cond_name, group = strat, col = strat)) +
  geom_point(position = position_dodge(width = 0.4)) +
  geom_errorbar(aes(xmin = low, xmax = high), 
                width = 0, position = position_dodge(width = 0.4)) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR (95% CI) for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "",
       col = "") +
  #scale_color_manual(values = c(`Non-Hispanic white` = "skyblue3", `Non-white` = "orange2")) +
  geom_text(aes(x = 0.995, y = cond_name, label = sig),
            position = position_dodge(width = 0.5),
            show.legend = FALSE) +
  theme_classic(base_size = 12) +
  theme(legend.position = "bottom",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        legend.text = element_text(size = 12)) +
  guides(color = guide_legend(reverse = TRUE))




############################################## 

# combine all of these into one plot

# can do facet_wrap but I don't want them all in one legend

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


library(cowplot)

all_plots <- align_plots(plot_old, plot_sex, plot_race, 
                         plot_dual, plot_census_region, plot_urban,
                         align = "hv")

pdf("results/figures/six_plots_stratified.pdf", height = 15, width = 13)
plot_grid(all_plots[[1]], all_plots[[2]], all_plots[[3]], 
          all_plots[[4]], all_plots[[5]], all_plots[[6]],
          ncol = 2)
dev.off()


###################################################################################

#--- get table with ORs from all models

# do a table with headers that go across multiple columns
# the column "names" can be 
# 27 rows (+nohosp) for each disease

# new column with proper formatting
pm25_df <- pm25_df %>%
  mutate(or_ci = paste0(
    formatC(round(OR, 3), format = "f", digits = 3), 
    " (", 
    formatC(round(low, 3), format = "f", digits = 3), 
    ", ", 
    formatC(round(high, 3), format = "f", digits = 3), 
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

# print in latex
print(xtable(or_tab,
             type = "latex",
             label = "tab:stratified_OR_table",
             # caption = "Odds ratios (95\\% confidence intervals) for mortality associated 
             # with a 1 \\(\mu g/m^3\\) increase in PM\\(_{2.5}\\) from the stratified models."
             caption = "Messy placeholder table. Has 28 rows and 21 columns."
             ),
      file = "results/tables/stratified_OR_table.tex", 
      sanitize.text.function = identity,
      include.rownames = FALSE)

