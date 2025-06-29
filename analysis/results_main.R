
### Get model results and create a figure for the effect of PM2.5 by subpop

library(tidyverse)
library(ggplot2)
library(patchwork) # plot layouts
library(xtable)


#----- load model results

# path to results

# path_res <- "results/models/unstratified/"
path_res <- "data/models/unstratified/"

# get all file names in directory
file_names <- list.files(path_res)

# restrict to the ones that aren't stratified
file_names <- file_names[!str_detect(file_names, "strat")]

# load into a list
coeff <- lapply(paste0(path_res, file_names), readRDS)

# name list elements with conditions
names(coeff) <- str_remove_all(file_names, pattern = "coeff_|.rds")


#----- get effect of PM2.5 and 95% CI

# ORs are for 10 ug/m3 increase in PM2.5

get_pm25_effect <- function(x){
  
  # get row for PM2.5
  pm25_row <- x["pm25", ]
  
  # get OR and CIs
  pm25_OR <- (pm25_row["Estimate"] * 10) |> exp()
  pm25_OR_low <- ((pm25_row["Estimate"] * 10) - 1.96 * (pm25_row["Std. Error"] * 10)) |> exp()
  pm25_OR_high <- ((pm25_row["Estimate"] * 10) + 1.96 * (pm25_row["Std. Error"] * 10)) |> exp()
  
  # get p-value
  pm25_pval <- pm25_row["Pr(>|z|)"]
  
  names(pm25_OR) <- names(pm25_OR_low) <- names(pm25_OR_high) <- names(pm25_pval) <- NULL

  return(c(OR = pm25_OR, low = pm25_OR_low, high = pm25_OR_high, pval = pm25_pval))
}

# get all results in df
pm25_df <- sapply(coeff, get_pm25_effect) |> 
  t() |> 
  as.data.frame()

# add column for condition
pm25_df$cond_abbr <- rownames(pm25_df)

# adjust condition-specific subgroup p-values with Benjamini–Yekutieli 
# don't adjust for fullpop
pm25_subs_df <- pm25_df %>%
  filter(cond_abbr != "fullpop") |>
  mutate(pval_adj = p.adjust(pval, method = "BY"))

# join back with fullpop row
pm25_df <- left_join(pm25_df, pm25_subs_df)



#---- prepare data for plotting

# get condition full names too
cond_abbr <- c("hypoth", "ami", 
               "alzh", "alzhdmta",
               "anemia", "asthma", "atrialfb", "hyperp", 
               "breastCancer", "colorectalCancer", "endometrialCancer", 
               "lungCancer", "prostateCancer", "cataract", "chrnkidn", 
               "copd", "depressn", "diabetes", "glaucoma", 
               "chf", "hipfrac", "hyperl", "hypert", 
               "ischmcht", "osteoprs", "ra_oa", 
               "stroke", "nohosp", 
               "fullpop")

cond_name_short <- c("Hypothyroidism", "AMI",
                     "Alzheimer's", "Dementia", 
                     "Anemia", "Asthma", "AFib", "BPH",
                     "Breast Cancer", "Colorectal Cancer", "Endometrial Cancer",
                     "Lung Cancer", "Prostate Cancer", "Cataract", "CKD",
                     "COPD/Bronchiectasis", "Depression", "Diabetes", "Glaucoma",
                     "Heart Failure", "Hip/Pelvic Fracture", "Hyperlipidemia", "Hypertension",
                     "CHD", "Osteoporosis", "RA/OA", 
                     "Stroke/TIA", "None", 
                     "Full cohort")

cond_name_long <- c("Acquired Hypothyroidism", "Acute Myocardial Infarction",
               "Alzheimer's Disease", "ADRD or Senile Dementia", 
               "Anemia", "Asthma", "Atrial Fibrillation", "Benign Prostatic Hyperplasia",
               "Breast Cancer", "Colorectal Cancer", "Endometrial Cancer",
               "Lung Cancer", "Prostate Cancer", "Cataract", "Chronic Kidney Disease",
               "COPD and Bronchiectasis", "Depression", "Diabetes", "Glaucoma",
               "Heart Failure", "Hip/Pelvic Fracture", "Hyperlipidemia", "Hypertension",
               "Ischemic Heart Disease", "Osteoporosis", "Rheumatoid Arthritis/Osteoarthritis", 
               "Stroke/Transient Ischemic Attack", "None of these hospitalizations", 
               "Full cohort")

cond_df <- data.frame("cond_abbr" = cond_abbr, 
                      "cond_name_short" = cond_name_short,
                      "cond_name_long" = cond_name_long)

# join with full names
pm25_df <- left_join(pm25_df, cond_df, by = "cond_abbr")

# get prevalence data
prev_df <- readRDS("results/subgroup_prevalence.rds")

# join with plotting data
pm25_df <- left_join(pm25_df, prev_df, by = "cond_abbr")

# new column with condition name and prevalence
pm25_df <- pm25_df |>
  mutate(cond_name_prev = paste0(cond_name_long, " (", 
                                 formatC(round(prevalence, 1), format = "f", digits = 1), 
                                 "%)"))

# sort by OR
low_to_high <- pm25_df$cond_name_prev[order(pm25_df$OR)]
pm25_df$cond_name_prev <- factor(pm25_df$cond_name_prev,
                            #levels = rev(pm25_df$cond_name_prev[order(pm25_df$OR)]),
                            levels = low_to_high)
# # but now get full population at the top
# pm25_df$cond_name_prev <- factor(pm25_df$cond_name_prev,
#                                  levels = c("Full cohort (100.0%)", low_to_high))

# save plotting levels (use same order for stratified models)
cond_abbr_order <- pm25_df$cond_abbr[order(pm25_df$OR)]
saveRDS(cond_abbr_order, "data/intermediate/cond_abbr_OR_order.rds")


#----- plot

# try facets
pdf("results/figures/subpop_effects.pdf", width = 11, height = 10)
pm25_df |> 
  filter(cond_abbr != "nohosp") |> # remove no previous hosp (biased)
  mutate(special = factor(ifelse(cond_abbr %in% c("nohosp", "fullpop"), TRUE, FALSE), 
                          levels = c(FALSE, TRUE)#,
                          #levels = c(TRUE, FALSE)
                          )#,
         # highrisk = ifelse(OR > pm25_df$OR[pm25_df$cond_abbr == "fullpop"], 
         #                   "high", "low")
         ) |>
  ggplot(aes(x = OR, y = cond_name_prev#, #col = special
             #col = highrisk
             )) +
  geom_point() +
  geom_errorbar(aes(xmin = low, xmax = high), width = 0) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  #geom_vline(xintercept = pm25_df$OR[pm25_df$cond_abbr == "fullpop"], lty = 2, col = "gray50") +
  labs(x = expression("OR (95% CI) for mortality with 10 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "") +
  #xlim(c(0.998, 1.015)) +
  #scale_color_manual(values = c(`TRUE` = "black", `FALSE` = "black")) +
  facet_grid(special ~ ., scales = "free_y", space = "free") +
  # geom_text(aes(x = 0.999, y = cond_name_prev, label = sig)) +
  theme_classic(base_size = 16) +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(),
        strip.text = element_blank(),
        axis.text.y = element_text(size = 16))
dev.off()


#########################################

# table with the same results

n_digits <- 3

pm25_df <- pm25_df %>%
  mutate(or_ci = paste0(
    formatC(round(OR, n_digits), format = "f", digits = n_digits),
    " (",
    formatC(round(low, n_digits), format = "f", digits = n_digits),
    ", ",
    formatC(round(high, n_digits), format = "f", digits = n_digits),
    ")"
  ))

# or_ci_df <- pm25_df %>%
#   arrange(OR) %>%
#   select(cond_name, or_ci)
# 
# 
# # set levels so it will be in the same order as the data
# or_ci_df$cond_name <- factor(or_ci_df$cond_name,
#                              levels = or_ci_df$cond_name)
# 
# names(or_ci_df) <- c("Condition", "OR (95% CI)")
# 
# # plot the table as text
# p_right <- or_ci_df %>%
#   ggplot() +
#   geom_text(aes(x = 0, y = Condition, label = `OR (95% CI)`)) +
#   labs(y = "") +
#   xlim(-0.01, 0.01) +
#   theme_void()
# 
# # layout for two plots
# layout <- c(
#   # left plot, starts at the top of the page (0) and goes 30 units down and 3 units to the right
#   area(t = 0, l = 0, b = 30, r = 10),
#   # right most plot starts at top of page, begins where middle plot ends (l=9, and middle plot is r=9),
#   # goes to bottom of page (b=30), and extends two units wide (r=11)
#   area(t = 0, l = 11, b = 30, r = 13)
# )
# 
# # lay out plots with patchwork
# p_both <- p_left + p_right + plot_layout(design = layout)
# 
# # save figure
# pdf("results/figures/subpop_effects_text.pdf", width = 10, height = 6)
# p_both
# dev.off()


#########################################

# save latex table with OR (CI) and p-values

main_res_df <- pm25_df %>%
  filter(cond_abbr != "nohosp") %>%
  arrange(desc(OR)) %>%
  arrange(desc(cond_abbr == "fullpop")) %>%
  select(cond_name_long, or_ci, pval, pval_adj) %>%
  mutate(pval = ifelse(pval < 0.001,
                       "\\textless 0.001",
                       format(round(pval, digits = n_digits), nsmall = n_digits)),
         pval_adj = ifelse(pval_adj < 0.001,
                       "\\textless 0.001",
                       format(round(pval_adj, digits = n_digits), nsmall = n_digits))) %>%
  mutate(pval = ifelse(pval > 0.999,
                       "\\textgreater 0.999",
                       pval),
         pval_adj = ifelse(pval_adj > 0.999,
                           "\\textgreater 0.999",
                           pval)) %>%
  mutate(pval_adj = ifelse(is.na(pval_adj), 
                           "---", 
                           pval_adj)) %>%
  rename(Subgroup = cond_name_long,
         `Odds ratio (95\\% CI)` = or_ci,
         `P-value` = pval,
         `Adjusted p-value` = pval_adj)

# print in latex
print(xtable(main_res_df[,1:2],
             type = "latex",
             label = "tab:main_OR"),
      file = "results/tables/main_OR.tex", 
      sanitize.text.function = identity,
      include.rownames = FALSE)

