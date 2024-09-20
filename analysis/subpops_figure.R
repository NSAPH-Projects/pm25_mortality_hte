
### Get model results and create a figure for the effect of PM2.5 by subpop

library(tidyverse)
library(ggplot2)

#----- load model results

# path to results
path_res <- "results/subpop/unstratified/"

# get all file names in directory
file_names <- list.files(path_res)

# restrict to the ones that aren't stratified
file_names <- file_names[!str_detect(file_names, "strat")]

# load into a list
coeff <- lapply(paste0(path_res, file_names), readRDS)

# name list elements with conditions
names(coeff) <- str_remove_all(file_names, pattern = "coeff_|.rds")


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

# get all results in df
pm25_df <- sapply(coeff, get_pm25_effect) |> 
  t() |> 
  as.data.frame()

# adjust p-values with Benjamini-Hochberg
pm25_df <- pm25_df %>%
  mutate(#pval_adj = p.adjust(pval, method = "BH")
         pval_adj = p.adjust(pval, method = "BY"))

# stars for significance
pm25_df <- pm25_df %>% 
  mutate(sig = ifelse(pval_adj < 0.001, "***", 
                      ifelse(pval_adj < 0.01 & pval_adj >= 0.001, "**", 
                             ifelse(pval_adj < 0.05 & pval_adj >= 0.01, "*", ""))))

#---- prepare data for plotting

# add column for condition
pm25_df$cond_abbr <- rownames(pm25_df)

# get condition full names too
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

# get prevalence data
prev_df <- readRDS("results/subpop_prevalence.rds")

# join with plotting data
pm25_df <- left_join(pm25_df, prev_df, by = "cond_abbr")

# new column with condition name and prevalence
pm25_df <- pm25_df |>
  mutate(cond_name_prev = paste0(cond_name, " (", 
                                 formatC(round(prevalence, 1), format = "f", digits = 1), 
                                 "%)"))

# sort by OR
pm25_df$cond_name_prev <- factor(pm25_df$cond_name_prev,
                            levels = pm25_df$cond_name_prev[order(pm25_df$OR)])

# save plotting levels (use same order for stratified models)
cond_abbr_order <- pm25_df$cond_abbr[order(pm25_df$OR)]
saveRDS(cond_abbr_order, "data/intermediate/cond_abbr_OR_order.rds")


#----- plot

pdf("results/figures/subpop_effects.pdf", width = 7, height = 6)
pm25_df |>
  mutate(special = ifelse(cond_abbr %in% c("nohosp", "fullpop"), TRUE, FALSE)) |>
  ggplot(aes(x = OR, y = cond_name_prev, col = special)) +
  geom_point() +
  geom_errorbar(aes(xmin = low, xmax = high), width = 0) +
  geom_vline(xintercept = 1, lty = 2, col = "gray50") +
  labs(x = expression("OR for mortality with 1 " * mu * "g/" * m^3 * " increase in " * PM[2.5]), 
       y = "") +
  scale_color_manual(values = c(`TRUE` = "salmon", `FALSE` = "black")) +
  geom_text(aes(x = 0.999, y = cond_name_prev, label = sig)) +
  theme_classic(base_size = 12) +
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line())
dev.off()
