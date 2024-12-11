
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
library(patchwork)
library(xtable)

# Load data
dt <- readRDS("data/intermediate/rolling_cohort.rds")
# dt <- readRDS("data/intermediate/rolling_cohort_1000.rds")

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

# what % of observations are in each subgroup?
prev_overall <- dt[#year == 2010
                    , 
                   lapply(.SD, function(x) sum(x) / .N * 100),
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
p_left <- prev_overall |>
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

pdf(paste0(fig_path, "ccw_prevalence.pdf"), width = 5, height = 5)
p_left
dev.off()



#################################

# get number of people and number of observations for each subgroup

# summarize data for each indicator column
subgroup_n <- rbindlist(lapply(cond_abbr, function(col) {
  res <- dt[get(col) == 1, .(
    num_rows = .N,                   
    num_indiv = uniqueN(qid),
    pm25_mean = mean(pm25) |> round(1),
    pm25_sd = sd(pm25) |> round(1),
    death_rate = 100*mean(dead_lead)
  )]
  res[, cond_abbr := col]
}))

# clean up data for table
subgroup_n <- subgroup_n %>%
  left_join(conditions, by = "cond_abbr") %>%
  arrange(desc(num_rows)) %>%
  select(all_of(c("cond_name", "num_rows", "num_indiv", "pm25_mean", "pm25_sd", "death_rate")))

fullpop_row <- data.frame(cond_name = "Full population",
                          num_rows = dt[, .N],
                          num_indiv = dt[, length(unique(qid))],
                          pm25_mean = dt[, mean(pm25)] |> round(1),
                          pm25_sd = dt[, sd(pm25)] |> round(1),
                          death_rate = dt[, 100*mean(dead_lead)])

# new column that = 1 for rows with no previous hosp (all other indicators = 0)
dt[, nohosp := as.integer(rowSums(.SD) == 0), .SDcols = cond_abbr]

# new column for individuals who have nohosp == 1 for ALL observations
dt[, all_nohosp := all(nohosp == 1), by = qid]

nohosp_row <- data.frame(cond_name = "No subgroups",
                         num_rows = dt[nohosp == 1, .N],
                         num_indiv = dt[all_nohosp == 1, length(unique(qid))],
                         pm25_mean = dt[nohosp == 1, mean(pm25)] |> round(1),
                         pm25_sd = dt[nohosp == 1, sd(pm25)] |> round(1),
                         death_rate = dt[nohosp == 1, 100*mean(dead_lead)])

subgroup_n <- rbind(fullpop_row, nohosp_row, subgroup_n)

subgroup_n <- subgroup_n %>%
  mutate(death_rate = as.character(round(death_rate, 1))) %>%
  mutate(pm25_mean_sd = paste0(pm25_mean, " (", pm25_sd, ")"),
         num_rows = format(num_rows, big.mark = ",", trim = TRUE),
         num_indiv = format(num_indiv, big.mark = ",", trim = TRUE)) %>%
  select(cond_name, num_rows, num_indiv, pm25_mean_sd, death_rate) %>%
  rename(Subgroup = cond_name,
         obs = num_rows,
         indiv = num_indiv,
         pm25 = pm25_mean_sd,
         death = death_rate)

# print in latex
print(xtable(subgroup_n,
             type = "latex",
             label = "tab:subgroup_n",
             caption = "The number of observations (two-year rolling windows) and the number
             of unique beneficiaries in each subgroup."),
      file = "results/tables/subgroup_n.tex",
      sanitize.text.function = identity,
      include.rownames = FALSE)


### attempt at creating patchwork with plot

# # plot the table as text
# p_middle <- subgroup_n %>%
#   ggplot() +
#   geom_text(aes(x = 0, y = cond_name_prev, label = num_rows)) +
#   labs(y = "") +
#   xlim(-0.01, 0.01) +
#   theme_void() +
#   scale_y_discrete(limits = subgroup_n$cond_name_prev)
# 
# # plot the table as text
# p_right <- subgroup_n %>%
#   ggplot() +
#   geom_text(aes(x = 0, y = cond_name_prev, label = num_indiv)) +
#   labs(y = "") +
#   xlim(-0.01, 0.01) +
#   theme_void() +
#   scale_y_discrete(limits = subgroup_n$cond_name_prev)
# 
# header_middle <- ggplot() +
#   geom_text(aes(x = 0, y = 1, label = "# of obs.")) +
#   theme_void() +
#   xlim(-0.01, 0.01)
# 
# header_right <- ggplot() +
#   geom_text(aes(x = 0, y = 1, label = "# indiv.")) +
#   theme_void() +
#   xlim(-0.01, 0.01)
# 
# # Create an empty header for the left plot (spacer)
# header_left <- ggplot() +
#   geom_blank() +  # Creates an empty header
#   theme_void()
# 
# # Combine headers with their corresponding plots
# p_left_combined <- header_left / p_left + plot_layout(heights = c(0.1, 1))
# p_middle_combined <- header_middle / p_middle + plot_layout(heights = c(0.1, 1))
# p_right_combined <- header_right / p_right + plot_layout(heights = c(0.1, 1))
# 
# # Combine everything horizontally: left plot with space, middle, and right columns
# final_plot <- p_left_combined | p_middle_combined | p_right_combined
# 
# # save figure
# pdf("results/figures/ccw_prevalence_text.pdf", width = 10, height = 6)
# final_plot
# dev.off()



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
                     cols = all_of(cond_abbr),
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
  select(names(dt)[grep("_ever", names(dt))])

#colnames(condition_cols) <- sub("_ever$", "", colnames(condition_cols))
#colnames(condition_cols) <- cond_name # only works if dfs are in the same order

# Pearson's correlation (works for binary data)
cor_mat <- cor(condition_cols)

# min/max correlation (aside from 1)
max(cor_mat[cor_mat != 1])
min(cor_mat)

# DOESN'T WORK (just max and min is fine)
# # which are the min and max?
# max_index <- arrayInd(which.max(cor_mat[cor_mat != 1]), dim(cor_mat))
# min_index <- arrayInd(which.min(cor_mat), dim(cor_mat))
# 
# # Get the row and column indices for the largest element
# row_col_indices <- arrayInd(max_index, dim(cor_mat))
# 
# # Extract the row and column names
# row_name <- rownames(cor_mat)[row_col_indices[1]]
# col_name <- colnames(cor_mat)[row_col_indices[2]]

paste0("Correlation column means:")
colMeans(cor_mat)


cor_df <- as.data.frame(cor_mat)

# make the rownames a column
cor_df$cond_a <- rownames(cor_df)

# pivot longer
cor_df_long <- pivot_longer(cor_df,
                            cols = all_of(cond_abbr),
                            names_to = "cond_b",
                            values_to = "pcorr")

# join with condition names
cor_df_long <- cor_df_long %>%
  left_join(conditions, by = c("cond_a" = "cond_abbr")) %>%
  rename(c(cond_name_a = cond_name)) %>%
  left_join(conditions, by = c("cond_b" = "cond_abbr")) %>%
  rename(c(cond_name_b = cond_name))

# # only get triangle
# # something is going wrong here! hard to read anyways
# cor_df_long <- cor_df_long %>%
#   filter(cond_a >= cond_b)

# make sure the two condition columns are factors with the same levels!
cor_df_long$cond_name_a <- factor(cor_df_long$cond_name_a,
                             levels = rev(cond_name))
cor_df_long$cond_name_b <- factor(cor_df_long$cond_name_b,
                             levels = rev(cond_name))

# plot
pdf(paste0(fig_path, "ccw_heatmap.pdf"), width = 10, height = 7.5)
cor_df_long %>%
  ggplot(aes(x = cond_name_a, y = cond_name_b)) +
  geom_tile(aes(fill = pcorr), color = "white") +
  scale_fill_gradient(low = "white", high = "red3") +
  labs(x = "", y = "", fill = "Pearson's correlation \ncoefficient \n") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)) +
  guides(fill = guide_colorbar(ticks.colour = NA,
                               barheight = 13,
                               barwidth = 1))
dev.off()


