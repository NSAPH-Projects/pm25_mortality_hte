########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### LASSO
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(dplyr)
library(glmnet)
library(stabs)
library(stringr)

load("data/intermediate/rolling_cohort.RData")

# Work with a random sample for now
set.seed(17)
keep_idx <- sample(unique(dt[,qid]), 30000000, replace = FALSE)
dt <- dt[qid %in% keep_idx,]
gc()

# save(dt, file = "data/intermediate/rolling_cohort_1000.RData")
# 
# load("data/intermediate/rolling_cohort_1000.RData")

# set.seed(17)
# keep_idx <- sample(unique(dt[,qid]), 100, replace = FALSE)
# dt <- dt[qid %in% keep_idx,]

#-------- prepare data

#--- get indicators for year, follow-up year, and census region

# make factors
dt[, year := factor(year)]
dt[, year_follow := factor(year_follow)]
dt[, census_region := factor(census_region)]

# then get dummy indicators
dummy_cols1 <- lapply(levels(dt$year), function(level) as.integer(dt$year == level))
names(dummy_cols1) <- levels(dt$year)
dummy_cols2 <- lapply(levels(dt$year_follow), function(level) as.integer(dt$year_follow == level))
names(dummy_cols2) <- paste0("follow_", levels(dt$year_follow))
dummy_cols3 <- lapply(levels(dt$census_region), function(level) as.integer(dt$census_region == level))
names(dummy_cols3) <- levels(dt$census_region)

# Bind the dummy columns to the original data.table
dt <- cbind(dt, as.data.table(dummy_cols1))
dt <- cbind(dt, as.data.table(dummy_cols2))
dt <- cbind(dt, as.data.table(dummy_cols3))
rm(dummy_cols1, dummy_cols2, dummy_cols3)


#----- try grouping chronic conditions -----#

#names(dt)

# create new columns for grouped conditions
dt[, c(
  "group_cancer_ever", "group_metabolic_ever", "group_blood_ever", "group_mental_ever",
  "group_circulatory_ever", "group_respiratory_ever", 
  "group_urinary_ever", "group_skeletal_ever"
) := list(
  as.integer(breastCancer_ever == 1 | colorectalCancer_ever == 1 | endometrialCancer_ever == 1,
             lungCancer_ever == 1 | prostateCancer_ever == 1),
  as.integer(hypoth_ever == 1 | diabetes_ever == 1),
  as.integer(anemia_ever == 1),
  as.integer(alzh_ever == 1 | alzhdmta_ever == 1, depressn_ever == 1),
  as.integer(ami_ever == 1 | chf_ever == 1 | atrialfb_ever == 1 | hypert_ever == 1 | 
               ischmcht_ever == 1 | stroke_ever == 1 | hyperl_ever == 1),
  as.integer(asthma_ever == 1 | copd_ever == 1),
  as.integer(chrnkidn_ever == 1 | hyperp_ever == 1),
  as.integer(osteoprs_ever == 1 | ra_oa_ever == 1, hipfrac_ever == 1)
)]



#----- get prevalence of groups in 2010 -----#

# # How common is each chronic condition? BEFORE EXPOSURE IN 2010
# prev_overall <- dt[year == 2010, lapply(.SD, function(x) sum(x) / .N * 100),
#                    .SDcols = names(dt)[grep("^group_", names(dt))]] |>
#   gather(key = "cond_abbr", value = "prev_overall") |>
#   arrange(prev_overall)
# 
# prev_overall$cond_abbr <- factor(prev_overall$cond_abbr, 
#                                  levels = prev_overall$cond_abbr[order(prev_overall$prev_overall)])
# 
# # plot
# prev_overall |>
#   ggplot(aes(x = prev_overall, y = cond_abbr)) +
#   xlim(c(0, 40)) +
#   geom_point(col = "skyblue3", size = 3, alpha = 0.9) +
#   labs(x = "% ever hosp before 2010",
#        y = "",
#        col = "") +
#   theme_bw()


#----- get correlations between chronic conditions in 2010 -----#

# library(proxy) # for heat map
# 
# # 27 chronic conditions
# condition_cols <-dt %>%
#   filter(year == "2010") %>%
#   #select(names(dt)[grep("^((?!pm25).)*_ever$", names(dt), perl = TRUE)])
#   select(names(dt)[grep("^group_", names(dt))])
# colnames(condition_cols) <- sub("_ever$", "", colnames(condition_cols))
# # Pearson's correlation (works for binary data)
# cor(condition_cols) %>%
#   heatmap(Rowv = NA, Colv = NA,
#           margins = c(10, 10),
#           symm = TRUE)
# max(cor(condition_cols)[cor(condition_cols) != 1])
# 
# 
# # chronic conditions (grouped)
# condition_cols <-dt %>%
#   filter(year == "2010") %>%
#   select(names(dt)[grep("^((?!group).)*_ever$", names(dt), perl = TRUE)])
# colnames(condition_cols) <- sub("_ever$", "", colnames(condition_cols))
# # Pearson's correlation (works for binary data)
# cor(condition_cols) %>%
#   heatmap(Rowv = NA, Colv = NA,
#           margins = c(7, 5),
#           symm = TRUE)
# # max val that isn't 1
# max(cor(condition_cols)[cor(condition_cols) != 1])


#--- make columns for interactions (so they can easily be in the matrix)

# function to make new columns for interactions
new_interact_col <- function(dt, col_name_vector){
  for (i in 1:length(col_name_vector)) {
    dt[, (paste0("pm25:", col_name_vector[i])) := pm25 * get(col_name_vector[i])]
  }
  return(dt)
}

# columns for interactions
interact_cols <- c("age", "sex", "dual", "race_black", "race_other", 
                   "race_asian", "race_native", "race_hispanic",
                   #names(dt)[grep("_ever", names(dt))],
                   names(dt)[grep("group_", names(dt))]
)

# get new interactions columns in dt
dt <- new_interact_col(dt, interact_cols)


######## prep ######## 

#----- prep X and y

# X is the covariate matrix
X <- dt[, c(
  
  # exposure
  "pm25",
  
  # Sex, age, dual eligibility, RTI race code
  "sex",
  "age",
  "dual",
  #"white",
  "race_black", "race_other", "race_asian", "race_native", "race_hispanic",
  
  # chronic conditions
  # names(dt)[grep("^((?!pm25).)*_ever$", names(dt), perl = TRUE)],
  names(dt)[grep("^group_", names(dt))],
  
  # year and follow-up year
  as.character(seq(2002, 2015)), # 2001 is reference
  paste0("follow_", 2:15), # year 1 is reference
  
  # census region indicators
  # "Midwest", # make midwest the reference
  "Northeast", "South", "West",
  
  # area-level potential confounders
  "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
  "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
  "pct_blk", "pct_hispanic",
  "summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax",
  
  # interactions
  paste0("pm25:", interact_cols)
  
), with = FALSE] |> as.matrix()


# and get X with interaction terms only (Falco wants to see this)
X_interact <- X[, paste0("pm25:", interact_cols)]


# y is outcome
y <- dt[,dead_lead]


########## stabs ##########

# # modify glmnet.lasso function in the stabs package
# # sometimes gets an error with small samples, but this works
# my.glmnet.lasso <- function (x, y, q){
#   fit <- glmnet::glmnet(x, y, 
#                         #pmax = q,
#                         family = "binomial", alpha = 1, standardize = TRUE,
#                         penalty.factor = c(rep(0, 62), rep(1, 78-62)))
#   selected <- predict(fit, type = "nonzero")
#   selected <- selected[[length(selected)]]
#   ret <- logical(ncol(x))
#   ret[selected] <- TRUE
#   names(ret) <- colnames(x)
#   cf <- fit$beta
#   sequence <- as.matrix(cf != 0)
#   return(list(selected = ret, path = sequence))
# }
# 
# stabs_res <- stabsel(x = X, y = y,
#                      fitfun = my.glmnet.lasso,
#                      cutoff = 0.7, PFER = 1)
# #save(stabs_res, file = "results/stabs/stabs_600000.RData")
# load("results/stabs/stabs_600000.RData")
# 
# # get variable names I'm interested in (interactions)
# interact_names <- colnames(X)[which(str_detect(colnames(X), pattern = ":"))]
# 
# # which variables were selected?
# selected_vars <- selected(stabs_res)
# names(selected_vars[names(selected_vars) %in% interact_names])
# 
# # plot phat across steps
# plot.stabsel(stabs_res,
#              type = "paths",
#              main = "")
# 
# # plot probability of being selected (with cutoff)
# plot.stabsel(stabs_res,
#              type = "maxsel",
#              main = "",
#              np = stabs_res$p)
# 
# # plot manually with only interactions (variables with penalties)
# # get last column
# p_hats <- stabs_res$phat[,ncol(stabs_res$phat)]
# 
# p_hats_df <- data.frame(variable = names(p_hats),
#                         p_hat = p_hats)
# 
# # change variable ordering
# p_hats_df$variable <- factor(p_hats_df$variable, 
#                              levels = p_hats_df$variable[order(p_hats_df$p_hat)])
# 
# # plot
# p_hats_df |>
#   # filter to columns of interest (interactions)
#   filter(variable %in% interact_names) |>
#   ggplot() +
#   geom_point(aes(x = p_hat, y = variable)) +
#   labs(x = "Probability of selection",
#        y = "") +
#   theme_bw()


########## stabs no main effects of CCW ##########

# columns_to_keep <- !(colnames(X) %in% names(dt)[grep("^group_", names(dt))])
# X_no_main_ccw <- X[, columns_to_keep]
# 
# # modify glmnet.lasso function in the stabs package
# # sometimes gets an error with small samples, but this works
# my.glmnet.lasso.nomainCCW <- function (x, y, q){
#   fit <- glmnet::glmnet(x, y, 
#                         #pmax = q,
#                         family = "binomial", alpha = 1, standardize = TRUE,
#                         penalty.factor = c(rep(0, 54), rep(1, 70-54)))
#   selected <- predict(fit, type = "nonzero")
#   selected <- selected[[length(selected)]]
#   ret <- logical(ncol(x))
#   ret[selected] <- TRUE
#   names(ret) <- colnames(x)
#   cf <- fit$beta
#   sequence <- as.matrix(cf != 0)
#   return(list(selected = ret, path = sequence))
# }
# 
# stabs_res_nomainCCW <- stabsel(x = X_no_main_ccw, y = y,
#                      fitfun = my.glmnet.lasso.nomainCCW,
#                      cutoff = 0.7, PFER = 1)
# save(stabs_res_nomainCCW, file = "results/stabs/stabs_nomainCCW_600000.RData")
# #load("results/stabs/stabs_nomainCCW_600000.RData")
# 
# 
# stabs_res <- stabs_res_nomainCCW
# 
# 
# # get variable names I'm interested in (interactions)
# interact_names <- colnames(X)[which(str_detect(colnames(X), pattern = ":"))]
# 
# # which variables were selected?
# selected_vars <- selected(stabs_res)
# names(selected_vars[names(selected_vars) %in% interact_names])
# 
# # plot phat across steps
# plot.stabsel(stabs_res,
#              type = "paths",
#              main = "")
# 
# # plot probability of being selected (with cutoff)
# plot.stabsel(stabs_res,
#              type = "maxsel",
#              main = "",
#              np = stabs_res$p)
# 
# # plot manually with only interactions (variables with penalties)
# # get last column
# p_hats <- stabs_res$phat[,ncol(stabs_res$phat)]
# 
# p_hats_df <- data.frame(variable = names(p_hats),
#                         p_hat = p_hats)
# 
# # change variable ordering
# p_hats_df$variable <- factor(p_hats_df$variable,
#                              levels = p_hats_df$variable[order(p_hats_df$p_hat)])
# 
# # plot
# p_hats_df |>
#   # filter to columns of interest (interactions)
#   filter(variable %in% interact_names) |>
#   ggplot() +
#   geom_point(aes(x = p_hat, y = variable)) +
#   labs(x = "Probability of selection",
#        y = "") +
#   theme_bw()


########## stabs no CCW at all ##########

# exclude all main effects and interactions with CCW
columns_to_keep <- !(colnames(X) %in% names(dt)[grep("group_", names(dt))])
X_no_ccw <- X[, columns_to_keep]

# modify glmnet.lasso function in the stabs package
# sometimes gets an error with small samples, but this works
my.glmnet.lasso.noCCW <- function (x, y, q){
  fit <- glmnet::glmnet(x, y, 
                        #pmax = q,
                        family = "binomial", alpha = 1, standardize = TRUE,
                        penalty.factor = c(rep(0, 54), rep(1, 62-54)))
  selected <- predict(fit, type = "nonzero")
  selected <- selected[[length(selected)]]
  ret <- logical(ncol(x))
  ret[selected] <- TRUE
  names(ret) <- colnames(x)
  cf <- fit$beta
  sequence <- as.matrix(cf != 0)
  return(list(selected = ret, path = sequence))
}

stabs_res_noCCW <- stabsel(x = X_no_ccw, y = y,
                               fitfun = my.glmnet.lasso.noCCW,
                               cutoff = 0.7, PFER = 1)
#save(stabs_res_noCCW, file = "results/stabs/stabs_noCCW_600000.RData")
#load("results/stabs/stabs_noCCW_600000.RData")


stabs_res <- stabs_res_noCCW


# get variable names I'm interested in (interactions)
interact_names <- colnames(X)[which(str_detect(colnames(X), pattern = ":"))]

# which variables were selected?
selected_vars <- selected(stabs_res)
names(selected_vars[names(selected_vars) %in% interact_names])

# plot phat across steps
plot.stabsel(stabs_res,
             type = "paths",
             main = "")

# plot probability of being selected (with cutoff)
plot.stabsel(stabs_res,
             type = "maxsel",
             main = "",
             np = stabs_res$p)

# plot manually with only interactions (variables with penalties)
# get last column
p_hats <- stabs_res$phat[,ncol(stabs_res$phat)]

p_hats_df <- data.frame(variable = names(p_hats),
                        p_hat = p_hats)

# change variable ordering
p_hats_df$variable <- factor(p_hats_df$variable,
                             levels = p_hats_df$variable[order(p_hats_df$p_hat)])

# plot
p_hats_df |>
  # filter to columns of interest (interactions)
  filter(variable %in% interact_names) |>
  ggplot() +
  geom_point(aes(x = p_hat, y = variable)) +
  labs(x = "Probability of selection",
       y = "") +
  xlim(c(0,1)) +
  theme_bw()



