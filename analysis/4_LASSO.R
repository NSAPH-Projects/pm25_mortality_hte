
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### LASSO
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(dplyr)
library(readr)

dt <- read_rds("data/intermediate/rolling_cohort.rds")

# # Work with a random sample for now
# set.seed(17)
# keep_idx <- sample(unique(dt[,qid]), 30000000, replace = FALSE)
# dt <- dt[qid %in% keep_idx,]
# gc()

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


######## LASSO ######## 

#----- prep X and y

# X is the covariate matrix
X <- dt[, c(
  
  # exposure
  "pm25",

  # Sex, age, dual eligibility, RTI race code
  "sex",
  "age",
  "dual",
  #"race_white",
  "race_black", "race_other", "race_asian", "race_native", "race_hispanic",

  # chronic conditions
  # names(dt)[grep("^((?!pm25).)*_ever$", names(dt), perl = TRUE)],
  names(dt)[grep("^group_", names(dt))],
            
  # year and follow-up year
  as.character(seq(2002, 2015)), # 2001 is reference
  paste0("follow_", 2:15), # year 1 is reference

  # census region indicators
  # "Midwest", # make midwest the reference (same as outcome model)
  "Northeast", "South", "West",
  
  # area-level potential confounders
  "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
  "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
  "pct_blk", "pct_hispanic",
  "summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax",
  
  # interactions
  paste0("pm25:", interact_cols)
  
), with = FALSE] |> as.matrix()


# y is outcome
y <- dt[,dead_lead]


#----- LASSO -----#

library(glmnet)



#####
# # cross-validation to get best lambda
#colnames(X)
set.seed(17)
lasso_cv <- cv.glmnet(X, y,
                      family = "binomial", alpha = 1, standardize = TRUE,
                      #penalty.factor = c(rep(0, 78), rep(1, 113-78)),
                      penalty.factor = c(rep(0, 62), rep(1, 78-62)))
plot(lasso_cv)
best_lambda <- lasso_cv$lambda.min
paste0("Best lambda: ", best_lambda)
lasso <- glmnet(X, y,
                family = "binomial", alpha = 1, standardize = TRUE,
                #penalty.factor = c(rep(0, 78), rep(1, 113-78)),
                penalty.factor = c(rep(0, 62), rep(1, 78-62)),
                lambda = best_lambda)

# or test many lambdas
# note: these test lambdas are in decreasing order!
# these values correspond to x-axis values but in flipped order
# test_lambdas <- 10^seq(-4, -1.5, length = 100)
# lasso <- glmnet(X, y, family = "binomial", alpha = 1, lambda = test_lambdas)
# plot(lasso, xvar = "lambda", label = T)
#
# lasso <- glmnet(X, y, family = "binomial", alpha = 1, lambda = 0.003)


# # or just choose a value for lambda
# lasso <- glmnet(X, y,
#                 family = "binomial", alpha = 1, standardize = TRUE,
#                 #penalty.factor = c(0, rep(1, ncol(X) - 1)),
#                 penalty.factor = c(rep(0, 78), rep(1, 113-78)),
#                 lambda = 0.0005)


#save(lasso, file = "results/lasso/lasso_cv.RData")
#save(lasso, file = "results/lasso/lasso_cv_groups.RData")
#save(lasso, file = "results/lasso/all_final_model.RData")
write_rds(lasso, file = "results/lasso/all_final.rds")

gc()

#load("results/lasso/lasso_cv.RData")
#load("results/lasso/lasso_cv_ccs.RData")

# view lasso results
coef(lasso)

#########################################################

# # now try without main effects for chronic conditions
# # for some reason this took like 3 hours to run
# 
# #columns_to_keep <- !(colnames(X) %in% colnames(X)[grep("^((?!pm25).)*_ever$", colnames(X), perl = TRUE)])
# columns_to_keep <- !(colnames(X) %in% names(dt)[grep("^group_", names(dt))])
# 
# 
# X_no_main_ccw <- X[, columns_to_keep]
# 
# #colnames(X_no_main_ccw)
# set.seed(17)
# lasso_cv <- cv.glmnet(X_no_main_ccw, y,
#                       family = "binomial", alpha = 1, standardize = TRUE,
#                       #penalty.factor = c(rep(0, 51), rep(1, 86-51)),
#                       penalty.factor = c(rep(0, 54), rep(1, 70-54)))
# plot(lasso_cv)
# best_lambda <- lasso_cv$lambda.min
# best_lambda
# lasso <- glmnet(X_no_main_ccw, y,
#                 family = "binomial", alpha = 1, standardize = TRUE,
#                 #penalty.factor = c(rep(0, 51), rep(1, 86-51)),
#                 penalty.factor = c(rep(0, 54), rep(1, 70-54)),
#                 lambda = best_lambda)
# 
# #save(lasso, file = "results/lasso/lasso_cv_no_main_ccw.RData")
# #save(lasso, file = "results/lasso/lasso_cv_no_main_ccw_groups.RData")
# save(lasso, file = "results/lasso/lasso_cv_no_main_ccw_ccs_6mil.RData")
# 
# 
# #load("results/lasso/lasso_cv_no_main_ccw.RData")
# #load("results/lasso/lasso_cv_no_main_ccw_ccs.RData")
# 
# # view lasso results
# coef(lasso)


#########################################################

# now try with three-way interactions
# interactions between PM2.5 and each combo of individual level

# # first, get two-way interactions between all individual-level variables
# two_way_matrix <- model.matrix(~ .^2 - 1, dt[, interact_cols, with = FALSE])
# 
# # ignore the main effects (keep interactions only)
# two_way_matrix <- two_way_matrix[, grep(":", colnames(two_way_matrix))]
# 
# # bind columns to dt
# dt <- cbind(dt, two_way_matrix)
# 
# # now make interactions between PM2.5 and all these two-way interactions
# # columns for interactions
# interact_cols <- colnames(two_way_matrix)
# 
# # get new interactions columns in dt
# dt <- new_interact_col(dt, interact_cols)
# 
# #### run lasso again
# 
# # keep all columns in X and all interactions
# X_three_way <- dt[, c(colnames(X), colnames(dt)[grep(":", colnames(dt))]), with = FALSE] |>
#   as.matrix()
# 
# lasso_cv <- cv.glmnet(X_three_way, y,
#                       family = "binomial", alpha = 1, standardize = TRUE,
#                       #penalty.factor = c(rep(0, 78), rep(1, ncol(X_three_way) - 78)),
#                       penalty.factor = c(rep(0, 60), rep(1, ncol(X_three_way) - 60)))
# plot(lasso_cv)
# best_lambda <- lasso_cv$lambda.min
# lasso_three_way <- glmnet(X_three_way, y,
#                 family = "binomial", alpha = 1, standardize = TRUE,
#                 #penalty.factor = c(rep(0, 78), rep(1, ncol(X_three_way) - 78)),
#                 penalty.factor = c(rep(0, 60), rep(1, ncol(X_three_way) - 60)),
#                 lambda = best_lambda)
# save(lasso_three_way, file = "results/lasso/lasso_cv_three_way.RData")
# #load("results/lasso/lasso_cv_three_way.RData")
# 
# lasso_coefs <- coef(lasso_three_way)[,1]
# selected_coefs <- lasso_coefs[lasso_coefs != 0]
# 
# selected_coefs


#########################################################




# ### also see what happens with all CCW but no penalty
# set.seed(17)
# lasso_cv <- cv.glmnet(X, y,
#                       family = "binomial", alpha = 1, standardize = TRUE,
#                       penalty.factor = c(0, rep(1, ncol(X) - 1)))
# plot(lasso_cv)
# best_lambda <- lasso_cv$lambda.min
# lasso <- glmnet(X, y,
#                 family = "binomial", alpha = 1, standardize = TRUE,
#                 penalty.factor = c(0, rep(1, ncol(X) - 1)),
#                 lambda = best_lambda)
# 
# save(lasso, file = "results/lasso/lasso_cv_one_penalty.RData")
# #load("results/lasso/lasso_cv_one_penalty.RData")
# 
# # view lasso results
# coef(lasso)

# ### and also run with interaction terms only
# 
# lasso_interact_cv <- cv.glmnet(X_interact, y,
#                       family = "binomial", alpha = 1, standardize = TRUE)
# plot(lasso_interact_cv)
# best_lambda <- lasso_interact_cv$lambda.min
# lasso_interact <- glmnet(X_interact, y,
#                 family = "binomial", alpha = 1, standardize = TRUE,
#                 lambda = best_lambda)
# 
# # lasso_interact <- glmnet(X_interact, y,
# #                 family = "binomial", alpha = 1, standardize = TRUE,
# #                 lambda = 0.02)
# 
# 
# save(lasso_interact, file = "results/lasso/lasso_interact_cv.RData")
# #load("results/lasso/lasso_interact_cv.RData")
# 
# # view lasso results
# coef(lasso_interact)



#----- stability selection -----#

# library(stabs)
# 
# # modify glmnet.lasso function in the stabs package
# # sometimes gets an error with small samples, but this works
# my.glmnet.lasso <- function (x, y, q){
#   fit <- suppressWarnings(glmnet::glmnet(x, y, pmax = q))
#   fit <- glmnet::glmnet(x, y, pmax = q,
#                         family = "binomial", alpha = 1, standardize = TRUE,
#                         penalty.factor = c(0, rep(1, ncol(x) - 1)))
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
# stabs_lasso <- stabsel(x = X, y = y, 
#                        fitfun = my.glmnet.lasso, 
#                        cutoff = 0.7, PFER = 1)
# #save(stabs_lasso, file = "results/lasso/stabs.RData")
# load("results/lasso/stabs.RData")
# 
# # which variables were selected?
# selected(stabs_lasso)
# 
# # plot phat across steps
# plot.stabsel(stabs_lasso, 
#              type = "paths",
#              main = "")
# 
# # plot probability of being selected (with cutoff)
# plot.stabsel(stabs_lasso,
#              type = "maxsel",
#              main = "",
#              np = 15)
# 
# # now interactions only (only difference is no penalty.factor)
# my.glmnet.lasso.interact <- function (x, y, q){
#   fit <- suppressWarnings(glmnet::glmnet(x, y, pmax = q))
#   fit <- glmnet::glmnet(x, y, pmax = q,
#                         family = "binomial", alpha = 1, standardize = TRUE)
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
# stabs_lasso_interact <- stabsel(x = X_interact, y = y, 
#                        fitfun = my.glmnet.lasso.interact, 
#                        cutoff = 0.7, PFER = 1)
# #save(stabs_lasso_interact, file = "results/lasso/stabs_interact.RData")
# load("results/lasso/stabs_interact.RData")
# 
# # which variables were selected?
# selected(stabs_lasso_interact)
# 
# # plot phat across steps
# plot.stabsel(stabs_lasso_interact, 
#              type = "paths",
#              main = "")
# 
# # plot probability of being selected (with cutoff)
# plot.stabsel(stabs_lasso_interact, 
#              type = "maxsel",
#              main = "", 
#              np = 12)



###################################
###################################
###################################
# OLD

#----- group LASSO -----#
# requires all vars in a group (e.g., race, age) to be selected together

# library(gglasso)
# 
# # only for X with all covariates
# y_group <- ifelse(y == 0, -1, y)
# 
# # var_group tells us the grouping of the variables (which ones must be selected together)
# # group race and interactions with race
# colnames(X)
# var_group <- c(1, 2, 3, 4, rep(5, 5),
#                seq(6, 56),
#                rep(56, 5),
#                seq(57, 60))
# length(var_group) == ncol(X)
# 
# # group lasso
# group_lasso <- gglasso(X,
#                        y_group,
#                        lambda = 0.002,
#                        group = var_group,
#                        loss = "logit",
#                        intercept = F)
# save(group_lasso, file = "results/group_lasso/group_lasso_1000.RData")
# # load("results/group_lasso/group_lasso_1000.RData")
# 
# # results
# group_lasso$beta



# # # first, just run stability selection with built in function
# # # this works!
# # stabs_lasso <- stabsel(x = X, 
# #                        y = y,
# #                        fitfun = glmnet.lasso,
# #                        cutoff = 0.6,
# #                        PFER = 1)
# 
# 
# 
# # fit LASSO with stability selection
# # lars.lasso or glmnet.lasso (two different lasso algorithms)
# stabs_lasso <- stabsel(x = X, 
#                        y = y,
#                        fitfun = lars.lasso,
#                         cutoff = 0.6,
#                       PFER = 1)
# #save(stabs_lasso, file = "results/stabs/stabs_all.RData")
# 
# 
# # ### and run with interactions only
# # stabs_lasso_interact <- stabsel(x = X_interact, 
# #                                 y = y,
# #                                 fitfun = lars.lasso, 
# #                                 cutoff = 0.7,
# #                                 PFER = 1)
# # save(stabs_lasso_interact, file = "results/stabs/stabs_interact.RData")
# 
# 
# # view stabs results
# 
# # which variables were selected?
# selected(stabs_lasso)
# 
# # plot phat across steps
# plot.stabsel(stabs_lasso, type = "paths")
# 
# # plot probability of being selected (with cutoff)
# plot.stabsel(stabs_lasso, type = "maxsel")
# 
# 
# 
# ########## and also try stabs with grouping for race (custom function)
# 
# # but first, try a custom function just with glmnet
# 
# # just doing this to match stabs notation
# x <- X
# q <- 5
# 
# # modifying stabs code for lars.lasso
# my.lars.lasso.fun <- function (x, y, q, ...) {
#   # if (!requireNamespace("lars", quietly = TRUE)) 
#   #   stop("Package ", sQuote("lars"), " needed but not available")
#   # if (is.data.frame(x)) {
#   #   message("Note: ", sQuote("x"), " is coerced to a model matrix without intercept")
#   #   x <- model.matrix(~. - 1, x)
#   # }
#   fit <- lars::lars(x, y, max.steps = q)
#   selected <- unlist(fit$actions)
#   if (any(selected < 0)) {
#     idx <- which(selected < 0)
#     idx <- c(idx, which(selected %in% abs(selected[idx])))
#     selected <- selected[-idx]
#   }
#   ret <- logical(ncol(x))
#   ret[selected] <- TRUE
#   names(ret) <- colnames(x)
#   cf <- fit$beta
#   sequence <- t(cf != 0)
#   return(list(selected = ret, path = sequence))
# }
# 
# with_my_lars_fun <- stabsel(x, y, fitfun = my.lars.lasso.fun, cutoff = 0.8, PFER = 0.1)
# # works!!!
# 
# 
# ### now try glmnet
# # didn't finish this but it doesn't matter, betas are a matrix here too
# 
# # my.glmnet.lasso.fun <- function (x, y, q, type = c("conservative", "anticonservative"), ...) {
# #   # if (!requireNamespace("glmnet", quietly = TRUE)) 
# #   #   stop("Package ", sQuote("glmnet"), " needed but not available")
# #   # if (is.data.frame(x)) {
# #   #   message("Note: ", sQuote("x"), " is coerced to a model matrix without intercept")
# #   #   x <- model.matrix(~. - 1, x)
# #   # }
# #   # if ("lambda" %in% names(list(...))) 
# #   #   stop("It is not permitted to specify the penalty parameter ", 
# #   #        sQuote("lambda"), " for lasso when used with stability selection.")
# #   type <- match.arg(type)
# #   if (type == "conservative") 
# #     fit <- suppressWarnings(glmnet::glmnet(x, y, pmax = q, 
# #                                            ...))
# #   # if (type == "anticonservative") 
# #   #   fit <- glmnet::glmnet(x, y, dfmax = q - 1, ...)
# #   selected <- predict(fit, type = "nonzero")
# #   selected <- selected[[length(selected)]]
# #   ret <- logical(ncol(x))
# #   ret[selected] <- TRUE
# #   names(ret) <- colnames(x)
# #   cf <- fit$beta
# #   sequence <- as.matrix(cf != 0)
# #   return(list(selected = ret, path = sequence))
# # }
# # 
# # with_my_glmnet_fun <- stabsel(x, y, fitfun = my.lars.lasso.fun, cutoff = 0.8, PFER = 0.1)
# 
# ####
# # now try using gglasso
# 
# # modifying stabs code for lars.lasso
# # need to figre out how to get cf
# my.gglasso.fun <- function (x, y, q, ...) {
#   fit.group <- gglasso(x,
#                  y_group,
#                  lambda = 0.05,
#                  group = var_group,
#                  loss = "logit",
#                  intercept = F)
#   ret <- ifelse(fit.group$beta[,1] == 0, FALSE, TRUE)
#   cf.group <- fit.group$beta
#   sequence <- t(cf != 0)
#   return(list(selected = ret, path = sequence))
# }
# 
# with_my_lars_fun <- stabsel(x, y, fitfun = my.gglasso.fun, cutoff = 0.8, PFER = 0.1)
# 
# 

