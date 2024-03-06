
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Group logit LASSO
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)

# # Work with a smaller random sample for now
load("data/intermediate/rolling_cohort.RData")
# set.seed(17)
# keep_idx <- sample(unique(dt[,qid]), 100000, replace = FALSE)
# dt <- dt[qid %in% keep_idx,]
# save(dt, file = "data/intermediate/rolling_cohort_100000.RData")
# 
# load("data/intermediate/rolling_cohort_100000.RData")


#----- fix race indicators (eventually do this in script 1)

# just make anything other than 0 a 1
dt[, white := ifelse(white == 0, white, 1)]
dt[, black := ifelse(black == 0, black, 1)]
dt[, hispanic := ifelse(hispanic == 0, hispanic, 1)]
dt[, asian := ifelse(asian == 0, asian, 1)]
dt[, native := ifelse(native == 0, native, 1)]
dt[, other := ifelse(other == 0, other, 1)]


#-----

dt[, race := factor(race)]

# # make year and follow-up year factors
dt[, year := factor(year)]
dt[, year_follow := factor(year_follow)]

# Create a list of dummy columns using lapply
dummy_cols1 <- lapply(levels(dt$year), function(level) as.integer(dt$year == level))
names(dummy_cols1) <- levels(dt$year)

dummy_cols2 <- lapply(levels(dt$year_follow), function(level) as.integer(dt$year_follow == level))
names(dummy_cols2) <- paste0("follow_", levels(dt$year_follow))

# Bind the dummy columns to the original data.table
dt <- cbind(dt, as.data.table(dummy_cols1))
dt <- cbind(dt, as.data.table(dummy_cols2))
rm(dummy_cols1, dummy_cols2)


# make columns for interactions

# function to make new columns for interactions
new_interact_col <- function(dt, col_name_vector){
  for (i in 1:length(col_name_vector)) {
    dt[, (paste0("pm25_", col_name_vector[i])) := pm25 * get(col_name_vector[i])]
  }
  return(dt)
}

interact_cols <- c("age", "sex", "dual", "black", "other", "asian", "native", "hispanic"#,
                   #"hypert_ever", "ischmcht_ever", "diabetes_ever", "chrnkidn_ever"
                   )

dt <- new_interact_col(dt, interact_cols)



########

## Group LASSO
# requires all vars in a group (e.g., race, age) to be selected together

#library(gglasso)

# X is covariate matrix
X <- dt[, c(
  
  # exposure
  "pm25",
  
  # Sex, age, dual eligibility, RTI race code
  "sex",
  "age",
  "dual", 
  #"white", 
  "black", "other", "asian", "native", "hispanic",  
  
  # # some prevalent chronic conditions
  "hypert_ever", "ischmcht_ever", "diabetes_ever", "chrnkidn_ever",
  
  # year and follow-up year
  as.character(seq(2001, 2015)),
  paste0("follow_", 1:15),
  
  # area-level potential confounders
  "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
  "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
  "pct_blk", "pct_hispanic",
  "summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax",
  
  # interactions
  paste0("pm25_", interact_cols)
  
), with = FALSE] |> as.matrix()



# use model.matrix to set up dummy cols for year and year_follow
# X <- model.matrix(~., X)[,-1]


# y is outcome
y <- dt[,dead_lead]
#y_group <- ifelse(y == 0, -1, y)

# # var_group tells us the grouping of the variables (which ones must be selected together)
# colnames(X)
# var_group <- c(1, 
#                2, 2, 
#                3, 
#                4, 4, 4, 4, 4,
#                rep(5, 14),
#                rep(6, 14),
#                #rep(7, 14),
#                seq(7, 7 + 13))
# length(var_group) == ncol(X)

# # group lasso
# group_lasso <- gglasso(X,
#                        y_group,
#                        lambda = 0.1,
#                        group = var_group,
#                        loss = "logit",
#                        intercept = F)
# save(group_lasso, file = "results/group_lasso_1000.RData")
# load("results/group_lasso_1000.RData")
# 
# # results
# summary(group_lasso)
# group_lasso$beta


########

## LASSO (not group)

# already standardized

library(glmnet)

# cross-validation to get best lambda
set.seed(17)
lasso_cv <- cv.glmnet(X, y, alpha = 1, family = "binomial")
plot(lasso_cv)
best_lambda <- lasso_cv$lambda.min
lasso <- glmnet(X, y, 
                family = "binomial", alpha = 1, standardize = TRUE,
                lambda = best_lambda)


# or test many lambdas
# note: these test lambdas are in decreasing order! 
# these values correspond to x-axis values but in flipped order
# test_lambdas <- 10^seq(-4, -1.5, length = 100)
# lasso <- glmnet(X, y, family = "binomial", alpha = 1, lambda = test_lambdas)
# plot(lasso, xvar = "lambda", label = T)
# 
# lasso <- glmnet(X, y, family = "binomial", alpha = 1, lambda = 0.003)


#save(lasso, file = "results/lasso_100000.RData")
save(lasso, file = "results/lasso_all.RData")

# load("results/lasso_1000.RData")


# now try again, shrinking more:
lasso <- glmnet(X, y, 
                family = "binomial", alpha = 1, standardize = TRUE,
                lambda = 0.002)

save(lasso, file = "results/lasso_all_lambda_0.002.RData")

# lasso results
coefs <- coef(lasso)
coefs


