
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Causal GPS
########################################################

library(data.table)
library(fst)
library(ggplot2)
library(tidyverse)

library("devtools")
install_github("NSAPH-Software/CausalGPS")
library("CausalGPS")

# Load data
load("data/intermediate/rolling_cohort.RData")
# Work with a random sample for now
set.seed(17)
keep_idx <- sample(unique(dt[,qid]), 1000, replace = FALSE)
dt <- dt[qid %in% keep_idx,]
save(dt, file = "data/intermediate/rolling_cohort_1000.RData")

load("data/intermediate/rolling_cohort_1000.RData")

# change _ever columns so they are either TRUE or FALSE (see script 3)
conditions <- c("hypert_ever", "ischmcht_ever", "diabetes_ever", "chrnkidn_ever")
dt[, (conditions) := lapply(.SD, function(x) !is.na(x)), .SDcols = (conditions)]

# need id column
dt[, id := 1:(.N)]

# set column with exposure data
exposure <- "pm25"

# w
w <- dt[, c("id", exposure), with = FALSE] |> as.data.frame()

# Create indicator variables
race_indicators <- dcast(dt, qid ~ race, fun.aggregate = length)

# Change race indicator column names
setnames(race_indicators,
         old = colnames(race_indicators)[2:7],
         new = c("white", "black", "other", "asian", "hispanic", "native"))

# Bind indicators to original data.table
dt <- merge(dt, race_indicators, by = "qid")
rm(race_indicators); gc()

# c
c <- dt[, c(
  "id",
  
  # Sex, age, dual eligibility, RTI race code
  "sex", "age", "dual", "white", "black", "other", "asian", "native", "hispanic",  
  
  # area-level potential confounders
  "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
  "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
  "pct_blk", "pct_hispanic",
  "summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax",
  
  # some prevalent chronic conditions
  "hypert_ever", "ischmcht_ever", "diabetes_ever", "chrnkidn_ever"
  
)] |> as.data.frame()

# make all columns in w and c numeric
w <- apply(w, 2, as.numeric) |> as.data.frame()
c <- apply(c, 2, as.numeric) |> as.data.frame()

# but make the ID integer
w$id <- as.integer(w$id)
c$id <- as.integer(c$id)


#----- estimate generalized propensity scores

# # get GPS estimates
# gps_res <- estimate_gps(w = w, c = c)
# save(gps_res, file = "results/gps_weights.Rdata")

# load GPS results
load("results/gps_weights.Rdata")

# # get generalized propensity scores
# gps <- gps_res$dataset$gps
# 
# # get inverse probability weights
# gps_wt <- 1/gps

# # trim below 5th and above 95th percentiles
# q5 <- quantile(gps_wt, 0.05)
# q95 <- quantile(gps_wt, 0.95)
# gps_wt <- gps_wt[gps_wt > q5 & gps_wt < q95]


#----- generate pseudo-population with weights (determined by GPS)


# get pseudo pop
set.seed(112)
m_d <- generate_syn_data(sample_size = 100)
data_with_gps <- estimate_gps(m_d[, c("id", "w")],
                              m_d[, c("id", "cf1","cf2","cf3","cf4","cf5","cf6")],
                              gps_density = "normal",
                              params = list(xgb_max_depth = c(3,4,5),
                                            xgb_nrounds=c(10,20,30,40,50,60)),
                              nthread = 1,
                              sl_lib = c("m_xgboost")
)
pd <- compile_pseudo_pop(data_obj = data_with_gps,
                         ci_appr = "matching",
                         gps_density = "normal",
                         bin_seq = NULL,
                         exposure_col_name = c("w"),
                         nthread = 1,
                         dist_measure = "l1",
                         covar_bl_method = 'absolute',
                         covar_bl_trs = 0.1,
                         covar_bl_trs_type= "mean",
                         delta_n = 0.5,
                         scale = 1)


#-----  get absolute correlation in original pop and pseudo pop

set.seed(291)
n <- 100
mydata <- generate_syn_data(sample_size=100)
year <- sample(x=c("2001","2002","2003","2004","2005"),size = n,
               replace = TRUE)
region <- sample(x=c("North", "South", "East", "West"),size = n,
                 replace = TRUE)
mydata$year <- as.factor(year)
mydata$region <- as.factor(region)
mydata$cf5 <- as.factor(mydata$cf5)
cor_val <- absolute_corr_fun(mydata[,2], mydata[, 3:length(mydata)])
print(cor_val$absolute_corr)



#----------

# logistic regression with weights


#----------

# just logistic regression with stepwise selection of interactions for now


#---------- quick function to get plot from model
library(glmnet)



plot_logistic_results <- function(model = logistic_fit,
                                  plot_title = ""){
  
  # initialize df
  mod.results <- data.frame(region = unique(dt[, census_region]),
                            OR = rep(NA, 4),
                            ci.low = rep(NA, 4),
                            ci.high = rep(NA, 4))
  
  # get OR with CI for each region
  for(i in 1:4){
    
    coefs <- summary(model[[i]])$coef
    pm_coef <- subset(coefs, row.names(coefs) == "z")[1,]
    
    mod.results$OR[i] <- exp(pm_coef[1])
    mod.results$ci.low[i] <- exp(pm_coef[1] - 1.96*pm_coef[2])
    mod.results$ci.high[i] <- exp(pm_coef[1] + 1.96*pm_coef[2])
    
    # print(paste0(unique(dt[, census_region])[i], ": OR = ", round(mod.results$OR[i], 3),
    #              " (", round(mod.results$ci.low[i], 3), ", ", round(mod.results$ci.high[i], 3), ")"))
  }
  
  # plot ORs by region
  mod.results |>
    ggplot(aes(x = OR, y = region)) +
    geom_point() +
    geom_errorbar(aes(xmin = ci.low, xmax = ci.high), width = 0.5) +
    geom_vline(xintercept = 1, lty = 2) +
    labs(x = "OR (95% CI)",
         y = "",
         title = plot_title) +
    theme_bw()
}


# file to save results
#logistic_file <- paste0("results/continuous/logistic_fit_5CCW.RData")

# if no file exists, fit a logistic regression model for each region
#if (!file.exists(logistic_file)) {

# initialize list for model fits
logistic_fit <- list()
sig_interactions <- list()

# loop through 4 regions
for(i in 1:4){
  
  region <- unique(dt[, census_region])[i]
  
  one_region <- dt[census_region == region]
  
  #---------- Set up data ----------#
  
  #--- y (outcome vector)
  y <- ifelse(one_region[, dead_12_16], 1, 0)
  
  
  #--- z (exposure vector)
  z <- one_region[, pm25]
  
  
  #--- X (covariate matrix)
  X <- one_region[, c(
    
    # Sex, age, dual eligibility, RTI race code
    "sex", "age", "dual", 
    #"white", 
    "black", "other", "asian", "native", "hispanic",  
    
    # area-level potential confounders
    "poverty", "popdensity", "medianhousevalue", "medhouseholdincome",
    "pct_owner_occ", "education", "smoke_rate", "mean_bmi",
    "pct_blk", "pct_hispanic",
    "summer_tmmx", "summer_rmax", "winter_tmmx", "winter_rmax",
    
    
    # 27 chronic conditions
    # top 5 most prevalent
    "hypert", "ischmcht", "diabetes", "osteoprs", "hyperl"
    
    # "alzhdmta", "alzh", "hypoth", "ami", "anemia", "asthma", "atrialfb", "hyperp", 
    # "breastCancer", "colorectalCancer", "endometrialCancer", "lungCancer", "prostateCancer", 
    # "cataract", "chrnkidn", "copd", "depressn", "diabetes", "glaucoma", "chf", 
    # "hipfrac", "hyperl", "hypert", "ischmcht", "osteoprs", "ra_oa", "stroke"
    
  )]
  
  # Predict death from covariates
  # logistic_fit[[i]] <- glm(y ~ ., family = "binomial", data = cbind.data.frame(X,y,z))
  
  # backwards selection
  initial_model <- glm(y ~ . + z:sex + z:age + z:dual + z:black + z:other + z:asian + 
                         z:native + z:hispanic + z:hypert + z:ischmcht + z:diabetes + 
                         z:osteoprs + z:hyperl, 
                       family = "binomial", data = cbind.data.frame(X,y,z))
  
  # Perform backward selection using step
  logistic_fit[[i]] <- step(initial_model, direction = "backward", 
                            scope = formula(initial_model))
  
  # # LASSO
  # 
  # lasso_model <- glmnet(x = X, 
  #                       y = y, 
  #                       family = "binomial", 
  #                       alpha = 1)
  
  sig_interactions[[i]] <- paste0(attr(terms(logistic_fit[[i]]), "term.labels")[grep(":", attr(terms(logistic_fit[[i]]), "term.labels"))])
  print(paste0("Done with logistic regression for ", region))
  
  rm(one_region, y, z, X)
  gc()
}

# Save
#save(logistic_fit, file = logistic_file)

# } else {
#   load(logistic_file)
# }


plot_logistic_results(model = logistic_fit,
                      plot_title = "w/ top 5 chronic conditions")

