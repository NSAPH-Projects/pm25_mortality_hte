
# get results across models in a table

library(dplyr)
library(stringr)
library(arrow)

model_path <- "results/outcome_model/"
#model_path <- "results/lasso/"

# load results from all outcome models
model_files <- list.files(model_path)
#model_files <- model_files[1:2]

#model_list <- lapply(paste0(model_path, model_files), load)

# load each model fit into a list
model_list <- lapply(model_files, function(file) {
  load(paste0(model_path, file))  # Load the model from the file
  return(get("lasso", envir = .GlobalEnv))  # Return the loaded model
  gc()
})

names(model_list) <- sub(".RData", "", model_files)

gc()

######################################
# fine, use silly fake models

# model_files <- c("m1", "m2", "m3")
# 
# # install.packages("mlbench")
# library(mlbench)
# data(BreastCancer, package="mlbench")
# bc <- BreastCancer[complete.cases(BreastCancer), ]  # create copy
# 
# 
# m1 <- glm(Class ~ Cell.shape, family="binomial", data = bc)
# m2 <- glm(Class ~ Cell.shape + Bare.nuclei, family="binomial", data = bc)
# m3 <- glm(Class ~ Bare.nuclei + Cl.thickness, family="binomial", data = bc)
# 
# model_list <- list(m1, m2, m3)
######################################



# function to extract coefficients
extract_coef <- function(fit) {
  
  coef_tab <- summary(fit)$coef |> as.data.frame()
  
  # make rownames a column
  coef_tab$Variable <- rownames(coef_tab)
  rownames(coef_tab) <- 1:nrow(coef_tab)
  
  # select cols of interest
  coef_tab <- coef_tab |>
    select(Variable, Estimate)
  
}

# get table of coefficients for each
model_coef <- lapply(model_list, extract_coef)
names(model_coef) <- model_files


# Initial data frame to start the join
table_df <- model_coef[[1]]

#left join
for (i in 2:length(model_coef)) {
  table_df <- full_join(table_df, model_coef[[i]], 
                        by = "Variable", 
                        suffix = c("", paste0("_", model_files[i]))
                        )
}

# now add suffix to first column of estimates
names(table_df)[2] <- paste0("Estimate_", model_files[1])

# save table
save(table_df, file = "results/outcome_model/results_table.RData")


table_df


