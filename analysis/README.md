**Run these scripts first:**

[1_model_subgroups.R](1_model_subgroups.R)
* Fit discrete-time logistic regression models for the full cohort and for each condition-specific subgroup
* Note: sbatch files run small, medium, and large subgroups separately (some subgroups need much more time/memory than others)

[2_model_subgroups_stratified.R](2_model_subgroups_stratified.R)
* Fit discrete-time logistic regression models, stratified by sex, race/ethnicity, Medicaid eligibility, and urbanicity
* Note: sbatch files run small, medium, and large subgroups separately (some subgroups need much more time/memory than others)

[3_calculate_prevalence.R](3_calculate_prevalence.R)
* Calculate prevelance of each condition (used in plots later)

**Then run the following scripts as needed, in any order:**

[ccw_figures.R](ccw_figures.R)
* Visualize medical history

[explore_data.R](explore_data.R)
* Basic exploratory data analysis

[pm25_map.R](pm25_map.R)
* Create a map of PM2.5 exposures across the United States

[results_main.R](results_main.R)
* Get results (figure and table) from main models (full cohort and condition-specific subgroups)

[results_stratified.R](results_stratified.R)
* Get results (figures and tables) from stratified models (condition-specific subgroups stratified by sex, race/ethnicity, Medicaid eligibility, and urbanicity

[table1.R](table1.R)
* Create Table 1 (cohort characteristics)
