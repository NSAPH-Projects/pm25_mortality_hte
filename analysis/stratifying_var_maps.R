
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Maps of stratifying variables
########################################################

library(data.table)
library(fst)
library(sf)
library(ggplot2)
library(tidyverse)
library(viridis)
library(cowplot)


# Load data
dt <- readRDS("data/intermediate/rolling_cohort.rds")

# # SAMPLE FOR NOW TO TEST ARRAY
# load("data/intermediate/rolling_cohort_1000.RData")

gc()

#######################################################################

# new columns

# above/below median age
median_age <- dt[, median(age)]
dt[, old := as.integer(age > median_age)]

# above/below median age
median_popdens <- dt[, median(popdensity)]
dt[, urban := as.integer(popdensity > median_popdens)]
  

#######################################################################

# load zip --> county crosswalk
zip2county <- read.csv("/n/dominici_lab_ro/lab/data/exposures/exposure/zip2county_master_xwalk/zip2county_master_xwalk_2010_2023_tot_ratio_one2few.csv")
gc()

# select columns of interest
zip2county <- zip2county %>%
  filter(year == 2010) %>%
  select(zip, county)

# merge with data
dt <- merge(dt, zip2county, by = "zip", all.x = TRUE, all.y = FALSE, 
            allow.cartesian = TRUE)
gc()


#######################################################################

# load county shapefile
county_sf <- read_sf("data/raw/shapefiles/cb_2015_us_county/cb_2015_us_county_20m.shp") %>%
  filter(!STATEFP %in% c("02","15", "66", "72", "60", "69", "78")) %>% 
  mutate(county = as.numeric(GEOID)) %>%
  arrange(county)

# load state shapefile
state_sf <- read_sf("data/raw/shapefiles/cb_2015_us_state/cb_2015_us_state_20m.shp") %>%
  filter(!STATEFP %in% c("02", "15", "66", "72", "60", "69", "78"))

# # join county and state
# state_county_sf <- left_join(county_sf, as.data.frame(state_sf), by = "STATEFP")

# merge with data
county_dat <- right_join(county_sf, dt, by = "county")
gc()


#######################################################################

# NOW GET PLOT DATA (county-level summarized variables)

plot_dat <- county_dat %>%
  group_by(county) %>%
  summarise(median_age = median(age, na.rm = TRUE),
            # skipping male/female
            pct_white = mean(race_white, na.rm = TRUE),
            pct_black = mean(race_black, na.rm = TRUE),
            pct_hispanic = mean(race_hispanic, na.rm = TRUE),
            log_median_popdens = log(median(popdensity, na.rm = TRUE)),
            pct_dual = mean(dual, na.rm = TRUE))
gc()

#######################################################################

# function to make map
plot_map <- function(strat_var, legend_title){
  
  gg_map <- plot_dat %>%
    ggplot() +
    geom_sf(aes_string(fill = strat_var), col = NA) +
    geom_sf(data = state_sf, fill = NA, col = "black") +
    labs(fill = paste0(legend_title, "  ")) +
    coord_sf(crs = st_crs(5070)) +
    scale_fill_viridis(option = "D") +
    theme_void() + 
    theme(plot.margin = margin(0, 1, 0, 1, "cm"),
          legend.position = "bottom") +
    guides(fill = guide_colorbar(ticks = FALSE,
                                 barheight = 1, barwidth = 7))
  
  return(gg_map)
}


#######################################################################

# maps

age_map <- plot_map("median_age", "Median age")
black_map <- plot_map("pct_black", "% Black")
hispanic_map <- plot_map("pct_hispanic", "% Hispanic")
popdens_map <- plot_map("log_median_popdens", "log (median popdensity)")
dual_map <- plot_map("pct_dual", "% Medicaid eligible")


gc()


#######################################################################

# separate map for census region
state_dat <- right_join(state_sf, dt, by = c("STUSPS" = "statecode"))

# group to state level
census_region_dat <- state_dat %>%
  group_by(NAME) %>%
  summarise(census_region = first(census_region))

# plot
region_map <- census_region_dat %>%
  ggplot() +
  geom_sf(aes(fill = census_region), col = "black") +
  labs(fill = "") +
  coord_sf(crs = st_crs(5070)) +
  scale_fill_manual(values = c("Midwest" = "#414A90", 
                               "Northeast" = "#3FAFD4",
                               "South" = "#3DB71B", 
                               "West" = "#C9D337")) +
  theme_void() + 
  theme(plot.margin = margin(0, 1, 0, 0, "cm"),
        legend.position = "bottom")
gc()

#######################################################################

# save all maps

all_plots <- align_plots(age_map, black_map, hispanic_map, 
                         dual_map, popdens_map, region_map,
                         align = "hv")

pdf("results/figures/strat_var_maps.pdf", height = 7, width = 7)
plot_grid(all_plots[[1]], all_plots[[2]], all_plots[[3]], 
          all_plots[[4]], all_plots[[5]], all_plots[[6]],
          ncol = 2)
dev.off()
