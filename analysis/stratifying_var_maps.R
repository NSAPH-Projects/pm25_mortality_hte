
########################################################
### Hetergeneous Effects of PM2.5 on Mortality
### Author: Lauren Mock
### Map stratifying variables
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
#load("data/intermediate/rolling_cohort_1000.RData")


###############################################################

# fix county.x county.y issue!!

setnames(dt, "county.x", "county")
dt[, county.y := NULL]
saveRDS(dt, file = "data/intermediate/rolling_cohort_fix_county.rds")

###############################################################3

# SAVE NEW SAMPLE

id_mini <- sample(dt[,qid], 1000, replace = FALSE)
dt_mini <- dt[qid %in% id_mini,]
saveRDS(dt_mini, file = "data/intermediate/rolling_cohort_1000_new.rds")


###############################################################


# # SAMPLE FOR NOW TO TEST ARRAY
# load("data/intermediate/rolling_cohort_1000.RData")

gc()

#######################################################################

# new columns

# # above/below median age
# median_age <- dt[, median(age)]
# dt[, old := as.integer(age > median_age)]

# # above/below median age
# median_popdens <- dt[, median(popdensity)]
# dt[, urban := as.integer(popdensity > median_popdens)]
#

#######################################################################

# load zip --> county crosswalk
zip2county <- read.csv("/n/dominici_lab_ro/lab/data/exposures/exposure/zip2county_master_xwalk/zip2county_master_xwalk_2010_2023_tot_ratio_one2one.csv")
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

colnames(county_sf)
colnames(dt)

county_dat <- right_join(county_sf, dt, by = "county")
gc()


#######################################################################

# FILTER OUT COUNTIES WITH <11 PEOPLE

paste0("Any counties with < 11 people?")
n_per_county <- dt[, .N, by = county]
paste0(sum(n_per_county$N < 11), " counties with < 11 people")
counties_exclude <- n_per_county$county[n_per_county$N < 11]

# NOW GET PLOT DATA (county-level summarized variables)

plot_dat <- county_dat %>%
  filter(!county %in% counties_exclude) %>%
  group_by(county) %>%
  summarise(# median_age = median(age, na.rm = TRUE),
            # skipping male/female
            pct_white = mean(race_white, na.rm = TRUE),
            pct_dual = mean(dual, na.rm = TRUE),
            urban = first(urban)
            )
gc()

#######################################################################

# % white map
map_pct_white <- plot_dat %>%
  ggplot() +
  geom_sf(aes(fill = pct_white), col = NA) +
  geom_sf(data = state_sf, fill = NA, col = "black") +
  labs(fill = "% White  ") +
  coord_sf(crs = st_crs(5070)) +
  scale_fill_viridis(option = "D") +
  theme_void() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(ticks = FALSE,
                               barheight = 1, barwidth = 7))

gc()

# % dual map
map_pct_dual <- plot_dat %>%
  ggplot() +
  geom_sf(aes(fill = pct_dual), col = NA) +
  geom_sf(data = state_sf, fill = NA, col = "black") +
  labs(fill = "% Medicaid eligible  ") +
  coord_sf(crs = st_crs(5070)) +
  scale_fill_viridis(option = "D") +
  theme_void() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(ticks = FALSE,
                               barheight = 1, barwidth = 7))

gc()

# urban map
map_urban <- plot_dat %>%
  ggplot() +
  geom_sf(aes(fill = urban), col = NA) +
  geom_sf(data = state_sf, fill = NA, col = "black") +
  labs(fill = "Urban?  ") +
  coord_sf(crs = st_crs(5070)) +
  theme_void() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "bottom") +
  guides(fill = guide_colorbar(ticks = FALSE,
                               barheight = 1, barwidth = 7))


gc()


# save maps

all_plots <- align_plots(map_pct_white, map_pct_dual, map_urban,
                         align = "hv")

pdf("results/figures/strat_var_maps.pdf", height = 5, width = 13)
plot_grid(all_plots[[1]], all_plots[[2]], all_plots[[3]],
          nrow = 1)
dev.off()


#######################################################################

# separate map for census region
state_dat <- right_join(state_sf, dt, by = c("STUSPS" = "statecode"))

# group to state level
census_region_dat <- state_dat %>%
  group_by(NAME) %>%
  summarise(census_region = first(census_region))

# plot
pdf("results/figures/regions_map.pdf", height = 5, width = 6)
region_map <- census_region_dat %>%
  ggplot() +
  geom_sf(aes(fill = census_region), col = "black") +
  labs(fill = "") +
  coord_sf(crs = st_crs(5070)) +
  scale_fill_manual(values = c("Midwest" = "#414A90",
                               "Northeast" = "#69c0dd",
                               "South" = "#36a118",
                               "West" = "#d8c94c")) +
  theme_void() +
  theme(plot.margin = margin(0, 1, 0, 0, "cm"),
        legend.position = "bottom")
dev.off()
gc()

