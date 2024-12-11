
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
# dt <- readRDS("data/intermediate/rolling_cohort_1000.rds")
gc()


#######################################################################

# remove counties with <11 people
dt <- dt[, if (uniqueN(qid) >= 11) .SD, by = county]

plot_dat <- dt[, .(
  # median_age = median(age, na.rm = TRUE), # Uncomment if needed
  pct_female = (1 - mean(sex, na.rm = TRUE)) * 100,
  pct_white = (mean(race_white, na.rm = TRUE)) * 100,
  pct_dual = (mean(dual, na.rm = TRUE)) * 100,
  urban = first(urban)
), by = county]
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
plot_dat <- right_join(county_sf, plot_dat, by = "county")
gc()

#######################################################################

# # % sex map
# map_sex <- plot_dat %>%
#   ggplot() +
#   geom_sf(aes(fill = pct_female), col = NA) +
#   geom_sf(data = state_sf, fill = NA, col = "black") +
#   labs(fill = "% Female  ") +
#   coord_sf(crs = st_crs(5070)) +
#   scale_fill_viridis(option = "D", direction = -1) +
#   theme_void() +
#   theme(plot.margin = margin(0, 1, 0, 1, "cm"),
#         legend.position = "bottom") +
#   guides(fill = guide_colorbar(ticks = FALSE,
#                                barheight = 1, barwidth = 7))
# gc()

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
  scale_fill_viridis(option = "A") +
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
  labs(fill = "") +
  scale_fill_manual(values = c("TRUE" = "gray50", "FALSE" = "#7aeb8d"),
                      labels = c("TRUE" = "Urban", "FALSE" = "Rural")) +
  coord_sf(crs = st_crs(5070)) +
  theme_void() +
  theme(plot.margin = margin(0, 1, 0, 1, "cm"),
        legend.position = "bottom")
gc()


# group to state level
state_dat <- dt[, .(census_region = first(census_region)), by = statecode]

# join with state geometry
state_dat <- right_join(state_sf, state_dat, by = c("STUSPS" = "statecode"))


# plot
region_map <- state_dat %>%
  mutate(census_region = factor(census_region,
                                levels = c("West", "Midwest", "South", "Northeast"))) %>%
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
gc()


# save maps
all_plots <- align_plots(map_pct_white, map_pct_dual, map_urban, region_map,
                         align = "hv")

pdf("results/figures/strat_var_maps.pdf", height = 6, width = 9)
plot_grid(all_plots[[1]], all_plots[[2]], all_plots[[3]], all_plots[[4]],
          nrow = 2)
dev.off()



