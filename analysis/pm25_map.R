
#--------- map of PM2.5 exposure data from Randall Martin (WashU)
# https://sites.wustl.edu/acag/datasets/surface-pm2-5/
# https://lego-catalog.netlify.app/#/dataset/lego.environmental/v0

library(tidyverse)
library(ggplot2)
library(tigris)
library(sf)
library(viridis)
library(zipcodeR)

options(tigris_use_cache = TRUE)


# path to PM2.5 data
pm25_path <- "/n/dominici_nsaph_l3/Lab/exposure/satellite_pm25_raster2polygon/annual/"

# load PM2.5 data
pm25 <- rbindlist(lapply(2001:2017, function(y) {
  file_path <- paste0(pm25_path, "satellite_pm25_zcta_", y, ".parquet")
  read_parquet(file_path)
}))

# average across all years (2001--2017)
pm25 <- pm25 |>
  group_by(zcta) |>
  summarise(mean_pm25 = mean(pm25, na.rm = TRUE))

# load zcta shapefiles from tigris package
zcta_sf <- zctas(year = 2010)

# # Get state info for each ZIP (ZCTA)
# zcta_states <- zipcodeR::zip_code_db %>%
#   select(zipcode, state) %>%
#   rename(GEOID10 = zipcode)


# Remove shapes with centroids far outside the contiguous U.S.
zcta_sf <- zcta_sf %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(lon = st_coordinates(centroid)[,1],
         lat = st_coordinates(centroid)[,2]) %>%
  filter(lon > -130 & lon < -60 & lat > 20 & lat < 50)


# join PM2.5 data with shapefiles
pm25 <- left_join(zcta_sf, pm25, by = c("GEOID10" = "zcta"))

# map
ggsave("results/figures/pm25_map_2001_2017.png", height = 10, width = 14, dpi = 300)
pm25 |>
  ggplot() +
  geom_sf(aes(fill = mean_pm25), color = NA) + 
  scale_fill_viridis(expression("Mean PM"[2.5]*" (\u00B5g/m"^3*"), 2001-2017    "),
                     option = "A") +
  coord_sf(crs = st_crs(5070)) +
  theme_void() +
  theme(legend.position = "bottom",
        #legend.direction = "horizontal", 
        legend.text = element_text(size = 8),
        #legend.text.align = 0.75,
        legend.title = element_text(size = 10),
        #legend.key.width = unit(150*k, "points"),
        legend.key.width = unit(75, "points"),
        legend.key.height = unit(25, "points"),
        plot.background = element_rect(fill = "white", color = NA),
        text = element_text(family = "Arial"))
dev.off()





#png("results/figures/pm25_map.jpeg", height = 1024 * 0.53 * k, width = 1024 * k)
gc()
png("results/figures/pm25_map_2010.jpeg", height = 1024 * 0.65 * k, width = 1024 * k)
grid %>%
  ggplot() +
  geom_point(aes(x = x, y = y, color = pm25),
             size = 0.15 * k) +
  # #xlim(-125, -65) + 
  xlim(-125, -66) + 
  ylim(25, 50) +
  # scale_color_gradient2(expression(paste("PM"[2.5],"   ")), 
  #                       low  = "#0084FF", mid = "#FCFC94", high = "#9C0000", midpoint = 8,
  #                       breaks = c(0, 4, 8, 12, 16),
  #                       labels = c("0", "4", "8", "12", "16+"), limits = c(0, 16), na.value = "white") +
  scale_color_viridis(expression("Mean annual PM"[2.5]*" (\u00B5g/m"^3*") in 2010    "),
                      option = "A",
                      breaks = c(0, 4, 8, 12, 16),
                      labels = c("0", "4", "8", "12", "16+"), limits = c(0, 16), na.value = "white") +
  labs(x = "", y = "") +
  coord_map("albers", lat0 = 30, lat1 = 40) +
  theme_void() +
  theme(#plot.title = element_text(size = 24*k, hjust = 0.5, vjust = 0.5),
    legend.position = "bottom",
    #legend.direction = "horizontal", 
    legend.text = element_text(size = 18*k),
    legend.text.align = 0.75,
    legend.title = element_text(size = 24*k),
    #legend.key.width = unit(150*k, "points"),
    legend.key.width = unit(75*k, "points"),
    legend.key.height = unit(25*k, "points"),
    strip.text = element_text(size = 26*k))
dev.off()



