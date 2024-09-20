#### CODE FROM:
# Riccardo Cadei 
# CRE Medicare application
# /n/dominici_nsaph_l3/Lab/projects/rcadei/medicare/pm_map.R


# Generate annual PM2.5 maps
library(tidyverse)
library(sp)
library(raster)
library(viridis)

# path to PM2.5 data
path_data <- "/n/dominici_lab_ro/lab/data/exposures/exposure/pm25/whole_us/annual/grid_pts/qd_new_predictions/"

# load PM2.5 data (change the year in two places in the file name!)
# these are very long row vectors
pm_2000 <- readRDS(paste0(path_data, "PredictionStep2_Annual_PM25_USGrid_20000101_20001231.rds"))
pm_2005 <- readRDS(paste0(path_data, "PredictionStep2_Annual_PM25_USGrid_20050101_20051231.rds"))
pm_2010 <- readRDS(paste0(path_data, "PredictionStep2_Annual_PM25_USGrid_20100101_20101231.rds"))
pm_2015 <- readRDS(paste0(path_data, "PredictionStep2_Annual_PM25_USGrid_20150101_20151231.rds"))

# load grid long/lat
grid <- readRDS(paste0(path_data, "USGridSite.rds"))

# get PM2.5 data in grid
grid$pm_2000 <- as.numeric(pm_2000)
grid$pm_2005 <- as.numeric(pm_2005)
grid$pm_2010 <- as.numeric(pm_2010)
grid$pm_2015 <- as.numeric(pm_2015)
grid <- as.data.frame(grid)


#----- spatial data processing

# code for each lat/long
coordinates(grid) <- ~Lon + Lat

# # map projection
# albers_eac <- "+proj=aea +lat_1=29.5 +lat_2=45.5 +lat_0=23 +lon_0=-96 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"
unproj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# unprojected (maybe I can just use a different projection here)
proj4string(grid) <- unproj
#proj4string(grid) <- albers_eac

# load county shapefile
counties <- shapefile("/n/dominici_nsaph_l3/Lab/projects/National_Causal/National_Causal/Maps/cb_2017_us_county_500k.shp")
counties <- spTransform(counties, unproj) # same projection

# link plotting points and shapefile (I think)
sp_points <- grid[counties,] # takes a few mins

grid <- cbind.data.frame(coordinates(sp_points), 
                         sp_points$pm_2000,
                         sp_points$pm_2005,
                         sp_points$pm_2010,
                         sp_points$pm_2015)
names(grid) <- c("x", "y", "pm_2000", "pm_2005", "pm_2010", "pm_2015")

# cut off at 16 ug/m3
grid[grid$pm_2000 > 16, ]$pm_2000 = 16
grid[grid$pm_2005 > 16, ]$pm_2005 = 16
grid[grid$pm_2010 > 16, ]$pm_2010 = 16
grid[grid$pm_2015 > 16, ]$pm_2015 = 16

# get data in long format with years for plotting
grid <- grid %>%
  pivot_longer(cols = c(pm_2000, pm_2005, pm_2010, pm_2015),
               names_to = "year",
               names_prefix = "pm_",
               values_to = "pm25")

# plot scaling
k = 4

# plot (takes a few mins)
#png("results/figures/pm25_map.jpeg", height = 1024 * 0.53 * k, width = 1024 * k)
gc()
png("results/figures/pm25_map.jpeg", height = 1024 * 0.65 * k, width = 1024 * k)
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
  scale_color_viridis(expression("Mean annual PM"[2.5]*" (\u00B5g/m"^3*")    "),
                      option = "A",
                      breaks = c(0, 4, 8, 12, 16),
                      labels = c("0", "4", "8", "12", "16+"), limits = c(0, 16), na.value = "white") +
  labs(x = "", y = "") +
  facet_wrap(~year, nrow = 2) +
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

