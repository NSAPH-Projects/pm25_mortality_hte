
# get seasonal averages for temperature and humidity

library(data.table)
library(lubridate)
library(arrow)

cols <- c("zcta", "date", "tmmn", "tmmx", "rmin", "rmax")

# read in raw daily temperature data
met_path <- "/n/dominici_nsaph_l3/Lab/lego/environmental/meteorology__gridmet/zcta_daily/"
met_list <- lapply(paste0(met_path, list.files(met_path)[1:19]), 
                   function(file) read_parquet(file, col_select = all_of(cols)))
met_data <- rbindlist(met_list)


# code taken from:
  # https://github.com/NSAPH/National-Causal-Analysis/blob/master/Confounders/earth_engine/
  #    code/6_calculate_seasonal_averages.R

met_data[, date := ymd(date)]

summer  <- NULL
winter <- NULL

# we need exposures in year 1 (which starts in 2001 and ends in 2017)
for (year_ in 2001:2017) {
  cat(year_, " ")
  summer_int <- interval(ymd(paste0(year_, "0601")), ymd(paste0(year_, "0831")))
  if (leap_year(year_)) {
    winter_int <- interval(max(ymd(paste0(year_ - 1,  "1201")), ymd("20000101")),  ymd(paste0(year_, "0229")))
  } else {
    winter_int <- interval(max(ymd(paste0(year_ - 1,  "1201")), ymd("20000101")),  ymd(paste0(year_, "0228")))
  }
  
  summer_year <- met_data[date %within% summer_int, .(summer_tmmx = mean(tmmx, na.rm = T), summer_rmax = mean(rmax,  na.rm = T)), by = zcta]
  summer_year[, year := year_]
  summer <- rbind(summer, summer_year)
  rm(summer_int, summer_year); gc()
  
  winter_year <- met_data[date %within% winter_int, .(winter_tmmx = mean(tmmx, na.rm = T), winter_rmax = mean(rmax, na.rm = T)), by = zcta]
  winter_year[, year := year_]
  winter <- rbind(winter, winter_year)
  rm(winter_int, winter_year); gc()
}

out <- merge(summer, winter, by = c("zcta", "year"))
write.csv(out, "data/intermediate/temp_humidity_seasonal_zcta.csv", row.names = FALSE)
