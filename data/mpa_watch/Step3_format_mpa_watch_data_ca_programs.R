

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "mpa_watch/raw")
outdir <- file.path(basedir, "mpa_watch/processed")
plotdir <- "data/mpa_watch/figures"

# Read data
data_orig <- read.csv(file.path(indir, "Surveys_2022-05-09_CaliforniaPrograms.csv"), as.is=T, na.strings=c(""))

# Read column key
col_key <- readxl::read_excel(file.path(outdir, "column_key.xlsx"))


# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rena
  janitor::clean_names("snake") %>% 
  # Rename columns
  rename(mpa_name=mpa,
         tide_ft=tide_height,
         air_temp_f=air_temperature) %>%
  # Convert date/time
  mutate(date=lubridate::ymd(date),
         time_start=lubridate::hms(time_start),
         time_end=lubridate::hms(time_end)) %>% 
  # Format strings
  mutate(beach_status=stringr::str_to_sentence(beach_status),
         clouds=stringr::str_to_sentence(clouds),
         visibility=stringr::str_to_sentence(visibility),
         visibility=recode(visibility, "Short only"="Shore only"),
         wind=stringr::str_to_sentence(wind)) %>% 
  # Format weather station long/lat
  mutate(weather_station_lon=ifelse(weather_station_lon==0, NA, weather_station_lon),
         weather_station_lat=ifelse(weather_station_lat==0, NA, weather_station_lat)) %>% 
  # Format tide station long/lat
  mutate(tide_station_lon=ifelse(tide_station_lon==0, NA, tide_station_lon),
         tide_station_lat=ifelse(tide_station_lat==0, NA, tide_station_lat)) %>% 
  # Format tide and wind speed
  mutate(tide_ft=ifelse(tide_ft==-9999, NA, tide_ft),
         wind_speed=ifelse(wind_speed==-9999, NA, wind_speed),
         air_temp_f=ifelse(air_temp_f %in% c(-9999, 0), NA, wind_speed)) %>% 
  # Gather
  gather(key="use", value="n_obs", 27:ncol(.))


# Inspect data
str(data)
# freeR::complete(data)

# Inspect data
table(data$program)
table(data$survey_site_type)
table(data$beach_status)
table(data$clouds)
table(data$precipitation)
table(data$visibility)
sort(unique(data$weather_station_name)) # imperfect
table(data$tide_level)
sort(unique(data$tide_station_name)) # imperfect - i'd bet lat/long problems too
table(data$wind)
table(data$temperature)
table(data$air_temperature)

# Inspect numeric data
range(data$date)
range(data$tide_ft, na.rm=T)
range(data$wind_speed, na.rm=T) # not sure what units are

# Weather station key
# Some lat/long need to be sweapped and some long need to be negative
weather_key <- data %>% 
  select(weather_station_name, weather_station_lon, weather_station_lat) %>% 
  unique() %>% 
  arrange(weather_station_name)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "MPA_Watch_2011_2022_surveys_ca_programs.Rds"))
