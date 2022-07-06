

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
col_key <- readxl::read_excel(file.path(outdir, "column_key_ca_programs.xlsx"))

# Read MPA data
mpas <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# To-do list
# 1) Determine wind speed units
# 2) The temperature column is messed up - handle if you care?
# 3) Clean weather/tide station names/coordinates -- probably optional

# Site key
################################################################################

# Site key
site_key <- data_orig %>% 
  # Unique sites
  janitor::clean_names("snake") %>% 
  select(mpa, mpa_id) %>% 
  unique() %>% 
  # Recode a few MPA names
  mutate(mpa=recode(mpa,
                    "Blue Cavern (Catalina Island) SMCA"="Blue Cavern Offshore SMCA",
                    "Cat Harbor (Catalina Island) SMCA"="Cat Harbor SMCA",
                    "Laguna Beach SMCA"="Laguna Beach SMCA (No-Take)",
                    "Año Nuevo SMCA"="Año Nuevo SMR",
                    "Point Vicente SMCA"="Point Vicente SMCA (No-Take)",
                    "Campus Point SMCA"="Campus Point SMCA (No-Take)",
                    "Lovers Point SMR"="Lovers Point - Julia Platt SMR")) %>%
  # Mark MPA or control
  mutate(survey_type=ifelse(!grepl("control", tolower(mpa)), "MPA", "Control")) %>% 
  # Format control names
  mutate(mpa=gsub("CONTROL |control |Control |", "", mpa))

# Confirm that all MPAs are in MPA key
site_key$mpa[!site_key$mpa %in% mpas$mpa & site_key$survey_type=="MPA"]
  

# Format data
################################################################################

# conve

# Format data
data <- data_orig %>%
  # Rename
  janitor::clean_names("snake") %>% 
  # Rename columns
  rename(tide_ft=tide_height,
         air_temp_f=air_temperature) %>%
  # Add corrected MPA name and site type
  select(-mpa) %>% 
  left_join(site_key, by="mpa_id") %>% 
  # Convert date/time
  mutate(date=lubridate::ymd(date),
         time_start1=lubridate::hms(time_start),
         time_end1=lubridate::hms(time_end),
         time_start2=as.POSIXlt(time_start, format = "%H:%M") %>% hour(.) + 
           as.POSIXlt(time_start, format = "%H:%M") %>% minute(.)/60,
         time_end2=as.POSIXlt(time_end, format = "%H:%M") %>% hour(.) + 
           as.POSIXlt(time_end, format = "%H:%M") %>% minute(.)/60) %>% 
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
  # Format tide station
  mutate(tide_station_name=stringr::str_trim(tide_station_name),
         # tide_station_name=ifelse(tide_station_name %in% c("0", ".9"), NA, tide_station_name),
         tide_station_name=recode(tide_station_name,
                             "TIde app"="Tide app",
                             "Tide app on my phone"="Tide app",
                             "Tides app on my phone"="Tide app",
                             "point reyes"="Point Reyes",
                             "persoanl"="Personal observation",
                             "personal observacion"="Personal observation",
                             "On-Site Observation + ezfshn website"="On-Site Observation + EZfishn website",
                             "On-Site Observation +EZfishn website"="On-Site Observation + EZfishn website")) %>% 
  # Format tide and wind speed
  mutate(tide_ft=ifelse(tide_ft==-9999, NA, tide_ft),
         wind_speed=ifelse(wind_speed==-9999, NA, wind_speed),
         wind=recode(wind, "Not windy"="Calm"),
         air_temp_f=ifelse(air_temp_f %in% c(-9999, 0), NA, wind_speed)) %>% 
  # Compute survey duration
  mutate(duration_hr=lubridate::time_length(time_end1-time_start1, unit="hours")) %>% 
  # Assume that NA in number of activities is a zero
  mutate_at(vars(c(beach_rec_sandy, beach_rec_rocky, wildlife_viewing_sandy:unknown_fishing)), ~replace_na(., 0)) %>% 
  # Arrange
  select(survey_id, program, mpa_id, mpa, survey_type, 
         survey_site:time_end, time_start1, time_end1, time_start2, time_end2, duration_hr, everything())

# Inspect data
str(data)
freeR::complete(data)

# Inspect data
table(data$program)
table(data$survey_type)
table(data$survey_site_type)
table(data$beach_status)
table(data$clouds)
table(data$precipitation)
table(data$visibility)
sort(unique(data$weather_station_name)) # imperfect
table(data$tide_level)
sort(unique(data$tide_station_name)) # imperfect - i'd bet lat/long problems too
table(data$wind)
table(data$temperature) # lots of problems
table(data$air_temp_f)  # lots of problems

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
tide_key <- data %>% 
  select(tide_station_name, tide_station_lon, tide_station_lat) %>% 
  unique() %>% 
  arrange(tide_station_name)


# Export data
################################################################################

# Export
saveRDS(data, file=file.path(outdir, "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))

