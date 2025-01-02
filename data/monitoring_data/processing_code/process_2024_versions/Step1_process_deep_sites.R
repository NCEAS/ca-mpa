# Process Deep ROV Sites
# Cori Lopazanski
# Jan 2025

# Locations of each dive/line ID provided by SLZ to CL on Jan 1 2025 via email.

library(tidyverse)
library(sf)

# Setup
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring"

# Locations of each dive/line ID
deep_sites <- readxl::read_xlsx(file.path(ltm.dir, "monitoring_deep-reef/DeepwaterROVlocations.xlsx")) %>% janitor::clean_names()

# Deep processed data
deep_data <- readRDS(file.path(ltm.dir,"processed_data/update_2024/deep_reef_processed.Rds"))  

# Get the individual transects from the site data and average the lat/lon and sum the distance/area
deep_latlon <- deep_sites %>% 
  group_by(survey_year, location, desgnation, dive, line, line_id) %>%
  summarize(distance_m = sum(distance_m),
            area_m2 = sum(area_m2),
            avg_lat = mean(avg_lat, na.rm = T),
            avg_lon = mean(avg_lon, na.rm = T)) %>% 
  mutate(dive = as.character(dive),
         line = as.character(line)) %>% 
  rename(year = "survey_year")

# Get the individual transects from the fish data
deep_data2 <- deep_data %>% 
  distinct(year, mpa_group, dive, line, line_id, transect_id_desig)

# Join to the main dataframe
deep_data3 <- deep_data2 %>% 
  full_join(deep_latlon)





