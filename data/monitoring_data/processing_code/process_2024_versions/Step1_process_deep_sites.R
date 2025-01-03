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
deep_data <- read_csv(file.path(ltm.dir,"processed_data/update_2024/deep_reef_processed.csv"))  %>% 
  distinct(year, mpa_group, type, designation, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, line_id, dive, line, transect_id_desig)

deep_raw <- read_csv(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/ROV_Dataset/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), 
                          na = c("N/A", "", " ")) %>% clean_names() %>% 
  distinct(survey_year, survey_date, location, mpa_group, type, designation, line_id, dive, line) %>% 
  filter(type %in% c("SMR", "SMCA", "Reference"))



# Get the individual transects from the site data and average the lat/lon and sum the distance/area
deep_latlon <- deep_sites %>% 
  # Per PI: Only keep transects affiliated with MPAs and that do not cross boundaries
 # filter(type %in% c("SMR", "SMCA", "Reference")) %>% 
  mutate(line = as.character(line)) %>% 
  rename(year = "survey_year") %>% 
  mutate(year = case_when(project == "CIMPA 2009" ~ 2009, T~year)) %>% 
  group_by(year, dive, line, line_id) %>%
  summarize(distance_m = sum(distance_m),
            area_m2 = sum(area_m2),
            avg_lat = mean(avg_lat, na.rm = T),
            avg_lon = mean(avg_lon, na.rm = T), .groups = 'drop') %>% 
  dplyr::select(year, dive, line, line_id, avg_lat, avg_lon) %>% 
  arrange(line_id)

# Get the individual transects from the fish data
deep_transects <- deep_data %>% 
  # Join to the main dataframe
  left_join(deep_latlon) %>% 
  arrange(year, dive, line, line_id) %>% 
  filter(!is.na(avg_lat))

# Test the ones that don't match to see
mismatch <- deep_data %>% # 78
  filter(!line_id %in% deep_transects$line_id)

saveRDS(deep_transects, file.path(ltm.dir,"processed_data/update_2024/deep_reef_transect_metadata.Rds"))


