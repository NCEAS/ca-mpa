# Process Monitoring Site Locations
# Cori Lopazanski
# Dec 2024
# About: Updated from Josh's script from 2022

rm(list=ls())

library(tidyverse)
library(janitor)

data.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring"

# Build ---------------------------------------------------------------------------
## Rocky Reef ---------------------
ccfrp_sites <- read.csv(file.path(data.dir, "monitoring_ccfrp/update_2024/MLPA_ccfrp_10.31.2024/CCFRP_location_table.csv")) %>% clean_names()

ccfrp <- ccfrp_sites %>% 
  distinct(grid_cell_id, mpa_status, lat_center_point_dd, lon_center_point_dd) %>% 
  rename(site = grid_cell_id, site_type = mpa_status, lat_dd = lat_center_point_dd, lon_dd = lon_center_point_dd) %>% 
  mutate(habitat = "Rocky reef",
         site_type = case_when(site_type == "MPA" ~ "MPA",
                               site_type == "REF" ~ "Reference", T~site_type)) %>% 
  drop_na() %>% 
  dplyr::select(habitat, site, site_type, lat_dd, lon_dd)

## Kelp Forest ---------------------
# Site table provided in MLPA data gives ~ one lat/lon point for each site
kelp_site_table <- read.csv(file.path(data.dir, "monitoring_kelp/update_2024/MLPA_kelpforest_07.25.2024/MLPA_kelpforest_site_table.6.csv"),
                       na.strings = c("N/A", "NA")) %>%
  clean_names()  %>%
  filter(str_detect(method, "FISH")) %>%
  distinct(site, site_status, ca_mpa_name_short, latitude, longitude) %>%
  mutate(site_type = case_when(site_status == "mpa" ~ "MPA",
                               site_status == "reference" ~ "Reference", T~site_status)) %>%
  mutate(site_type = case_when(site == "HORSESHOE_REEF_E" ~ "Reference",
                               site == "HORSESHOE_REEF_W" ~ "Reference",
                               site == "POINT_LOMA_CEN" ~ "Reference",
                               T~site_type)) %>%
  dplyr::select(site, site_type, lat_dd = latitude, lon_dd = longitude) %>%
  group_by(site, site_type) %>%
  # A few duplicates with different lat/lon in a single year
  summarize(lat_dd = mean(lat_dd, na.rm = T),
            lon_dd = mean(lon_dd, na.rm = T), .groups = 'drop') 

# Use the actual fish data to reduce to the sites where fish monitoring has taken place and
# also made it into the final dataset (the one on DataOne, not ours)
kelp_fish_sites <- read_csv(file.path(data.dir, "monitoring_kelp/update_2024/MLPA_kelpforest_07.25.2024", "MLPA_kelpforest_fish.6.csv"),
                            na = c("N/A", "NA", "na", "'n/a'")) %>% 
  distinct(site) %>% 
  mutate(site = snakecase::to_screaming_snake_case(site)) %>% 
  mutate(across(where(is.character), ~ trimws(.))) 

# Site centroids were provided by MLPA monitoring groups but does not cover all sites in data
kelp_centroids <- read_sf("/home/shares/ca-mpa/data/sync-data/gis_data/raw/MLPA_AM_site_centroids") %>% clean_names() %>% 
  filter(!campus == "RCCA") %>% 
  dplyr::select(site_sd, mpa_status, campus, x, y, geometry) %>% 
  mutate(across(where(is.character), ~ trimws(.))) %>% 
  # Many sites are listed as the old names. Create new column that's the screaming snake case version of the name:
  mutate(site = snakecase::to_screaming_snake_case(site_sd)) %>% 
  # Reclassify the original site name as the old names
  dplyr::select(site, site_name_old = site_sd, everything()) %>% 
  # Remove the old name if the new name is the same 
  mutate(site_name_old = if_else(site_name_old == site, NA, site_name_old)) %>% 
  mutate(site = str_replace_all(site, "EAST$", "E") %>% 
           str_replace_all(., "WEST$", "W") %>% 
           str_replace_all(., "NORTH$", "N") %>% 
           str_replace_all(., "SOUTH$", "S") %>% 
           str_replace_all(., "_S_", "S_")) %>% # fixes X's to Xs
  mutate(site = recode(site,
                       "PYRAMIND_POINT_2" = "PYRAMID_POINT_2",
                       "LEO_CARILLO" = "LEO_CARRILLO",
                       "DEEP_HOLE_E" = "DEEP_HOLE_EAST",
                       "SCAI_LIONS_HEAD" = "SCAI_LION_HEAD",
                       "3_PALMS_E" = "3_PALMS_EAST")) %>% 
  dplyr::select(site, geometry) %>% 
  group_by(site) %>% 
  summarize(geometry = st_centroid(st_union(geometry)), .groups = 'drop')

# Transform kelp site table to match 4326:
kelp_centroids <- st_transform(kelp_centroids, 4326)

# Create point geometry for site table:
kelp_site_table <- kelp_site_table %>% 
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = 4326, remove = T, sf_column_name = "geometry") 
  
# Determine which centroids are matches for the fish data:
pick_centroid <- kelp_centroids %>% 
  filter(site %in% kelp_fish_sites$site)

pick_site_table <- kelp_site_table %>% 
  filter(site %in% kelp_fish_sites$site) %>% 
  filter(!site %in% pick_centroid$site) %>% 
  select(site, geometry)

# Create full site dataset:
kelp <- bind_rows(pick_centroid, pick_site_table) %>% 
  st_set_geometry("geometry") %>% 
  left_join(kelp_site_table %>% select(site, site_type) %>% st_drop_geometry()) %>% 
  mutate(lon_dd = st_coordinates(geometry)[,1],
         lat_dd = st_coordinates(geometry)[,2]) %>% 
  mutate(habitat = "Kelp forest") %>% 
  dplyr::select(habitat, site, site_type, lat_dd, lon_dd) %>% st_drop_geometry()

## Surf Zone ----------------------
# Read the site names for matching with the habitat site names (boo Chris bad processing making extra work!)
surf_sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/site_tables/processed/surf_site_names.Rds") %>% 
  rename(lon_dd = long_dd)

surf <- surf_sites %>%
  mutate(site = paste(site_name, site_type)) %>% 
  distinct(habitat, site, site_type, lat_dd, lon_dd)



# Join --------------------------------------------------------------------
site_locations <- bind_rows(ccfrp, kelp, surf)

# Export
saveRDS(site_locations, file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations.Rds"))
