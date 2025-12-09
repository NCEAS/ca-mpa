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
ccfrp_sites <- read.csv(file.path(data.dir, "monitoring_ccfrp/CCFRP_derived_data_tables_DataONE/CCFRP_location_table.csv")) %>% clean_names()

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
  dplyr::select(site_sd, campus, mpa_status:geometry) %>% 
  mutate(across(where(is.character), ~ trimws(.))) %>% 
  filter(!campus == "RCCA") %>% 
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
                       "SCAI_LIONS_HEAD" = "SCAI_LION_HEAD")) %>% 
  group_by(site) %>% 
  summarize(geometry = st_centroid(st_union(geometry)), .groups = 'drop')
  
kelp <- kelp_fish_sites %>% 
  # Start by matching by the available centroids
  left_join(kelp_centroids, by = c("site")) %>% 
  # Where not available, use the points from the site table
  full_join(kelp_site_table) %>% 
  dplyr::select(site, site_type, lat_dd, lon_dd, centroid = geometry) %>% 
  filter(!is.na(lat_dd)) %>% # three sites with no centroid or lat/lon info
  st_as_sf(coords = c("lon_dd", "lat_dd"), crs = 4326, remove = F, sf_column_name = "new_point") %>%
  st_transform(st_crs(kelp_centroids)) # new point becomes sticky here

# # Create two sf objects from different geometry columns
# centroids_sf <- kelp %>% st_set_geometry("centroid")
# new_points_sf <- kelp %>% st_set_geometry("new_point")
# 
# # Plot them together
# tm_shape(new_points_sf) +  # Base layer with new points
#   tm_dots(fill = "black") +
#   tm_text("site", size = 1) +
#   tm_shape(centroids_sf) +  # Centroids in red
#   tm_dots(fill = "red")

# Bridge the two together - pick centroid where available, otherwise the point
kelp <- kelp %>% 
  # Create a new column prioritizing centroid and using site info where centroid missing
  mutate(geometry = if_else(st_is_empty(centroid), new_point, centroid)) %>%   
  st_set_geometry("geometry") %>% 
  dplyr::select(site, site_type, geometry) %>% 
  st_transform(crs = 4326) %>% 
  # Use the new geometry column to extract the lat/lon points
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


## Deep Reef -------------
# deep_sites <- readRDS(file.path(data.dir,"processed_data/update_2024/deep_reef_transect_metadata.Rds"))
# 
# deep <- deep_sites %>% 
#   mutate(habitat = "Deep reef") %>% 
#   mutate(designation = str_sub(transect_id_desig, -3)) %>% 
#   mutate(site_type = if_else(designation == "ref", "Reference", "MPA")) %>% 
#   dplyr::select(habitat, site = transect_id_desig, site_type, lat_dd = avg_lat, lon_dd = avg_lon) %>% 
#   distinct() 
#   

## Rocky process ------------------------
# rocky_sites <- read.csv(file.path(data_path,  "/monitoring_rocky-intertidal/CA_MPA_sites_20210907b.csv"))
# rocky <- rocky_sites %>%
#   mutate(group="rocky",
#          mpa_class=mpa_designation)%>%
#   select(group, affiliated_mpa=mpa_name, mpa_class,
#                         mpa_designation, site=marine_site_name, lat=latitude, lon=longitude)
# rocky_process$mpa_designation <- recode_factor(rocky_process$mpa_designation, "NONE"="ref")
# rocky_process$mpa_designation <- tolower(rocky_process$mpa_designation)
# rocky_process$affiliated_mpa <- tolower(rocky_process$affiliated_mpa)
# rocky_process$mpa_class <- tolower(rocky_process$mpa_class)
# rocky_data <- rocky_process


# Join --------------------------------------------------------------------
site_locations <- bind_rows(ccfrp, kelp, surf)

# Export
saveRDS(site_locations, file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations.Rds"))
