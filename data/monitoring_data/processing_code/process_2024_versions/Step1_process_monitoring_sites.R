# Process Monitoring Site Locations
# Cori Lopazanski
# Dec 2024
# About: Updated from Josh's script from 2022


rm(list=ls())

require(dplyr)
require(tidyr)

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
kelp_sites <- read.csv(file.path(data.dir, "monitoring_kelp/update_2024/MLPA_kelpforest_07.25.2024/MLPA_kelpforest_site_table.6.csv"), 
                       na.strings = c("N/A", "NA")) %>% clean_names()

kelp <- kelp_sites %>%
  distinct(site, site_status, latitude, longitude) %>%
  mutate(habitat = "Kelp forest",
         site_type = case_when(site_status == "mpa" ~ "MPA",
                               site_status == "reference" ~ "Reference", T~site_status)) %>%
  mutate(site_type = case_when(site == "HORSESHOE_REEF_E" ~ "Reference",
                               site == "HORSESHOE_REEF_W" ~ "Reference", T~site_type)) %>% 
  filter(!is.na(site_type)) %>% 
  dplyr::select(habitat, site, site_type, lat_dd = latitude, lon_dd = longitude) %>% 
  group_by(habitat, site, site_type) %>% 
  # A few duplicates with different lat/lon in a single year
  summarize(lat_dd = mean(lat_dd, na.rm = T),
            lon_dd = mean(lon_dd, na.rm = T), .groups = 'drop')

## Surf Zone ----------------------
# Read the site names for matching with the habitat site names (boo Chris bad processing making extra work!)
surf_sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/site_tables/processed/surf_site_names.Rds") %>% 
  rename(lon_dd = long_dd)

surf <- surf_sites %>%
  mutate(site = paste(site_name, site_type)) %>% 
  distinct(habitat, site, site_type, lat_dd, lon_dd)


## Deep Reef -------------
deep_sites <- readRDS(file.path(data.dir,"processed_data/update_2024/deep_reef_transect_metadata.Rds"))

deep <- deep_sites %>% 
  mutate(habitat = "deep") %>% 
  dplyr::select(habitat, site = transect_id_desig, site_type = designation, lat_dd = avg_lat, lon_dd = avg_lon) %>% 
  distinct() 
  

#   dplyr::select(affiliated_mpa, site_type = designation, designation = type, lat_dd = lat, lon_dd = long) %>% 
#   mutate(designation = if_else(str_detect(designation, "/"), "SMR", designation)) %>% 
#   mutate(designation = if_else(grepl("^\\s*$", designation), "SMR", designation)) %>% 
#   mutate(habitat = "Deep reef")
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

# Export deep (process separately)
saveRDS(deep, file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_deep.Rds"))


