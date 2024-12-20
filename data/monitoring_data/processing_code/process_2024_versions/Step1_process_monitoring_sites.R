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
# SKIP IN CASE WE CAN GET LAT LON FROM THE START
# deep_sites <- read.csv(file.path(data.dir, "monitoring_deep-reef/ROV_Dataset/MidDepth_ROV_Site_Table.csv")) %>% clean_names() %>% 
#   # There are some duplicates - looks like 256 distinct sites
#   distinct(mpa_group, type, designation, mpa_name, ca_mpa_name_short, lat, long)
# 
# deep_raw <- read.csv(file.path(data.dir, "monitoring_deep-reef/ROV_Dataset/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), 
#                           na = c("N/A", "", " ")) %>% clean_names() 
# 
# deep_processed <- read_csv(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed/deep_reef_fish_biomass_updated.csv")) 
# 
# deep_lengths <- readxl::read_excel(file.path(data.dir, "monitoring_deep-reef/ROV_Dataset/MidDepth_ROV_Fish_Size.xlsx")) %>% clean_names() %>% 
#   distinct(region_mpa)
# 
# deep_count <-read_csv(file.path(data_path, "monitoring_deep-reef/ROV_Dataset/MidDepth_ROV_Fish_Count.csv")) %>% clean_names()
# 
# test <- deep_lengths %>% 
#   distinct(region, mpa_group, type, designation, lat, lon)
# 
# 
# 
# deep_orig <- read_csv(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed/deep_reef_fish_biomass_updated.csv")) %>% 
#   distinct(mpa_group, type, designation, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation)
# 
# deep <- deep_reef_raw %>% 
#   left_join(., deep_sites)
# 
# 
# 
# deep <- deep_sites %>% 
#   mutate(ca_mpa_name_short = if_else(designation == "Reference", paste(mpa_group, type), ca_mpa_name_short)) %>% 
#   mutate(affiliated_mpa = str_to_lower(ca_mpa_name_short)) %>% 
#   # FYI - some of the names now don't have SMR/SMCA because it is technically a reference for both.
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
#                 
# rocky_process$mpa_designation <- recode_factor(rocky_process$mpa_designation, "NONE"="ref")
# 
# rocky_process$mpa_designation <- tolower(rocky_process$mpa_designation)
# 
# rocky_process$affiliated_mpa <- tolower(rocky_process$affiliated_mpa)
# 
# rocky_process$mpa_class <- tolower(rocky_process$mpa_class)
# 
# rocky_data <- rocky_process


# Join --------------------------------------------------------------------
site_locations <- bind_rows(ccfrp, kelp, surf)

# Export
saveRDS(site_locations, file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations.Rds"))




