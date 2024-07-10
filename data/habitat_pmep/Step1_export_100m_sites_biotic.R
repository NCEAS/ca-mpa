# Export the habitat data within 1000m of the monitoring sites
# Cori Lopazanski
# June 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Load PMEP zones --------------------------------------------------------------------------------------
zones_ca <- read_sf(dsn = gdb.dir, layer = "West_Coast_USA_Nearshore_Zones") %>% 
  st_zm() %>% # convert to xy
  filter(State == "CA")

# Load monitoring sites --------------------------------------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")

# Convert to sf object
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform CRS to UTM Zone 10N for CA (uses meters)
sites <- st_transform(sites, crs = 32610)  

# Create buffer polygons around each point, e.g., 1000 meters radius
sites <- st_buffer(sites, dist = 1000)

# Add buffer polygons as a new column to the original sf object
#sites_sf$buffer_1000m <- sites_buffer$geometry


# Load biotic layer ---------------------------------------------------------------------------------
# WARNING: TAKES A BIT. COMMENTED OUT TO AVOID ACCIDENT RUN.
# biotic <- read_sf(dsn = file.path(bio.dir, "biotic_ca"), 
#                   layer = 'West_Coast_USA_Nearshore_CMECS_Biotic_Habitat') 

att_bio <- readRDS(file.path(bio.dir, "West_Coast_USA_Nearshore_CMECS_Biotic_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") %>% 
  filter(!(CMECS_BC_Code == '9.9.9.9.9')) %>% # drop unclassified
  filter(!(CMECS_BC_Code == '1.2')) # drop floating plants

# Drop the z dimension and transform to match UTM 10
biotic <- st_zm(biotic) %>% 
  st_transform(biotic, crs = 32610)  

# Extract intersection
biotic_subset <- st_intersection(biotic, sites)

# Calculate the area of each polygon
biotic_subset$area <- st_area(biotic_subset)

# Save the shapefiles and associated metadata
#saveRDS(biotic_subset, file.path(bio.dir, "biotic_mlpa_sites_1000m.Rds"))

# Create a summary table for each site
biotic_area <- biotic_subset %>% 
  st_drop_geometry() %>% 
  mutate(area = as.numeric(area)) %>% 
  left_join(., att_bio, by = "NS_PolyID") %>% 
  group_by(habitat, mpa, site, site_type, PMEP_Section, PMEP_Zone, 
           CMECS_BC_Category_Code, CMECS_BC_Category, CMECS_BC_Name) %>% 
  summarize(total_area_m2 = sum(area, na.rm = TRUE)) %>% 
  filter(!is.na(CMECS_BC_Category))

# Save biotic table
#saveRDS(biotic_area, file.path(bio.dir, "biotic_mlpa_sites_1000m_totals.Rds"))


