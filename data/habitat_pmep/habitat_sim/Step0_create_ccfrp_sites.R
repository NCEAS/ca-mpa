# Create footprints for habitat continuity simulation
# Cori Lopazanski
# October 2024

# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Read substrate attributes and CRS --------------------------------------------
att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

crs_info <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))
bio_crs <- st_crs(st_read(file.path(bio.dir, "biotic_ca/sections/biotic_section_23.gpkg"), quiet = TRUE))


# Build site buffers  ----------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")

# Filter to just CCFRP
sites <- sites %>% 
  filter(habitat == "Rocky reef")

# Convert to sf object
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform CRS to UTM Zone 10N for CA (uses meters)
sites <- st_transform(sites, crs = 32610)  

# Create buffer polygons around each point, e.g., 1000 meters radius
sites <- st_buffer(sites, dist = 1000)


# Read MPAs and filter to those in CCFRP data ----------------------------------
mpas <- readRDS("/home/shares/ca-mpa/data/sync-data/gis_data/processed/CA_MPA_polygons.Rds") %>% 
  filter(name %in% sites$mpa) %>% 
  st_transform(mpas, crs = 32610) 

mpa_footprint <- st_union(mpas)

# Create each MPA as its own polygon of sites
sites <- sites %>% 
  group_by(mpa) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop')

sites_buffer <- st_buffer(sites, 3000) %>% 
  group_by(mpa) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop')

ggplot() +
  geom_sf(data = sites_buffer) +
  geom_sf(data = mpas, fill = "red")


# Transform sites
sites_buffer  <- st_transform(sites_buffer, crs = crs_info)


saveRDS(sites_buffer, file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim", "ccfrp_site_buffers.Rds"))




