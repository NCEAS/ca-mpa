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


# Read substrate attributes and CRS --------------------------------------------
att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

crs_info <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))


# Build 1000m site buffers  ----------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")

# Convert to sf object
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform CRS to UTM Zone 10N for CA (uses meters)
sites <- st_transform(sites, crs = 32610)  

# Create buffer polygons around each point, e.g., 1000 meters radius
sites <- st_buffer(sites, dist = 1000)

# Transform sites
sites <- st_transform(sites, crs = crs_info)

# Merge sites
sites_merged <- st_union(sites)

# Intersect merged site footprint and substrate --------------------------------

# For substrate:
intersect_sections <- function(section){
  print(paste0("Section: ", section))
  sect <- st_read(file.path(sub.dir, "substrate_ca/sections", paste0("substrate_section_", section, ".gpkg")))
  print("Read complete")
  section_intersect <- st_intersection(sect, sites_merged)
  print("Intersection complete")
  saveRDS(section_intersect, file.path(sub.dir, paste0("substrate_intersect_", section, ".Rds")))
  print("Save complete")
}

sections <- unique(att$PMEP_Section)
sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
# 50 & 52 has error, multisurface geometry most likely

lapply(sections, intersect_sections)


# Find intersections by site + export ------------------------------------------

intersect_by_site <- function(section){
  print(paste0("Section: ", section))
  sub <- readRDS(file.path(sub.dir, paste0("substrate_intersect_", section, ".Rds")))
  print("Read section complete.")
  sub_sites <- st_intersection(sub, sites)
  print("Intersection complete.")
  sub_sites$area <- st_area(sub_sites)
  print("Area calculation complete.")
  saveRDS(sub_sites, file.path(sub.dir, paste0("substrate_sites_1000m/substrate_sites_section_", section, ".Rds")))
}


sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
lapply(sections, intersect_by_site)



