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
bio_crs <- st_crs(st_read(file.path(bio.dir, "biotic_ca/sections/biotic_section_23.gpkg"), quiet = TRUE))

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


# Intersect merged site footprint and biotic --------------------------------

sites_merged <- st_transform(sites_merged, bio_crs)

# For biotic:
intersect_biotic <- function(section){
  print(paste0("Section: ", section))
  sect <- st_read(file.path(bio.dir, "biotic_ca/sections", paste0("biotic_section_", section, ".gpkg")))
  print("Read complete")
  section_intersect <- st_intersection(sect, sites_merged)
  print("Intersection complete")
  saveRDS(section_intersect, file.path(bio.dir, paste0("biotic_intersect_", section, ".Rds")))
  print("Save complete")
}

sections <- unique(att$PMEP_Section)

lapply(sections, intersect_biotic)
intersect_biotic(section = '23') # redo this after exporting non-CA portions (some sites span just over the border)

# Find intersections by site + export ------------------------------------------

sites <- st_transform(sites, crs = bio_crs)

intersect_by_site <- function(section){
  print(paste0("Section: ", section))
  bio <- readRDS(file.path(bio.dir, paste0("biotic_intersect_", section, ".Rds")))
  print("Read section complete.")
  bio_sites <- st_intersection(bio, sites)
  print("Intersection complete.")
  bio_sites$area <- st_area(bio_sites)
  print("Area calculation complete.")
  saveRDS(bio_sites, file.path(bio.dir, paste0("biotic_sites_1000m/intersect_by_site/biotic_sites_section_", section, ".Rds")))
}


sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
lapply(sections, intersect_by_site)
intersect_by_site(section = '23')


# Clean details by site + export ------------------------------------------
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")

section <- "31"

biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_1000m/biotic_sites_section_", section, ".Rds")))
crs_info <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))

clean_biotic <- function(section){
  biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_1000m/intersect_by_site/biotic_sites_section_", section, ".Rds"))) %>% 
    rename(geometry = Shape) %>% 
    filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
    group_by(across(all_of(site_columns)), CMECS_BC_Category_Code, CMECS_BC_Category, CMECS_BC_Code, CMECS_BC_Name,
             across(all_of(bio_columns))) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
    st_transform(., crs = crs_info)
  
  saveRDS(biotic, file.path(bio.dir, paste0("biotic_sites_1000m/biotic_sites_section_", section, ".Rds")))
}

clean_biotic(section = "23")

sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
lapply(sections, clean_biotic)


