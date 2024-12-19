# Export the habitat data within proximity of the monitoring sites
# Cori Lopazanski
# June 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Read substrate attributes and CRS --------------------------------------------
att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

crs_sub <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))
crs_bio <- st_crs(st_read(file.path(bio.dir, "biotic_ca/sections/biotic_section_23.gpkg"), quiet = TRUE))

# Build 1000m site buffers  ----------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds") %>% 
  filter(!habitat == "Rocky intertidal") %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"), crs = 4326)

# Project CRS to use meters
sites <- st_transform(sites, crs = 26910)  

# Create buffer polygons around each point, e.g., 1000 meters radius
sites <- st_buffer(sites, dist = 500)

# Intersect merged site footprint and substrate --------------------------------

# For substrate:
intersect_substrate <- function(section){
  print(paste0("Section: ", section))
  
  sect <- st_read(file.path(sub.dir, "substrate_ca/sections", paste0("substrate_section_", section, ".gpkg")))
  print("Read complete")
  
  section_intersect <- st_intersection(sect, sites)
  print("Intersection complete")
  
  saveRDS(section_intersect, file.path(sub.dir, "substrate_sites_500m", paste0("substrate_sites_section_", section, ".Rds")))
  print("Save complete")
}

# Apply to sites
# Transform sites to match substrate
sites <- st_transform(sites, crs = crs_sub)
sections <- c("23", "30", "31", "32", "33", "40", "41")
lapply(sections, intersect_substrate)

# For biotic:
intersect_biotic <- function(section){
  print(paste0("Section: ", section))
  sect <- st_read(file.path(bio.dir, "biotic_ca/sections", paste0("biotic_section_", section, ".gpkg")))
  print("Read complete")
  
  section_intersect <- st_intersection(sect, sites)
  print("Intersection complete")
  
  saveRDS(section_intersect, file.path(bio.dir, "biotic_sites_500m", paste0("biotic_sites_section_", section, ".Rds")))
  print("Save complete")
}


# Apply to biotic:
sites <- st_transform(sites, crs = crs_bio)
lapply(sections, intersect_biotic)


# Clean details by site + export ------------------------------------------
# Did not do this for V2 yet but leaving here in case need these details (CL 18 Dec 2024)
# site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
# bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")
# 
# section <- "31"
# 
# clean_substrate <- function(section){
#   substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_1000m/intersect_by_site/substrate_sites_section_", section, ".Rds"))) %>% 
#     rename(geometry = Shape) %>% 
#     filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
#     group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category, CMECS_SC_Code, CMECS_SC_Name) %>% 
#     summarize(geometry = st_union(geometry), .groups = 'drop') 
#   
#   saveRDS(substrate,  file.path(sub.dir, paste0("substrate_sites_1000m/substrate_sites_section_", section, ".Rds")))
# }
# 
# sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
# lapply(sections, clean_substrate)

