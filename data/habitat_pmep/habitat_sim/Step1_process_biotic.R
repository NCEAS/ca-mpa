# Export the habitat data within the CCFRP site footprints
# Cori Lopazanski
# June 2024

# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"
sim.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim"

# Read substrate attributes and CRS --------------------------------------------
att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

crs_info <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))
bio_crs <- st_crs(st_read(file.path(bio.dir, "biotic_ca/sections/biotic_section_23.gpkg"), quiet = TRUE))

# Read site buffers  ----------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim/ccfrp_site_buffers.Rds")


# Intersect merged site footprint and biotic --------------------------------
sites_merged <- st_transform(sites, bio_crs)

# For biotic:
intersect_biotic <- function(section){
  print(paste0("Section: ", section))
  sect <- st_read(file.path(bio.dir, "biotic_ca/sections", paste0("biotic_section_", section, ".gpkg")))
  print("Read complete")
  section_intersect <- st_intersection(sect, sites_merged)
  print("Intersection complete")
  saveRDS(section_intersect, file.path(sim.dir, paste0("biotic_intersect_", section, ".Rds")))
  print("Save complete")
}

sections <- unique(att$PMEP_Section)

lapply(sections, intersect_biotic)


# Clean details by site + export ------------------------------------------
site_columns <- c("mpa", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")

crs_info <- st_crs(st_read(file.path(sub.dir, "substrate_ca/sections/substrate_section_23.gpkg"), quiet = TRUE))

clean_biotic <- function(section){
  biotic <- readRDS(file.path(sim.dir, paste0("biotic_intersect_", section, ".Rds"))) %>% 
    rename(geometry = Shape) %>% 
    filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
    group_by(across(all_of(site_columns)), CMECS_BC_Category_Code, CMECS_BC_Category, CMECS_BC_Code, CMECS_BC_Name,
             across(all_of(bio_columns))) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
    st_transform(., crs = crs_info)
  
  saveRDS(biotic, file.path(sim.dir, paste0("biotic_clean/biotic_clean_", section, ".Rds")))
}

clean_biotic(section = "23")

sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
lapply(sections, clean_biotic)

