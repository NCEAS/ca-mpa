# Build the data tables for CCFRP sites
# Cori Lopazanski
# Oct 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/habitat_sim/combined"


# Combined (Detailed) ----------------------------------------------------------------------
# Find the common geometry classification
# Define columns used to create groups: want to combine per site, per PMEP Zone (depth)
site_columns <- c("mpa", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")

combine_detailed <- function(section){
  substrate <- readRDS(file.path(sub.dir, paste0("substrate_clean/substrate_clean_", section, ".Rds"))) 
  biotic <- readRDS(file.path(bio.dir, paste0("biotic_clean/biotic_clean_", section, ".Rds"))) 
  print("Read complete.")
  
  overlap <- st_intersection(biotic, substrate) %>% 
    select(-contains(".1")) %>% 
    mutate(habitat_class = "Overlap")
  print("Overlap intersection complete.")
  
  biotic_only <- st_difference(biotic, st_union(substrate)) %>% 
    mutate(habitat_class = "Biotic")
  print("Biotic only complete.")
  
  substrate_only <- st_difference(substrate, st_union(biotic)) %>% 
    mutate(habitat_class = "Substrate")
  print("Substrate only complete.")
  
  combined <- bind_rows(list(overlap, substrate_only, biotic_only))
  saveRDS(combined, file.path(com.dir, paste0("combined_detailed_", section, ".Rds")))
  
}

combine_detailed(section = "23")
combine_detailed(section = "30")
combine_detailed(section = "31")
combine_detailed(section = "32")
combine_detailed(section = "33")
combine_detailed(section = "40")
combine_detailed(section = "41") 

