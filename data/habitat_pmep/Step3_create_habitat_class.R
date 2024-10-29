# Step 3: Combine habitat details by class
# Cori Lopazanski
# June 2024

# About: This script takes the detailed combined substrate + biotic habitat,
# that was created in Step 2. Combine Habitat By Section, and combines the detailed
# information into the four broad habitat types: hard bottom, hard bottom biotic, 
# soft bottom, and soft bottom biotic.

# Inputs: combined/combined_mlpa_sites_100m/combined_detailed_
# Outputs: combined/combined_mlpa_sites_100m/combined_hsb_

# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Combined by habitat class ----------------------------------------------------
# Define columns used to create groups: want to combine per site, per PMEP Zone (depth)

site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", 
                 "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")

combine_by_class <- function(section){
  combined <- readRDS(file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_detailed_", section, ".Rds"))) 
  print("Read complete.")
  combined <- combined %>% 
    mutate(substrate = case_when(CMECS_SC_Category %in% c("Rock Substrate", "Anthropogenic Substrate") ~ "Hard Bottom",
                                 CMECS_SC_Name %in% c("Boulder", "Cobble", "Very Coarse Woody Debris") ~ "Hard Bottom",
                                 CMECS_SC_Category %in% c("Unconsolidated Mineral Substrate", "Coarse Unconsolidated Substrate", "Fine Unconsolidated Substrate") ~ "Soft Bottom",
                                 CMECS_SC_Name %in% c("Shell Hash") ~ "Soft Bottom")) %>% 
    mutate(biotic = case_when(habitat_class %in% c("Overlap", "Biotic") ~ "Biotic")) %>% 
    mutate(habitat_class = case_when(habitat_class == "Overlap" ~ paste(substrate, biotic, sep = " "),
                                     habitat_class == "Substrate" ~ substrate,
                                     habitat_class == "Biotic" ~ biotic))
  print(Sys.time())
  print("Starting combine.")
  combined_by_class <- combined %>% 
    group_by(across(all_of(site_columns)), habitat_class) %>%  
    summarize(geometry = st_union(geometry), .groups = 'drop') 
  print(Sys.time())
  print("Combine complete.")
  saveRDS(combined_by_class, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_hsb_", section, ".Rds")))

}

combine_by_class(section = "23")
combine_by_class(section = "30")
combine_by_class(section = "31")
combine_by_class(section = "32")
combine_by_class(section = "33")
combine_by_class(section = "40")
combine_by_class(section = "41")

