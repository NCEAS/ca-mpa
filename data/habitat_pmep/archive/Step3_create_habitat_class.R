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


# # Combined by habitat class ----------------------------------------------------
# # Define columns used to create groups: want to combine per site, per PMEP Zone (depth)
# 
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae",
                 "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")
# 
# combine_by_class <- function(section){
#   combined <- readRDS(file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_detailed_", section, ".Rds"))) 
#   print("Read complete.")
#   combined <- combined %>% 
#     mutate(substrate = case_when(CMECS_SC_Category %in% c("Rock Substrate", "Anthropogenic Substrate") ~ "Hard Bottom",
#                                  CMECS_SC_Name %in% c("Boulder", "Cobble", "Very Coarse Woody Debris") ~ "Hard Bottom",
#                                  CMECS_SC_Category %in% c("Unconsolidated Mineral Substrate", "Coarse Unconsolidated Substrate", "Fine Unconsolidated Substrate") ~ "Soft Bottom",
#                                  CMECS_SC_Name %in% c("Shell Hash") ~ "Soft Bottom")) %>% 
#     mutate(biotic = case_when(habitat_class %in% c("Overlap", "Biotic") ~ "Biotic")) %>% 
#     mutate(habitat_class = case_when(habitat_class == "Overlap" ~ paste(substrate, biotic, sep = " "),
#                                      habitat_class == "Substrate" ~ substrate,
#                                      habitat_class == "Biotic" ~ biotic))
#   print(Sys.time())
#   print("Starting combine.")
#   combined_by_class <- combined %>% 
#     group_by(across(all_of(site_columns)), habitat_class) %>%  
#     summarize(geometry = st_union(geometry), .groups = 'drop') 
#   print(Sys.time())
#   print("Combine complete.")
#   saveRDS(combined_by_class, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_hsb_", section, ".Rds")))
# 
# }
# 
# combine_by_class(section = "23")
# combine_by_class(section = "30")
# combine_by_class(section = "31")
# combine_by_class(section = "32")
# combine_by_class(section = "33")
# combine_by_class(section = "40")
# combine_by_class(section = "41")


# Create revised classes that remove floating plans, faunal beds, and benthic/unidentified macroalgae 
# that isn't otherwsie classified as an aquatic vegetation bed or seagrass/kelp

bio_columns_keep <- c("AquaticVegetationBed", "Kelp", "Seagrass", "AquaticVascularVegetation") # These are overlap columns where we will keep the overlap geometry

revised_by_class <- function(section){
  combined <- readRDS(file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_detailed_", section, ".Rds"))) 
  print("Read complete.")
  combined <- combined %>% 
    mutate(habitat_class = case_when(habitat_class == "Overlap" & CMECS_BC_Category_Code %in% c("1.2", "2.2") ~ "Substrate", # Remove floating plants and faunal beds - if overlap, keep substrate
                                     habitat_class == "Biotic"  &  CMECS_BC_Category_Code %in% c("1.2", "2.2") ~ "Drop", # If biotic-only, just drop them
                                     habitat_class == "Overlap" & CMECS_BC_Category_Code == "2" & any(c_across(all_of(bio_columns_keep)) == "Yes") ~ "Overlap", # For the "Benthic/Attached" class, keep if overlaps with retained types
                                     habitat_class == "Overlap" & CMECS_BC_Category_Code == "2" & if_all(all_of(bio_columns_keep), is.na) ~ "Substrate", # If doesn't overlap with retained types, keep just substrate info
                                     habitat_class == "Biotic"  & CMECS_BC_Category_Code == "2" & any(c_across(all_of(bio_columns_keep)) == "Yes") ~ "Biotic", # Keep if overlaps with retained types
                                     habitat_class == "Biotic" & CMECS_BC_Category_Code == "2" & if_all(all_of(bio_columns_keep), is.na) ~ "Drop", # If doesn't overlap with retained types, drop if biotic only
                                     T~habitat_class)) %>% 
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
  saveRDS(combined_by_class, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_hsb_revised", section, ".Rds")))
  
}


revised_by_class(section = "23")
revised_by_class(section = "30")
revised_by_class(section = "31")
revised_by_class(section = "32")
revised_by_class(section = "33")
revised_by_class(section = "40")
revised_by_class(section = "41")

       
         
         
