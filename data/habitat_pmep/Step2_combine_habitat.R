# Step 2 Alternate Version: Get Substrate and Biotic Separately 
# Cori Lopazanski
# Dec 2024

# This script avoid doing a spatial overlap between the substrate and biotic classes.
# Instead, it takes the spatial exports from Step 1 and builds X.

# Setup   ----------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined"

site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type") # remove PMEP Zone because we create our own below
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")


# Read attribute tables  to generate classes ------------------------------------------------------------------------------------------------------------
# For substrate:
sub_att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

sub_habitat <- sub_att %>% 
  dplyr::select(CMECS_SC_Code:CMECS_SC_Category_Code, CMECS_SC_Subgroup:CMECS_SC_Group) %>% distinct() %>% 
  mutate(habitat_class = case_when(
    CMECS_SC_Category == "Rock Substrate" ~ "Hard Bottom", 
    CMECS_SC_Category == "Coarse Unconsolidated Substrate" & CMECS_SC_Name %in% c("Boulder", "Cobble") ~ "Hard Bottom",
    CMECS_SC_Category == "Coarse Unconsolidated Substrate" & CMECS_SC_Name %in% c("Gravel", "Pebble", "Gravel Mixes", "Sandy Gravel", 
                                                                                  "Gravelly", "Gravelly Sand", "Gravelly Muddy Sand") ~ "Soft Bottom",
    CMECS_SC_Category == "Fine Unconsolidated Substrate" ~ "Soft Bottom",
    CMECS_SC_Category == "Unconsolidated Mineral Substrate" ~ "Soft Bottom",
    CMECS_SC_Category == "Anthropogenic Substrate" ~ "Hard Bottom", 
    CMECS_SC_Category == "Biogenic Substrate" & CMECS_SC_Name == "Very Coarse Woody Debris" ~ "Hard Bottom",
    CMECS_SC_Category == "Biogenic Substrate" & CMECS_SC_Name == "Shell Hash" ~ "Soft Bottom",
    CMECS_SC_Category == "Unclassified" ~ "Unclassified",
    T~NA))

sub_depth <- sub_att %>% 
  distinct(PMEP_Zone) %>% 
  mutate(depth_zone = factor(case_when(PMEP_Zone == "Landward Zone"~ "landward",
                                       PMEP_Zone == "Core Zone (Shoreline to -30m)" ~ "0_30m",
                                       PMEP_Zone == "Seaward Zone (-30m to -100m)" ~ "30_100m",
                                       PMEP_Zone == "Deep Shelf or Sound (-100m to -200m)" ~  "100_200m",
                                       PMEP_Zone == "Outside PMEP Scope (>-200m) or International Waters" ~ "200m"),
                             levels = c("landward", "0_30m", "30_100m", "100_200m", "200m")))

# For biotic:
bio_att <- readRDS(file.path(bio.dir, "West_Coast_USA_Nearshore_CMECS_Biotic_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA")

bio_depth <- bio_att %>% 
  distinct(PMEP_Zone) %>% 
  mutate(depth_zone = factor(case_when(PMEP_Zone == 0 ~ "landward",
                                       PMEP_Zone %in% c(1, 2, 3) ~"0_30m",
                                       PMEP_Zone %in% c(4, 5) ~ "30_100m",
                                       PMEP_Zone %in% c(6, 7) ~ "100_200m", 
                                       PMEP_Zone == 8 ~ "200m"),
                             levels = c("landward", "0_30m", "30_100m", "100_200m", "200m")))
    



# Build table for each section ------------------------------------------------------------------------------------------------------------------------

build_habitat <- function(section){
  print(paste("Section: ", section))
  substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_500m/substrate_sites_section_", section, ".Rds"))) 
  
  sub_crs <- st_crs(substrate)
  
  sub <- substrate %>% 
    left_join(sub_habitat) %>% 
    left_join(sub_depth) %>% 
    group_by(across(all_of(site_columns)), habitat_class, depth_zone) %>% 
    summarize(geometry = st_union(Shape), .groups = "drop") 
  
  biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_500m/biotic_sites_section_", section, ".Rds"))) 
  
  seagrass <- biotic %>% 
    filter(!habitat == "Rocky intertidal") %>% 
    filter(Seagrass == "Yes") 
  
  if (nrow(seagrass) > 0) {
    seagrass <- seagrass %>% 
      mutate(habitat_class = "Seagrass") %>% 
      left_join(bio_depth) %>% 
      group_by(across(all_of(site_columns)), habitat_class, depth_zone) %>% 
      summarize(geometry = st_union(Shape), .groups = "drop") %>% 
      st_transform(., crs = sub_crs)
  } else {
    seagrass <- NULL
  }
  
  aqu_veg <- biotic %>% 
    filter(!habitat == "Rocky intertidal") %>% 
    filter(AquaticVegetationBed == "Yes") 
  
  if (nrow(aqu_veg) > 0) {
    aqu_veg <- aqu_veg %>% 
      mutate(habitat_class = "Aquatic Vegetation Bed") %>% 
      left_join(bio_depth) %>%
      group_by(across(all_of(site_columns)), habitat_class, depth_zone) %>% 
      summarize(geometry = st_union(Shape), .groups = "drop") %>% 
      st_transform(., crs = sub_crs)
    
  } else {
    aqu_veg <- NULL
  }
  
  aqu_vas <- biotic %>% 
    filter(!habitat == "Rocky intertidal") %>% 
    filter(AquaticVascularVegetation == "Yes") 
  
  if (nrow(aqu_vas) > 0) {
    aqu_vas <- aqu_vas %>% 
      mutate(habitat_class = "Aquatic Vascular Vegetation") %>% 
      left_join(bio_depth) %>% 
      group_by(across(all_of(site_columns)), habitat_class, depth_zone) %>% 
      summarize(geometry = st_union(Shape), .groups = "drop") %>% 
      st_transform(., crs = sub_crs)
  } else {
    aqu_vas <- NULL
  }
  
  habitat <- bind_rows(sub, seagrass, aqu_veg, aqu_vas)
  
  saveRDS(habitat, file.path(com.dir, paste0("combined_", section, ".Rds")))
  
}

sections <- c("23", "30", "31", "32", "33", "40", "41")
lapply(sections, build_habitat)


