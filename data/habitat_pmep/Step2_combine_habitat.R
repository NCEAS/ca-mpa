# Step 2 Alternate Version: Get Substrate and Biotic Separately 
# Cori Lopazanski
# Dec 2024

# This script avoid doing a spatial overlap between the substrate and biotic classes.
# Instead, it takes the spatial exports from Step 1 and builds X.

print("Starting Step 2: Combine Habitat")

# Setup   ----------------------------------------------------------------------------------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined"

site_columns <- c("habitat", "site", "site_type") # remove PMEP Zone because we create our own below
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
    

# Address unclassified substrate for each section w/ nearest neighbor ----------------------
classify_substrate <- function(section){
  # Read the substrate file for the section
  substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_500m/substrate_sites_section_", section, ".Rds"))) 
  
  # Build the substrate table and simplify across habitat classes
  sub <- substrate %>% 
    left_join(sub_habitat) %>% 
    left_join(sub_depth) %>% 
    group_by(across(all_of(site_columns)), habitat_class, depth_zone) %>% 
    summarize(geometry = st_union(Shape), .groups = "drop") 
  
  # Use nearest neighbor to fix unclassified polygons
  unclassified <- sub %>% 
    filter(habitat_class == "Unclassified")
  
  classified <- sub %>% 
    filter(habitat_class != "Unclassified")
  
  if (nrow(unclassified) > 0 && nrow(classified) > 0) {
    nearest_indices <- st_nearest_feature(unclassified, classified)
    
    unclassified <- unclassified %>%
      mutate(habitat_class = classified$habitat_class[nearest_indices])
    
    sub <- bind_rows(classified, unclassified) %>% 
      group_by(across(all_of(site_columns)), habitat_class, depth_zone) %>% 
      summarize(geometry = st_union(geometry), .groups = "drop") 
  }
  
  saveRDS(sub, file.path(sub.dir, paste0("substrate_sites_500m/substrate_sites_section_classified_", section, ".Rds")))
  
  }
  
print("  Classifying substrate.")

classify_substrate(section = "23")
classify_substrate(section = "30")
classify_substrate(section = "31")
classify_substrate(section = "32")
classify_substrate(section = "33")
classify_substrate(section = "40")
classify_substrate(section = "41")



# Build table for each section ------------------------------------------------------------------------------------------------------------------------

build_habitat <- function(section){
  print(paste("Section: ", section))
  substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_500m/substrate_sites_section_classified_", section, ".Rds"))) 
  
  sub_crs <- st_crs(substrate)
  
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
  
  habitat <- bind_rows(substrate, seagrass, aqu_veg, aqu_vas)
  
  saveRDS(habitat, file.path(com.dir, paste0("combined_", section, ".Rds")))
  
}

print("  Building habitat table.")

sections <- c("23", "30", "31", "32", "33", "40", "41")
lapply(sections, build_habitat)


# Use the substrate data to create site footprints and centers ------

create_footprints <- function(section) {
  
  substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_500m/substrate_sites_section_classified_", section, ".Rds"))) %>% 
    filter(depth_zone != "landward") %>% 
    ungroup() 
  
  footprint <- substrate %>% 
    group_by(habitat, site, site_type) %>% 
    summarize(geometry = st_union(st_combine(geometry)), .groups = "drop") %>% 
    st_make_valid()
  
  return(footprint)
}

# Process all sections and combine into one dataframe
print("  Starting footprints.")

combined_footprints <- bind_rows(lapply(sections, create_footprints))

combined_footprints2 <- combined_footprints %>% 
  group_by(habitat, site, site_type) %>%  # because some sites span across sections
  summarize(geometry = st_union(geometry), .groups = 'drop') 

combined_footprints3 <- combined_footprints2 %>% 
  st_make_valid()

# Save the final combined dataframe
saveRDS(combined_footprints3, file.path(sub.dir, "substrate_sites_500m/site_footprints.Rds"))

