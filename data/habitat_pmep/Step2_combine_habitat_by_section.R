# Build the data tables within 1000m of the monitoring sites
# Cori Lopazanski
# June 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Build  ----------------------------------------------------------------------
# Find the common geometry classification
# Define columns used to create groups: want to combine per site, per PMEP Zone (depth)
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")


process_habitat <- function(section){
  substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_1000m/substrate_sites_section_", section, ".Rds"))) %>% 
    rename(geometry = Shape) %>% 
    filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
    group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') 
  print("Read substrate complete.")
  
  biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_1000m/biotic_sites_section_", section, ".Rds"))) %>% 
    rename(geometry = Shape) %>% 
    filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
    group_by(across(all_of(site_columns)), CMECS_BC_Category_Code, CMECS_BC_Category,
             across(all_of(bio_columns))) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') 
    st_transform(., crs = st_crs(substrate))
  print("Read biotic complete.")
  
  overlap <- st_intersection(biotic, substrate) %>% 
    select(-contains(".1")) %>% 
    # Define "habitat class" based on substrate
    mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom Biotic",
                                     !is.na(CMECS_SC_Category) ~ "Soft Bottom Biotic"))
  print("Overlap intersection complete.")
  
  biotic_only <- st_difference(biotic, st_union(substrate)) %>% 
    mutate(habitat_class = "Biotic")
  print("Biotic only complete.")
  
  substrate_only <- st_difference(substrate, st_union(biotic)) %>% 
    mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom",
                                     !is.na(CMECS_SC_Category) ~ "Soft Bottom"))
  print("Substrate only complete.")
  
  combined <- bind_rows(list(overlap, substrate_only, biotic_only))
  saveRDS(combined, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_detailed_", section, ".Rds")))
  
  combined_by_class <- combined %>% 
    group_by(across(all_of(site_columns)), habitat_class) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') 
  print("Combined by class complete.")
  
  combined_by_class$area_m2 <- as.numeric(st_area(combined_by_class$geometry))
  
  saveRDS(combined_by_class, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_hsb_", section, ".Rds")))
  
}

# Apply function to each section ----

#sections <- c("23", "30", "31", "32", "33", "53", "40", "41")
# 50 & 52 has error, multisurface geometry most likely

process_habitat(section = "23")
process_habitat(section = "30")
process_habitat(section = "31")
process_habitat(section = "32")
process_habitat(section = "33")
process_habitat(section = "40")
#process_habitat(section = "41")


# Section 41  ----
section <- "41"


## 41 Overlap (Complete) ----
# Start with full read and process overlap - this works fine
substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_1000m/substrate_sites_section_", section, ".Rds"))) %>% 
  rename(geometry = Shape) %>% 
  filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
  group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category) %>% # Group by category (Rock, Fine UMS, Coarse UMS, UMS)
  summarize(geometry = st_union(geometry), .groups = 'drop') 

biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_1000m/biotic_sites_section_", section, ".Rds"))) %>% 
  rename(geometry = Shape) %>% 
  filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
  group_by(across(all_of(site_columns)), CMECS_BC_Category_Code, CMECS_BC_Category, # Group by category + overlap details
           across(all_of(bio_columns))) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
  st_transform(crs = st_crs(substrate))
  
overlap <- st_intersection(biotic, substrate) %>% 
  select(-contains(".1")) %>% 
  mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom Biotic",
                                   !is.na(CMECS_SC_Category) ~ "Soft Bottom Biotic"))

# Get some space to avoid crash - this is ~ 20k obs
saveRDS(overlap, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_overlap_41.Rds"))
rm(overlap)
gc()

## 41 Biotic Only (Complete) ----
biotic_only <- st_difference(biotic, st_union(substrate)) %>% 
  mutate(habitat_class = "Biotic")

saveRDS(biotic_only, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_biotic_only_41.Rds"))
rm(biotic_only)
gc()


biotic <- st_union(biotic)

## 41 Substrate Only ----

substrate <- readRDS("substrate_sites_section_41.Rds") %>% 
    rename(geometry = Shape) %>% 
    filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
    group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') 

biotic <- readRDS("biotic_sites_section_41.Rds") %>% 
  rename(geometry = Shape) %>% 
  filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
  group_by(across(all_of(site_columns)), CMECS_BC_Category_Code, CMECS_BC_Category,
           across(all_of(bio_columns))) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
  st_transform(biotic, crs = st_crs(substrate))

substrate <- st_buffer(substrate, 0)

substrate_only <- st_difference(substrate, biotic)

# Saved the first two processed substrate data to save time:
#saveRDS(substrate, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_substrate_41.Rds"))
#saveRDS(biotic, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_biotic_union_41.Rds"))

# Can read these in to start from here.


# Process substrate only = this doesn't work, unclear why not
# Try using the substrate footprint to do the difference, then intersect by site
sub <- readRDS(file.path(sub.dir, paste0("substrate_intersect_", section, ".Rds"))) %>% 
  filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
  select(PMEP_Section: CMECS_SC_Cartography_Detail, CMECS_SC_Code:PMEP_NSID, NS_PolyID, geometry = Shape)

bio <- readRDS(file.path(bio.dir, paste0("biotic_intersect_", section, ".Rds"))) %>% 
  filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
  st_transform(crs = st_crs(sub))

bio <- st_union(bio)

# Filter to the depth zones that have both substrate and biotic
overlap_zones <- sub %>% 
  filter(PMEP_Zone %in% c("2", "4", "6"))

# Process the difference in the zones that have both substrate and biotic
substrate_only <- st_difference(overlap_zones, bio)

# Summarize to the category (rock, ums, fine ums, coarse ums, anth)
substrate_only <- substrate_only %>% 
  group_by(PMEP_Section, PMEP_Zone, CMECS_SC_Category, CMECS_SC_Category_Code) %>% 
  summarize(geometry = st_union(geometry, .groups = 'drop'))

#saveRDS(substrate_only, file.path(com.dir, "temp_substrate_only_overlap_zones_41.Rds"))
#substrate_only <- readRDS(file.path(com.dir, "temp_substrate_only_overlap_zones_41.Rds"))

library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")
section <- "41"

substrate_only <- readRDS(file.path(com.dir, "temp_substrate_only_overlap_zones_41.Rds"))
print("Substrate read.")

# Intersect by site:
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)
sites <- st_transform(sites, crs = 32610)  
sites <- st_buffer(sites, dist = 1000)
sites <- st_transform(sites, crs = st_crs(substrate_only))
print("Sites constructed and transformed")

# Reduce to sites that overlap with the substrate in this section:
site_intersects <- st_intersects(sites, substrate_only, sparse = FALSE)
print("Site intersects complete.")

sites <- sites[rowSums(site_intersects) > 0, ]
saveRDS(sites, file.path(com.dir, "temp_sites.Rds"))


sites <- readRDS("temp_sites.Rds")
substrate_only <- readRDS("temp_substrate_only_overlap_zones_41.Rds")

substrate_only <- st_make_valid(substrate_only)

# Apply smmal buffer to try to fix topology issues
substrate_only <- st_buffer(substrate_only, 0)
substrate_only_sites <- st_intersection(sites, substrate_only)
saveRDS(substrate_only_sites, "substrate_only_overlap_sites_41.Rds")


substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_1000m/substrate_sites_section_", section, ".Rds"))) %>% 
  rename(geometry = Shape) %>% 
  filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
  group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') 
print("Read substrate complete.")

biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_1000m/biotic_sites_section_", section, ".Rds"))) %>% 
  rename(geometry = Shape) %>% 
  filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
  group_by(across(all_of(site_columns)), CMECS_BC_Category_Code, CMECS_BC_Category,
           across(all_of(bio_columns))) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
  st_transform(., crs = st_crs(substrate))
print("Read biotic complete.")

substrate_only <- st_difference(substrate, st_union(biotic)) %>% 
  mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom",
                                   !is.na(CMECS_SC_Category) ~ "Soft Bottom"))
print("Substrate difference complete.")
saveRDS(substrate_only, file.path(com.dir, "temp_substrate_only_41.Rds"))







#saveRDS(overlap, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_overlap_41.Rds"))
#saveRDS(biotic_only, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_biotic_only_41.Rds"))

combined <- bind_rows(list(overlap, substrate_only, biotic_only))
saveRDS(combined, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_detailed_", section, ".Rds")))

combined_by_class <- combined %>% 
  group_by(across(all_of(site_columns)), habitat_class) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') 
print("Combined by class complete.")

combined_by_class$area_m2 <- as.numeric(st_area(combined_by_class$geometry))

saveRDS(combined_by_class, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_hsb_", section, ".Rds")))




