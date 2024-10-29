# Build the data tables within 1000m of the monitoring sites
# Cori Lopazanski
# June 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Combined (Detailed) ----------------------------------------------------------------------
# Find the common geometry classification
# Define columns used to create groups: want to combine per site, per PMEP Zone (depth)
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")
bio_columns <- c("FaunalBed", "AquaticVegetationBed", "BenthicMacroalgae", "Kelp", "OtherMacroalgae", "EmergentWetland", "ScrubShrubWetland", "ForestedWetland", "Seagrass", "AquaticVascularVegetation", "FloatingSuspendedBiota")

combine_detailed <- function(section){
  substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_1000m/substrate_sites_section_", section, ".Rds"))) 
  biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_1000m/biotic_sites_section_", section, ".Rds"))) 
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
  saveRDS(combined, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_detailed_", section, ".Rds")))
  
}

combine_detailed(section = "23")
combine_detailed(section = "30")
combine_detailed(section = "31")
combine_detailed(section = "32")
combine_detailed(section = "33")
combine_detailed(section = "40")
#combine_detailed(section = "41") # this crashes when handling the substrate_only section

## Section 41 ------------------------------------------------------------------------

section <- "41"
substrate <- readRDS(file.path(sub.dir, paste0("substrate_sites_1000m/substrate_sites_section_", section, ".Rds"))) 
biotic <- readRDS(file.path(bio.dir, paste0("biotic_sites_1000m/biotic_sites_section_", section, ".Rds"))) 

# Complete and save overlap, biotic only, substrate-only zones
overlap <- st_intersection(biotic, substrate) %>% 
  select(-contains(".1")) %>% 
  mutate(habitat_class = "Overlap")

biotic_only <- st_difference(biotic, st_union(substrate)) %>% 
  mutate(habitat_class = "Biotic")

substrate_zones <- substrate %>% 
  # These are the only zones that have overlap - other zones will be sub-only by default
  filter(!(PMEP_Zone %in% c("2", "4", "6"))) %>% 
  mutate(habitat_class = "Substrate")

combined <- bind_rows(list(overlap,biotic_only, substrate_zones))
saveRDS(combined, file.path(com.dir, paste0("combined_mlpa_sites_1000m/combined_detailed_partial_", section, ".Rds")))
rm(overlap, biotic_only, substrate_zones, combined)
gc()


# Use the substrate footprint to do the difference, then intersect by site
sub <- readRDS(file.path(sub.dir, "substrate_intersect", paste0("substrate_intersect_", section, ".Rds"))) %>% 
  filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
  select(PMEP_Section: CMECS_SC_Cartography_Detail, CMECS_SC_Code:PMEP_NSID, NS_PolyID, geometry = Shape) %>% 
  filter(PMEP_Zone %in% c("2", "4", "6")) # these are the only zones with overlap with biotic layer

bio <- readRDS(file.path(bio.dir, "biotic_intersect", paste0("biotic_intersect_", section, ".Rds"))) %>% 
  filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
  st_transform(crs = st_crs(sub))

bio <- st_union(bio)

substrate_only <- st_difference(sub, bio)

# Make sites
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)
sites <- st_transform(sites, crs = 32610)  
sites <- st_buffer(sites, dist = 1000)
sites <- st_transform(sites, crs = st_crs(substrate_only))

# Reduce to sites that overlap with the substrate in this section:
site_intersects <- st_intersects(sites, substrate_only, sparse = FALSE)

sites <- sites[rowSums(site_intersects) > 0, ]

# Weird geometry error - st_make_valid and st_buffer magic dust
substrate_only <- st_make_valid(substrate_only)
substrate_only <- st_buffer(substrate_only, 0)

# Intersect by site
substrate_only_sites <- st_intersection(sites, substrate_only) %>% 
  mutate(habitat_class = "Substrate")
#saveRDS(substrate_only_sites, file.path(com.dir, "combined_mlpa_sites_1000m/temp_substrate_only_41.Rds"))

# Combine with previous dataframe
combined <- readRDS(file.path(com.dir, "combined_mlpa_sites_1000m/combined_detailed_partial_41.Rds"))
combined <- bind_rows(combined, substrate_only_sites)
saveRDS(combined, file.path(com.dir, "combined_mlpa_sites_1000m/combined_detailed_41.Rds"))

