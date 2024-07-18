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

# Apply function to each section

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

biotic_only <- st_difference(biotic, st_union(substrate)) %>% 
  mutate(habitat_class = "Biotic")

saveRDS(biotic_only, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_biotic_only_41.Rds"))
rm(biotic_only)
gc()


biotic <- st_union(biotic)
#saveRDS(substrate, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_substrate_41.Rds"))
#saveRDS(biotic, file.path(com.dir, "combined_mlpa_sites_1000m", "temp_biotic_union_41.Rds"))

# Process substrate only = this doesn't work, unclear why not
# Try using the substrate footprint to do the difference, then intersect by site
sub <- readRDS(file.path(sub.dir, paste0("substrate_intersect_", section, ".Rds"))) %>% 
  filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
  select(PMEP_Section: CMECS_SC_Cartography_Detail, CMECS_SC_Code:PMEP_NSID, NS_PolyID, geometry = Shape)

bio <- readRDS(file.path(bio.dir, paste0("biotic_intersect_", section, ".Rds"))) %>% 
  filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
  st_transform(crs = st_crs(sub))

bio <- st_union(bio)

overlap_zones <- sub %>% 
  filter(PMEP_Zone %in% c("2", "4", "6"))

substrate_only <- st_difference(overlap_zones, bio)

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

substrate_only <- substrate_only %>%
  # Get the non-overlap zones 
  filter(!PMEP_Zone %in% c("2", "4", "6")) %>% 
  # Bind the substrate_only rows from the overlap areas
  bind_rows(substrate_only) 

# Intersect by site:
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")

# Convert to sf object
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform CRS to UTM Zone 10N for CA (uses meters)
sites <- st_transform(sites, crs = 32610)  

# Create buffer polygons around each point, e.g., 1000 meters radius
sites <- st_buffer(sites, dist = 1000)

# Transform sites
sites <- st_transform(sites, crs = st_crs(substrate_only))

# Intersect by site
sub_sites <- st_intersection(substrate_only, sites)

substrate_only <- sub_sites %>% 
  group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category) %>% # Group by category (Rock, Fine UMS, Coarse UMS, UMS)
  summarize(geometry = st_union(geometry), .groups = 'drop') 

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




# OLD DUMP ----


sub <- readRDS(file.path(sub.dir, "substrate_intersect_30.Rds"))

sub_site <- st_intersection(sub, sites)

sub_site$area <- st_area(sub_site)


# Create a summary table for each site
substrate_area <- substrate_subset %>% 
  st_drop_geometry() %>% 
  mutate(area = as.numeric(area)) %>% 
  left_join(., att_sub, by = "NS_PolyID") %>% 
  group_by(habitat, mpa, site, site_type, PMEP_Section, PMEP_Zone, 
           CMECS_SC_Category_Code, CMECS_SC_Category, CMECS_SC_Name) %>% 
  summarize(total_area_m2 = sum(area, na.rm = TRUE)) %>% 
  filter(!is.na(CMECS_BC_Category))

# Save substrate table
saveRDS(substrate_area, file.path(sub.dir, "substrate_mlpa_sites_1000m_totals.Rds"))

# Clean up -----
rm(substrate_area, substrate)

# Combine biotic and substrate within 1000m of MLPA sites ------------------------

# Load substrate within 1000m of site
substrate <- readRDS(file.path(sub.dir, "substrate_mlpa_sites_1000m.Rds"))
sub_att <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") %>%
  filter(!(CMECS_SC_Code == "9.9.9.9.9")) %>% # unclassified
  filter(PMEP_Section < 50) # estuary sections (invalid geometries)


# Combine polygons to main substrate categories
# 1.1 Rock
# 1.2 Unconsolidated mineral
# 1.2.1 Fine unconsolidated
# 1.2.2 Coarse unconsolidated
substrate <- substrate %>% 
  left_join(sub_att, by = "NS_PolyID") %>% 
  group_by(habitat, mpa, mpa_orig, site, site_type, PMEP_Section, 
           PMEP_Zone, CMECS_SC_Category, CMECS_SC_Category_Code) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop')


# Load biotic within 1000m of site
biotic <- readRDS(file.path(bio.dir, "biotic_mlpa_sites_1000m.Rds"))
bio_att <- readRDS(file.path(bio.dir, "West_Coast_USA_Nearshore_CMECS_Biotic_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") %>% 
  filter(!(CMECS_BC_Code == '9.9.9.9.9')) %>% # drop unclassified
  filter(!(CMECS_BC_Code == '1.2')) # drop floating plants

biotic <- biotic %>% 
  left_join(bio_att, by = "NS_PolyID") %>% 
  group_by(habitat, mpa, mpa_orig, site, site_type, PMEP_Section, 
           PMEP_Zone, CMECS_BC_Category, CMECS_BC_Category_Code) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') %>%
  filter(!is.na(CMECS_BC_Category))


# Combine the biotic and substrate layers, keeping site-specific columns
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")

biotic <- biotic %>% mutate(layer = "biotic")
substrate <- substrate %>% mutate(layer = "substrate")

combined <- bind_rows(biotic, substrate) %>%
  st_as_sf() %>%
  group_by(across(all_of(site_columns)))


# Export ----
saveRDS(combined, file.path(com.dir, "combined_mlpa_sites_1000m.Rds"))








## OLD -----
# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"


# Load PMEP zones --------------------------------------------------------------------------------------
zones_ca <- read_sf(dsn = gdb.dir, layer = "West_Coast_USA_Nearshore_Zones") %>% 
  st_zm() %>% # convert to xy
  filter(State == "CA")

# Load monitoring sites --------------------------------------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")

# Convert to sf object
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform CRS to UTM Zone 10N for CA (uses meters)
sites <- st_transform(sites, crs = 32610)  

# Create buffer polygons around each point, e.g., 1000 meters radius
sites <- st_buffer(sites, dist = 1000)

# Add buffer polygons as a new column to the original sf object
#sites_sf$buffer_1000m <- sites_buffer$geometry


# Load biotic layer ---------------------------------------------------------------------------------
# WARNING: TAKES A BIT. COMMENTED OUT TO AVOID ACCIDENT RUN.
# biotic <- read_sf(dsn = file.path(bio.dir, "biotic_ca"), 
#                   layer = 'West_Coast_USA_Nearshore_CMECS_Biotic_Habitat') 

att_bio <- readRDS(file.path(bio.dir, "West_Coast_USA_Nearshore_CMECS_Biotic_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") %>% 
  filter(!(CMECS_BC_Code == '9.9.9.9.9')) %>% # drop unclassified
  filter(!(CMECS_BC_Code == '1.2')) # drop floating plants

# Drop the z dimension and transform to match UTM 10
biotic <- st_zm(biotic) %>% 
  st_transform(biotic, crs = 32610)  

# Extract intersection
biotic_subset <- st_intersection(biotic, sites)

# Calculate the area of each polygon
biotic_subset$area <- st_area(biotic_subset)

# Save the shapefiles and associated metadata
#saveRDS(biotic_subset, file.path(bio.dir, "biotic_mlpa_sites_1000m.Rds"))

# Create a summary table for each site
biotic_area <- biotic_subset %>% 
  st_drop_geometry() %>% 
  mutate(area = as.numeric(area)) %>% 
  left_join(., att_bio, by = "NS_PolyID") %>% 
  group_by(habitat, mpa, site, site_type, PMEP_Section, PMEP_Zone, 
           CMECS_BC_Category_Code, CMECS_BC_Category, CMECS_BC_Name) %>% 
  summarize(total_area_m2 = sum(area, na.rm = TRUE)) %>% 
  filter(!is.na(CMECS_BC_Category))

# Save biotic table
#saveRDS(biotic_area, file.path(bio.dir, "biotic_mlpa_sites_1000m_totals.Rds"))




# Read habitat data within 1000m of monitoring sites
sub <- readRDS(file.path(sub.dir, "substrate_sites_1000m/substrate_sites_section_23.Rds")) %>% 
  rename(geometry = Shape)
bio <- readRDS(file.path(bio.dir, "biotic_sites_1000m/biotic_sites_section_23.Rds")) %>% 
  rename(geometry = Shape)



sub2 <- sub %>% 
  filter(!CMECS_SC_Category_Code == "9.9.9.9.9") %>% 
  group_by(across(all_of(site_columns)), CMECS_SC_Category_Code, CMECS_SC_Category) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
  mutate(area_m2 = as.numeric(st_area(geometry)))

bio2 <- bio %>% 
  filter(!CMECS_BC_Category_Code == "9.9.9.9.9") %>% 
  group_by(across(all_of(site_columns)), CMECS_BC_Category_Code, CMECS_BC_Category,
           across(all_of(bio_columns))) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') %>% 
  mutate(area_m2 = as.numeric(st_area(geometry)))

bio3 <- st_transform(bio2, crs = st_crs(sub2))

# Use intersection of biotic + substrate to create hard/soft bottom biotic -------------------------------
substrate <- sub2
biotic <- bio3

overlap <- st_intersection(biotic, substrate)

overlap <- overlap %>% 
  select(-contains(".1")) %>% 
  select(!area_m2) %>% 
  # Define "habitat class" based on substrate
  mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom Biotic",
                                   !is.na(CMECS_SC_Category) ~ "Soft Bottom Biotic"))


biotic_only <- st_difference(biotic, st_union(substrate))

biotic_only <- biotic_only %>% 
  select(!area_m2) %>% 
  mutate(habitat_class = "Biotic")

substrate_only <- st_difference(substrate, biotic)

substrate_only <- substrate_only %>% 
  select(!area_m2) %>% 
  mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom",
                                   !is.na(CMECS_SC_Category) ~ "Soft Bottom"))


combined <- bind_rows(list(overlap, substrate_only, biotic_only))

combined_by_class <- combined %>% 
  group_by(across(all_of(site_columns)), habitat_class) %>% 
  summarize(geometry = st_union(geometry), .groups = 'drop') 

combined_by_class$area_m2 <- as.numeric(st_area(combined_by_class$geometry))

saveRDS(combined_by_class, file.path(com.dir, "combined_mlpa_sites_1000m", "combined_hsb_23.Rds"))




# Use intersection of biotic + substrate to create hard/soft bottom biotic -------------------------------
process_overlap <- function(group) {
  biotic_layer <- group %>% filter(layer == "biotic")
  substrate_layer <- group %>% filter(layer == "substrate")
  
  site_info <- group %>% select(all_of(site_columns)) %>% slice(1) %>% as.data.frame()
  
  result <- tryCatch({
    overlap <- st_intersection(biotic_layer, substrate_layer)
    overlap
  }, error = function(e) {
    # Store error information
    error_list <<- append(error_list, list(cbind(site_info, error = e$message)))
    # Return Null
    return(NULL)
  })
  
  return(result)
}

# Apply the function to each site group
overlaps <- combined %>%
  group_split() %>%
  map_dfr(process_overlap)

# Clean up the overlap dataframe
overlaps <- overlaps %>% 
  mutate(CMECS_BC_Category = ifelse(is.na(CMECS_BC_Category), CMECS_BC_Category.1, CMECS_BC_Category),
         CMECS_BC_Category_Code = ifelse(is.na(CMECS_BC_Category_Code), CMECS_BC_Category_Code.1, CMECS_BC_Category_Code),
         CMECS_SC_Category = ifelse(is.na(CMECS_SC_Category), CMECS_SC_Category.1, CMECS_SC_Category),
         CMECS_SC_Category_Code = ifelse(is.na(CMECS_SC_Category_Code), CMECS_SC_Category_Code.1, CMECS_SC_Category_Code)) %>% 
  select(-contains(".1")) %>% 
  select(-layer) %>% 
  # Define "habitat class" based on substrate
  mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom Biotic",
                                   !is.na(CMECS_SC_Category) ~ "Soft Bottom Biotic"))

# Merge the polygons within each habitat class (for each site and depth zone)
habitat_area <- overlaps %>% 
  group_by(across(all_of(site_columns)), habitat_class) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop') %>%
  mutate(area = st_area(geometry))

habitat_area$area <- as.numeric(habitat_area$area)

habitat_area_simple <- habitat_area %>% st_drop_geometry()

## Save hard/soft bottom biotic ----
#saveRDS(habitat_area, file.path(com.dir, "combined_mlpa_sites_1000m_hard_soft_biotic.Rds"))


# Create biotic only dataframe -----

# Function to process each site and find biotic only areas
process_biotic_only <- function(group) {
  biotic_layer <- group %>% filter(layer == "biotic")
  substrate_layer <- group %>% filter(layer == "substrate")
  
  site_info <- group %>% select(all_of(site_columns)) %>% slice(1) %>% as.data.frame()
  
  result <- tryCatch({
    # Find areas with only biotic (difference)
    biotic_only <- st_difference(biotic_layer, st_union(substrate_layer))
    biotic_only
  }, error = function(e) {
    # Store error information
    error_list <<- append(error_list, list(cbind(site_info, error = e$message)))
    # Return NULL to indicate failure
    return(NULL)
  })
  
  return(result)
}

# Apply the function to each site group
biotic_only <- combined %>%
  group_split() %>%
  map_dfr(process_site_biotic_only)

# Clean up the dataframe
biotic_only <- biotic_only %>% 
  select(-layer) %>% 
  # Define "habitat class" based on substrate
  mutate(habitat_class = case_when(is.na(CMECS_SC_Category) ~ "Biotic",
                                   TRUE ~ NA))

# Merge the polygons within each habitat class (for each site and depth zone)
biotic_area <- biotic_only %>% 
  group_by(across(all_of(site_columns)), habitat_class) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop') %>%
  mutate(area = st_area(geometry))

biotic_area$area <- as.numeric(biotic_area$area)

## Save hard/soft bottom biotic and biotic only ----
# Combine
habitat_area <- bind_rows(habitat_area, biotic_area)
saveRDS(habitat_area, file.path(com.dir, "combined_mlpa_sites_1000m_hard_soft_biotic.Rds"))
habitat_area <- readRDS(file.path(com.dir, "combined_mlpa_sites_1000m_hard_soft_biotic.Rds"))

# Create substrate only dataframe
# First create the soft and hard bottom categories to simplify the difference process
combined <- combined %>%
  ungroup() %>%
  group_by(across(all_of(site_columns))) %>% 
  select(habitat:CMECS_BC_Category_Code, CMECS_SC_Category, CMECS_SC_Category_Code, layer, geometry)

# Function to process each site and find substrate-only areas
process_substrate_only <- function(group) {
  biotic_layer <- group %>% filter(layer == "biotic")
  substrate_layer <- group %>% filter(layer == "substrate")
  
  # Check if either layer is empty
  if (nrow(substrate_layer) == 0) {
    substrate_only <- substrate_layer  # Return empty substrate layer
  } else if (nrow(biotic_layer) == 0) {
    substrate_only <- substrate_layer  # Return the entire substrate layer
  } else {
    # Find areas with only substrate (difference)
    substrate_only <- st_difference(substrate_layer, st_union(biotic_layer))
    
    # Check if substrate_only is empty
    if (nrow(substrate_only) == 0) {
      substrate_only <- substrate_layer[0, ]  # Return empty substrate layer with the same structure
    }
  }
  
  return(substrate_only)
}


# Apply the function to each site group
substrate_only <- combined %>%
  group_split() %>%
  map_dfr(process_substrate_only)

# Clean up the dataframe
substrate_only <- substrate_only %>% 
  select(-layer) %>% 
  # Define "habitat class" based on substrate
  mutate(habitat_class = case_when(CMECS_SC_Category == "Rock Substrate" ~ "Hard Bottom",
                                   CMECS_SC_Category == "Anthropogenic Substrate"~ "Hard Bottom",
                                   TRUE~"Soft Bottom"))

# Combine geometries within each habitat class without dissolving boundaries
substrate_area <- substrate_only %>%
  group_by(across(all_of(site_columns)), habitat_class) %>%
  summarize(geometry = st_combine(geometry), .groups = 'drop') %>%
  mutate(geometry = st_cast(geometry, "MULTIPOLYGON"))


substrate_area$area <- as.numeric(substrate_area$area)

test <- substrate_only %>% 
  filter(mpa == "Anacapa Island SMR") %>% 
  filter(habitat == "Kelp")


ggplot(data = test) + 
  geom_sf(aes(fill = habitat_class)) +
  facet_wrap(~site+PMEP_Zone)

## Save hard/soft bottom biotic and biotic only ----
# Combine
habitat_area <- bind_rows(habitat_area, biotic_area)
saveRDS(habitat_area, file.path(com.dir, "combined_mlpa_sites_1000m_hard_soft_biotic.Rds"))










# OLD STUFF THAT HAD ISSUES ----

# Function to perform spatial operations for each group (site)
# process_site <- function(group) {
#   biotic_layer <- group %>% filter(layer == "biotic")
#   substrate_layer <- group %>% filter(layer == "substrate")
#   
#   # Find overlap (intersection)
#   overlap <- tryCatch(st_intersection(biotic_layer, substrate_layer), error = function(e) create_empty_sf(biotic_layer))
#   
#   # Find areas with only biotic (difference)
#   biotic_only <- tryCatch(st_difference(biotic_layer, st_union(substrate_layer)), error = function(e) create_empty_sf(biotic_layer))
#   
#   # Find areas with only substrate (difference)
#   substrate_only <- tryCatch(st_difference(substrate_layer, st_union(biotic_layer)), error = function(e) create_empty_sf(substrate_layer))
#   
#   # Combine results
#   result <- bind_rows(
#     if (nrow(overlap) > 0) overlap %>% mutate(overlap_type = "biotic_and_substrate") else create_empty_sf(overlap),
#     if (nrow(biotic_only) > 0) biotic_only %>% mutate(overlap_type = "biotic_only") else create_empty_sf(biotic_only),
#     if (nrow(substrate_only) > 0) substrate_only %>% mutate(overlap_type = "substrate_only") else create_empty_sf(substrate_only)
#   )
#   
#   return(result)
# }

# New Function
process_site <- function(group) {
  biotic_layer <- group %>% filter(layer == "biotic")
  substrate_layer <- group %>% filter(layer == "substrate")
  
  site_info <- group %>% select(all_of(site_columns)) %>% slice(1)
  
  result <- tryCatch({
    # Find overlap (intersection)
    overlap <- st_intersection(biotic_layer, substrate_layer)
    
    # Find areas with only biotic (difference)
    biotic_only <- st_difference(biotic_layer, st_union(substrate_layer))
    
    # Find areas with only substrate (difference)
    substrate_only <- st_difference(substrate_layer, st_union(biotic_layer))
    
    # Combine results with checks for non-empty objects
    combined_result <- bind_rows(
      if (nrow(overlap) > 0) mutate(overlap, overlap_type = "biotic_and_substrate") else overlap,
      if (nrow(biotic_only) > 0) mutate(biotic_only, overlap_type = "biotic_only") else biotic_only,
      if (nrow(substrate_only) > 0) mutate(substrate_only, overlap_type = "substrate_only") else substrate_only
    )
    combined_result
  }, error = function(e) {
    # Store error information
    error_list <<- append(error_list, list(cbind(site_info, error = e$message)))
    # Return an empty sf object
    create_empty_sf(biotic_layer)
  })
  
  return(result)
}


# Apply the function to each site group
sites_processed <- combined %>% 
  group_split() %>% 
  map_dfr(process_site)

# Convert error list to df
error_df <- do.call(rbind, error_list) %>% as.data.frame()

error_df_simple <- error_df %>% select(-geometry)

# Clean up the dataframe
sites_processed <- sites_processed %>% 
  mutate(CMECS_BC_Category = ifelse(is.na(CMECS_BC_Category), CMECS_BC_Category.1, CMECS_BC_Category),
         CMECS_BC_Category_Code = ifelse(is.na(CMECS_BC_Category_Code), CMECS_BC_Category_Code.1, CMECS_BC_Category_Code),
         CMECS_SC_Category = ifelse(is.na(CMECS_SC_Category), CMECS_SC_Category.1, CMECS_SC_Category),
         CMECS_SC_Category_Code = ifelse(is.na(CMECS_SC_Category_Code), CMECS_SC_Category_Code.1, CMECS_SC_Category_Code)) %>% 
  select(-contains(".1")) %>% 
  select(-layer) 

# Add habitat classification for each combination
sites_processed <- sites_processed %>% 
  mutate(habitat_class = case_when(overlap_type == "biotic_and_substrate" & CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom Biotic",
                                   overlap_type == "biotic_and_substrate" & !is.na(CMECS_SC_Category) ~ "Soft Bottom Biotic",
                                   overlap_type == "biotic_only" ~ "Biotic",
                                   overlap_type == "substrate_only" & CMECS_SC_Category == "Biogenic Substrate"~ "Soft Bottom", # shell hash, woody debris
                                   overlap_type == "substrate_only" & CMECS_SC_Category == "Anthropogenic Substrate"~ "Hard Bottom", # mainly rock rubble
                                   overlap_type == "substrate_only" & CMECS_SC_Category == "Rock Substrate" ~ "Hard Bottom",
                                   overlap_type == "substrate_only" & CMECS_SC_Category %in% c("Fine Unconsolidated Substrate",
                                                                                               "Unconsolidated Mineral Substrate",
                                                                                               "Coarse Unconsolidated Substrate") ~ "Soft Bottom",
                                   TRUE ~ NA))

#saveRDS(sites_processed, file.path(com.dir, "sites_processed_mlpa_sites_1000m.Rds"))


# Combine by habitat class
habitat_area <- sites_processed %>%
  group_by(across(all_of(site_columns)), habitat_class) %>%
  summarize(geometry = st_union(geometry), .groups = 'drop') %>%
  mutate(area = st_area(geometry))

#saveRDS(habitat_area, file.path(com.dir, "sites_processed_habitat_class_mlpa_sites_1000m.Rds"))

habitat_area <- st_as_sf(habitat_area)
habitat_area$area <- as.numeric(habitat_area$area)


# Something seems off
anacapa <- habitat_area %>% 
  filter(habitat == "Kelp") %>% 
  filter(mpa == "Anacapa Island SMR") 

anacapa_simple

ggplot(data = anacapa)+ 
  geom_sf(aes(fill = habitat_class))+
  geom_sf_text(aes(label = site)) +
  facet_wrap(~site+PMEP_Zone)

# Biotic-and-substrate
biotic_substrate <- sites_processed %>% 
  filter(overlap_type == "biotic_and_substrate")

combos <- sites_processed %>% 
  st_drop_geometry() %>% 
  select(CMECS_BC_Category, CMECS_SC_Category, overlap_type) %>% 
  distinct() %>% 
  mutate(habitat_class = case_when(overlap_type == "biotic_and_substrate" & CMECS_SC_Category == "Rock Substrate"  ~ "Hard Bottom Biotic",
                                   overlap_type == "biotic_and_substrate" & !is.na(CMECS_SC_Category) ~ "Soft Bottom Biotic",
                                   overlap_type == "biotic_only" ~ "Biotic",
                                   overlap_type == "substrate_only" & CMECS_SC_Category == "Biogenic Substrate"~ "Soft Bottom", # shell hash, woody debris
                                   overlap_type == "substrate_only" & CMECS_SC_Category == "Anthropogenic Substrate"~ "Hard Bottom", # mainly rock rubble
                                   overlap_type == "substrate_only" & CMECS_SC_Category == "Rock Substrate" ~ "Hard Bottom",
                                   overlap_type == "substrate_only" & CMECS_SC_Category %in% c("Fine Unconsolidated Substrate",
                                                                                               "Unconsolidated Mineral Substrate",
                                                                                               "Coarse Unconsolidated Substrate") ~ "Soft Bottom",
                                   TRUE ~ NA))










