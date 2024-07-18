# Build the data tables within 1000m of the monitoring sites
# Cori Lopazanski
# June 2024


# Setup   ----------------------------------------------------------------------
library(tidyverse)
library(sf)

bio.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/biotic"
sub.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/substrate"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined"
gdb.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Nearshore_Zones_and_Habitat.gdb" 


# Read habitat data within 1000m of monitoring sites
# (Biotic and substrate layers combined in one dataframe)
combined <- readRDS(file.path(com.dir, "combined_mlpa_sites_1000m.Rds"))

# Define columns used to create groups: want to combine per site, per PMEP Zone (depth)
site_columns <- c("habitat", "mpa", "mpa_orig", "site", "site_type", "PMEP_Section", "PMEP_Zone")


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










