# Export the habitat data within 1000m of the monitoring sites
# Cori Lopazanski
# June 2024


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

# There's a challenge getting the sites data to process - reduce complexity by 
# combining the spatial footprint of the sites into a simpler object to extract
# Function to convert a polygon to its bounding box
polygon_to_square <- function(polygon) {
  bbox <- st_bbox(polygon) # Get the bounding box of the polygon
  square <- st_as_sfc(bbox) # Convert the bounding box to an sf object
  return(square)
}

# Ensure the CRS is preserved
crs_info <- st_crs(sites)

# Apply the function to each geometry in the sites dataframe and retain CRS
sites_squares <- sites %>%
  st_as_sf() %>%
  mutate(geometry = do.call(c, lapply(st_geometry(.), polygon_to_square))) %>%
  st_set_crs(crs_info)

# Convert the list of squares back to sf object and set CRS
sites_squares <- st_sf(geometry = st_sfc(sites_squares$geometry), crs = crs_info)

# Merge all squares into a single polygon
merged_sites <- st_union(sites_squares) %>%
  st_set_crs(crs_info)


# Load substrate layer ---------------------------------------------------------------------------------------
substrate <- read_sf(dsn = file.path(sub.dir, "substrate_ca"), 
                     layer = 'West_Coast_USA_Nearshore_CMECS_Substrate_Habitat') 

att_sub <- readRDS(file.path(sub.dir, "West_Coast_USA_Nearshore_CMECS_Substrate_Habitat_Attributes.Rds")) %>% 
  filter(State == "CA") %>%
  filter(!(CMECS_SC_Code == "9.9.9.9.9")) # unclassified

# Drop the z dimension
substrate <- st_zm(substrate) 

# Drop observations for unclassified 
substrate <- substrate %>%
  filter(NS_PolyID %in% att_sub$NS_PolyID)

# Transform to match UTM 10
#substrate <- st_transform(substrate, crs = 32610)  

#substrate <- st_make_valid(substrate) # Warning: takes forever, maybe doesn't work right.

# First time through, I tried using st_make_valid to fix the geometries, then
# did the functions to calculate the intersection. It worked, but there were some 
# issues with the geometries in the deeper zones - seemed to have dissolved the
# internal boundaries (filling "holes" in the data - e.g. where the land in anacapa is)
# so have stepped back to explore which step created this issue.

# 0. Examine PMEP_Sections Individually -----
# # Function to process each PMEP_Section and PMEP_Zone and find intersections
# substrate_intersection <- function(section, zone) {
#   # Filter sub_att for the given PMEP_Section and PMEP_Zone
#   sub_subset <- att_sub %>%
#     filter(PMEP_Section == section, PMEP_Zone == zone) %>% 
#     left_join(substrate, by = "NS_PolyID") %>% 
#     st_as_sf()
#   
#   # Intersect with sites_buffer
#   int <- st_intersection(sub_subset, sites_buffer)
#   
#   return(int)
# }
# 
# # Example usage: Process all sections and zones
# sections <- 33 #unique(att_sub$PMEP_Section)
# zones <-  4 #unique(att_sub$PMEP_Zone)
# 
# # Initialize an empty list to store results
# all_results <- list()
# 
# # Process each section and zone
# for (section in sections) {
#   for (zone in zones) {
#     result <- substrate_intersection(section, zone)
#     all_results <- append(all_results, list(result))
#   }
# }
# 
# # Manually running through each PMEP_Section to see where the errors are
# # Could potentially cycle through PMEP_NSID instead?
# # Section 23, 30, 31, 32, 52, 53  = fine   
# # Section 33 = Zone 0, 2, good; 3 seems empty, 4 HANGS!
# # Section 40 = PROBLEM
# # Section 41 = PROBLEM
# # Section 50 = PROBLEM
# # For 33, 40, 41, 50: Going to try looping through the different zones to see which are the issues
# 

# 1. Process partial intersection (valid sections) ---- 
#   Go ahead and process the intersection between substrate and the sites for 
#    these no-problem PMEP_Sections: 

#  Completed partial substrate overlap export 
att_subset <- att_sub %>% 
  filter(PMEP_Section %in% c(23, 30, 31, 32, 52, 53))

substrate <- substrate %>% 
  filter(NS_PolyID %in% att_subset$NS_PolyID)

# Extract intersection
substrate_subset <- st_intersection(substrate, sites)

# Calculate the area of each polygon
substrate_subset$area <- st_area(substrate_subset)

# Save the shapefiles and associated metadata
#saveRDS(substrate_subset, file.path(sub.dir, "substrate_mlpa_sites_1000m_partial.Rds"))

# 2. Loop through sites for valid intersections ----

#    Start fresh, by running rm(list = ls()) and reloading the sites and substrate
#    prior to the start of Step 1. Then now let's explore what's going on with the other
#    PMEP_Sections, which weren't working as planned.

## a. Reduce to focal sections ----
# Figure out what's going on with the other sections - filter to just the substrate
# data within the problematic sections (lower memory) 
att_sub <- att_sub %>% 
  filter(!(PMEP_Section %in% c(23, 30, 31, 32, 52, 53)))

substrate <- substrate %>%
  filter(NS_PolyID %in% att_sub$NS_PolyID)

## b. Filter subtrate that intersects with the sites ----
# Identify which polygons from substrate intersect the sites dataframe
int_indices <- st_intersects(substrate, sites, sparse = FALSE)

# Filter the substrate dataframe to retain only the data that intersect with
# the sites (but not actually performing the spatial intersection)
substrate <- substrate[apply(int_indices, 1, any), ]

rm(int_indices) # clean up

# Combine the attributes and substrate for this reduced set:
att_sub <- att_sub %>% 
  filter(NS_PolyID %in% substrate$NS_PolyID) %>% 
  left_join(substrate) %>% 
  st_as_sf()



## c. Filter sites that intersect with remaining substrate data ----
# Identify which polygons from substrate intersect the sites dataframe
int_indices <- st_intersects(sites, att_sub, sparse = FALSE)

# Filter the sites dataframe to retain only intersecting polygons
sites <- sites[apply(int_indices, 1, any), ]

rm(int_indices)

## Save  ----
#saveRDS(att_sub, file.path(com.dir, "temp_substrate.Rds"))
#saveRDS(sites, file.path(com.dir, "temp_sites.Rds"))

## Start ----
att_sub <- readRDS(file.path(com.dir, "temp_substrate.Rds"))
sites <- readRDS(file.path(com.dir, "temp_sites.Rds")) %>% mutate(id = row_number())

## d. Loop through sites to find intersection where possible ----

# Function to process intersections and handle errors
process_intersections <- function(sites, att_sub) {
  
  # Initialize lists to store results and errors
  valid_intersections <- list()
  error_log <- data.frame()
  
  # Loop through each site
  for (i in seq_len(nrow(sites))) {
    site <- sites[i, ]
    
    # Try to find the intersection
    tryCatch({
      intersection <- st_intersection(att_sub, site)
      
      # Check for invalid geometries
      if (all(st_is_valid(intersection))) {
        valid_intersections[[i]] <- intersection
      } else {
        error_log <- rbind(error_log, data.frame(site_id = site$id, error = "Invalid geometry"))
      }
    }, error = function(e) {
      # Capture error details
      error_log <- rbind(error_log, data.frame(site_id = site$id, error = e$message))
    })
  }
  
  # Combine all valid intersections into a single dataframe
  valid_intersections_df <- do.call(rbind, valid_intersections)
  
  return(list(valid_intersections = valid_intersections_df, errors = error_log))
}

### Apply the function to the sites ----
result <- process_intersections(sites, att_sub)

### Extract the valid intersections ----
valid_intersections <- result$valid_intersections

### Extract the errors ----
error_log <- result$errors
print(error_log)

### Save outputs ----
#saveRDS(valid_intersections, file.path(com.dir, "temp_valid_intersections.Rds"))
#saveRDS(result, file.path(com.dir, "temp_results.Rds"))

result <- readRDS(file.path(com.dir, "temp_results.Rds"))
valid_intersections <- readRDS(file.path(com.dir, "temp_valid_intersections.Rds"))

## e. Verify function worked ---- 

# Confirm that these match the full intersections for each of the sites by
# comparing the result from intersecting the first five valid sites:
head_sites <- sites %>% 
  filter(id %in% valid_intersections$id) %>% 
  head(5)

head_int <- st_intersection(att_sub, head_sites)
head_fxn <- valid_intersections %>% filter(id %in% head_int$id)

all.equal(head_fxn, head_int) # only rownames differ; this ok if geometries match
all.equal(head_fxn$geometry, head_int$geometry) #geometries match

# Try again with the five last sites, just to be sure:
tail_sites <- sites %>% 
  filter(id %in% valid_intersections$id) %>% 
  tail(5)

tail_int <- st_intersection(att_sub, tail_sites)
tail_fxn <- valid_intersections %>% filter(id %in% tail_int$id)

all.equal(tail_fxn, tail_int) # only rownames differ; this ok if geometries match
all.equal(tail_fxn$geometry, tail_int$geometry) #geometries match

## f. Add to exported df (from #1) ----
# Since these sites have been properly intersected, go ahead and add them to
# the partial intersected dataframe from above (#1):
partial_df <- readRDS(file.path(sub.dir, "substrate_mlpa_sites_1000m_partial.Rds"))

valid_to_add <- valid_intersections %>% 
  select(NS_PolyID, habitat:geometry)

partial_df <- bind_rows(partial_df, valid_to_add)

saveRDS(partial_df, file.path(sub.dir, "substrate_mlpa_sites_1000m_partial_step2.Rds"))

## g. Pull remaining problem areas ----

missing_sites <- sites %>% # recall this sites is already reduced from #1
  filter(!(id %in% valid_intersections$id)) #317 remaining

# Identify which polygons from substrate intersect those remaining sites
int_indices <- st_intersects(att_sub, missing_sites, sparse = FALSE)

substrate <- att_sub[apply(int_indices, 1, any), ]

## Save ----
#saveRDS(substrate, file.path(com.dir, "temp_substrate2.Rds"))
#saveRDS(missing_sites, file.path(com.dir, "temp_sites2.Rds"))

# 3. Examine remaining problem areas -----

## a. Load remaining substrate and sites ----
substrate <- readRDS(file.path(com.dir, "temp_substrate2.Rds"))
missing_sites <- readRDS(file.path(com.dir, "temp_sites2.Rds"))

# This takes a few minutes:
#is_valid <- as.data.frame(st_is_valid(substrate, reason = T))

## b. Convert missing sites to squares and merge together ----
# Apply the function to each geometry in the sites dataframe and retain CRS
missing_squares <- missing_sites %>%
  st_as_sf() %>%
  mutate(geometry = do.call(c, lapply(st_geometry(.), polygon_to_square))) %>%
  st_set_crs(crs_info)

# Convert the list of squares back to sf object and set CRS
missing_squares <- st_sf(geometry = st_sfc(missing_squares$geometry), crs = crs_info)

# Merge all squares into a single polygon
missing_merged <- st_union(missing_squares) %>%
  st_set_crs(crs_info)

## c. Plot the merged missing sites ----

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")
state_waters_poly <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/processed/CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/gis_data/processed/CA_state_waters_polyline.Rds"))

# Plot 
ggplot() +
  geom_sf(data = missing_merged) +
  geom_sf(data=state_waters_line, color="grey40", lwd=0.1) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data = missing_merged) +
  coord_sf(xlim = c(729172.2, 1041084.8), ylim = c(3623745.4,  3819350.6))


# Other things I tried that didn't work:
# Using "magic dust" zero buffer (crashes when only doing with one obs.)
# ---> st_buffer(test_one_obs, 0)

## d. Cast to polygon to see if can eliminate offending areas ----
# Create a function that will do this:
# 1. Start with the geometries in substrate that are invalid (each is a large multipolygon)
# 2. Loop through each observation in substrate,
# 3. Print the non-geometry attributes
# 3. Cast the multipolygon to polygon, call this new casted data sub_poly
# 4. Extract the invalid geometries from sub_poly: sub_poly_invalid <- sub_poly[!st_is_valid(sub_poly), ]
# 5. Get the intersections: invalid_intersects <- st_intersects(sub_poly_invalid, missing_sites) 
# 6. If sum(invalid_intersects) > 0, store the original attributes from the observation in a dataframe
# 7. If sum(invalid_intersects) = 0, extract the valid geometries using: sub_poly_valid <- sub_poly[st_is_valid(sub_poly), ] and then sub_poly_merge <- st_union(sub_poly_valid)
# 8. Store the resulting sub_poly_merge into a dataframe called substrate_valid

# Define the function
process_invalid_geometries <- function(substrate, missing_sites) {
  # DataFrame to store results
  results_df <- data.frame()
  substrate_valid <- list()
  
  # Loop through each observation in substrate
  for (i in seq_len(nrow(substrate))) {
    obs <- substrate[i, ]
    print(st_drop_geometry(obs))
    
    # Cast the multipolygon to polygon
    sub_poly <- st_cast(obs, "POLYGON")
    print("Cast to polygon complete")
    
    # Extract invalid geometries
    invalid_geometries <- !st_is_valid(sub_poly)  # Added logical indexing
    sub_poly_invalid <- sub_poly[invalid_geometries, ]  # Used logical indexing
    print("Invalid geometries extracted")
    
    # Get the intersections
    invalid_intersects <- st_intersects(sub_poly_invalid, missing_sites, sparse = FALSE)
    print("Intersections complete")
    
    # Check if there are any intersections
    if (any(invalid_intersects)) {  # Use any instead of sum
      # Store the original attributes in a dataframe
      results_df <- rbind(results_df, st_drop_geometry(obs))
      print("Intersection invalid")
    } else {
      # Extract valid geometries and merge them
      sub_poly_valid <- sub_poly[!invalid_geometries, ]  
      sub_poly_merge <- st_union(sub_poly_valid)
      print("Intersection is valid - merge is complete")
      
      # Create an sf object with the original attributes and the merged geometry
      valid_obs <- st_sf(st_drop_geometry(obs), geometry = sub_poly_merge)
      
      # Store the resulting valid_obs into a list
      substrate_valid <- append(substrate_valid, list(valid_obs))
    }
  }
  
  # Combine the valid geometries into a single sf object
  if (length(substrate_valid) > 0) {
    substrate_valid <- do.call(rbind, substrate_valid)
  } else {
    substrate_valid <- st_as_sf(NULL)
  }
  
  return(list(results_df = results_df, substrate_valid = substrate_valid))
}


# Subset to only the invalid observations
substrate_invalid <- substrate[!st_is_valid(substrate), ]

# Apply to the invalid substrate dataframe (n = 76)
results <- process_invalid_geometries(substrate = substrate_invalid, 
                                      missing_sites = missing_sites)

# Access the results
results_df <- results$results_df
substrate_valid <- results$substrate_valid

# saveRDS(results_df, file.path(sub.dir, "temp_cast_results.Rds"))
# saveRDS(substrate_valid, file.path(sub.dir, "temp_cast_sub_valid.Rds"))
results_df <- readRDS(file.path(sub.dir, "temp_cast_results.Rds"))
substrate_valid <- readRDS(file.path(sub.dir, "temp_cast_sub_valid.Rds"))

# Test whether substrate_valid truly contains only valid polygons
#test <- as.data.frame(st_is_valid(substrate_valid))

# Update so substrate dataframe only contains the observations that were
# already valid, and the new polygons that dropped the invalid sections because
# they didn't overlap with the sites
substrate_new <- substrate[st_is_valid(substrate), ] %>% 
  bind_rows(substrate_valid)

# Ensure this still has the correct invalid geoms
substrate_invalid <- substrate %>% 
  filter(!(NS_PolyID %in% substrate_new$NS_PolyID))

# Test these intersections
new_intersects <- st_intersects(missing_sites, substrate_new, sparse = F)
invalid_intersects <- st_intersects(missing_sites, substrate_invalid, sparse = F)

# Find the missing_sites that have invalid intersections
has_new <- rowSums(new_intersects) > 0
has_invalid <- rowSums(invalid_intersects) > 0

# Find the missing_sites that intersect with new valid but not invalid
new_sites <- missing_sites[has_new & !has_invalid, ]

# LOL THERE'S ONLY 6 OUT OF THE 317

anacapa <- missing_sites %>% 
  filter(habitat == "Kelp") %>% 
  filter(mpa == "Anacapa Island SMR")

# Find what intersects with anacapa, filter to those:
anacapa_int <- st_intersects(substrate, anacapa, sparse = F)
anacapa_sub <- substrate[apply(anacapa_int, 1, any), ]

anacapa_bbox <- st_bbox(anacapa) 

ggplot() + 
  geom_sf(data = anacapa_sub, aes(fill = CMECS_SC_Category)) +
  geom_sf(data = anacapa, col = "red", fill = NA) +
  coord_sf(xlim = c(anacapa_bbox["xmin"], anacapa_bbox["xmax"]), 
           ylim = c(anacapa_bbox["ymin"], anacapa_bbox["ymax"])) +
  theme_minimal()

anacapa_valid <- st_make_valid(anacapa_sub)

ggplot() + 
  geom_sf(data = anacapa_valid, aes(fill = CMECS_SC_Category)) +
  geom_sf(data = anacapa, col = "red", fill = NA) +
  coord_sf(xlim = c(anacapa_bbox["xmin"], anacapa_bbox["xmax"]), 
           ylim = c(anacapa_bbox["ymin"], anacapa_bbox["ymax"])) +
  theme_minimal()

substrate_simple <- substrate %>% st_drop_geometry()

substrate_valid <- as.data.frame(st_is_valid(substrate, reason = T))







# Old stuff: some potential problems here


# Challenge with invalid geometries - check
substrate_invalid <- !st_is_valid(substrate)

# Extract intersection
substrate_subset <- st_intersection(att, sites)

# Calculate the area of each polygon
substrate_subset$area <- st_area(substrate_subset)

# Save the shapefiles and associated metadata
#saveRDS(substrate_subset, file.path(sub.dir, "substrate_mlpa_sites_1000m.Rds"))


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



