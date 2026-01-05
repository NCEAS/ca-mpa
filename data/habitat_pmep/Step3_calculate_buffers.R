# Step 3 Calculate Sequential Buffers
# Cori Lopazanski lopazanski@bren.ucsb.edu
# July 2024


# Setup   ----------------------------------------------------------------------
rm(list = ls())
gc()

library(tidyverse)
library(sf)

com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined"

# Read  ----------------------------------------------------------------------
# Read sites
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", 
                           "site_locations_corrected.Rds"))

# Function to calculate buffers
calculate_buffers <- function(section, buffer) {
  print(paste("Section:", section))
  print(paste("Buffer:", buffer))
  
  habitat <- readRDS(file.path(com.dir, paste0("combined_", section, ".Rds"))) 
  sites_buffer <- st_buffer(sites, dist = buffer)
  sites_buffer <- st_transform(sites_buffer, crs = st_crs(habitat))
  
  intersect <- st_intersection(sites_buffer, habitat)
  print("Intersection complete")
  
  intersect <- intersect %>%
    # Filter out the overlap among different sites and habitat types (don't care about those)
    filter(habitat == habitat.1) %>% 
    filter(site == site.1) %>%
    filter(site_type == site_type.1) %>% 
    dplyr::select(!contains(".1"))
  
  # Transform intersection to the desired CRS
  intersect <- st_transform(intersect, crs = 26910)
  print("Transform complete")
  
  # Save the result with the geometry
  saveRDS(intersect, file.path(com.dir, "buffers", paste0(buffer, "m/habitat_buffers_", section, "_", buffer, "m.Rds"))) 
  
}

# Create grid of all sections and buffers
sections <- c(23, 30, 31, 32, 33, 40, 41)
buffers <- c(25, 50, 100, 250, 500)
section_buffers <- expand.grid(section = sections, buffer = buffers)

# Run function and combine all results
map2_dfr(section_buffers$section, section_buffers$buffer, calculate_buffers)


#  Combine across sections ----------------------------------------------------------------
combine_sections <- function(buffer){
  buffer_files <- list.files(file.path(com.dir, "buffers", paste0(buffer, "m")), full.names = T)
  
  combined_df <- buffer_files %>%
    lapply(readRDS) %>%  
    bind_rows()  %>% 
    mutate(geometry = st_make_valid(geometry)) %>%  # Fix invalid geometries (500m issue)
    group_by(habitat, site, site_type, habitat_class, depth_zone) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') %>% # Merge those that span across sections
    mutate(area_m2 = as.numeric(st_area(geometry))) %>% 
    mutate(buffer = buffer)
  
  saveRDS(combined_df, file.path(com.dir, "buffers", paste0("habitat_buffers_", buffer, "m.Rds")))
}

# First three are quick, 250 and 500 take a minute!
combine_sections(buffer = 25)
combine_sections(buffer = 50)
combine_sections(buffer = 100)
combine_sections(buffer = 250)
combine_sections(buffer = 500)

# Fix the Anacapa kelp forest buffers ----------------------------------------------------------

# Since the island is so narrow, the habitat buffer spans both sides of the island. 
# Where the site is a Reference (on the South side of the island) we will mask the North side
# Where the site is MPA site (on N side) we will mask the south side

# Create polygon that will mask northern anacapa
coords_n <- matrix(c(-119.450075, 34.015529,   # West Anacapa - west tip
                     -119.407631, 34.005924,   # West Anacapa - east tip
                     -119.391323, 34.004394,   # Middle Anacapa - center
                     -119.378620, 34.011616,   # East Anacapa - west tip
                     -119.349009, 34.018623,   # East Anacapa - east tip
                     -119.340315, 34.054571,   # Northeast point 
                     -119.455874, 34.054068,   # Northwest point
                     -119.450075, 34.015529),   # close polygon),
                   ncol = 2,
                   byrow = TRUE)

# Create polygon that will mask southern anacapa
coords_s <- matrix(c(-119.450075, 34.015529,   # West Anacapa - west tip
                     -119.407631, 34.005924,   # West Anacapa - east tip
                     -119.391323, 34.004394,   # Middle Anacapa - center
                     -119.378620, 34.011616,   # East Anacapa - west tip
                     -119.349009, 34.018623,   # East Anacapa - east tip
                     -119.339303, 33.988147,   # Southeast pt
                     -119.453904, 33.990107,   # Southwest pt
                     -119.450075, 34.015529),   # close polygon),
                   ncol = 2,
                   byrow = TRUE) 

poly_n <- st_polygon(list(coords_n)) %>% st_sfc(crs = 4326) %>% st_transform(., crs = 26910) 
poly_s <- st_polygon(list(coords_s)) %>% st_sfc(crs = 4326) %>% st_transform(., crs = 26910)

st_write(poly_s, file.path(com.dir, "anacapa_south_mask.shp"))
st_write(poly_n, file.path(com.dir, "anacapa_north_mask.shp"))

fix_anacapa <- function(buffer) {
  
  df <- readRDS(file.path(com.dir, "buffers", paste0("habitat_buffers_", buffer, "m.Rds")))
  
  # Split into anacapa vs everything else
  anacapa <- df %>% filter(str_detect(site, "ANACAPA"))
  other <- df %>% filter(!str_detect(site, "ANACAPA"))
  
  # Apply geometric differences based on site type
  refs <-  anacapa %>% 
    filter(site_type == "Reference") 
  
  refs_diff <- st_difference(refs, poly_n)

  refs_fixed <- refs_diff %>% 
    group_by(habitat, site, site_type, habitat_class, depth_zone) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') %>% # Merge those that span across sections
    mutate(area_m2 = as.numeric(st_area(geometry))) %>% 
    mutate(buffer = buffer)
  
  mpas <- anacapa %>% 
    filter(site_type == "MPA") 
  
  mpas_diff <- st_difference(mpas, poly_s)
  
  mpas_fixed <- mpas_diff %>%
    group_by(habitat, site, site_type, habitat_class, depth_zone) %>% 
    summarize(geometry = st_union(geometry), .groups = 'drop') %>% # Merge those that span across sections
    mutate(area_m2 = as.numeric(st_area(geometry))) %>% 
    mutate(buffer = buffer)
  
  # Recombine with non-anacapa rows
  df_fixed <- bind_rows(other, refs_fixed, mpas_fixed)
  
  # Save
  saveRDS(df_fixed, file.path(com.dir, "buffers", paste0("habitat_buffers_", buffer, "m.Rds")))
  
}

fix_anacapa(buffer = 25)
fix_anacapa(buffer = 50)
fix_anacapa(buffer = 100)
fix_anacapa(buffer = 250)
fix_anacapa(buffer = 500)


# Read the dfs and combine all without geometries ---------------------------------
buffer_results <- map_df(buffers, function(buffer){
  data <- readRDS(file.path(com.dir, "buffers", paste0("habitat_buffers_", buffer, "m.Rds")))
  data <- data %>% 
    st_drop_geometry() 
})


# There are a few sites and MPAs that span across sections - group by to consolidate these
buffer_df <- buffer_results %>% 
  # Drop landward for all except surf zone - skip this until sure its okay
  #filter(!(!habitat == "Surf zone" & depth_zone == "landward")) %>% 
  mutate(habitat_depth = paste0(snakecase::to_snake_case(habitat_class), "_", depth_zone))

# Save the combined dataframe
saveRDS(buffer_df, file.path(com.dir, "buffers", "habitat_buffers_combined.Rds"))


