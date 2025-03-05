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
# map2_dfr(section_buffers$section, section_buffers$buffer, calculate_buffers)


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


# Area for each site shows that there are some surf zone sites that only have landward
# -- Likely due to the location GPS point, so will keep in mind for those
site_area <- buffer_df %>% # calculate total area of each site
  group_by(habitat, site, site_type, buffer) %>% 
  summarize(area_site_m2 = sum(area_m2),
            types =  paste(unique(habitat_class), collapse = ", "),
            depths = paste(unique(depth_zone), collapse = ", "), .groups = 'drop')

site_depth_area <- buffer_df %>% # calculate total area for each depth zone
  group_by(habitat, site, site_type, buffer, depth_zone) %>% 
  summarize(area_depth_m2 = sum(area_m2), .groups = 'drop') 


