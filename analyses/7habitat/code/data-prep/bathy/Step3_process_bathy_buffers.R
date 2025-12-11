# Calculate bathymetry information for each site and buffer
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024

library(tidyverse)
library(sf)
library(terra)
library(tmap)

rm(list = ls())
gc()

ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures/site-plots/bathy"

# General Data Prep  -----------------------------------------------------------

# Read the combined bathymetry raster (use CA version)
bathy_30m <- rast(file.path(proc.dir, "combined_30m_bathy_ca.tif"))

# Read the slope and TRI data (calculated from the combined bathy raster)
slope <- rast(file.path(proc.dir, "combined_30m_bathy_ca_slope.tif"))
tri   <- rast(file.path(proc.dir, "combined_30m_bathy_ca_tri.tif"))

# Read the LTM sites
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024",
                           "site_locations_corrected.Rds")) %>%
  arrange(site) %>%
  mutate(site_id = row_number()) # unique site ID for joining later

# Project sites to a linear CRS
sites_proj <- st_transform(sites, crs = crs(bathy_30m))
sites_vect <- vect(sites_proj)


# Define function to calculate buffers  ----------------------------------------

# Create terrain stack
terrain <- c(bathy_30m, slope, tri)
names(terrain) <- c("depth", "slope", "tri")


process_buffer <- function(buffer, raster_set) {
  
  print(paste("Processing buffer:", buffer, "meters"))
  
  # Buffer sites and reproject
  sites_buffer <- buffer(sites_vect, width = buffer)
  sites_buffer <- project(sites_buffer, crs(raster_set))
  
  # Extract values within buffer
  extracted <- terra::extract(raster_set, sites_buffer, weights = TRUE, exact = TRUE, df = TRUE)

  # Summarize results
  extracted %>%
    mutate(w_depth = ifelse(is.na(depth), 0, weight),
           w_slope = ifelse(is.na(slope), 0, weight),
           w_tri   = ifelse(is.na(tri),   0, weight)) %>%
    group_by(ID) %>%
    summarize(depth_mean = sum(depth * w_depth, na.rm = TRUE) / sum(w_depth, na.rm = T),
              depth_sd   = sqrt(sum(w_depth * (depth - depth_mean)^2, na.rm = TRUE) / sum(w_depth, na.rm = T)), 
              depth_cv   = depth_sd / abs(depth_mean), 
              slope_mean = sum(slope * w_slope, na.rm = TRUE) / sum(w_slope, na.rm = TRUE),
              tri_mean   = sum(tri   * w_tri,   na.rm = TRUE) / sum(w_tri,   na.rm = TRUE), .groups = "drop") %>%
    mutate(buffer  = buffer) 
}

# Define buffers
buffers <- c(25, 50, 100, 250, 500)

# Calculate depth variables
depth_30m <- map_dfr(buffers, ~process_buffer(.x, terrain)) # fairly quick

depth_df <- depth_30m %>% 
  left_join(sites %>% st_drop_geometry(), by = c("ID" = "site_id")) %>% 
  mutate(depth_mean = depth_mean * -1)

# Fix greyhound rock reference: buffers at 25 and 50 are both NA because does not
# go that shallow; instead use the 100 buffer for that site (plots below)
grey_mean <- depth_df$depth_mean[depth_df$site == "Greyhound Rock Reference" & depth_df$buffer == 100]
grey_sd  <- depth_df$depth_sd[depth_df$site == "Greyhound Rock Reference" & depth_df$buffer == 100]
grey_cv  <- depth_df$depth_cv[depth_df$site == "Greyhound Rock Reference" & depth_df$buffer == 100]

depth_df <- depth_df %>%
  mutate(depth_mean = ifelse(site == "Greyhound Rock Reference" & buffer %in% c(25, 50) & is.na(depth_mean), grey_mean, depth_mean),
         depth_sd   = ifelse(site == "Greyhound Rock Reference" & buffer %in% c(25, 50) & is.na(depth_sd), grey_sd, depth_sd),
         depth_cv   = ifelse(site == "Greyhound Rock Reference" & buffer %in% c(25, 50) & is.na(depth_cv), grey_cv, depth_cv)) %>% 
  dplyr::select(ID, habitat, site, site_type, depth_mean, depth_sd, depth_cv, slope_mean, tri_mean, buffer) %>% 
  pivot_longer(cols = c(depth_mean, depth_sd, depth_cv, slope_mean, tri_mean), names_to = "variable", values_to = "value_m") %>% 
  mutate(habitat_buffer = paste0(variable, "_", buffer)) %>% 
  dplyr::select(ID, habitat, site, site_type, habitat_buffer, value_m) %>% 
  pivot_wider(names_from = "habitat_buffer", values_from = "value_m") 

saveRDS(depth_df, file.path(ltm.dir, "site_depth_agg.Rds"))
