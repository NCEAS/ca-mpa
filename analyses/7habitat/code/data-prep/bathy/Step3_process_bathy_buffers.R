# Calculate bathymetry information for each site and buffer
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024

library(tidyverse)
library(sf)
library(terra)

rm(list = ls())
gc()

ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"

# General Data Prep  -----------------------------------------------------------

# Read the combined bathymetry raster
bathy_30m <- rast(file.path(proc.dir, "combined_30m_bathy.tif"))

# Read the LTM sites
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024",
                           "site_locations_corrected.Rds")) %>%
  arrange(site) %>%
  mutate(site_id = row_number()) # unique site ID for joining later


# Project sites to a linear CRS
sites_proj <- st_transform(sites, crs = crs(bathy_30m))
sites_vect <- vect(sites_proj)

# crs(sites_vect) == crs(bathy_30m) # true

# Define function to calculate buffers  ----------------------------------------

process_buffer <- function(buffer, raster_layer) {
  
  print(paste("Processing buffer:", buffer, "meters"))
  
  # Buffer sites and reproject
  sites_buffer <- buffer(sites_vect, width = buffer)
  sites_buffer <- project(sites_buffer, crs(raster_layer))
  
  # Extract values within buffer
  extracted <- terra::extract(raster_layer, sites_buffer, weights = TRUE, exact = TRUE, df = TRUE)
  depth_col <- names(extracted[2])
  
  # Summarize results
  extracted %>%
    group_by(ID) %>%
    summarize(
      n_total = sum(weight, na.rm = TRUE),
      n_valid = sum(ifelse(is.na(.data[[depth_col]]), 0, weight), na.rm = TRUE),
      n_na    = sum(ifelse(is.na(.data[[depth_col]]), weight, 0), na.rm = TRUE),
      depth_mean  = sum(.data[[depth_col]] * weight, na.rm = TRUE) / n_valid,
      depth_var   = sum(weight * (.data[[depth_col]] - depth_mean)^2, na.rm = TRUE) / n_valid,
      depth_sd    = sqrt(depth_var),
      .groups = "drop") %>%
    mutate(buffer  = buffer) 
}

# Define buffers
buffers <- c(25, 50, 100, 250, 500)

# Calculate depth variables
depth_30m <- map_dfr(buffers, ~process_buffer(.x, bathy_30m))

depth_df <- depth_30m %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, buffer) %>% 
  left_join(sites, by = c("ID" = "site_id")) %>% 
  st_drop_geometry()


# Fix greyhound rock reference: buffers at 25 and 50 are both NA because does not
# go that shallow; instead use the 100 buffer for that site

grey_mean <- depth_df$depth_mean[depth_df$site == "Greyhound Rock Reference" & depth_df$buffer == 100]
grey_var  <- depth_df$depth_var[depth_df$site == "Greyhound Rock Reference" & depth_df$buffer == 100]
grey_sd   <- depth_df$depth_sd[depth_df$site == "Greyhound Rock Reference" & depth_df$buffer == 100]

depth_df <- depth_df %>%
  mutate(depth_mean = ifelse(site == "Greyhound Rock Reference" & buffer %in% c(25, 50) & is.na(depth_mean), grey_mean, depth_mean),
         depth_var  = ifelse(site == "Greyhound Rock Reference" & buffer %in% c(25, 50) & is.na(depth_var), grey_var, depth_var),
         depth_sd   = ifelse(site == "Greyhound Rock Reference" & buffer %in% c(25, 50) & is.na(depth_sd),grey_sd, depth_sd)) %>% 
  mutate(depth_cv = (depth_sd/abs(depth_mean))*100)  %>%           
  dplyr::select(ID, habitat, site, site_type, depth_mean, depth_sd, depth_cv, buffer) %>% 
  pivot_longer(cols = c(depth_mean, depth_sd, depth_cv), names_to = "variable", values_to = "value_m") %>% 
  mutate(habitat_buffer = paste0(variable, "_", buffer)) %>% 
  dplyr::select(ID, habitat, site, site_type, habitat_buffer, value_m) %>% 
  pivot_wider(names_from = "habitat_buffer", values_from = "value_m") 

saveRDS(depth_df, file.path(ltm.dir, "site_depth_agg.Rds"))


# Examine sites for issues ---------------

shallow_sites <- sites_proj %>%
  filter(str_detect(site, "^Grey"))

# Create a list to store plots and a dataframe to store corrected points
plots <- list()
corrected_points_list <- list()
my_site <- shallow_sites$site[1]
raster_layer <- bathy_30m


library(tmap)
tmap_mode("plot")
tmap_options(component.autoscale = F)

# Process each site
for (my_site in unique(shallow_sites$site)) {

  # Filter for the current site
  site_point <- sites %>% filter(site == !!my_site)

  # Calculate buffer to find raster in proxmity of the site
  site_500 <- st_buffer(site_point, dist = 500)
  site_25 <- st_buffer(site_point, dist = 25)

  # Convert to vector object and reproject to raster CRS
  sites_vect <- vect(site_500)
  sites_vect <- project(sites_vect, crs(raster_layer))

  # Crop raster to the site
  raster_site <- crop(raster_layer, sites_vect)

    # Plot
  plot <-
    tm_shape(raster_site) +
    tm_raster(col.scale = tm_scale_intervals(breaks = seq(-40, 0, by = 1)),
              col.legend =  tm_legend_hide())+
    tm_shape(site_25) +
      tm_borders(col = "red") +
    tm_shape(site_500) +
    tm_borders(col = "red") +
    tm_title(paste(my_site), position = tm_pos_on_top(), frame = F, size = 1) +
    tm_layout(outer.margins = c(0.001, 0.001, 0.001, 0.001), frame = F)

  plot
  plots[[my_site]] <- plot
}

tmap_arrange(plots, ncol = 6, outer.margins = 0.0001)
