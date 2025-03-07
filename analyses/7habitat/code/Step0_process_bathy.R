# Calculate bathymetry information for each site and buffer
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024

library(tidyverse)
library(sf)
library(terra)

wcds.dir <- "/home/shares/ca-mpa/data/sync-data/wcdsci-bathymetry"
ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
mbes.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/depth_MBES_CA"
caba.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/CA_Bathy_30m"
proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"

# Read Data -----------------------------------------------------------------------

# Read the LTM sites (these are ones incldued in the habitat analyses)
# This is useful only to shorten the processing time and/or look depeer at one of
# the monitoring groups specifically - the full code has been run without issue.
# sites_included <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% distinct(habitat, site, site_type, bioregion, region4, affiliated_mpa)  %>% 
#   bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% distinct(habitat, site, site_name, site_type, bioregion, region4, affiliated_mpa)) %>%
#   bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% distinct(habitat, site, site_type, bioregion, region4, affiliated_mpa)) 

sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", 
                           "site_locations_corrected.Rds")) %>% 
  arrange(site) %>% 
  mutate(site_id = row_number()) 

footprints <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_sites_500m", "site_footprints.Rds")) %>%
  mutate(site_id = row_number()) 


# General Data Prep  -----------------------------------------------------------------------

# List files for 2m bathy layers 
bathy_2m_rasters <- data.frame(region = list.files(mbes.dir, pattern = "2m_bathy\\.tif$", full.names = FALSE)) %>% 
  mutate(region = str_remove(region, "_2m_bathy.tif$")) %>% 
  filter(!region == "bat_scsr_is") # this just duplicates the south one

# Define regions and buffers
regions <- bathy_2m_rasters$region
buffers <- c(25, 50, 100, 250, 500)

# Aggregate the 2m rasters to 30m resolution ------------------------------------------
# For each region, create and save a 30m version of the 2m raster

# prep_rasters_30m <- function(regions, mbes.dir) {
#   walk(regions, function(region_name) {
#     original <- rast(file.path(mbes.dir, paste0(region_name, "_2m_bathy.tif")))
#     agg_30m  <- aggregate(original, fact = 15, fun = mean, na.rm = TRUE)
#     writeRaster(agg_30m, file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")), overwrite=TRUE)
#   })
# }

# prep_rasters_30m(regions, mbes.dir)
# this has been done - can just use the processed rasters below

# Calculate buffers  -------------------------------------------

# Project sites to a linear CRS
sites_proj <- st_transform(sites, crs = 26910)

# Project site footprints to linear CRS
foot_proj <- st_transform(footprints, crs = 26910)

process_buffer <- function(buffer, raster_layer) {
  print(paste("Processing buffer:", buffer, "meters"))
  
   # Buffer sites
  sites_buffer <- st_buffer(sites_proj, dist = buffer)
  
  # Intersect buffers and footprints
  sites_intersect <- st_intersection(sites_buffer, foot_proj) %>% 
    filter(site == site.1) 
  
  # Convert to vector object and reproject
  sites_vect <- vect(sites_intersect)
  sites_vect <- project(sites_vect, crs(raster_layer))
  
  # Extract values within buffer
  extracted <- terra::extract(raster_layer, sites_vect, weights = TRUE, exact = TRUE, df = TRUE)
  depth_col <- names(extracted[2])
  
  # Summarize results
  extracted %>%
    group_by(ID) %>%
    summarize(
      n_total = sum(weight, na.rm = TRUE),
      n_valid = sum(ifelse(is.na(.data[[depth_col]]), 0, weight), na.rm = TRUE),
      n_na    = sum(ifelse(is.na(.data[[depth_col]]), weight, 0), na.rm = TRUE),
      w_mean  = sum(.data[[depth_col]] * weight, na.rm = TRUE) / n_valid,
      w_var   = sum(weight * (.data[[depth_col]] - w_mean)^2, na.rm = TRUE) / n_valid,
      w_sd    = sqrt(w_var),
      .groups = "drop"
    ) %>%
    mutate(
      prop_na = n_na    / n_total,
      buffer  = buffer
    )
}

# 1. Upsampled 30m rasters from CDFW
process_cdfw_rasters <- function(regions) {
  cdfw_df <- map_dfr(regions, function(region_name) {
    
    regional_raster <- rast(file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")))
    regional_df <- map_dfr(buffers, ~process_buffer(.x, regional_raster))
    
    regional_df %>% 
      mutate(prop_na = round(prop_na, 3),
             region = region_name,
             layer = "cdfw_2m_upscaled_to_30m") %>% 
      rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>%
      dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, region, buffer, layer)
  })
  
  # Save the combined dataframe only once
  saveRDS(cdfw_df, file.path(proc.dir, "cdfw_depth_buffers.Rds"))
  return(cdfw_df)
}

# process_cdfw_rasters(regions)

# 2. 30m raster from Kelp People
bathy_30m <- rast(file.path(caba.dir, "depth_30m_all_CA.tif"))

depth_30m <- map_dfr(buffers, ~process_buffer(.x, bathy_30m))

csmp_df <- depth_30m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer) %>% 
  mutate(layer = "anita_30m")

saveRDS(csmp_df, file.path(proc.dir, "csmp_depth_buffers.Rds"))

# 3. Bathymetry raster from WCDSCI
bathy_25m <- rast(file.path(wcds.dir, "raw/WCDSCI_EXPRESS_25m_0_1200mWD.tif"))

crs(bathy_25m) <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

depth_25m <- map_dfr(buffers, ~process_buffer(.x, bathy_25m))

wcdsci_df <- depth_25m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer) %>% 
  mutate(layer = "wcdsci_25m")

saveRDS(wcdsci_df, file.path(proc.dir, "wcdsci_depth_buffers.Rds"))


# Join all three ---- 
wcdsci_df <- readRDS(file.path(proc.dir, "wcdsci_depth_buffers_surf.Rds"))
cdfw_df <- readRDS(file.path(proc.dir, "cdfw_depth_buffers_surf.Rds"))
csmp_df <- readRDS(file.path(proc.dir, "csmp_depth_buffers_surf.Rds"))

depth_all <- full_join(cdfw_df, csmp_df) %>%
  full_join(., wcdsci_df) %>%  
  left_join(sites %>% dplyr::select(habitat, site, site_id) %>% st_drop_geometry(), by = c("ID" = "site_id"))

saveRDS(depth_all, file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers_agg_surf.Rds"))

# Build --------------------------------------------------------------------------------

depth_all <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers_agg.Rds")) %>% 
  left_join(sites %>% dplyr::select(habitat, site, site_id) %>% st_drop_geometry(), by = c("ID" = "site_id"))

depth <- depth_all %>%
  arrange(ID, buffer, prop_na, -n_total) %>%  # Arrange by ID, buffer, resolution (factor), and prop_na (ascending)
  group_by(ID, buffer) %>%                      # Group by ID and buffer
  slice(1) %>%                                  # Select the first row per group (best resolution and lowest prop_na)
  ungroup() %>% 
  mutate(depth_cv = (depth_sd/depth_mean)*100) %>% 
  mutate(depth_cv = if_else(is.na(depth_cv), 0, depth_cv))

depth2 <- depth %>%           
  dplyr::select(ID, depth_mean, depth_sd, depth_cv, buffer) %>% 
  pivot_longer(cols = c(depth_mean, depth_sd, depth_cv), names_to = "habitat", values_to = "value_m") %>% 
  mutate(habitat_buffer = paste0(habitat, "_", buffer)) %>% 
  dplyr::select(ID, habitat_buffer, value_m) %>% 
  pivot_wider(names_from = "habitat_buffer", values_from = "value_m") %>% 
  left_join(sites, by = c("ID" = "site_id")) %>% 
  dplyr::select(habitat, site, site_type, everything(), -ID, -geometry) 

saveRDS(depth2, file.path(ltm.dir, "site_depth_agg.Rds"))

# Examine the extremely shallow sites to see if there is a logistical issue ---------------

# shallow_sites <- sites_proj %>% 
#   filter(habitat == "Surf zone" | site %in% depth2$site[depth2$depth_mean_25 > -2]) %>% 
#   arrange(habitat)

#shallow_sites <- head(shallow_sites, 6)

# Create a list to store plots and a dataframe to store corrected points
# plots <- list()
# corrected_points_list <- list()
# my_site <- shallow_sites$site[1]
# raster_layer <- bathy_30m
# 
# 
# library(tmap)
# tmap_mode("plot")
# tmap_options(component.autoscale = F)
# 
# # Process each site
# for (my_site in unique(shallow_sites$site)) {
#   
#   # Filter for the current site
#   site_point <- sites %>% filter(site == !!my_site)
#   site_foot  <- foot_proj %>% filter(site == !!my_site)
#   
#   # Calculate buffer to find raster in proxmity of the site
#   site_500 <- st_buffer(site_point, dist = 500)
#   site_25 <- st_buffer(site_point, dist = 25)
#   
#   # Convert to vector object and reproject to raster CRS
#   sites_vect <- vect(site_500)
#   sites_vect <- project(sites_vect, crs(raster_layer))
#   
#   # Crop raster to the site
#   raster_site <- crop(raster_layer, sites_vect)
#   
#   # Determine whether to show the legend (only on the first plot)
#   #legend_setting <- if (my_site == "Ano Nuevo MPA") tm_legend(title = "", position = tm_pos_in("left", "top")) else tm_legend_hide()
#   
#   # Plot  
#   plot <- 
#     tm_shape(raster_site) +
#     tm_raster(col.scale = tm_scale_intervals(breaks = seq(-40, 0, by = 1)),
#               col.legend =  tm_legend_hide())+ 
#     tm_shape(site_foot) +
#       tm_borders(col = "green") +
#     tm_shape(site_25) + 
#       tm_borders(col = "red") + 
#     tm_title(paste(my_site), position = tm_pos_on_top(), frame = F, size = 1) +
#     tm_layout(outer.margins = c(0.001, 0.001, 0.001, 0.001), frame = F) 
#   
#   plot
#   plots[[my_site]] <- plot
# }

#tmap_options(component.autoscale = FALSE)
#tmap_arrange(plots, ncol = 6, outer.margins = 0.0001)
