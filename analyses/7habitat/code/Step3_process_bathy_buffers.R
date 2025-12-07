# Calculate bathymetry information for each site and buffer
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024

library(tidyverse)
library(sf)
library(terra)

rm(list = ls())
gc()

wcds.dir <- "/home/shares/ca-mpa/data/sync-data/wcdsci-bathymetry"
ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
mbes.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/depth_MBES_CA"
caba.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/raw/CA_Bathy_30m"
proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"

# General Data Prep  -----------------------------------------------------------

# Read the LTM sites
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024",
                           "site_locations_corrected.Rds")) %>%
  arrange(site) %>%
  # Give them a unique site ID
  mutate(site_id = row_number())


# Project sites to a linear CRS
sites_proj <- st_transform(sites, crs = 26910)
sites_vect <- vect(sites_proj)

# Define buffers
buffers <- c(25, 50, 100, 250, 500)

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
      w_mean  = sum(.data[[depth_col]] * weight, na.rm = TRUE) / n_valid,
      w_var   = sum(weight * (.data[[depth_col]] - w_mean)^2, na.rm = TRUE) / n_valid,
      w_sd    = sqrt(w_var),
      .groups = "drop") %>%
    mutate(prop_na = n_na    / n_total,
           buffer  = buffer) %>% 
    filter(!is.na(w_mean)) %>% 
    filter(prop_na < 1)
}


# Calculate buffers for each data set  -----------------------------------------

# 30m raster from California Sea Floor Mapping Program (AGO et al.)
bathy_30m <- rast(file.path(proc.dir, "csmp_30m_bathy.tif"))
depth_30m <- map_dfr(buffers, ~process_buffer(.x, bathy_30m))

csmp_df <- depth_30m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer) %>% 
  mutate(layer = "csmp_30m") %>% 
  left_join(sites, by = c("ID" = "site_id"))

saveRDS(csmp_df, file.path(proc.dir, "csmp_depth_buffers.Rds"))

# 3. Bathymetry raster from WCDSCI
bathy_25m <- rast(file.path(proc.dir, "wcdsci_25m_bathy.tif"))
depth_25m <- map_dfr(buffers, ~process_buffer(.x, bathy_25m))

wcdsci_df <- depth_25m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer) %>% 
  mutate(layer = "wcdsci_25m") %>% 
  left_join(sites, by = c("ID" = "site_id"))

saveRDS(wcdsci_df, file.path(proc.dir, "wcdsci_depth_buffers.Rds"))

# Upsampled 30m rasters from CDFW
# process_cdfw_rasters <- function(regions) {
#   cdfw_df <- map_dfr(regions, function(region_name) {
#     
#     regional_raster <- rast(file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")))
#     regional_df <- map_dfr(buffers, ~process_buffer(.x, regional_raster))
#     
#     regional_df %>% 
#       filter(!is.na(w_mean)) %>% 
#       mutate(prop_na = round(prop_na, 3),
#              region = region_name,
#              layer = "cdfw_2m_upscaled_to_30m") %>% 
#       rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>%
#       dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, region, buffer, layer)
#   })
#   
#   # Save the combined dataframe only once
#   saveRDS(cdfw_df, file.path(proc.dir, "cdfw_depth_buffers.Rds"))
#   return(cdfw_df)
# }
# 
# process_cdfw_rasters(regions)


# Join  ---- 
wcdsci_df <- readRDS(file.path(proc.dir, "wcdsci_depth_buffers.Rds"))
#cdfw_df <- readRDS(file.path(proc.dir, "cdfw_depth_buffers.Rds"))
csmp_df <- readRDS(file.path(proc.dir, "csmp_depth_buffers.Rds"))

depth_all <- full_join(wcdsci_df, csmp_df)


# Inspect 
coverage <- depth_all %>% 
  st_drop_geometry() %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  group_by(habitat, ID, site, site_type, buffer) %>% 
  summarize(n = n(),
            layers = paste(unique(layer), collapse = ", "),
            prop_na = list(prop_na))
  
depth <- depth_all %>% st_drop_geometry() %>% 
  filter(!prop_na == 1) %>% 
  # Anita's interpolated data seems best for shallow areas, so start with that
  mutate(layer = factor(layer, levels = c("csmp_30m", "wcdsci_25m"))) %>% 
  # Confirm for kelp forest we want Anita's data for all sites at 50m 
  filter(!(habitat == "Kelp forest" & buffer == 50 & layer != "csmp_30m")) %>% 
  filter(!(str_detect(site, "^SCAI") & layer != "csmp_30m")) %>% 
  arrange(ID, habitat, site, site_type, buffer, prop_na, layer) %>%  # Arrange by ID, buffer, resolution (factor), and prop_na (ascending)
  group_by(ID, habitat, site, site_type, buffer) %>%                      # Group by ID and buffer
  slice(1) %>%                                  # Select the first row per group (best resolution and lowest prop_na)
  ungroup() %>% 
  mutate(depth_cv = (depth_sd/depth_mean)*100) 

depth2 <- depth %>%           
  dplyr::select(ID, habitat, site, site_type, depth_mean, depth_sd, depth_cv, buffer) %>% 
  pivot_longer(cols = c(depth_mean, depth_sd, depth_cv), names_to = "variable", values_to = "value_m") %>% 
  mutate(habitat_buffer = paste0(variable, "_", buffer)) %>% 
  dplyr::select(ID, habitat, site, site_type, habitat_buffer, value_m) %>% 
  pivot_wider(names_from = "habitat_buffer", values_from = "value_m") 

saveRDS(depth2, file.path(ltm.dir, "site_depth_agg.Rds"))

# Examine sites for issues ---------------

shallow_sites <- sites_proj %>%
  filter(str_detect(site, "^SCAI"))

shallow_sites <- head(shallow_sites, 6)

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
  site_foot  <- foot_proj %>% filter(site == !!my_site)

  # Calculate buffer to find raster in proxmity of the site
  site_500 <- st_buffer(site_point, dist = 500)
  site_25 <- st_buffer(site_point, dist = 25)

  # Convert to vector object and reproject to raster CRS
  sites_vect <- vect(site_500)
  sites_vect <- project(sites_vect, crs(raster_layer))

  # Crop raster to the site
  raster_site <- crop(raster_layer, sites_vect)

  # Determine whether to show the legend (only on the first plot)
  #legend_setting <- if (my_site == "Ano Nuevo MPA") tm_legend(title = "", position = tm_pos_in("left", "top")) else tm_legend_hide()

  # Plot
  plot <-
    tm_shape(raster_site) +
    tm_raster(col.scale = tm_scale_intervals(breaks = seq(-40, 0, by = 1)),
              col.legend =  tm_legend_hide())+
    tm_shape(site_foot) +
      tm_borders(col = "green") +
    tm_shape(site_25) +
      tm_borders(col = "red") +
    tm_title(paste(my_site), position = tm_pos_on_top(), frame = F, size = 1) +
    tm_layout(outer.margins = c(0.001, 0.001, 0.001, 0.001), frame = F)

  plot
  plots[[my_site]] <- plot
}

tmap_arrange(plots, ncol = 6, outer.margins = 0.0001)
