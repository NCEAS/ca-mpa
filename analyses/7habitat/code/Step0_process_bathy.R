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

# Read Data -----------------------------------------------------------------------

# Read the LTM sites (these are ones in the habitat analyses)
# This is useful only to shorten the processing time - the full code has been run without issue.
# sites_included <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% distinct(habitat, site, site_type, bioregion, region4, affiliated_mpa)  %>%
#   bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% distinct(habitat, site, site_name, site_type, bioregion, region4, affiliated_mpa)) %>%
#   bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% distinct(habitat, site, site_type, bioregion, region4, affiliated_mpa))

sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024",
                           "site_locations_corrected.Rds")) %>%
  filter(!habitat == "Deep reef") %>% 
  arrange(site) %>%
  mutate(site_id = row_number())


# General Data Prep  -----------------------------------------------------------------------

# Get files for 2m CDFW layers 
bathy_2m_rasters <- data.frame(region = list.files(mbes.dir, pattern = "2m_bathy\\.tif$", full.names = FALSE)) %>% 
  mutate(region = str_remove(region, "_2m_bathy.tif$")) %>% 
  filter(!region == "bat_scsr_is") # this just duplicates the south one

# Get files for the 30m CSMP layer
# bathy_30m <- rast(file.path(caba.dir, "depth_30m_all_CA.tif"))

# Get files for the 25m WCDSCI layer
# bathy_25m <- rast(file.path(wcds.dir, "raw/WCDSCI_EXPRESS_25m_0_1200mWD.tif"))
# crs(bathy_25m) <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


# Define regions and buffers
regions <- bathy_2m_rasters$region
buffers <- c(25, 50, 100, 250, 500)

# Prep the rasters by removing >= 0 depth, matching resolutions ------------------------------------------

# For each region, create and save a 30m version of the 2m raster
# FYI Couldn't really get the ifel to run on originals so did it this way. ALAS. 

# prep_rasters_30m <- function(regions, mbes.dir, proc.dir) {
#   walk(regions, function(region_name) {
#     original <- rast(file.path(mbes.dir, paste0(region_name, "_2m_bathy.tif")))
#     agg_30m  <- aggregate(masked, fact = 15, fun = mean, na.rm = TRUE)
#     writeRaster(agg_30m, file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")), overwrite = TRUE)
#     
#     agg_30m <- rast(file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")))
#     masked <- ifel(agg_30m >= 0, NA, agg_30m)
#     writeRaster(masked, file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")), overwrite = TRUE)
#     
#     }
#   )
# }

# prep_rasters_30m(regions, mbes.dir, proc.dir)

# For CSMP and WCDSCI layer just need to remove land
# bathy_30m_masked <- app(bathy_30m, fun = function(x) ifelse(x >= 0, NA, x))    
# writeRaster(bathy_30m_masked, file.path(proc.dir, "csmp_30m_bathy.tif"), overwrite=TRUE)

# bathy_25m_masked <- ifel(bathy_25m >= 0, NA, bathy_25m)
# writeRaster(bathy_25m_masked, file.path(proc.dir, "wcdsci_25m_bathy.tif"), overwrite=TRUE)

# Resample the WCDSCI layer to match the CSMP layer?
# Skip for now.


# Calculate buffers  -------------------------------------------

# Project sites to a linear CRS
sites_proj <- st_transform(sites, crs = 26910)
sites_vect <- vect(sites_proj)

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


# 2. 30m raster from Kelp People
bathy_30m <- rast(file.path(proc.dir, "csmp_30m_bathy.tif"))
#depth_30m <- map_dfr(buffers, ~process_buffer(.x, bathy_30m))

csmp_df <- depth_30m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer) %>% 
  mutate(layer = "anita_30m") %>% 
  left_join(sites, by = c("ID" = "site_id"))

#saveRDS(csmp_df, file.path(proc.dir, "csmp_depth_buffers.Rds"))

# 3. Bathymetry raster from WCDSCI
bathy_25m <- rast(file.path(proc.dir, "wcdsci_25m_bathy.tif"))
#depth_25m <- map_dfr(buffers, ~process_buffer(.x, bathy_25m))

wcdsci_df <- depth_25m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer) %>% 
  mutate(layer = "wcdsci_25m") %>% 
  left_join(sites, by = c("ID" = "site_id"))

#saveRDS(wcdsci_df, file.path(proc.dir, "wcdsci_depth_buffers.Rds"))

# 1. Upsampled 30m rasters from CDFW
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
  mutate(layer = factor(layer, levels = c("anita_30m", "wcdsci_25m"))) %>% 
  # Confirm for kelp forest we want Anita's data for all sites at 50m 
  filter(!(habitat == "Kelp forest" & buffer == 50 & layer != "anita_30m")) %>% 
  filter(!(str_detect(site, "^SCAI") & layer != "anita_30m")) %>% 
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
