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
sites_included <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% distinct(habitat, site, site_type, bioregion, region4, affiliated_mpa)  %>% 
  bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% distinct(habitat, site, site_name, site_type, bioregion, region4, affiliated_mpa)) %>%
  bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% distinct(habitat, site, site_type, bioregion, region4, affiliated_mpa)) %>% 
  bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/deep_full.Rds")) %>% distinct(habitat, site, site_type, affiliated_mpa)) %>% 
  arrange(site) %>% 
  mutate(site_id = row_number()) 

sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", 
                           "site_locations_corrected.Rds")) %>% 
  filter(site %in% sites_included$site) %>% 
  arrange(site) %>% 
  mutate(site_id = row_number()) 

footprints <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/substrate/substrate_sites_500m", "site_footprints.Rds")) %>% 
  filter(site %in% sites_included$site) %>% 
  arrange(site) %>% 
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

prep_rasters_30m <- function(regions, mbes.dir) {
  walk(regions, function(region_name) {
    original <- rast(file.path(mbes.dir, paste0(region_name, "_2m_bathy.tif")))
    agg_30m  <- aggregate(original, fact = 15, fun = mean, na.rm = TRUE)
    writeRaster(agg_30m, file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")), overwrite=TRUE)
  })
}

#prep_rasters_30m(regions, mbes.dir)


# Process the upsampled 30m rasters ---------------------------------------------

process_depth <- function(sites, footprints, mbes.dir, regions, buffers) {
  
  # Project once if all data are in the same CRS
  crs_linear <- "EPSG:26910"  # example
  sites_proj <- st_transform(sites, crs_linear)
  foot_proj  <- st_transform(footprints, crs_linear)
  
  # For each region, load the 30m raster, then extract for each buffer
  map_dfr(regions, function(region_name) {
    cat("Processing region:", region_name, "\n")
    
    regional_raster_30 <- rast(file.path(proc.dir, paste0(region_name, "_30m_bathy.tif")))
    
    map_dfr(buffers, function(buffer) {
      cat("  Processing buffer:", buffer, "meters\n")
      
      # Buffer sites
      sites_buffer <- st_buffer(sites_proj, dist = buffer)
      
      # Intersect
      sites_intersect <- st_intersection(sites_buffer, foot_proj) %>%
        filter(site == site.1)
      
      # Convert to SpatVector & reproject
      sites_vect   <- vect(sites_intersect)
      sites_vect <- project(sites_vect, crs(regional_raster_30))
      
      # Extract weighted values
      extracted <- terra::extract(
        regional_raster_30, 
        sites_vect, 
        df = TRUE,
        weights = TRUE,
        exact = TRUE
      )
      
      depth_col <- names(extracted)[2]
      
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
          prop_na    = n_na    / n_total,
          region     = region_name,
          buffer     = buffer
        )
    })
  })
}

depth_df <- process_depth(sites, footprints, mbes.dir, regions, buffers)
# Skip for deep

# Format the depth data frame
depth_df2 <- depth_df %>%
  filter(!is.na(w_mean)) %>%
  mutate(prop_na = round(prop_na, 3)) %>%
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>%
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, region, buffer)

# Save to disk
saveRDS(depth_df2, file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers_aggregated_2mto30m.Rds"))


# Calculate buffers with the 30m layer  -------------------------------------------

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


# Read the 30m raster from Kelp People
bathy_30m <- rast(file.path(caba.dir, "depth_30m_all_CA.tif"))

# Read the bathymetry raster from WCDSCI
bathy_25m <- rast(file.path(wcds.dir, "raw/WCDSCI_EXPRESS_25m_0_1200mWD.tif"))

crs(bathy_25m) <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# Apply over buffers
depth_30m <- map_dfr(buffers, ~process_buffer(.x, bathy_30m))
depth_25m <- map_dfr(buffers, ~process_buffer(.x, bathy_25m))

depth_30m_df <- depth_30m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer)

depth_25m_df <- depth_25m %>% 
  filter(!is.na(w_mean)) %>% 
  mutate(prop_na = round(prop_na, 3)) %>% 
  rename(depth_mean = w_mean, depth_var = w_var, depth_sd = w_sd) %>% 
  dplyr::select(ID, depth_mean, depth_var, depth_sd, n_total, n_na, prop_na, buffer)

# Join
depth_all <- full_join(depth_df2, depth_25m_df) %>%
  full_join(., depth_30m_df)


saveRDS(depth_all, file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers_agg.Rds"))

# Build --------------------------------------------------------------------------------
rm(list = setdiff(ls(), c("sites_included", "ltm.dir")))
gc()

depth_all <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers_agg.Rds")) 

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
  left_join(sites_included, by = c("ID" = "site_id")) %>% 
  dplyr::select(habitat:affiliated_mpa, everything(), -ID)

saveRDS(depth2, file.path(ltm.dir, "site_depth_agg.Rds"))
