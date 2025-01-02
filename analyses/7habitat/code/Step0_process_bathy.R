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

# Read the LTM sites (these are ones incldued in the habitat analyses)
sites_included <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) %>% distinct(site, site_type) %>% 
  bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% distinct(site, site_name, site_type)) %>% 
  bind_rows(., readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% distinct(site, site_type))

sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", 
                           "site_locations_corrected.Rds")) %>% 
  mutate(site_id = row_number())

# Project sites to a linear CRS
sites_proj <- st_transform(sites, crs = 26910)
sites_vect <- vect(sites_proj)

# List files for 2m bathy layers 
bathy_2m_rasters <- data.frame(region = list.files(mbes.dir, pattern = "2m_bathy\\.tif$", full.names = FALSE)) %>% 
  mutate(region = str_remove(region, "_2m_bathy.tif$")) %>% 
  filter(!region == "bat_scsr_is") # this just duplicates the south one

# Define regions and buffers
regions <- bathy_2m_rasters$region
buffers <- c(25, 50, 100, 250, 500)

# Loop through all combinations
depth_df <- do.call(rbind, lapply(regions, function(region_name) {
  print(paste("Processing region:", region_name))
  
  lapply(buffers, function(buffer) {
    print(paste("  Processing buffer:", buffer, "meters"))
    
    # Load raster
    regional_raster <- rast(file.path(mbes.dir, paste0(region_name, "_2m_bathy.tif")))
    
    # Buffer sites
    sites_buffer <- buffer(sites_vect, width = buffer)
    
    # Reproject sites to raster
    sites_proj <- project(sites_buffer, crs(regional_raster))
    
    # Aggregate raster to 30m resolution
    
    # Extract values within buffer
    extracted <- terra::extract(regional_raster, sites_proj, df = TRUE) 
    depth_col <- names(extracted[2])
    
    extracted %>%
      group_by(ID) %>%
      summarize(depth_mean = mean(.data[[depth_col]], na.rm = TRUE),
                depth_sd = sd(.data[[depth_col]], na.rm = TRUE),
                na = sum(is.na(.data[[depth_col]])),
                n = n(),  .groups = "drop") %>%
      filter(!na == n) %>% 
      mutate(region = region_name, 
             buffer = buffer)
  }) %>% bind_rows()
}))

# Format the depth data frame
depth_2m_df <- depth_df %>% 
  filter(!na == n) %>% 
  mutate(prop_na = na/n*100) %>% 
  mutate(resolution = 2)

# Save to disk
saveRDS(depth_2m_df, file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers_2m.Rds"))

# Calculate buffers with the 30m layer  -------------------------------------------

process_buffer <- function(buffer, raster_layer) {
  print(paste("Processing buffer:", buffer, "meters"))
  
  # Buffer sites
  sites_buffer <- buffer(sites_vect, width = buffer)
  
  # Reproject sites to raster
  sites_proj <- project(sites_buffer, crs(raster_layer))
  
  # Extract values within buffer
  extracted <- terra::extract(raster_layer, sites_proj, df = TRUE)
  depth_col <- names(extracted[2])
  
  # Summarize results
  extracted %>%
    group_by(ID) %>%
    summarize(depth_mean = mean(.data[[depth_col]], na.rm = TRUE),
              depth_sd = sd(.data[[depth_col]], na.rm = TRUE),
              na = sum(is.na(.data[[depth_col]])),
              n = n(), .groups = "drop") %>%
    mutate(buffer = buffer)
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
  filter(!na == n) %>% 
  mutate(prop_na = na/n*100) %>% 
  mutate(resolution = 30)

depth_25m_df <- depth_25m %>% 
  filter(!na == n) %>% 
  mutate(prop_na = na/n*100) %>% 
  mutate(resolution = 25)

# Join
depth_all <- full_join(depth_2m_df, depth_25m_df) %>% 
  full_join(., depth_30m_df)

saveRDS(depth_all, file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers.Rds"))


# Build --------------------------------------------------------------------------------

depth_all <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/habitat_anita/processed/depth_buffers.Rds"))
  
depth <- depth_all %>% 
  mutate(resolution = factor(resolution, levels = c("2", "30", "25"))) %>% 
  mutate(depth_sd = if_else(is.na(depth_sd), 0, depth_sd)) %>% 
  arrange(ID, buffer, resolution, prop_na) %>%  # Arrange by ID, buffer, resolution (factor), and prop_na (ascending)
  group_by(ID, buffer) %>%                      # Group by ID and buffer
  slice(1) %>%                                  # Select the first row per group (best resolution and lowest prop_na)
  ungroup()  

ggplot(data = depth) +
  geom_density(aes(x = depth_sd, fill = resolution, color = resolution), alpha = 0.5) +
  scale_x_continuous(limits = c(0, 10)) +
  facet_wrap(~buffer)

ggplot(data = depth) +
  geom_density(aes(x = depth_mean, fill = resolution, color = resolution), alpha = 0.5) +
  facet_wrap(~buffer)

ggplot(data = depth) +
  geom_point(aes(x = depth_mean, y = depth_sd, fill = resolution, color = resolution), alpha = 0.5) +
  facet_wrap(~buffer)

depth2 <- depth %>%           
  dplyr::select(ID, depth_mean, depth_sd, buffer, resolution) %>% 
  pivot_longer(cols = c(depth_mean, depth_sd), names_to = "habitat", values_to = "value_m") %>% 
  mutate(habitat_buffer = paste0(habitat, "_", buffer)) %>% 
  dplyr::select(ID, habitat_buffer, value_m) %>% 
  pivot_wider(names_from = "habitat_buffer", values_from = "value_m")

# Merge with sites
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", 
                           "site_locations_corrected.Rds")) %>% 
  mutate(ID = row_number()) %>% 
  st_drop_geometry()

sites_depth <- sites %>% left_join(depth) %>% dplyr::select(-ID)

saveRDS(sites_depth, file.path(ltm.dir, "site_depth.Rds"))

        