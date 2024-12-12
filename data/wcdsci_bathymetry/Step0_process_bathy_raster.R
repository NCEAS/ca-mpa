# Step 0. Intersect Bathymetry Rasters with Sites
# Cori Lopazanski lopazanski@bren.ucsb.edu
# Dec 2024

# There are several available bathymetry rasters:
# 1. Bathy 30m Resolution
#    Provided to CL by AG in Dec 2024. Considered "best" quality.
#     From AG: 2m resolution is very raw, affected by white zones that could not 
#     be mapped (shallow, rough seas, heavy kelp). This layer was processed by 
#     people in the M. Carr and P. Raimondi labs. Aggregated to 30m resolution 
#     and used a linear interpolation on nearshore data.
# 2. Bathy 25m Resolution
#    Provided to CL by ES and CC (NOAA) in Oct 2023. Second "best" quality.
#     Continuous bathymetry data produced by the West Coast Deep Sea Coral Initiative
#     This layer likely contains deeper areas than the 30m layer above. Metadata
#     provided and data is now likely publicly available (somewhere).
# 3. Bathy 2m Resolution
#     The raw 2m data that AG is referencing above, provided in regional tifs.
#     Data is good where it exists but not all areas are mapped. 


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

sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds") %>% 
  filter(!habitat == "Rocky intertidal") %>% 
  filter(site %in% sites_included$site) %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"), crs = 4326) %>% 
  mutate(site_id = row_number())

sites_proj <- st_transform(sites, crs = 26910)
sites_buffer <- st_buffer(sites_proj, 25)
sites_vect <- vect(sites_buffer)


# Read the 30m raster from Kelp People
bathy_30m <- rast(file.path(caba.dir, "depth_30m_all_CA.tif"))

# Read the bathymetry raster from WCDSCI
bathy_25m    <- rast(file.path(wcds.dir, "raw/WCDSCI_EXPRESS_25m_0_1200mWD.tif"))
crs(bathy_25m) <- "+proj=omerc +lat_0=39 +lonc=-125 +alpha=75 +k=0.9996 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"


# Check overlap with 30m raster ---------------------------------------------------------------
# Project sites to the 30m raster CRS
sites_proj_30m <- project(sites_vect, crs(bathy_30m))

# Extract values for each site 
overlap_30m <- extract(bathy_30m, sites_proj_30m, df = T) %>% 
  group_by(ID) %>% 
  summarize(n_na = sum(is.na(depth_mean_nc.all_wInterp_30m)),
            total = n(),
            depth_mean = mean(depth_mean_nc.all_wInterp_30m, na.rm = T), .groups = 'drop') %>% 
  mutate(raster = "30m") %>% 
  filter(!(n_na == total))


# Check overlap with 25m raster ---------------------------------------------------------------
# Project sites to the 25m raster CRS
sites_proj_25m <- project(sites_vect, crs(bathy_25m))

# Extract raster values for each site
overlap_25m <- terra::extract(bathy_25m, sites_proj_25m, df = TRUE) %>% 
  group_by(ID) %>%  # Group by site ID
  summarize(n_na = sum(is.na(WCDSCI_EXPRESS_25m_0_1200mWD)),
            total = n(),
            depth_mean = mean(WCDSCI_EXPRESS_25m_0_1200mWD, na.rm = T), .groups = 'drop') %>% 
  mutate(raster = "25m") %>% 
  filter(!(n_na == total))

# Check overlap with 2m rasters ---------------------------------------------------------------

# List files for 2m bathy layers and create a dataframe
bathy_2m_rasters <- data.frame(region = list.files(mbes.dir, pattern = "2m_bathy\\.tif$", full.names = FALSE)) %>% 
  mutate(region = str_remove(region, "_2m_bathy.tif$")) 

# Loop through 2m bathy layers and find sites with data
overlap <- lapply(bathy_2m_rasters$region, function(region_name) {
  # Load raster and get extent
  regional_raster <- rast(file.path(mbes.dir, paste0(region_name, "_2m_bathy.tif")))
  regional_extent <- as.polygons(ext(regional_raster))
  
  # Reproject in case of different CRS
  sites_reproj <- project(sites_vect, crs(regional_raster))
  
  # Check which sites overlap
  overlaps <- terra::extract(regional_raster, sites_reproj, df = TRUE) 
  depth_col <- names(overlaps[2])
  
  overlaps_df <- overlaps %>% 
    group_by(ID) %>%
    summarize(n_na = sum(is.na(.data[[depth_col]])),
              total = n(),
              mean_depth = mean(.data[[depth_col]], na.rm = TRUE), .groups = "drop") %>% 
    filter(!(n_na == 1 & total == 1)) %>% 
    mutate(region = region_name)
  
  return(overlaps_df)
}) 

# Combine results into a single dataframe
overlap_2m <- bind_rows(overlap) %>% 
  filter(!(n_na == total)) %>% 
  rename(depth_mean = mean_depth) %>% 
  mutate(raster = "2m") 

# Combine results ------------------------------------------------------------------------------
overlap_results <- bind_rows(overlap_30m, overlap_25m) %>% 
  pivot_wider(id_cols = ID, names_from = raster, values_from = c(n_na, total, depth_mean))

rm(list = setdiff(ls(), c("overlap_results", "overlap_30m", "overlap_2m", "overlap_25m", "sites")))

overlap_2m_means <- overlap_2m %>% 
  group_by(ID) %>% 
  summarize(depth_mean_2m = mean(depth_mean, na.rm = T),
            n_na_2m = mean(n_na, na.rm = T),
            total_2m = mean(total, na.rm = T),
            regions = paste(region, collapse = ",")) %>% 
  filter(n_na_2m < 10)
  
mean_comparison <- overlap_results %>% 
  full_join(overlap_2m_means) %>% 
  mutate(raster = case_when(!is.na(depth_mean_2m) ~ "2m", 
                            is.na(depth_mean_2m) & !is.na(depth_mean_30m) ~ "30m",
                            is.na(depth_mean_2m) & is.na(depth_mean_30m) & !is.na(depth_mean_25m) ~ "25m"
                            T~NA))

# Calculate average depth and standard deviation in depth for each site and buffer



depth_raster <- rast(path)


# Loop through 2m bathy layers and find sites with data
overlap <- lapply(bathy_2m_rasters$region, function(region_name) {
  # Load raster and get extent
  regional_raster <- rast(file.path(mbes.dir, paste0(region_name, "_2m_bathy.tif")))
  regional_extent <- as.polygons(ext(regional_raster))
  
  # Reproject in case of different CRS
  sites_reproj <- project(sites_vect, crs(regional_raster))
  
  # Check which sites overlap
  overlaps <- terra::extract(regional_raster, sites_reproj, df = TRUE) 
  depth_col <- names(overlaps[2])
  
  overlaps_df <- overlaps %>% 
    group_by(ID) %>%
    summarize(n_na = sum(is.na(.data[[depth_col]])),
              total = n(),
              mean_depth = mean(.data[[depth_col]], na.rm = TRUE), .groups = "drop") %>% 
    filter(!(n_na == 1 & total == 1)) %>% 
    mutate(region = region_name)
  
  return(overlaps_df)
}) 

















