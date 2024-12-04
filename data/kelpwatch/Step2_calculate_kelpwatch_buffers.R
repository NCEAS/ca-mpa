# Get kelp information for each site and year
# Cori Lopazanski
# December 2024

# Setup ------------------------------------------------------------------------
library(sf)
library(terra)

kelp.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2024/processed"


# Read Data --------------------------------------------------------------------
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds") %>% 
  filter(!habitat == "Rocky intertidal") 
  
# Convert to sf object
sites <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326)

# Transform to match kelp data CRS
# sites <- st_transform(sites, crs = 26910)

# Build Data -------------------------------------------------------------------

# Function to calculate kelp area for a single buffer
calculate_kelp_buffer <- function(kelp_raster, sites, buffer, year) {
  # Transform site geometries to match raster CRS
  sites_buffer <- st_buffer(sites, dist = buffer)
  
  # Extract kelp area values within the buffer
  kelp_areas <- terra::extract(kelp_raster, sites_buffer, fun = sum, na.rm = TRUE, weights = TRUE)
  
  # Combine results with site metadata and add year/buffer info
  kelp_results <- sites_buffer %>%
    st_drop_geometry() %>%
    bind_cols(kelp_areas) %>%
    rename(kelp_area_m2 = sum) %>%
    mutate(year = year, buffer = buffer)
  
  # Replace NaN with 0 (no kelp detected)
  kelp_results$kelp_area_m2[is.nan(kelp_results$kelp_area_m2)] <- 0
  
  return(kelp_results)
}

# Main loop for processing
buffers <- c(25, 50, 100, 250, 500)
years <- seq(2003, 2023)
all_kelp_results <- data.frame()

for (year in years) {
  print(paste("Processing year:", year))
  
  kelp_raster <- terra::rast(file.path(kelp.dir, paste0("kelp_canopy_", year, ".tif")))
  sites <- st_transform(sites, crs = terra::crs(kelp_raster))
  
  for (buffer in buffers) {
    print(paste("  Buffer:", buffer, "m"))
    
    kelp_results <- calculate_kelp_buffer(kelp_raster, sites, buffer, year)
    all_kelp_results <- bind_rows(all_kelp_results, kelp_results)
  }
  
  rm(kelp_raster)
  gc()
}

# Format the consolidated results
kelp <- all_kelp_results %>% 
  mutate(habitat_buffer = paste0("kelp_annual_", buffer)) %>% 
  dplyr::select(-ID, -buffer) %>% 
  pivot_wider(names_from = habitat_buffer, values_from = kelp_area_m2)

# Save the consolidated results
saveRDS(kelp, file.path(kelp.dir, "kelp_site_buffers.Rds"))
