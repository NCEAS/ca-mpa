# Get kelp information for each site and year
# Cori Lopazanski
# December 2024

# Setup ------------------------------------------------------------------------
library(sf)
library(terra)
library(tidyverse)
library(parallel)

kelp.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2024/processed"
ltm.dir  <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
kelp2024.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2025/processed"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined" # for anacapa masked polygons

# Read Data --------------------------------------------------------------------
# Read the LTM sites (these are ones incldued in the habitat analyses)
sites <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024", "site_locations_corrected.Rds")) 

# Transform site geometries to match raster CRS 
sites <- st_transform(sites, crs = 26910)

# Separate Anacapa sites
# Convert to vector object to align with terra raster processing
anacapa_vect <- sites[grepl("ANACAPA", sites$site), ] %>% vect()
sites_vect   <- sites[!grepl("ANACAPA", sites$site), ] %>% vect()

# Reset the ID column (sequential without anacapa):
sites_vect$ID <- seq_len(nrow(sites_vect))

# Build Main 2000-2024 Data -------------------------------------------------------------------

# Function to calculate kelp area for a single buffer
calculate_kelp_buffer <- function(kelp_raster, sites, buffer, year) {
  # Buffer to the appropriate distance
  sites_buffer <- buffer(sites, width = buffer)
  
  # Extract kelp area values within the buffer
  kelp_areas <- terra::extract(kelp_raster, sites_buffer, fun = sum, na.rm = TRUE, weights = TRUE)

  # Combine results with site metadata and add year/buffer info
  kelp_results <- sites %>%
    as.data.frame(geom = NULL) %>%
    left_join(kelp_areas, by = "ID") %>%
    rename(kelp_area_m2 = sum) %>%
    mutate(year = year, buffer = buffer)
  
  # Replace NaN with 0 (no kelp detected)
  kelp_results$kelp_area_m2[is.nan(kelp_results$kelp_area_m2)] <- 0
  
  return(kelp_results)
}

# Details for function
buffers <- c(25, 50, 100, 150, 200, 250, 300, 400, 500) 
years <- seq(2000, 2024) 
all_kelp_results <- data.frame() # empty df to store results

# Function that will process all buffers for a single year
process_year_buffer <- function(year, buffers, sites_vect) {
  
  year_results <- list()  # store the results for the year
  kelp_dir <- if (year == 2024) kelp2024.dir else kelp.dir # specify correct directory
  kelp_raster <- terra::rast(file.path(kelp_dir, paste0("kelp_canopy_", year, ".tif"))) # read annual kelp raster
  
  # Loop over buffers and calculate kelp area for each
  for (buffer in buffers) {
    kelp_results <- calculate_kelp_buffer(kelp_raster, sites = sites_vect, buffer, year)
    year_results[[as.character(buffer)]] <- kelp_results  # Store the results for each buffer
  }
  
  rm(kelp_raster)  # Clean up memory after processing the year
  gc()
  
  return(year_results)
}

# Parallel processing over years and buffers
num_cores <- round(detectCores()/5)
all_kelp_results_list <- mclapply(years, process_year_buffer, buffers = buffers, sites_vect = sites_vect, mc.cores = num_cores)
all_kelp_results <- map_dfr(all_kelp_results_list, ~ bind_rows(.x))

# Format the consolidated results
kelp <- all_kelp_results %>% 
  mutate(habitat_buffer = paste0("kelp_annual_", buffer)) %>% 
  dplyr::select(-ID, -buffer) %>% 
  pivot_wider(names_from = habitat_buffer, values_from = kelp_area_m2)

# Save the consolidated results
saveRDS(kelp, file.path(kelp.dir, "kelp_site_buffers.Rds"))


# Fix Anacapa 2000-2023 Data -------------------------------------------------------------------
# Since Anacapa is so narrow, many of the site buffers extend to the opposite side of the island.
# We already separated the Anacapa sites, now we need to:
# Read the raster file for the given year
# Crop it to the anacapa bbox to make masking faster
# Mask the south to process the MPA sites
# Mask the north to process the Ref sites
# Split the anacapa sites by side (MPA/Ref)
# Give them IDs

# Get bounding box for anacapa to crop the kelp rasters:
ana_bbox <- buffer(anacapa_vect, 500)

# Read Anacapa mask polygons
poly_s <- read_sf(file.path(com.dir, "anacapa_south_mask.shp")) %>% st_transform(., crs(sites_vect)) %>% vect()
poly_n <- read_sf(file.path(com.dir, "anacapa_north_mask.shp")) %>% st_transform(., crs(sites_vect)) %>% vect()

# Empty df to store results:
anacapa_kelp_results <- data.frame()

process_anacapa_buffer <- function(year, buffers, ana_bbox, poly_s, poly_n, anacapa_vect, kelp.dir, kelp2024.dir) {
  print(paste("Processing year:", year))
  kelp_dir <- if (year == 2024) kelp2024.dir else kelp.dir
  kelp_raster <- terra::rast(file.path(kelp_dir, paste0("kelp_canopy_", year, ".tif"))) # read annual kelp raster
  
  # Crop the kelp raster to the bbox to make masking faster:
  kelp_anacapa <- crop(kelp_raster, ana_bbox)
  
  # For the MPAs, we want to keep the North side:
  kelp_anacapa_mpa <- mask(kelp_anacapa, poly_s, inverse = TRUE) 
  
  # For the Refs, we want to keep the South side:
  kelp_anacapa_ref <- mask(kelp_anacapa, poly_n, inverse = TRUE)
  
  # Split Anacapa sites by side
  ana_mpa <- anacapa_vect[anacapa_vect$site_type == "MPA", ]
  ana_ref <- anacapa_vect[anacapa_vect$site_type == "Reference", ]
  
  # Give each sequential ID:
  ana_mpa$ID <- seq_len(nrow(ana_mpa))
  ana_ref$ID <- seq_len(nrow(ana_ref))
  
  # List to store results for each buffer
  year_results <- list()
  
  for (buffer in buffers) {
    print(paste("  Buffer:", buffer, "m"))
    
    # Buffer sites 
    ana_mpa_buffer <- buffer(ana_mpa, width = buffer)
    ana_ref_buffer <- buffer(ana_ref, width = buffer)
    
    # Extract values within buffer
    extracted_mpa <- terra::extract(kelp_anacapa_mpa, ana_mpa_buffer, fun = sum, na.rm = TRUE, weights = TRUE) 
    extracted_ref <- terra::extract(kelp_anacapa_ref, ana_ref_buffer, fun = sum, na.rm = TRUE, weights = TRUE)
    
    extracted_mpa <- extracted_mpa %>% 
      left_join(as.data.frame(ana_mpa), by = c("ID"))
    
    extracted_ref <- extracted_ref %>% 
      left_join(as.data.frame(ana_ref), by = c("ID"))
    
    extracted <- bind_rows(extracted_mpa, extracted_ref) %>% 
      rename(kelp_area_m2 = layer) %>%
      mutate(year = year, buffer = buffer) %>% 
      select(habitat:buffer, kelp_area_m2)
    
    # Replace NaN with 0 (no kelp detected)
    extracted$kelp_area_m2[is.nan(extracted$kelp_area_m2)] <- 0
    
    # Store results for this buffer
    year_results[[as.character(buffer)]] <- extracted  
  }
  
  # Return the results for this year
  return(year_results)
  
}

anacapa_kelp_results_list <- mclapply(years, process_anacapa_buffer, buffers = buffers, 
                                      ana_bbox = ana_bbox, poly_s = poly_s, poly_n = poly_n, anacapa_vect = anacapa_vect, 
                                      kelp.dir = kelp.dir, kelp2024.dir = kelp2024.dir, mc.cores = num_cores)

anacapa_kelp_results <- map_dfr(anacapa_kelp_results_list, ~ bind_rows(.x))

# Format the consolidated results
kelp_anacapa <- anacapa_kelp_results  %>% 
  mutate(habitat_buffer = paste0("kelp_annual_", buffer)) %>% 
  dplyr::select(-buffer) %>% 
  pivot_wider(names_from = habitat_buffer, values_from = kelp_area_m2)


# Combine all results (other sites + anacapa) into a single dataframe:
kelp <- bind_rows(kelp, kelp_anacapa)

# Save the consolidated results
saveRDS(kelp, file.path(kelp.dir, "kelp_site_buffers.Rds"))

