# Step 3 Calculate Sequential Buffers
# Cori Lopazanski lopazanski@bren.ucsb.edu
# July 2024


# Setup   ----------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(sf)

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined/combined_mlpa_sites_1000m"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

# Read  ----------------------------------------------------------------------
# Read the sites
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites_raw <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")

sites_raw <- st_as_sf(sites, coords = c("long_dd", "lat_dd"), crs = 4326) %>% 
  st_transform(crs = 32610) 

# Read the habitat data
habitat.files <- list.files(file.path(com.dir), pattern = "combined_hsb", full.names = T) 

# Function to calculate buffers
calculate_buffers <- function(section, buffer){
  print(paste("Section: ", section))
  print(paste("Buffer: ", buffer))
  habitat <- readRDS(file.path(com.dir, paste0("combined_hsb_", section, ".Rds")))
  
  sites <- st_buffer(sites_raw, dist = buffer)
  sites <- st_transform(sites, crs = st_crs(habitat))
  
  habitat <- st_intersection(habitat, st_union(sites))
  habitat$area_m2 <- as.numeric(st_area(habitat$geometry))
  
  saveRDS(habitat, file.path(com.dir, "buffers", paste0(buffer, "m/combined_hsb_", section, "_", buffer, "m.Rds")))
}

# Create grid of all sections and buffer options
sections <- c(23, 30, 31, 32, 33, 40, 41)
buffers <- c(25, 50, 100, 250, 500)
section_buffers <- expand.grid(section = sections, buffer = buffers)

# Calculate buffers for each section
walk2(section_buffers$section, section_buffers$buffer, calculate_buffers)

