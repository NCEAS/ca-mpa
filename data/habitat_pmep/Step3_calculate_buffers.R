# Step 3 Calculate Sequential Buffers
# Cori Lopazanski lopazanski@bren.ucsb.edu
# July 2024


# Setup   ----------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(sf)

com.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"


# Read  ----------------------------------------------------------------------
# Read the sites
# Created here: data/monitoring_data/processing_code/archive/clean_monitoring_sites.R
sites_raw <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds") %>% 
  filter(!habitat == "Rocky intertidal") %>% 
  st_as_sf(., coords = c("long_dd", "lat_dd"), crs = 4326) 

# Project so the buffer can be in meters
sites <- st_transform(sites_raw, crs = 26910) 

# Function to calculate buffers
calculate_buffers <- function(section, buffer) {
  print(paste("Section:", section))
  print(paste("Buffer:", buffer))
  
  habitat <- readRDS(file.path(com.dir, paste0("combined_", section, ".Rds"))) 
  sites_buffer <- st_buffer(sites, dist = buffer)
  sites_buffer <- st_transform(sites_buffer, crs = st_crs(habitat))
  
  intersect <- st_intersection(sites_buffer, habitat)
  print("Intersection complete")
  
  intersect <- intersect %>%
    filter(habitat == habitat.1) %>%
    filter(mpa == mpa.1) %>%
    filter(mpa_orig == mpa_orig.1) %>%
    filter(site == site.1) %>%
    filter(site_type == site_type.1) %>% 
    dplyr::select(!contains(".1"))
  
  # Transform intersection to the desired CRS
  intersect <- st_transform(intersect, crs = 32610)
  print("Transform complete")
  
  # Save the result
  saveRDS(intersect, file.path(com.dir, "buffers", paste0(buffer, "m/combined_hsb_revised", section, "_", buffer, "m.Rds"))) # remove "_revised" to use old version
}

# Create grid of all sections and buffer options
sections <- c(23, 30, 31, 32, 33, 40, 41)
buffers <- c(25, 50, 100, 250, 500)
section_buffers <- expand.grid(section = sections, buffer = buffers)

# Calculate buffers for each section
walk2(section_buffers$section, section_buffers$buffer, calculate_buffers)

section <- "23"
buffer <- 100
habitat <- readRDS(file.path(com.dir, paste0("combined_", section, ".Rds"))) 
sites_buffer <- st_buffer(sites, dist = buffer)
sites_buffer <- st_transform(sites_buffer, crs = st_crs(habitat))

intersect <- st_intersection(sites_buffer, habitat)

intersect <- intersect %>% 
  filter(habitat == habitat.1) %>%
  filter(mpa == mpa.1) %>%
  filter(mpa_orig == mpa_orig.1) %>%
  filter(site == site.1) %>%
  filter(site_type == site_type.1) %>% 
  dplyr::select(!contains(".1"))

kelp <- intersect %>% 
  filter(site == "PYRAMID_POINT_1")

kelp_orig <- habitat %>% 
  filter(site == "PYRAMID_POINT_1")

ggplot(kelp) +
  geom_sf(aes(fill = habitat_class)) 

ggplot(kelp_orig)+
  geom_sf(aes(fill = habitat_class)) 
