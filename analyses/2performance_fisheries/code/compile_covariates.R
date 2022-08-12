# Compile Block Covariates
# Cori Lopazanski
# 13 July 2022

# List of covariates and their locations:
# 1. MPA Coverage 
# 2. Distance from Shore
# 3. Average Depth
# 4. Distance from Port
# 5. Area
# 6. Buffered MPA Density


# Setup --------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

## Packages
library(tidyverse)

## Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
gis.dir <- file.path(base.dir, "gis_data", "processed")

blocks <- wcfish::blocks

# Read Data --------------------------------------------------------------------
block_stats <- readRDS(file.path(gis.dir,"block_mpa_coverage_reduced_types.Rds")) 
depth <- readRDS(file.path(gis.dir, "block_mean_depth.Rds")) 
shore <- readRDS(file.path(gis.dir, "block_distance_to_shore.Rds"))
port <- readRDS(file.path(gis.dir, "block_distance_to_port.Rds"))

# Build Data -------------------------------------------------------------------

# Join covariates
data <- block_stats %>% 
  left_join(., depth) %>% 
  left_join(., shore) %>% 
  left_join(., port)

# Add treatment column
data2 <- data %>% 
  mutate(block_treatment = ifelse(is.na(mpa_n), 0, 1)) %>% 
  select(block_id, block_treatment, block_area_km2, mpa_km2, block_mean_depth_fa, 
         distance_to_shore_km, dist_to_port_km)

data2$mpa_km2[is.na(data2$mpa_km2)] <- 0         


  
  