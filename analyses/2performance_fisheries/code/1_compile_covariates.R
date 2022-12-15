# Compile Block Covariates
# Cori Lopazanski
# 13 July 2022

# List of covariates and their locations:
# 1. MPA Coverage 
# 2. Distance from Shore
# 3. Average Depth
# 4. Distance from Port
# 5. Area
# 6. Buffered MPA Density (not necessarily included in matching)
# 7. Historical Fishing Intensity 


# Setup --------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

## Packages
library(tidyverse)
library(MatchIt)
library(Polychrome)
library(cobalt)

## Directories
### Full working group sync-data folder
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local

### Processed covariate GIS data
gis.dir <- file.path(base.dir, "gis_data", "processed")

### Processed covariate fisheries landings data
land.dir <- file.path("/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed")


# Read Data --------------------------------------------------------------------
## Full block data from CF's data package
blocks <- wcfish::blocks
blocks_simple <- blocks %>% sf::st_drop_geometry()

blocks_subset <- wcfish::blocks %>% 
  filter(!(block_id %in% blocks$block_id[blocks$block_type %in% c("Offshore", "Midshore")])) %>% 
  filter(block_state == "California")

# Add lat/lon for proximity
block_latlon <- blocks_simple %>% 
  select(block_id, block_long_dd, block_lat_dd)

## Block stats
block_stats <- readRDS(file.path(gis.dir,"block_mpa_coverage_reduced_types.Rds")) 
depth <- readRDS(file.path(gis.dir, "block_mean_depth.Rds")) 
shore <- readRDS(file.path(gis.dir, "block_distance_to_shore.Rds"))
port <- readRDS(file.path(gis.dir, "block_distance_to_port.Rds"))
fishing <- readRDS(file.path(land.dir, "block_fishing_intensity.Rds")) %>% 
  select(block_id, total_lb, total_lb_sqkm)

# Build Data -------------------------------------------------------------------

# Join covariates
data <- block_stats %>% 
  left_join(., depth) %>% 
  left_join(., shore) %>% 
  left_join(., port) %>% 
  left_join(., block_latlon) %>% 
  left_join(., fishing) %>% 
  ## Add treatment column
  mutate(block_treatment = ifelse(is.na(mpa_n), 0, 1)) %>% 
  ## Remove midshore and offshore blocks
  filter(!(block_id %in% blocks$block_id[blocks$block_type %in% c("Offshore", "Midshore")])) %>% 
  ## Reduce columns
  select(block_id, block_area_km2, block_mean_depth_m:block_treatment)

# Export covariates

saveRDS(data, file.path("analyses", "2performance_fisheries", "analyses", "blocks", "block_all_covariates.Rds"))

