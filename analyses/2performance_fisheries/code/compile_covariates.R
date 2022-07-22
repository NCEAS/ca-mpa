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



# Read Data --------------------------------------------------------------------------
depth <- readRDS(file.path(gis.dir, "block_mean_depth_fathoms.Rds"))
shore <- read_csv(file.path(gis.dir, "CA_blocks_"))
test <- readRDS(file.path(gis.dir, "CA_blocks_stats_all_mpa_types.Rds"))
