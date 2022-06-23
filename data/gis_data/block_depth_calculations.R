# Calculate Average/Median Depth in Each Block 
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
## Clear workspace
rm(list = ls())

## Packages
library(tidyverse)
library(stars)

## Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
outdir <- file.path(basedir, "gis_data/processed")

## Get blocks
blocks_orig <- wcfish::blocks

## Get depth data
## Source: https://filelib.wildlife.ca.gov/Public/R7_MR/BATHYMETRY/
depthdir <- "/Users/lopazanski/Documents/github/nceas/200mEEZ_BathyGrids"
depth_raw <- read_stars(file.path(depthdir, "bd200m_v2i", "w001001.adf"))

# Process Depth Data  ----------------------------------------------------------
## Transform to wgs84
depth <- sf::st_transform(depth_raw, crs = st_crs("+proj=longlat +datum=WGS84"))
names(depth) = "depth"

## Simplify to CA
blocks <- blocks_orig %>% 
  filter(block_state == "California") 

## Get depth values for each block
depth_mean <- aggregate(depth, by = blocks, mean, na.rm = TRUE)
depth_median <- aggregate(depth, by = blocks, median, na.rm = TRUE)

## Convert to sf object
depth_mean_sf <- st_as_sf(depth_mean)
depth_median_sf <- st_as_sf(depth_median)

## Export mean and median data
saveRDS(depth_mean_sf, file.path(outdir, "block_mean_depth.Rds"))
saveRDS(depth_median_sf, file.path(outdir, "block_mean_depth.Rds"))


# Plot -------------------------------------------------------------------------
plot(depth_mean_sf)
plot(depth_median_sf)
