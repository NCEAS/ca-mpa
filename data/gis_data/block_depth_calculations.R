# Calculate Average/Median Depth in Each Block 
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
## Clear workspace
rm(list = ls())

## Packages
library(tidyverse)
library(stars)

## Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
data.dir <- file.path(base.dir, "gis_data/raw")
out.dir <- file.path(base.dir, "gis_data/processed")

## Get blocks
blocks_orig <- wcfish::blocks

## Get depth data
## Source: https://filelib.wildlife.ca.gov/Public/R7_MR/BATHYMETRY/
depth_raw <- read_stars(file.path(data.dir, "200mEEZ_BathyGrids", "bd200m_v2i", "w001001.adf")) # meters
depth_raw_fa <- read_stars(file.path(data.dir, "200mEEZ_BathyGrids", "bd200fa_v2i", "w001001.adf")) # fathoms

# Process Depth Data  (Meters) ----------------------------------------------------------
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
saveRDS(depth_mean_sf, file.path(out.dir, "block_mean_depth_meters.Rds"))
saveRDS(depth_median_sf, file.path(out.dir, "block_median_depth_meters.Rds"))

# Process Depth Data (Fathoms) -------------------------------------------------
## Since fishermen work in fathoms it could be more relevant to do our depth
## analyses in fathoms

## Clear some space
rm(depth, depth_raw, depth_mean, depth_median, depth_mean_sf, depth_median_sf) 
gc()

## Transform to wgs84
depth_fa <- sf::st_transform(depth_raw_fa, crs = st_crs("+proj=longlat +datum=WGS84"))
names(depth_fa) = "depth"

## Get depth values for each block
depth_fa_mean <- aggregate(depth_fa, by = blocks, mean, na.rm = TRUE)
depth_fa_median <- aggregate(depth_fa, by = blocks, median, na.rm = TRUE)

## Convert to sf object
depth_fa_mean_sf <- st_as_sf(depth_fa_mean)
depth_fa_median_sf <- st_as_sf(depth_fa_median)

## Export mean and median data
saveRDS(depth_fa_mean_sf, file.path(out.dir, "block_mean_depth_fathoms.Rds"))
saveRDS(depth_fa_median_sf, file.path(out.dir, "block_median_depth_fathoms.Rds"))


# Plot -------------------------------------------------------------------------
#plot(depth_mean_sf)
#plot(depth_median_sf)


ggplot() +
  geom_point(aes(x = depth_fa_mean_sf$depth, y = depth_fa_median_sf$depth)) +
  labs(x = "Mean Depth (Fathoms)",
       y = "Median Depth (Fathoms)") +
  theme_classic()
