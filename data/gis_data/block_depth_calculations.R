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

# Process Depth Data  (Meters) --------------------------------------------------
## Transform to wgs84
depth <- sf::st_transform(depth_raw, crs = st_crs("+proj=longlat +datum=WGS84"))
names(depth) = "depth"

## Simplify blocks to CA
blocks <- blocks_orig %>% 
  filter(block_state == "California") 

## Get depth values for each block
depth_mean <- aggregate(depth, by = blocks, mean, na.rm = TRUE)
depth_median <- aggregate(depth, by = blocks, median, na.rm = TRUE)

## Convert to sf object
depth_mean_sf <- st_as_sf(depth_mean)
depth_median_sf <- st_as_sf(depth_median)

## Add to block dataframe
data <- blocks %>% 
  mutate(block_mean_depth_m = depth_mean_sf$depth)

## Drop geometry
data2 <- data %>% 
  sf::st_drop_geometry()

# Convert Depth Data to Fathoms -------------------------------------------------
## Since fishermen work in fathoms it could be more relevant to do our depth
## analyses in fathoms.

# Number of fathoms in 1 meter
fa_in_m <- 0.546807

# Convert 
data3 <- data2 %>% 
  mutate(block_mean_depth_fa = block_mean_depth_m*fa_in_m) %>% 
  select(block_id, block_mean_depth_m, block_mean_depth_fa)


## Export mean and median data
saveRDS(data3, file.path(out.dir, "block_mean_depth.Rds"))

# Plot -------------------------------------------------------------------------
#plot(depth_mean_sf)
#plot(depth_median_sf)


ggplot() +
  geom_point(aes(x = depth_fa_mean_sf$depth, y = depth_fa_median_sf$depth)) +
  labs(x = "Mean Depth (Fathoms)",
       y = "Median Depth (Fathoms)") +
  theme_classic()
