# Plot Block Depths
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
## Clear workspace
rm(list = ls())

## Packages
library(tidyverse)
library(stars)

## Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "gis_data/processed")

# Read Data
depth_mean <- readRDS(file.path(datadir, "block_mean_depth.Rds"))
depth_median <- readRDS(file.path(datadir, "block_mean_depth.Rds"))
blocks_orig <- wcfish::blocks

# CA blocks
blocks <- blocks_orig %>% 
  filter(block_state == "California")


# Plot ------------------------------------------------------------------------
ggplot() +
  geom_sf(data = blocks, color = "grey60", lwd = 0.1) +
  geom_sf(data = depth_mean, 
          mapping=aes(fill=depth)) +
  scale_y_continuous(breaks = 32:42) +
  coord_sf(xlim = c(-128.5, -117), ylim = c(32.5, 42))

ggplot() +
  geom_sf(data = blocks, color = "grey60", lwd = 0.1) +
  geom_sf(data = depth_median, 
          mapping=aes(fill=depth)) +
  scale_y_continuous(breaks = 32:42) +
  coord_sf(xlim = c(-128.5, -117), ylim = c(32.5, 42))
