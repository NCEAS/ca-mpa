# Calculate Block Distance to Port
# Cori Lopazanski
# 5 July 2022

# Setup ------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)
library(sf)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed"
gisdir <- file.path(basedir, "gis_data")
plotdir <- "analyses/2performance_fisheries/figures"

## Read data ---- 
port_totals <- readRDS(file.path(datadir, "annual_landings_per_port_lb.Rds"))
ports <- readRDS(file.path(gisdir, "processed", "CA_ports.Rds"))
blocks_orig <- wcfish::blocks

# Build Data -------------------------------------------------------------------

## Reduce blocks to just CA ----
blocks <- blocks_orig %>% 
  filter(block_state == "California")

## Get block centroids ----
block_centroids <- st_centroid(blocks)

## Join port totals with shapefile data ----
port_comb <- ports %>% 
  full_join(port_totals, by = c("port_code" = "port_id")) 

## Reduce port shapefile to those in "major port" list
port_major <- port_comb %>% 
  filter(total < 99) # currently it's the top 99%

## Get port centroids ----
port_major_centroids <- st_centroid(port_major)

# Calculate  -------------------------------------------------------------------

## Get nearest port for each block ----
nearest_port <- st_nearest_feature(block_centroids, port_major_centroids)

## Calculate distance from block centroid to port centroid ----
nearest_port_dist <- st_distance(block_centroids, 
                                 port_major_centroids[nearest_port,], by_element = TRUE) %>% 
  as.numeric() %>%
  measurements::conv_unit(., "m", "km")

## Join to block df ----
block_port_dist <- blocks %>% 
  select(block_id) %>% 
  mutate(dist_to_port_km = nearest_port_dist)

# Plot -------------------------------------------------------------------------
ggplot() +
  geom_sf(data = block_port_dist, color = "grey60", 
          mapping=aes(fill=dist_to_port_km), lwd = 0.1) +
  geom_sf(data = port_major_centroids, color = "red") +
  theme_classic()

# Export -----------------------------------------------------------------------
block_port_dist <- block_port_dist %>% 
  st_drop_geometry()

saveRDS(block_port_dist, file = file.path(gisdir, "processed", "block_distance_to_port.Rds"))
