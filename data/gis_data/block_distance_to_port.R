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
ports <- sf::st_read(file.path(gisdir, "raw", "Ports", "CUL_CA_Ports.shp"))
blocks_orig <- wcfish::blocks

state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
block_mpa_key <- read.csv(file.path(gisdir, "CA_blocks_with_mpas.csv"), as.is=T)
coast <- sf::st_read(file.path(basedir, "gis_data/raw", "Coastn83", "coastn83.shp"))


# Build Data -------------------------------------------------------------------

## Reduce blocks to just CA ----
blocks <- blocks_orig %>% 
  filter(block_state == "California")

## Get block centroids ----
block_centroids <- st_centroid(blocks)

## Join port totals with shapefile data ----
port_comb <- ports %>% 
  full_join(port_totals, by = c("PORTCODE" = "port_id")) %>% 
  sf::st_transform(crs = st_crs("+proj=longlat +datum=WGS84"))


## Reduce port shapefile to those in "major port" list
port_major <- port_comb %>% 
  filter(total < 99)

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
