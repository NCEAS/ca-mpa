# Calculate Block Distance from Shore
# Cori Lopazanski


# Setup ------------------------------------------------------------------------

## Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)
library(sf)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/2performance_fisheries/figures"

## Read data ---- 
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
coast <- sf::st_read(file.path(basedir, "gis_data/raw", "Coastn83", "coastn83.shp"))


# Get blocks
blocks_orig <- wcfish::blocks

# Coast Data Source: https://filelib.wildlife.ca.gov/Public/R7_MR/BASE/

# Build Data -------------------------------------------------------------------

## Reduce blocks to just CA ----
blocks <- blocks_orig %>% 
  filter(block_state == "California")

## Get block centroids ----
block_centroids <- st_centroid(blocks)


## Reduce coastline to single geometry ----
coast_union <- st_union(coast)
coast_union_transform <- coast_union %>% 
  sf::st_transform(crs = st_crs("+proj=longlat +datum=WGS84"))

## Calculate distance from block to coastline ----
dist_to_shore <- st_distance(blocks, coast_union_transform) %>% 
  as.numeric() %>%
  measurements::conv_unit(., "m", "km")

## Create block key ----
data1 <- blocks %>% 
  select(block_id) %>% 
  mutate(distance_to_shore_km = dist_to_shore)

## Remove geom from block key for export ----
data2 <- data1 %>% 
  sf::st_drop_geometry()
  

## Export block key ----
saveRDS(data2, file=file.path(gisdir, "block_distance_to_shore.Rds"))


# Plot Data --------------------------------------------------------------------
ggplot() +
  geom_sf(data = data1, color = "grey60", 
          mapping=aes(fill=distance_to_shore_km), lwd = 0.1) +
  geom_sf(data = state_waters_line, color = "grey20", lwd = 0.2) + 
  geom_sf(data = block_centroids, color = "black", lwd = 0.1) +
  geom_sf(data = coast_union_transform, color = "red", lwd = 0.2) +
  scale_y_continuous(breaks = 32:42) +
  coord_sf(xlim = c(-128.5, -117), ylim = c(32.5, 42))
  
