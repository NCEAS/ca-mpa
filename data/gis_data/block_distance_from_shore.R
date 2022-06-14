# Calculate Block Distance from Shore
# Cori Lopazanski


# Setup
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/2performance_fisheries/figures"

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
block_mpa_key <- read.csv(file.path(gisdir, "CA_blocks_with_mpas.csv"), as.is=T)

# Tried uploading the coast shapefile to drive, but it is not letting me grab it 
# instead I have it locally and am reading from there...
coast <- sf::st_read(file.path("Coastn83", "coastn83.shp"))

# Get blocks
blocks_orig <- wcfish::blocks

# Coast Source:
#https://filelib.wildlife.ca.gov/Public/R7_MR/BASE/

# Build Data
################################################################################
# Reduce blocks to just CA
blocks <- blocks_orig %>% 
  filter(block_state == "California")

# Centroid of each block
block_centroids <- st_centroid(blocks)


# Reduce coastline to single geometry
coast_union <- st_union(coast)
coast_union_transform <- coast_union %>% 
  sf::st_transform(crs = st_crs("+proj=longlat +datum=WGS84"))

# Calculate distance from block to coastline
dist_to_shore <- st_distance(blocks, coast_union_transform) %>% 
  as.numeric() %>%
  measurements::conv_unit(., "m", "km")

# Create block key 
data <- blocks %>% 
  sf::st_drop_geometry() %>% 
  select(block_id) %>% 
  mutate(distance_to_shore_km = dist_to_shore)

# Export block key
write.csv(data, file=file.path(gisdir, "CA_blocks_dist_from_shore.csv"), row.names=F)


# Plot Data
################################################################################
ggplot() +
  geom_sf(data = blocks, color = "grey60", lwd = 0.1) +
  geom_sf(data = state_waters_line, color = "grey20", lwd = 0.2) + 
  geom_sf(data = block_centroids, color = "blue", lwd = 0.2) +
  geom_sf(data = coast_union_transform, color = "red", lwd = 0.2) +
  scale_y_continuous(breaks = 32:42) +
  coord_sf(xlim = c(-128.5, -117), ylim = c(32.5, 42)) 
  
