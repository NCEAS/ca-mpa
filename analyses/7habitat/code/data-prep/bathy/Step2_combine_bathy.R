# Combine bathymetry datasets into single raster
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2025


# Combine the CSMP, CDFW, and WCDSCI layers ------------------------------------------

# Setup ----
library(tidyverse)
library(sf)
library(terra)

rm(list = ls())
gc()

proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"
gis.dir <- "/home/shares/ca-mpa/data/sync-data/gis_data/processed"


# Read ----

# Read the resampled bathy datasets
csmp <- rast(file.path(proc.dir, "csmp_30m_bathy_res.tif"))
cdfw <- rast(file.path(proc.dir, "cdfw_30m_bathy_res.tif"))
wcds <- rast(file.path(proc.dir, "wcds_30m_bathy_res.tif"))


# Combine ----
# Choose CSMP first where available, fill gaps with CDFW:
final <- merge(csmp, cdfw) # fairly quick, a few minutes

# Using the CSMP-CDFW combo, fill remaining gaps with WCDSCI:
final <- merge(final, wcds) 

# Write ----
writeRaster(final, file.path(proc.dir, "combined_30m_bathy.tif"), overwrite=TRUE)


# Crop to California area (remove oregon and washington) ----------------------------

# Read in the final combined layer produced above
final <- rast(file.path(proc.dir, "combined_30m_bathy.tif"))

# Read state waters polygon
state_waters_poly <- readRDS(file.path(gis.dir, "CA_state_waters_polygons.Rds")) %>% 
  st_transform(., crs = 26910) %>% # transform to linear CRS for buffering
  st_buffer(., dist = 1000) # expand 1k to ensure have northermnost data

# Project state waters to match
state_waters_poly <- vect(state_waters_poly)
state_waters_poly <- project(state_waters_poly, crs(final))

# Build extent that matches the northern limit of state waters + 1k buffer
north_limit <- ymax(ext(state_waters_poly))

# Current extent of final raster:
current_ext <- ext(final)
clip_ext <- ext(current_ext$xmin, current_ext$xmax, current_ext$ymin, north_limit)

plot(current_ext)
plot(clip_ext, add=T, col = "blue")

# Crop raster to new extent:
final_ca <- crop(final, clip_ext)

# Write raster to file:
writeRaster(final_ca, file.path(proc.dir, "combined_30m_bathy_ca.tif"), overwrite=TRUE)


# Get slope and TRI ----------------------------

final_ca <- rast(file.path(proc.dir, "combined_30m_bathy_ca.tif"))

slope  <- terra::terrain(final_ca, v = "slope", unit = "degrees", neighbors = 8)
writeRaster(slope, file.path(proc.dir, "combined_30m_bathy_ca_slope.tif"), overwrite=TRUE)

tri    <- terra::terrain(final_ca, v = "TRI", neighbors = 8)
writeRaster(tri, file.path(proc.dir, "combined_30m_bathy_ca_tri.tif"), overwrite=TRUE)

# Both slope and TRI have extremes at edges because of missing data near land,
# so will mask to only include these where they have a full 8-cell neighborhood
# - Tested using terra::focal on full raster and was not possible due to memory
#   creep, so will do that on the next step with the buffers loaded instead...
