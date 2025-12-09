# Combine bathymetry datasets into single raster
# Cori Lopazanski (lopazanski@bren.ucsb.edu)
# December 2024



# Combine the CSMP, CDFW, and WCDSCI layers ------------------------------------------

# Setup ----
library(tidyverse)
library(sf)
library(terra)

rm(list = ls())
gc()

proc.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_anita/processed"


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

