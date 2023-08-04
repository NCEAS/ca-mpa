

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "habitat_cdfw/processed")
plotdir <- "data/habitat_cdfw/figures"

# Read data
cdfw_orig <- terra::rast(file.path(datadir, "CA_bottom_substrate_10m.tiff")) 


# Build data
################################################################################

# Format CDFW raster
# 1-4 = soft; 5-8 = hard
cdfw <- cdfw_orig %>% 
  terra::classify(rcl=matrix(c(1, 0,
                               2, 0, 
                               3, 0,
                               4, 0,
                               5, 1,
                               6, 1, 
                               7, 1, 
                               8, 1), ncol=2, byrow=T))

# Export data
terra::writeRaster(cdfw, filename = file.path(datadir, "CA_bottom_substrate_10m_reclassed.tiff"), overwrite=T)


