
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Users/cfree/Library/CloudStorage/GoogleDrive-cfree@ucsb.edu/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
datadir <- file.path(basedir, "habitat_merge")

# Read data
data_orig <- raster::raster(file.path(datadir, "CA_bottom_substrate_10m.tif"))

# Projections
wgs84 <- sf::st_crs("+proj=longlat +datum=WGS84")


# Plot data
################################################################################

# Format data
data <- data_orig %>% 
  raster::projectRaster(crs="+proj=longlat +datum=WGS84")

