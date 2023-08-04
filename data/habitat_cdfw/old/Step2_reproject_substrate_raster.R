

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
indir <- file.path(basedir, "habitat_cdfw/raw")
outdir <- file.path(basedir, "habitat_cdfw/processed")
plotdir <- "data/habitat_cdfw/figures"

# Read data
substrate_orig <- raster::raster(file.path(outdir, "CA_bottom_substrate_10m.tiff")) 

# Reproject data
substrate <- substrate_orig %>% 
  raster::projectRaster(crs="+proj=longlat +datum=WGS84")

# Export data
raster::writeRaster(substrate, filename = file.path(outdir, "CA_bottom_substrate_10m_wgs84.tiff"), overwrite=T)






