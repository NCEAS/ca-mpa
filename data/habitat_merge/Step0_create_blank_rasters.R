

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir1 <- file.path(basedir, "habitat_cdfw/processed")
datadir2 <- file.path(basedir, "habitat_stanford/processed")
datadir3 <- file.path(basedir, "habitat_anita/processed")
outdir <- file.path(basedir, "habitat_merge")
plotdir <- "data/habitat_cdfw/figures"

# Read data
cdfw_orig <- terra::rast(file.path(datadir1, "CA_bottom_substrate_10m_reclassed.tiff")) 
stanford_orig <- terra::rast(file.path(datadir2, "CA_substrate_polygon_stanford_10m.tiff"))
anita_orig <- terra::rast(file.path(datadir3, "CI_substrate_10m_avg_reclassed.tif"))

# Raster math
################################################################################

# Format
anita <- anita_orig * 0 + 1
plot(anita)
raster::writeRaster(anita, filename = file.path(outdir, "anita_footprint.tiff"), overwrite=T)

cdfw <- cdfw_orig * 0 + 2
raster::writeRaster(cdfw, filename = file.path(outdir, "cdfw_footprint.tiff"), overwrite=T)
stanford <- stanford_orig * 0 + 3
raster::writeRaster(stanford, filename = file.path(outdir, "stanford_footprint.tiff"), overwrite=T)


# Build data
################################################################################

data <- cdfw_orig + anita_orig

cdfw_no_ci <- terra::mask(x = cdfw_orig, mask=anita_orig, inverse=T)

# Merge data
data <- terra::mosaic(anita_ras, cdfw_ras, overlap=T)

# Export data
raster::writeRaster(data_ras, filename = file.path(outdir, "CA_bottom_substrate_type_10m.tiff"), overwrite=T)

# Merge rasters
# terra::merge goes bottom-to-top from left-to-right
# data <- terra::merge(stanford_orig, cdfw_orig, anita, first=T)
# data_ras <- raster::raster(data)

# Export data
# terra::writeRaster(data, filename = file.path(outdir, "CA_bottom_substrate_type_10m.tiff"), overwrite=T)
# raster::writeRaster(data_ras, filename = "~/Desktop/CA_bottom_substrate_type_10m.tiff", overwrite=T)







