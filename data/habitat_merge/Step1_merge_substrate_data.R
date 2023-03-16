

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
anita_orig <- terra::rast(file.path(datadir3, "CI_substrate_10m_qgis_new2.tif"))
plot(anita_orig)

# Format data
################################################################################

# Format data
rcl_mat <- matrix(c(1, 0, 
                    2, 1), byrow=T, ncol=2)
anita <- anita_orig %>% 
  terra::clamp(lower=0.5, upper=2.5, values=F) %>% 
  terra::classify(rcl=rcl_mat)
plot(anita)

# Convert to rasters
cdfw_ras <- raster::raster(cdfw_orig)
stanford_ras <- raster::raster(stanford_orig)
anita_ras <- raster::raster(anita_orig)


# Build data
################################################################################

# Merge data
data <- raster::merge(anita_ras, cdfw_ras, stanford_ras, overlap=T)

# Export data
raster::writeRaster(data_ras, filename = file.path(outdir, "CA_bottom_substrate_type_10m.tiff"), overwrite=T)

# Merge rasters
# terra::merge goes bottom-to-top from left-to-right
# data <- terra::merge(stanford_orig, cdfw_orig, anita, first=T)
# data_ras <- raster::raster(data)

# Export data
# terra::writeRaster(data, filename = file.path(outdir, "CA_bottom_substrate_type_10m.tiff"), overwrite=T)
# raster::writeRaster(data_ras, filename = "~/Desktop/CA_bottom_substrate_type_10m.tiff", overwrite=T)







