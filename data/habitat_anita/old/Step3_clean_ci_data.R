

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "habitat_anita/processed")

# Read data
anita_orig <- terra::rast(file.path(datadir, "CI_substrate_10m_qgis_new2.tif"))
terra::plot(anita_orig)

# Format data
################################################################################

# Format data
rcl_mat <- matrix(c(1, 0, 
                    2, 1), byrow=T, ncol=2)
anita <- anita_orig %>% 
  terra::clamp(lower=0.5, upper=2.5, values=F) %>% 
  terra::classify(rcl=rcl_mat)
terra::plot(anita)


# Export data
################################################################################

# Export data
terra::writeRaster(anita, filename = file.path(datadir, "CI_substrate_10m_clean.tiff"), overwrite=T)







