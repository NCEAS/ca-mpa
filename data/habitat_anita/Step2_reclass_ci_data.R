

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(raster)
library(terrainr)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data" #Chris
indir <- file.path(basedir, "habitat_anita/raw")
outdir <- file.path(basedir, "habitat_anita/processed")

# Read CI data
# 1=hard substrate, 0=soft abstrate
ci_orig <- terra::rast(file.path(outdir, "CI_substrate_10m.tiff"))
plot(ci_orig)

# Read data
################################################################################

thresh <- 0.05

# Reclassify
ci <- ci_orig %>% 
  terra::classify(rcl=matrix(c(-0.1, thresh, 1.1), ncol=1))

ci1 <- ci %>% 
  terra::classify(rcl=matrix(c("(-0.1–0.05]", "0",
                               "(0.05–1.1]", "1"), ncol=2, byrow=T))


# Export
raster::writeRaster(ci, filename = file.path(outdir, "CI_substrate_10m.tiff"), overwrite=T)




