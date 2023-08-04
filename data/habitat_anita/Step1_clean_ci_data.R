

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

# Read full to use as template
data_all <- raster::raster(file.path(basedir, "habitat_cdfw/processed/CA_bottom_substrate_10m.tiff"))

# Read CI data
# 1=hard substrate, 0=soft abstrate
ci_orig <- raster::raster(file.path(indir, "CIN_rock/All_CI_N_rock_2m.tif"))


# Project
################################################################################

# Reproject 
ci <- raster::projectRaster(from=ci_orig, to=data_all, fun="mean", background=NA, na.rm=T)

# Export
raster::writeRaster(ci, filename = file.path(outdir, "CI_substrate_10m_avg.tiff"), overwrite=T)


# Performed reclass in QGIS
# Reclassify raster with table
# 0-0.5 = 0, 0.5-1 = 1

