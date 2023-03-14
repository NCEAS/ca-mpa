

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

# Read CDFW data


# Read CI data
# 1=hard substrate, 0=soft abstrate
sub_ci <- terra::rast(file.path(indir, "CIN_rock/All_CI_N_rock_2m.tif"))

# Inspect regional data
list.files(file.path(indir, "rock"))

# Read regional data
# 2 m resolution
# 1=hard substrate, NA=soft substrate
sub_n <- raster::raster(file.path(indir, "rock/hab_ncsr_rock_2m.tif"))
sub_nc <- raster::raster(file.path(indir, "rock/hab_nccsr_rock_2m.tif"))
sub_c <- raster::raster(file.path(indir, "rock/hab_ccsr_rock_2m.tif"))
sub_s <- raster::raster(file.path(indir, "rock/hab_scsr_rock_2m.tif"))

# Inspect data
################################################################################

plot(sub_n)
plot(sub_ci)


# Read data
################################################################################



# CRS
crs_use <- "+proj=utm +zone=10 +datum=NAD83 +units=m +no_defs"

# Reproject a few
sub_s_new <- terra::project(x=sub_s, y=crs_use)
sub_ci_new <- terra::project(x=sub_ci, y=crs_use)

# Merge data
data <- terra::merge(sub_s_new, sub_c, sub_nc, sub_n, sub_ci_new)

# Export merged data
terra::writeRaster(x=data, 
                   filename=file.path(outdir, "substrate.tiff"), 
                   overwrite=T)

# Expert individual data
terra::writeRaster(x=sub_n, filename=file.path(outdir, "substrate_n.tiff"), overwrite=T)
terra::writeRaster(x=sub_nc, filename=file.path(outdir, "substrate_nc.tiff"), overwrite=T)
terra::writeRaster(x=sub_c, filename=file.path(outdir, "substrate_c.tiff"), overwrite=T)
terra::writeRaster(x=sub_s, filename=file.path(outdir, "substrate_s.tiff"), overwrite=T)
terra::writeRaster(x=sub_ci, filename=file.path(outdir, "substrate_ci.tiff"), overwrite=T)

# Merge data
# sub2 <- raster::merge(sub_s_new, sub_c)
# sub3 <- raster::merge(sub2, sub_nc)
# sub4 <- raster::merge(sub3, sub_n)
# sub5 <- raster::merge(sub4, sub_ci)

