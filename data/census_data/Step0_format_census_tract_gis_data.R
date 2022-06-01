
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(tidycensus)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data/"
indir <- file.path(basedir, "census_data/raw")
outdir <- file.path(basedir, "census_data/processed")
plotdir <- "data/census/figures"

# Read data
data_orig <- sf::st_read(file.path(indir, "nhgis0001_shapefile_tl2010_us_tract_2010", "US_tract_2010.shp"))


# Format data
################################################################################

# Extarct attributes
data_df <- data_orig %>% 
  sf::st_drop_geometry()
