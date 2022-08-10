

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rerddap)
library(lubridate)
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- file.path(basedir, "environmental/figures")
inputdir <- file.path(basedir, "environmental/raw")
outputdir <- file.path(basedir, "environmental/processed")

# Source
# https://oceanview.pfeg.noaa.gov/dashboard/
# https://oceanview.pfeg.noaa.gov/erddap/tabledap/index.html?page=1&itemsPerPage=1000

# Multi-scale Ultra-high Resolution (MUR) SST Analysis fv04.1, Global, 0.01°, 2002-present, Monthly

# Build data
################################################################################

# Look up datasets (to future self: URL must end in /)
datasets_grids <- ed_datasets(which="griddap", url="https://coastwatch.pfeg.noaa.gov/erddap/")
datasets_tables <- ed_datasets(which="tabledap", url="https://coastwatch.pfeg.noaa.gov/erddap/")


# Get data
#################################################

# Get data
# Downloading in two batches b/c of size limit
data_info <- info("jplMURSST41mday")
data_orig <- griddap(x=data_info, 
                     time = c("2002-06-16", "2022-07-16"),
                     longitude = c(-125, -117), latitude = c(32, 42), 
                     fields="sst")
# data_orig2 <- griddap(x=data_info, 
#                       time = c("2012-07-17", "2022-07-16"),
#                       longitude = c(-130, -114), latitude = c(32, 50), 
#                       fields="sst")


# Convert to data frame
data_df <- data_orig$data
data_df1 <- data_df %>% 
  # Rename
  rename(lat_dd=lat, long_dd=lon, sst_c=sst) %>% 
  # Add date
  mutate(date=gsub("T00:00:00Z", "", time)) %>% 
  # Format lat/long
  mutate(lat_dd=round(lat_dd, 2), 
         long_dd=round(long_dd, 2)) %>% 
  # Arrange
  select(date, lat_dd, long_dd, sst_c)

# Inspect
head(data_df1)
str(data_df1)

# Export
saveRDS(data_df1, file=file.path(outputdir, "2002_2022_mursst_monthly.Rds"))


# Convert to raster
#################################################

# Test dataframe
test <- data_df1 %>% 
  filter(date=="2002-06-16") %>% 
  mutate(lat_dd=round(lat_dd, 2),
         long_dd=round(long_dd, 2))

# Convert df to raster brick
data_ras <- freeR::df2brick(data_df1, x_col = "long_dd", y_col="lat_dd", z_col = "sst_c", layer_col = "date")

# Project
data_ras_prj <- data_ras 
raster::crs(data_ras_prj) <- "+proj=longlat +datum=WGS84"

# Chekc
raster::plot(data_ras_prj, 3)

# Export
raster::writeRaster(data_ras_prj, file.path(outputdir, "2002_2022_mursst_monthly_raster.grd"))





