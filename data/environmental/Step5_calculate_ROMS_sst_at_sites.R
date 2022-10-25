
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(raster)
library(tidyverse)

# Directories
basedir <- "/home/shares/ca-mpa/data/sync-data"
plotdir <- file.path(basedir, "environmental/figures")
inputdir <- file.path(basedir, "environmental/raw")
outputdir <- file.path(basedir, "environmental/processed")

# Read monitoring sites
sites <- readRDS(file.path(basedir, "monitoring/monitoring_sites_clean.Rds"))

# Read ROMS product
sst_ROMS <- read.csv(file.path(inputdir, "ROMS_Temp_All_MPA.csv"))

# Read MURSST data
sst <- brick(file.path(outputdir, "2002_2022_mursst_monthly_raster.grd"))


# Intersect sites with SST
################################################################################

# Convert sites to spatial points
sites_sp <- sites %>% 
  # Convert to SF
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=crs(sst))

#Concert ROMS to spatial points
ROMS_sst <- sst_ROMS %>% 
  # Convert to SF
  sf::st_as_sf(coords=c("lon", "lat"), crs=crs(sst))

# Extract SST
ROMS_sst_ts_orig <- extract(x=ROMS_sst, y=sites_sp)

# Format SST extraction
sst_ts <- sst_ts_orig %>% 
  # Convert to df
  as.data.frame() %>% 
  # Add site
  mutate(site=sites$site) %>% 
  select(site, everything()) %>% 
  # Gather
  gather(key="date", value="sst_c", 2:ncol(.)) %>% 
  # Format date
  mutate(date=gsub("X", "", date) %>% lubridate::ymd(.)) %>% 
  # Add metadata
  left_join(sites %>% select(-c(long_dd, lat_dd))) %>% 
  # Arrange
  select(habitat, mpa, site, site_type, date, everything()) %>% 
  arrange(habitat, mpa, site, site_type, date)





