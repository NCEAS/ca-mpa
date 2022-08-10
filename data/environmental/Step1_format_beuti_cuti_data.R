

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- file.path(basedir, "environmental/figures")
inputdir <- file.path(basedir, "environmental/raw")
outputdir <- file.path(basedir, "environmental/processed")

# Downloaded from: https://mjacox.com/upwelling-indices/

# Format CUTI
################################################################################

# Read CUTI
cuti_orig <- read.csv(file.path(inputdir, "CUTI_daily.csv"), as.is=T)

# Format CUTI
cuti <- cuti_orig %>% 
  # Gather
  gather(key="lat_dd_bin", value="cuti", 4:ncol(.)) %>% 
  # Format lat bin
  mutate(lat_dd_bin=gsub("X|N", "", lat_dd_bin) %>% as.numeric(.)) %>% 
  # Build date
  mutate(date=paste(year, month, day, sep="-") %>% lubridate::ymd()) %>% 
  # Arrange
  select(year, month, day, date, lat_dd_bin, cuti)


# Format BEUTI
################################################################################

# Read CUTI
beuti_orig <- read.csv(file.path(inputdir, "BEUTI_daily.csv"), as.is=T)

# Format CUTI
beuti <- beuti_orig %>% 
  # Gather
  gather(key="lat_dd_bin", value="beuti", 4:ncol(.)) %>% 
  # Format lat bin
  mutate(lat_dd_bin=gsub("X|N", "", lat_dd_bin) %>% as.numeric(.)) %>% 
  # Build date
  mutate(date=paste(year, month, day, sep="-") %>% lubridate::ymd()) %>% 
  # Arrange
  select(year, month, day, date, lat_dd_bin, beuti)


# Merge data
################################################################################

# Build data
data <- cuti %>% 
  left_join(beuti) %>% 
  # Add lat lo/hi
  mutate(lat_dd_lo=lat_dd_bin-0.5,
         lat_dd_hi=lat_dd_bin+0.5) %>% 
  # Arrange
  select(year:date, lat_dd_bin, lat_dd_lo, lat_dd_hi)

# Inspect
table(data$lat_dd_bin)

# Export
saveRDS(data, file.path(outputdir, "1988_2022_cuti_beuti_daily.Rds"))


