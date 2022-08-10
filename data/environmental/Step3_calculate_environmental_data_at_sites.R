

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

# Read monitoring sites
sites <- readRDS(file.path(basedir, "monitoring/monitoring_sites_clean.Rds"))

# Read CUTI/BEUTI
upwelling <- readRDS(file.path(outputdir, "1988_2022_cuti_beuti_daily.Rds"))



# Build time series
################################################################################



