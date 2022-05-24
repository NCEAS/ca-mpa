

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(rinat)
library(tidyverse)

# Directories
indir <- "data/mpa_watch/raw"
outdir <- "data/mpa_watch/processed"
plotdir <- "data/mpa_watch/figures"

# Read data
data_orig <- sf::st_read(file.path(indir, "Monitoring_sites_shapefile/MPAsControlSites_2022-05-09.shp"))

# Format data
################################################################################

# Format data
data <- data_orig %>%
  # Rename columns
  janitor::clean_names("snake") %>%
  rename(mpa_id=id, mpa_name=name, mpa_type=type, mpa_region)


