###############################################################################
# CDFW MPA Attributes Data 
#      1. Process Raw Data
#           Input:  mpa_attributes.csv
#           Output: mpa_attributes_clean.csv
################################################################################

# Packages 
library(tidyverse)
library(janitor)
library(here)

# Directories
# Cori Local Base:
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local

# Aurora Base:
#base.dir <- "/home/shares/ca-mpa/data/sync-data/"

in.dir <- here::here(base.dir, "mpa_traits", "raw")
out.dir <- here::here(base.dir, "mpa_traits", "processed")
plot.dir <- here::here(base.dir, "figures")

# Read Raw Data
raw <- read_csv(file.path(in.dir, "mpa_attributes.csv"), na = c("", ".", "na"))

################################################################################
# Processing
################################################################################
data <- raw %>% 
  # Clean variable names (mainly dashes in depth ranges)
  janitor::clean_names() %>% 
  
  # Rename the mis-labeled submarine canyon column
  rename(submarine_canyon_200_3000m_km2 = soft_substratecal_200_3000m_km2)

################################################################################
# Export
################################################################################
write_csv(data, file.path(out.dir, "mpa_attributes_clean.csv"))
