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
  
  # Calculate total amount of hard substrate
  mutate(total_hard_substrate = 
           rowSums(select(., c(hard_substrate_predicted_0_30m_km2, 
                               hard_substrate_30_100m_km2, 
                               hard_substrate_100_200m_km2,
                               hard_substrate_200_3000m_km2)))) %>% 
  
  # Calculate total amount of soft substrate
  mutate(total_soft_substrate = 
           rowSums(select(., c(soft_substrate_predicted_0_30m_km2, 
                               soft_substrate_30_100m_km2, 
                               soft_substrate_100_200m_km2, 
                               soft_substratecal_200_3000m_km2)))) %>% 
  
  # Calculate depth range
  mutate(depth_range = max_depth_m - min_depth_m)

################################################################################
# Export
################################################################################
write_csv(data, file.path(out.dir, "mpa_attributes_clean.csv"))
