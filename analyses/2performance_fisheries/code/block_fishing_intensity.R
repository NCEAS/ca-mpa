# Pre-MPA Fishing Intensity
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/raw"
plotdir <- "analyses/2performance_fisheries/figures"
outdir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed"

## Read Data ----
landings <- readRDS(file.path(datadir, "CDFW_2000_2020_landings_receipts.Rds"))
port_key <- readRDS(file.path(datadir, "CDFW_port_key.Rds"))
blocks <- wcfish::blocks %>% sf::st_drop_geometry()


# Build ------------------------------------------------------------------------

data <- landings %>% 
  filter(year %in% 2000:2006) %>% 
  group_by(block_id) %>% 
  summarize(total_lb = sum(landings_lb, na.rm = T)) %>% 
  ungroup() %>% 
  # Add area
  full_join(blocks %>% select(block_id, block_sqkm), by="block_id") %>% 
  # Correct NAs in total to zeroes
  mutate(total_lb = if_else(is.na(total_lb), 0, total_lb)) %>% 
  # Calculate pressure
  mutate(total_lb_sqkm = total_lb/block_sqkm) 

saveRDS(data, file.path(outdir, "block_fishing_intensity.Rds"))  
