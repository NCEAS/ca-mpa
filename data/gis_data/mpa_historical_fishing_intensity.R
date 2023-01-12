# Calculates historical fishing intensity for each MPA
# Cori Lopazanski
# Jan 2023

# Setup ------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

## Packages ----
library(tidyverse)
library(countrycode)
library(tidycensus)

## Directories ----
basedir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/raw"
plotdir <- "analyses/2performance_fisheries/figures"
outdir <- "/Volumes/GoogleDrive-105151121202188525604/My Drive/Research/NCEAS - California MPA Working Group/fisheries-data/processed"

## Read Data ----
landings_raw <- readRDS(file.path(datadir, "CDFW_2000_2020_landings_receipts.Rds"))
#port_key <- readRDS(file.path(datadir, "CDFW_port_key.Rds"))
blocks <- wcfish::blocks %>% sf::st_drop_geometry()

mpa_block_pairs <- readRDS(file.path(basedir, "gis_data/processed", "mpa_block_overlap_pairs.Rds"))

# Build ------------------------------------------------------------------------
landings <- landings_raw %>% 
  # Filter for only pre-implementation years (2000-2006)
  filter(year %in% c(2000:2006)) %>% 
  # Calculate total annual landings for each block
  group_by(block_id, year) %>% 
  summarize(annual_lb = sum(landings_lb, na.rm = T)) %>% 
  # Average annual landings for each block
  group_by(block_id) %>% 
  summarize(annual_avg_lb = mean(annual_lb, na.rm = T)) %>% 
  ungroup() %>% 
  # Add block area 
  left_join(blocks %>% select(block_id, block_sqkm), by = "block_id") %>% 
  # Calculate landings per square kilometer
  mutate(annual_avg_lb_sqkm = annual_avg_lb/block_sqkm) 

data <- mpa_block_pairs %>% 
  # Join landings for each block with block pairs 
  left_join(landings, by="block_id") %>% 
  group_by(name) %>% 
  summarize(annual_avg_lb_sqkm_20002006 = sum(annual_avg_lb_sqkm))

# Export
saveRDS(data, file.path("analyses/2performance_fisheries/analyses/blocks/pre_mpa_fishing_pressure_by_mpa.Rds"))


