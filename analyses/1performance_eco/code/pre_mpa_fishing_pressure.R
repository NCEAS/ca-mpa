
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)
library(tidycensus)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "data")
plotdir <- file.path(basedir, "figures")
traitdir <- file.path(basedir, "mpa_traits/processed")
outputdir <- file.path("analyses/1performance_eco/output")

# Read MPAs
mpas <- readRDS(file.path(traitdir, "CA_mpa_metadata.Rds"))

# Read data
landings_orig <- readRDS("/Users/cfree/Dropbox/Chris/UCSB/projects/california/cdfw_data/data/confidential/landings_receipts/processed/CDFW_2000_2020_landings_receipts.Rds")

# Read blocks
blocks <- wcfish::blocks

# Summarize catch by block
################################################################################

# Build data
landings <- landings_orig %>% 
  # 2000-2007
  filter(year %in% 2000:2006) %>% 
  # Summarize
  group_by(block_id,) %>% 
  summarize(landings_lb=sum(landings_lb, na.rm=T)) %>% 
  ungroup() %>% 
  # Add area
  left_join(blocks %>% select(block_id, block_sqkm), by="block_id") %>% 
  # Calculate pressure desity
  mutate(landings_lb_sqkm=landings_lb/block_sqkm)

# Format data
data <- mpas %>% 
  # Convert block id
  mutate(block_id=as.numeric(block_id)) %>% 
  # Add 2000-2007 sum landings
  left_join(landings, by="block_id") %>% 
  rename(landings_lb_20002006=landings_lb,
         landings_lb_sqkm_20002006=landings_lb_sqkm) %>% 
  # Simplify
  select(mpa, landings_lb_20002006, landings_lb_sqkm_20002006)

# Export
saveRDS(data, file=file.path(outputdir, "pre_mpa_fishing_pressure_by_mpa.Rds"))




