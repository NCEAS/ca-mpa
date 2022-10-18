

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- file.path(basedir, "mpa_traits/processed")
plotdir <- "analyses/3performance_human/figures"

# Read MPA file
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))

# Blocks
blocks <- wcfish::blocks


# Format data
################################################################################

# Find block MPA falls inside
sp::over(mpas_orig %>% sf::as_Spatial(), blocks %>% sf::as_Spatial())
dist_mat <- sf::st_distance(mpas_orig, blocks)
rownames(dist_mat) <- mpas$mpa
colnames(dist_mat) <- blocks$block_id
dist_mat_df <- dist_mat %>% 
  as.data.frame() %>% 
  rownames_to_column(var="mpa") %>% 
  gather(key="block_id", value="dist_m", 2:ncol(.)) %>% 
  # Find closest MPA
  group_by(mpa) %>% 
  arrange(mpa, dist_m) %>% 
  slice(1) %>% 
  ungroup()

# MLPA MPA types
mlpa_types <- c("SMR", "SMCA", "SMCA (No-Take)", "SMRMA", "SMP")

# Format data
mpas <- mpas_orig %>% 
  # Drop geometry
  sf::st_drop_geometry() %>% 
  # Rename
  rename(mpa=name, 
         mpa_full=name_full,
         mpa_short=name_short,
         region_code=region) %>% 
  # Add region
  mutate(region=recode_factor(region_code,
                              "NCSR"="North Coast",  
                              "NCCSR"="North Central Coast", 
                              "SFBSR"="San Francisco Bay",
                              "CCSR"="Central Coast", 
                              "SCSR"="South Coast")) %>% 
  # Add authority
  mutate(authority=ifelse(type %in% c("FMCA", "FMR", "Special Closure"), "Federal", "State")) %>% 
  # Mark whether part of the MLPA network
  mutate(mlpa=ifelse(region!="San Francisco Bay" & type %in% mlpa_types, "MLPA", "Non-MLPA")) %>% 
  # Add block id
  left_join(dist_mat_df %>% select(mpa, block_id), by="mpa") %>% 
  # Arrange
  select(mpa, mpa_full, mpa_short, mlpa,
         authority, type, ccr, ccr_int, region_code, region, block_id, 
         area_sqkm, long_dd, lat_dd)

# Inspect
str(mpas)
freeR::complete(mpas)
table(mpas$region_code)
table(mpas$region)
table(mpas$type)
table(mpas$mlpa)

# Export data
################################################################################

# Export data
saveRDS(mpas, file=file.path(datadir, "CA_mpa_metadata.Rds"))




