

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


# Format data
################################################################################

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
  # Arrange
  select(mpa, mpa_full, mpa_short, 
         authority, type, ccr, ccr_int, region_code, region, 
         area_sqkm, long_dd, lat_dd)

# Inspect
str(mpas)
freeR::complete(mpas)
table(mpas$region_code)
table(mpas$region)
table(mpas$type)

# Export data
################################################################################

# Export data
saveRDS(mpas, file=file.path(datadir, "CA_mpa_metadata.Rds"))




