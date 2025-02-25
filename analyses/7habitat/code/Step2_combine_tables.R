# Cori Lopazanski
# August 2024

# About ------------------------------------------------------------------------------------

# Merge the species, habitat, and monitoring tables into one df for models

# Setup -------------------------------------------------------------------------------------------------------------------------
library(tidyverse) 

rm(list = ls())

# Directories
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"
kw.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2024/processed"

# Read Data --------------------------------------------------------------------------------------------------------------------
# Estimates for area of each habitat type stratified by depth and buffer
habitat <- readRDS(file.path(int.dir, "habitat_buffers_by_site_v2.Rds")) %>% 
  dplyr::select(-habitat)

# Estimates for area of each habitat by buffer (across all depths)
habitat_combined <- readRDS(file.path(int.dir, "habitat_buffers_by_site_combined.Rds")) %>% 
  dplyr::select(-habitat)

habitat_kelp <- readRDS(file.path(kw.dir, "kelp_site_buffers.Rds"))

# Kelp -------------------------------------------------------------------------------------------------------------------

kelp_raw <- readRDS(file.path(ltm.dir, "kelp_biomass_subset.Rds")) 

kelp <- kelp_raw %>%
  left_join(habitat) %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

# Rock (CCFRP) ----------------------------------------------------------------------------------------------
rock_raw <- readRDS(file.path(ltm.dir, "rock_biomass_subset.Rds")) 

rock <- rock_raw %>% 
  left_join(habitat) %>%
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

# Surf zone (seines) ----------------------------------------------------------------------------------------------

surf_raw <- readRDS(file.path(ltm.dir, "surf_biomass_subset.Rds")) 

surf <- surf_raw %>% 
  left_join(habitat) %>%
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

# Deep ----------------------------------------------------------------------------------------------
deep_raw <- readRDS(file.path(ltm.dir, "deep_biomass_subset.Rds")) 

deep <- deep_raw %>%  
  left_join(habitat) %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 


# Export 
saveRDS(kelp, file.path(ltm.dir, "combine_tables/kelp_full.Rds"))  # Last write 21 Feb 2025
saveRDS(surf, file.path(ltm.dir, "combine_tables/surf_full.Rds"))  # Last write 21 Feb 2025
saveRDS(rock, file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) # Last write 21 Feb 2025
saveRDS(deep, file.path(ltm.dir, "combine_tables/deep_full.Rds"))  # Last write 21 Feb 2025

