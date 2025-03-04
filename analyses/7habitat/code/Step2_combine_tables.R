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
habitat <- readRDS(file.path(int.dir, "habitat_buffers_by_site_v3.Rds")) %>% # v2 has old depth, v3 only updated for KF
  dplyr::select(-habitat) %>% ungroup()

# Estimates for area of each habitat by buffer (across all depths)
habitat_combined <- readRDS(file.path(int.dir, "habitat_buffers_by_site_combined_v3.Rds")) %>% 
  dplyr::select(-habitat) %>% ungroup()

habitat_kelp <- readRDS(file.path(kw.dir, "kelp_site_buffers.Rds")) %>% dplyr::select(-habitat, -site_id)

# Kelp -------------------------------------------------------------------------------------------------------------------

kelp_raw <- readRDS(file.path(ltm.dir, "kelp_biomass_subset.Rds")) 

kelp <- kelp_raw %>%
 # left_join(habitat) %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

# Rock (CCFRP) ----------------------------------------------------------------------------------------------
rock_raw <- readRDS(file.path(ltm.dir, "rock_biomass_subset.Rds")) 

rock <- rock_raw %>% 
  #left_join(habitat) %>%
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

# Surf zone (seines) ----------------------------------------------------------------------------------------------

surf_raw <- readRDS(file.path(ltm.dir, "surf_biomass_subset.Rds")) 

surf <- surf_raw %>% 
 # left_join(habitat) %>%
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

# Deep ----------------------------------------------------------------------------------------------
deep_raw <- readRDS(file.path(ltm.dir, "deep_biomass_subset.Rds")) 

# Since we have upscaled deep from transect to dive, pull the IDs to match the new sites
deep_match <- readRDS(file.path(ltm.dir, "deep_biomass_complete.Rds")) %>% 
  distinct(year, site, site_type, dive) %>% 
  mutate(year_dive_type = paste(year, dive, site_type, sep = "-")) %>% 
  dplyr::select(-dive)

# Join the habitat data with the transect-level metadata, summarize across "sites"
deep_site <- deep_match %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) %>% 
  dplyr::select(-site) %>% 
  group_by(year, year_dive_type, site_type) %>% 
  summarize(across(everything(), mean, na.rm = TRUE), .groups = 'drop') %>% 
  rename(site = year_dive_type)

# Join site data to fish data
deep <- deep_raw %>% 
  left_join(deep_site)

# deep_subset2 <- deep_subset %>% 
#   mutate(year_dive_type = paste(year, dive, site_type, sep = "-")) %>% 
#   group_by(year, year_dive_type, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class,
#            mpa_defacto_designation, implementation_year, size_km2, age_at_survey, species_code, sciname, genus,
#            target_status, assemblage, assemblage_new) %>% 
#   summarize(biomass_kg = sum(biomass_kg),
#             count = sum(count),
#             kg_per_m2 = sum(kg_per_m2)/n(),
#             count_per_m2 = sum(count_per_m2)/n(), .groups = 'drop')
# 
# deep_subset3 <- deep_subset2 %>% 
#   rename(site = year_dive_type)

# deep <- deep_raw %>%  
#  # left_join(habitat) %>% 
#   left_join(habitat_combined) %>% 
#   left_join(habitat_kelp) 


# Export 
saveRDS(kelp, file.path(ltm.dir, "combine_tables/kelp_full.Rds"))  # Last write 2 Mar 2025
saveRDS(surf, file.path(ltm.dir, "combine_tables/surf_full.Rds"))  # Last write 21 Feb 2025
saveRDS(rock, file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) # Last write 2 Mar 2025
saveRDS(deep, file.path(ltm.dir, "combine_tables/deep_full.Rds"))  # Last write 3 Mar 2025

