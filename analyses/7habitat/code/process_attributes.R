# Process Attributes for Community Analyses

library(tidyverse)
library(vegan)

# Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local

# Read Attribute (Habitat) Data
att_raw <- read_csv(file.path(base.dir, "mpa_traits", "processed", "mpa_attributes_clean.csv"))

# Process
att_data <- att_raw %>% 
  # Combine predicted and mapped columns for 0-30m
  mutate(hard_substrate_0_30m_km2_comb = hard_substrate_predicted_0_30m_km2 + hard_substrate_mapped_0_30m_km2,
         soft_substrate_0_30m_km2_comb = soft_substrate_0_30m_km2 + soft_substrate_predicted_0_30m_km2) %>% 
  # Select variables for inclusion in NMDS
  select(name,
         region = four_region_north_ci,
         size_km2,
         sandy_beach_km,
         rocky_inter_km,
         max_kelp_canopy_cdfw_km2,
         hard_substrate_0_30m_km2_comb,
         hard_substrate_30_100m_km2,
         hard_substrate_100_200m_km2,
         hard_substrate_200_3000m_km2,
         soft_substrate_0_30m_km2_comb,
         soft_substrate_30_100m_km2,
         soft_substrate_100_200m_km2,
         soft_substrate_200_3000m_km2,
         submarine_canyon_0_30m_km2,
         submarine_canyon_30_100m_km2,
         submarine_canyon_100_200m_km2,
         submarine_canyon_200_3000m_km2,
         estuary_km2,
         surfgrass_km,
         eelgrass_km2,
         coastal_marsh_km2)

# Export
saveRDS(att_data, file.path("analyses", "7habitat", "intermediate_data", "mpa_attributes_nmds_subset.Rds"))

