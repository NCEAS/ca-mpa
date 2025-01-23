# Build habitat buffer table
# Cori Lopazanski; lopazanski@bren.ucsb.edu
# July 2024 


# Setup   ----------------------------------------------------------------------
rm(list = ls())

library(tidyverse)

int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"
hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"


# Build  ----------------------------------------------------------------------

habitat <- readRDS(file.path(hab.dir, "buffers", "habitat_buffers_combined.Rds")) 

habitat_bathy <- readRDS(file.path(ltm.dir, "site_depth.Rds")) %>% 
  mutate(site_type = if_else(site_type == "REF", "Reference", site_type))

habitat_depth <- habitat %>% # create identifier matching depth to the habitat_depth combined class
  distinct(habitat_class, habitat_depth, depth_zone)

habitat2 <- habitat %>% 
  dplyr::select(!c(depth_zone, habitat_class)) %>% 
  # Widen to fill in the appropriate zeroes across each site
  pivot_wider(names_from = habitat_depth, values_from = area_m2) %>% 
  mutate_at(vars(grep("^(hard|soft|aq|sea)", names(.), value = TRUE)), ~ replace(., is.na(.), 0)) %>% 
  # Lengthen 
  pivot_longer(cols = grep("^(hard|soft|aq|sea)", names(.), value = TRUE), names_to = "habitat_depth", values_to = "area_m2") %>% 
  left_join(habitat_depth) %>% 
  dplyr::select(habitat, site, site_type, buffer, habitat_class, habitat_depth, depth_zone, area_m2) %>% 
  # Create identifier based on habitat and buffer
  mutate(habitat_depth_buffer = paste(habitat_depth, buffer, sep = "_")) %>% 
  # Create identifier for substrate or biotic
  mutate(habitat_type = if_else(str_detect(habitat_depth, "bottom"), "substrate", "biotic")) %>% 
  # Retain biotic habitat types where the depth is 0-30m
  filter(!(habitat_type == "biotic" & depth_zone %in% c("30_100m", "100_200m", "200m")))

# Convert to wide format
habitat3 <- habitat2 %>% # should be 828
  dplyr::select(habitat, site, site_type, area_m2, habitat_depth_buffer) %>% 
  pivot_wider(names_from = "habitat_depth_buffer", values_from = "area_m2") 

# Add bathy
habitat4 <- habitat3 %>% 
  left_join(habitat_bathy) %>% 
  mutate_at(vars(grep("^depth_mean|depth_cv", names(.), value = TRUE)), ~ .x * -1)

saveRDS(habitat4, file.path(int.dir, "habitat_buffers_by_site_v2.Rds"))

# Create version that combines across depth  ----------------------------------------

habitat_combined <- habitat %>% 
  group_by(habitat, site, site_type, habitat_class, buffer) %>% 
  summarize(area_m2 = sum(area_m2, na.rm = T), .groups = 'drop') %>% 
  mutate(habitat_buffer = paste(snakecase::to_snake_case(habitat_class), buffer, sep = "_")) %>% 
  dplyr::select(-c(habitat_class, buffer)) %>% 
  pivot_wider(names_from = habitat_buffer, values_from = area_m2) %>% 
  mutate_at(vars(grep("^(hard|soft|aq|sea)", names(.), value = TRUE)), ~ replace(., is.na(.), 0)) %>% 
  left_join(habitat_bathy) %>% 
  mutate_at(vars(grep("^depth_mean|depth_cv", names(.), value = TRUE)), ~ .x * -1)

saveRDS(habitat_combined, file.path(int.dir, "habitat_buffers_by_site_combined.Rds"))









