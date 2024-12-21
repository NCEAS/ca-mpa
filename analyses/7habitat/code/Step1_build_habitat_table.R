# Build habitat buffer table
# Cori Lopazanski; lopazanski@bren.ucsb.edu
# July 2024 


# Setup   ----------------------------------------------------------------------
rm(list = ls())

library(tidyverse)

int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"
hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed_v2/combined"

# Build  ----------------------------------------------------------------------

habitat <- readRDS(file.path(hab.dir, "buffers", "habitat_buffers_combined.Rds")) 

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

saveRDS(habitat2, file.path(int.dir, "habitat_buffers_by_site_v2.Rds"))
