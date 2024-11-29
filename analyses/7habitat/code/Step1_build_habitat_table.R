# Build habitat buffer table
# Cori Lopazanski; lopazanski@bren.ucsb.edu
# July 2024 


# Setup   ----------------------------------------------------------------------
rm(list = ls())

library(tidyverse)
library(janitor)
library(sf)
library(dplyr)
library(purrr)
library(stringr)

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"
hab.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed/combined/combined_mlpa_sites_1000m"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

# Read  ----------------------------------------------------------------------
buffers <- c(25, 50, 100, 250, 500)  
buffer_dirs <- file.path(hab.dir, "buffers", paste0(buffers, "m"))

rds_files <- unlist(lapply(buffer_dirs, function(dir) {
  list.files(dir, pattern = "combined_hsb_revised", full.names = TRUE) # rm _revised for old version
}))

# Read each RDS file into a dataframe, drop geometry, and add the buffer column
combined_df <- rds_files %>% 
  lapply(function(file) {
    buffer <- str_extract(file, "_\\d+m\\.Rds$") %>% str_remove_all("[_m\\.Rds]") %>% as.numeric()
    readRDS(file) %>% 
      st_drop_geometry() %>%
      mutate(buffer = buffer)
  }) %>%
  bind_rows()


# Save the combined dataframe
saveRDS(combined_df, file.path(hab.dir, "buffers/combined_hsb_revised_buffers.Rds")) # rm _revised for old version

# Build  ----------------------------------------------------------------------

habitat_vars <- c("hard_bottom_landward", "hard_bottom_0_30m", "hard_bottom_30_100m", "hard_bottom_100_200m", "hard_bottom_200m",
                  "soft_bottom_landward", "soft_bottom_0_30m","soft_bottom_30_100m", "soft_bottom_100_200m", "soft_bottom_200m",
                  "hard_bottom_biotic_0_30m", "hard_bottom_biotic_30_100m", "hard_bottom_biotic_100_200m",
                  "soft_bottom_biotic_0_30m","soft_bottom_biotic_30_100m", "soft_bottom_biotic_100_200m",     
                  "biotic_0_30m","biotic_30_100m")  

combined_df <- readRDS(file.path(hab.dir, "buffers/combined_hsb_revised_buffers.Rds"))

habitat <- combined_df %>% 
  filter(!is.na(habitat_class)) %>% # "Drop" class from the update to biotic habitat variables
  mutate(PMEP_Zone = as.numeric(PMEP_Zone)) %>% 
  mutate(habitat_class = snakecase::to_snake_case(habitat_class)) %>% 
  # Combine biotic-only into hard bottom (for now)
  pivot_wider(names_from = "habitat_class", values_from = "area_m2") %>% 
  mutate(hard_bottom_biotic = if_else(!(is.na(hard_bottom_biotic) & is.na(biotic)), rowSums(across(c(hard_bottom_biotic, biotic)), na.rm = TRUE), NA)) %>% 
  dplyr::select(-biotic) %>% 
  pivot_longer(c(soft_bottom, hard_bottom, hard_bottom_biotic, soft_bottom_biotic), names_to = "habitat_class", values_to = "area_m2", values_drop_na = T) %>% 
  mutate(depth_zone = factor(case_when(PMEP_Zone == 0 ~ "Landward",
                                       PMEP_Zone %in% c(1, 2, 3) ~ "Shoreline to -30m",
                                       PMEP_Zone %in% c(4, 5) ~ "-30m to -100m",
                                       PMEP_Zone %in% c(6, 7) ~ "-100m to -200m",
                                       PMEP_Zone == 8 ~ "-200m or International Waters"),
                             levels = c("Landward", "Shoreline to -30m",
                                        "-30m to -100m", "-100m to -200m", "-200m or International Waters")),
         depth_zone_simple = factor(case_when(depth_zone == "Landward" ~ "landward",
                                       depth_zone == "Shoreline to -30m" ~ "0_30m",
                                       depth_zone == "-30m to -100m" ~ "30_100m",
                                       depth_zone == "-100m to -200m" ~ "100_200m", 
                                       depth_zone == "-200m or International Waters" ~ "200m"),
                                    levels = c("landward", "0_30m", "30_100m", "100_200m", "200m"))) %>% 
  # There are a few sites that span across sections - this grouping will summarize those totals
  group_by(habitat, mpa, mpa_orig, site, site_type,
           habitat_class, buffer, depth_zone, depth_zone_simple) %>% 
  summarize(area_m2 = sum(area_m2, na.rm = T), .groups = 'drop') %>% 
  mutate(habitat_depth = paste0(habitat_class, "_", depth_zone_simple))

site_depth_area <- habitat %>% # calculate total area for each depth zone
  group_by(habitat, mpa, mpa_orig, site, site_type, buffer, depth_zone_simple) %>% 
  summarize(area_depth_m2 = sum(area_m2)) %>% ungroup()

site_area <- habitat %>% # calculate total area of each site
  group_by(habitat, mpa, mpa_orig, site, site_type, buffer) %>% 
  summarize(area_site_m2 = sum(area_m2)) %>% ungroup()

habitat_depth <- habitat %>% # create identifier matching depth to the habitat_depth combined class
  distinct(habitat_class, habitat_depth, depth_zone_simple)

habitat2 <- habitat %>% 
  dplyr::select(!c(depth_zone, depth_zone_simple, habitat_class)) %>% 
  # Widen to fill in the appropriate zeroes across each site
  pivot_wider(names_from = habitat_depth, values_from = area_m2) %>% 
  mutate_at(vars(grep("^(hard|soft)", names(.), value = TRUE)), ~ replace(., is.na(.), 0)) %>% 
  # Reclassify the biotic in deeper depth zones as just the hard/soft b/c data not consistently mapped:
  mutate(hard_bottom_30_100m = rowSums(across(c(hard_bottom_30_100m, hard_bottom_biotic_30_100m)), na.rm = T),
         soft_bottom_30_100m = rowSums(across(c(soft_bottom_30_100m, soft_bottom_biotic_30_100m)), na.rm = T),
         soft_bottom_100_200m = rowSums(across(c(soft_bottom_100_200m, soft_bottom_biotic_100_200m)), na.rm = T)) %>% 
  dplyr::select(!c(hard_bottom_biotic_30_100m, soft_bottom_biotic_30_100m, soft_bottom_biotic_100_200m)) %>% 
  # Lengthen
  pivot_longer(cols = grep("^(hard|soft)", names(.), value = TRUE), names_to = "habitat_depth", values_to = "area_m2") %>% 
  left_join(habitat_depth) %>% 
  left_join(site_depth_area) %>% 
  left_join(site_area) %>% 
  mutate_at(vars(area_depth_m2), ~ replace(., is.na(.), 0)) %>% 
  dplyr::select(habitat, mpa, affiliated_mpa = mpa_orig, site, site_type, buffer, habitat_class, habitat_depth, depth_zone = depth_zone_simple, area_m2, area_depth_m2, area_site_m2) %>% 
  # Create identifier based on habitat and buffer
  mutate(habitat_depth_buffer = paste(habitat_depth, buffer, sep = "_")) 

saveRDS(habitat2, file.path(int.dir, "habitat_buffers_by_site_revised.Rds"))  # rm _revised for old version
# Last write: 25 Nov 2024

habitat3 <- habitat2 %>% 
  dplyr::select(mpa, affiliated_mpa, site, site_type, area_m2, habitat_depth_buffer) %>% 
  pivot_wider(names_from = "habitat_depth_buffer", values_from = "area_m2") 

habitat3_na <- habitat3[rowSums(is.na(habitat3)) > 0, ] # still som NA to deal with...

habitat_mpa <- habitat %>% 
  # Find the total and average for each habitat class, across all the sites affiliated with each MPA
  group_by(habitat, mpa, mpa_orig, site_type, buffer, habitat_depth) %>% 
  summarize(total_area_m2 = sum(area_m2),
            mean_area_m2 = mean(area_m2), .groups = 'drop') 

kelp <- habitat2 %>% filter(habitat == "Kelp")

ggplot(data = kelp %>% filter(buffer == 50)) +
  geom_bar(aes(x = area_m2/area_depth_m2, y = site, fill = habitat_class), stat = "identity", position = "stack") + 
  facet_wrap(~depth_zone)
