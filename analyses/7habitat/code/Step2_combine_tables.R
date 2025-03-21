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

habitat_kelp <- readRDS(file.path(kw.dir, "kelp_site_buffers.Rds")) %>% dplyr::select(-habitat, -site_id) %>% distinct()

# Kelp -------------------------------------------------------------------------------------------------------------------

kelp_raw <- readRDS(file.path(ltm.dir, "kelp_biomass_subset.Rds")) 

kelp <- kelp_raw %>%
 # left_join(habitat) %>% 
  left_join(habitat_combined) %>% 
  left_join(habitat_kelp) 

# CASPAR_2 did not have depth data; too shallow
unique(kelp$site[is.na(kelp$depth_mean_25)])

kelp <- kelp %>% filter(site != "CASPAR_2")  # too shallow

# Examine the range for each habitat characteristic
kelp_sites <- kelp %>% 
  dplyr::select(site, site_type, affiliated_mpa, hard_bottom_25:depth_cv_500) %>% distinct() %>% 
  pivot_longer(cols = hard_bottom_25:depth_cv_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "aquatic|soft|seagrass|depth_sd")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+"))) %>% 
  mutate(habitat = str_remove(habitat_variable, "_\\d+")) %>% 
  arrange(desc(habitat), scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable)))

kelp_sites_scaled <- kelp %>% 
  distinct(site, site_type, affiliated_mpa, across(all_of(grep("^hard|depth_mean|depth_cv", names(.), value = TRUE)))) %>%
  mutate_at(vars(grep("^hard|depth_mean|depth_cv", names(.), value = TRUE)), scale) %>% 
  pivot_longer(cols = hard_bottom_25:depth_cv_500, names_to = "habitat_variable", values_to = "value") %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+"))) %>% 
  mutate(habitat = str_remove(habitat_variable, "_\\d+")) %>% 
  arrange(desc(habitat), scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable)))

kelp_sites_flagged <- kelp_sites_scaled %>% 
  filter(!between(value, -3.5, 3.5))

kelp_sites_balanced <- kelp %>% 
  filter(!site %in% kelp_sites_flagged$site) %>% 
  distinct(site, site_type, affiliated_mpa, year) %>% 
  group_by(affiliated_mpa, site_type) %>% 
  summarize(n_site_year = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = n_site_year) %>% 
  filter(is.na(MPA) | is.na(Reference))

ggplot(data = kelp_sites %>% 
        filter(!site %in% kelp_sites_flagged$site) %>% 
        filter(!affiliated_mpa %in% kelp_sites_balanced$affiliated_mpa)
         ) +
  geom_density(aes(x = value, color = site_type, fill = site_type), alpha = 0.3) + 
  labs(x = "Value of habitat characteristic", y = "Density", fill = NULL, color = NULL)+
  theme_minimal() + 
  theme(legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable, scales = "free", ncol = 5)


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
saveRDS(kelp, file.path(ltm.dir, "combine_tables/kelp_full.Rds"))  # Last write 7 Mar 2025
saveRDS(surf, file.path(ltm.dir, "combine_tables/surf_full.Rds"))  # Last write 7 Mar 2025
saveRDS(rock, file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) # Last write 7 Mar 2025
saveRDS(deep, file.path(ltm.dir, "combine_tables/deep_full.Rds"))  # Last write 7 Mar 2025

