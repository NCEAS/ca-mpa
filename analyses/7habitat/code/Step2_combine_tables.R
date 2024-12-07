# Cori Loapzanski
# August 2024

# About ------------------------------------------------------------------------------------

# Merge the species, habitat, and monitoring tables into one df for models

# Setup -------------------------------------------------------------------------------------------------------------------------
library(tidyverse) 
rm(list = ls())

# Directories
fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
int.dir <- "~/ca-mpa/analyses/7habitat/intermediate_data"
kw.dir <- "/home/shares/ca-mpa/data/sync-data/kelpwatch/2024/processed"

# Read Data --------------------------------------------------------------------------------------------------------------------
habitat_raw <- readRDS(file.path(int.dir, "habitat_buffers_by_site_revised.Rds")) %>% # rm _revised for old version
  filter(!habitat == "Rocky intertidal")

# Original version of habitat (hard, soft, biotic, etc.)
habitat <- habitat_raw %>% # should be 913 sites (all except RI; 986 in full)
  dplyr::select(mpa, affiliated_mpa, site, site_type, area_m2, habitat_depth_buffer) %>% 
  pivot_wider(names_from = "habitat_depth_buffer", values_from = "area_m2") %>% 
  dplyr::select(-affiliated_mpa, -site_type) 

# Revised version after incorporating kelpwatch (retaining both for now)
# This changes anything _biotic to just a substrate instance, and combines the hard/soft within each depth/buffer
habitat_revised <- habitat_raw %>%
  mutate(across(c(habitat_class, habitat_depth, habitat_depth_buffer), ~ str_remove(., "_biotic"))) %>% 
  filter(!habitat_depth %in% c("hard_bottom_200m", "soft_bottom_200m")) %>% # drop the 200m category for now
  filter(!str_detect(habitat_depth, "landward")) %>% # drop landward for now
  group_by(habitat, mpa, affiliated_mpa, site, site_type, 
           buffer, habitat_class, habitat_depth, depth_zone, habitat_depth_buffer) %>% 
  summarize(area_m2 = sum(area_m2, na.rm = T), .groups = 'drop') %>% 
  dplyr::select(mpa, affiliated_mpa, site, site_type, area_m2, habitat_depth_buffer) %>% 
  pivot_wider(names_from = "habitat_depth_buffer", values_from = "area_m2") %>% 
  dplyr::select(-affiliated_mpa, -site_type) 

  
kelp_watch <- readRDS(file.path(kw.dir, "kelp_site_buffers.Rds"))

# Kelp -------------------------------------------------------------------------------------------------------------------

kelp_raw <- readRDS(file.path(ltm.dir, "kelp_biomass_complete.Rds")) 

kelp_sites <- kelp_raw %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, affiliated_mpa, implementation_year, size_km2) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, affiliated_mpa, implementation_year, size_km2, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = n_before + n_after, .groups = 'drop') %>% 
  # Drop sites with no inside/outside information
  filter(!is.na(site_type)) %>%
  # Drop sites that haven't been visited at least 5 times
  filter(n_after >= 5) %>% 
  left_join(habitat_revised)

kelp_mpas <- kelp_sites %>%
  group_by(bioregion, affiliated_mpa, implementation_year, size_km2, site_type) %>%
  summarize(n_total = sum(n_total), .groups = 'drop') %>%
  pivot_wider(names_from = site_type, values_from = n_total) %>% 
  filter(!is.na(Reference)) %>% 
  filter(!is.na(MPA))

kelp <- kelp_raw %>% 
  # Drop observations for dropped sites 
  filter(site %in% kelp_sites$site) %>% 
  # Drop observations for dropped MPAs
  filter(affiliated_mpa %in% kelp_mpas$affiliated_mpa) %>% 
  # Drop MPAs that are not defacto SMRs
  filter(mpa_defacto_class == "smr") %>% 
  # Drop before data
  filter(age_at_survey >= 0) %>% 
  # Join habitat and site visitation information
  left_join(kelp_sites) %>% 
  # Join annual kelp canopy estimates
  left_join(kelp_watch) %>% 
  # Log-transformed biomass
  mutate(log_kg_per_m2 = log(kg_per_m2 + 1))


# sp <- data %>% 
#   filter(kg_per_m2 > 0) %>% 
#   group_by(species_code, sciname, target_status, bioregion) %>% 
#   summarize(total_biomass = sum(kg_per_m2),
#             total_count = sum(count_per_m2),
#             n_obs = n()) %>% 
#   filter(n_obs > 20) %>% dplyr::select(species_code, sciname, target_status, bioregion, total_biomass) %>% 
#   pivot_wider(names_from = "bioregion", values_from = "total_biomass") %>% 
#   filter(North > 0 & Central > 0 & South > 0)

### Test Transformations 

# Box-Cox 
# 
# species_list <- unique(data$species_code)
# transformed_list <- list()
# error_species <- list()
# 
# for (species in species_list) {
#   data_sp <- data %>% 
#     filter(species_code == species) %>% 
#     mutate(kg_per_m2_0.01 = kg_per_m2 + 0.01)
#   
#   tryCatch({
#     model <- lm(kg_per_m2_0.01 ~ age_at_survey * site_type, data = data_sp)
#     bc <- boxcox(model, lambda = seq(-4, 4, 0.1))
#     best_lambda <- bc$x[which.max(bc$y)]
#     
#     # Apply Box-Cox transformation
#     data_sp$bc_kg_per_m2 <- if (best_lambda >= -2 && best_lambda <= 2) 
#     {(data_sp$kg_per_m2_0.01^best_lambda - 1) / best_lambda} 
#     else {NA}
#     
#     data_sp$lambda <- if (best_lambda >= -2 && best_lambda <= 2) {best_lambda} else {NA}
#     
#     # Store transformed data
#     transformed_list[[species]] <- data_sp
#     
#   }, error = function(e){
#     message(paste("Box-Cox transformation error for species:", species))
#     message("Error details:", e$message)
#     error_species <<- c(error_species, species)
#   })
#   
# }
# 
# # Combine all transformed data back into a single data frame
# data_transformed <- bind_rows(transformed_list)
# 


# Rock (CCFRP) ----------------------------------------------------------------------------------------------

rock_raw <- readRDS(file.path(ltm.dir, "rock_biomass_complete.Rds")) 

rock_sites <- rock_raw %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, affiliated_mpa, mpa_defacto_class, implementation_year) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, affiliated_mpa, mpa_defacto_class, implementation_year, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after), # max visited is 16; most visited <5 times
            n_total = n_before + n_after, .groups = 'drop') %>% 
  # Decide to not filter grid cells because there are so many options for each MPA/Ref
  left_join(habitat_revised)

rock_mpas <- rock_sites %>%
  group_by(bioregion, affiliated_mpa, mpa_defacto_class, implementation_year, site_type) %>%
  summarize(n_total = sum(n_total), .groups = 'drop') %>%
  pivot_wider(names_from = site_type, values_from = n_total) %>% 
  filter(MPA > 10 & Reference > 10) # at least 10 site-years; 15 MPAs total

rock <- rock_raw %>% 
  # Drop observations for dropped MPAs
  filter(affiliated_mpa %in% rock_mpas$affiliated_mpa) %>% 
  # Drop before data
  filter(age_at_survey >= 0) %>% 
  # Join habitat and site visitation information
  left_join(rock_sites) %>% 
  # Join kelp canopy annual estimates 
  left_join(kelp_watch, by = c("year", "site", "site_type")) %>% 
  # Log-transformed biomass
  mutate(log_bpue_kg = log(weight_kg + 1))


# Surf zone (seines) ----------------------------------------------------------------------------------------------

surf_raw <- readRDS(file.path(ltm.dir, "surf_biomass_complete.Rds")) 

surf_sites <- surf_raw %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, affiliated_mpa, mpa_defacto_class, implementation_year) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, affiliated_mpa, mpa_defacto_class, implementation_year, site_type) %>% 
  summarize(n_total = n(), .groups = 'drop') %>% # Started in 2019 so no need to do breakdown
  left_join(habitat_revised)

# Even sampling across years and MPAs, no neeed to filter

surf <- surf_raw %>% 
  # Join habitat and site visitation information
  left_join(surf_sites) %>% 
  # Join kelp canopy annual estimates
  left_join(kelp_watch) %>% 
  # Log-transformed biomass
  mutate(log_kg_per_haul = log(kg_per_haul + 1))

# Export 

saveRDS(kelp, file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds"))  # Last write 7 Dec 2024
saveRDS(surf, file.path(ltm.dir, "combine_tables/surf_combine_table.Rds"))  # Last write 7 Dec 2024
saveRDS(rock, file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) # Last write 7 Dec 2024

