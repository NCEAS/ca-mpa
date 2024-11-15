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

# Read Data --------------------------------------------------------------------------------------------------------------------
habitat_raw <- readRDS(file.path(int.dir, "habitat_buffers_by_site_revised.Rds")) %>% # rm _revised for old version
  rename(affiliated_mpa = mpa_orig)

habitat <- habitat_raw %>% 
  # Create identifier based on habitat and buffer
  mutate(habitat_depth_buffer = paste(habitat_depth, buffer, sep = "_")) %>% 
  dplyr::select(mpa, affiliated_mpa, site, site_type, area_m2, habitat_depth_buffer) %>% 
  pivot_wider(names_from = "habitat_depth_buffer", values_from = "area_m2") %>% 
  dplyr::select(-affiliated_mpa, -site_type)

# Kelp -------------------------------------------------------------------------------------------------------------------

kelp_raw <- readRDS(file.path(ltm.dir, "kelp_biomass_complete.Rds")) 

kelp_sites <- kelp_raw %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, affiliated_mpa, implementation_year) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, affiliated_mpa, implementation_year, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = n_before + n_after, .groups = 'drop') %>% 
  # Drop sites with no inside/outside information
  filter(!is.na(site_type)) %>%
  # Drop sites that haven't been visited at least 5 times
  filter(n_after >= 5) %>% 
  left_join(habitat)

kelp_mpas <- kelp_sites %>%
  group_by(bioregion, affiliated_mpa, implementation_year, site_type) %>%
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


# CCFRP Rock ----------------------------------------------------------------------------------------------

rock_raw <- readRDS(file.path(ltm.dir, "ccfrp_biomass_site_year.Rds")) %>% ungroup()
rock_effort_raw <- readRDS(file.path(ltm.dir, "ccfrp_site_year_effort.Rds")) %>% ungroup()

rock_sites <- rock_raw %>% 
  distinct(year, site, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  left_join(mpas %>% dplyr::select(bioregion, affiliated_mpa, implementation_year)) %>% 
  mutate(before = if_else(year <= implementation_year, 1, 0),
         after = if_else(year > implementation_year, 1, 0)) %>% 
  group_by(site, bioregion, affiliated_mpa, implementation_year, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = n_before + n_after) %>% 
  filter(!is.na(site_type)) %>% ungroup()

habitat_rock <- habitat_raw %>% 
  filter(habitat == "Rocky reef") %>% 
  filter(site %in% rock_sites$site) %>% 
  mutate(habitat_depth_buffer = paste(habitat_depth, buffer, sep = "_"))

habitat_rock_wide <- habitat_rock %>% 
  dplyr::select(mpa, affiliated_mpa, site, site_type, area_m2, habitat_depth_buffer) %>% 
  pivot_wider(names_from = "habitat_depth_buffer", values_from = "area_m2") %>% 
  dplyr::select(-affiliated_mpa, -site_type)

rock_effort <- rock_effort_raw %>% 
  filter(site %in% rock_sites$site) 

rock <- rock_raw %>% 
  filter(site %in% rock_sites$site) %>% 
  group_by(species_code, sciname, target_status, assemblage_new, year, bioregion, affiliated_mpa, site) %>% 
  summarize(bpue_kg = sum(total_bpue_kg),
            count = sum(total_count), .groups = 'drop') 

rock_assemblage <- rock %>%
  distinct(species_code, sciname, target_status, bioregion, assemblage_new)

rock_subset <- rock %>% 
  mutate(assemblage_new = case_when(sciname == "Paralabrax clathratus" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes miniatus" ~ "Hard Bottom", 
                                    T~assemblage_new)) %>% 
  filter(sciname %in% c("Semicossyphus pulcher", #sheephead
                        "Caulolatilus princeps", # ocean whitefish
                        "Sebastes atrovirens", # kelp rockfish
                        "Ophiodon elongatus", # lingcod
                        "Sebastes entomelas") | # widow rockfish
           species_code %in% c("OYT", #olive yellowtail
                               "PCLA" # kelp bass
           )) 


# Create Complete Kelp Data -------------------------------------------
rock_complete <- rock_effort %>% 
  expand_grid(species_code = unique(rock$species_code)) %>% 
  left_join(rock %>% dplyr::select(-target_status, -assemblage_new, -sciname), by = c("year", "bioregion", "affiliated_mpa", "site", "species_code")) %>% 
  left_join(rock %>% distinct(species_code, bioregion, sciname, target_status, assemblage_new), by = c("species_code", "bioregion")) %>% 
  mutate_at(vars(kg_per_m2), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count_per_m2), ~ replace(., is.na(.), 0)) %>% 
  left_join(kelp_sites %>% dplyr::select(site, region, affiliated_mpa, implementation_year, site_type)) %>% 
  mutate(region = factor(region, levels = c("North Coast", "North Central Coast", "Central Coast", "South Coast")),
         age_at_survey = year-implementation_year) %>% 
  dplyr::select(year:mpa_defacto_designation, region, implementation_year, age_at_survey, site_type, species_code, sciname, target_status, assemblage_new, kg_per_m2, count_per_m2) %>% 
  left_join(habitat_wide, by = "site") %>%
  mutate(status = factor(if_else(age_at_survey < 0, "before", "after")))









