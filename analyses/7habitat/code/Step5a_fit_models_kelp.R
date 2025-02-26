# Step5 Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the kelp forest models.

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)
library(lmerTest)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step4_build_habitat_models.R")  # Load the function from the file


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
#ltm.dir <- "/Users/lopazanski/Desktop/ltm/update_2024"

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
#pred_kelp_int  <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_interactions.Rds"))
#pred_kelp_max  <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_max.Rds"))
pred_kelp_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_2way.Rds"))

# Define subset for modeling (reduced number of columns)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg:count_per_m2, 
                all_of(pred_kelp$predictor))

# Remove sites with outliers in permanent characteristics (depth and structural characteristics)
sites_drop <- data_kelp %>% 
  dplyr::select(site, site_type, affiliated_mpa, bioregion, region4, depth_cv_100:depth_mean_500, hard_bottom_100:hard_bottom_500) %>% 
  distinct() %>% 
  mutate_at(vars(grep("^hard|soft|kelp|depth", names(.), value = TRUE)), scale) %>% 
  pivot_longer(cols = depth_cv_100:hard_bottom_500, names_to = "variable", values_to = "value") %>% 
  filter(!between(value, -3, 3)) %>% 
  pivot_wider(names_from = variable, values_from = value)


# Examine outliers in kelp forest canopy cover
kelp_drop <- data_kelp %>% 
  dplyr::select(site, site_type, affiliated_mpa, region4, year, kelp_annual_100:kelp_annual_500) %>% 
  distinct() %>% 
  group_by(year) %>% 
  summarize(mean = mean(kelp_annual_500))

data_kelp_subset <- data_kelp %>% 
  filter(!site %in% sites_drop$site)

# sites_drop <- data_kelp_subset %>% 
#   dplyr::select(site, site_type, affiliated_mpa, bioregion, region4, depth_cv_100:depth_mean_500, hard_bottom_100:hard_bottom_500) %>% 
#   distinct() %>% 
#   mutate_at(vars(grep("^hard|soft|kelp|depth", names(.), value = TRUE)), scale) %>% 
#   pivot_longer(cols = depth_cv_100:hard_bottom_500, names_to = "variable", values_to = "value") 
# 
# ggplot(data = sites_drop) +
#   geom_density(aes(x = value)) +
#   facet_wrap(~variable, scales = "free")

# Define Kelp Species Lists --------------------------------------------------------------------------
# Bioregion version
# sp_kelp <- data_kelp %>%
#   filter(kg_per_m2 > 0) %>%
#   group_by(species_code, sciname, target_status, bioregion) %>%
#   summarize(total_biomass = sum(weight_kg),
#             total_count = sum(count),
#             n_obs = n(), .groups = 'drop') %>%
#   filter(n_obs > 40) %>%
#   pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
#   mutate(south = if_else(total_count_South < 500 | is.na(n_obs_South), 0, 1),
#          central = if_else(total_count_Central < 500 | is.na(n_obs_Central), 0, 1),
#          north = if_else(total_count_North < 500 | is.na(n_obs_North), 0, 1)) %>%
#   filter(!species_code %in% c("SNEB", "GBY", "KGB"))

# Only model within the region they are prevalent in (defined as > 40 site-year observations)
# kelp_all  <- sp_kelp %>% filter(south == 1 & central == 1 & north == 1) %>% pull(species_code)
# kelp_s    <- sp_kelp %>% filter(south == 1 & central == 0 & north == 0) %>% pull(species_code)
# kelp_c    <- sp_kelp %>% filter(south == 0 & central == 1 & north == 0) %>% pull(species_code)
# kelp_n    <- sp_kelp %>% filter(south == 0 & central == 0 & north == 1) %>% pull(species_code)
# kelp_sc   <- sp_kelp %>% filter(south == 1 & central == 1 & north == 0) %>% pull(species_code)
# kelp_nc   <- sp_kelp %>% filter(south == 0 & central == 1 & north == 1) %>% pull(species_code)

# 4 region version
sp_kelp <- data_kelp_subset %>%
  filter(kg_per_m2 > 0) %>%
  group_by(species_code, sciname, target_status, region4) %>%
  summarize(total_count = sum(count),
            n_obs = n(), .groups = 'drop') %>%
  filter(total_count > 20) %>% 
  pivot_wider(names_from = region4, values_from = c(total_count, n_obs)) %>% 
  mutate(ci = if_else(`n_obs_N. Channel Islands` < 100 | is.na(n_obs_South), 0, 1),
         south = if_else(n_obs_South < 150 | is.na(n_obs_South), 0, 1),
         central = if_else(n_obs_Central < 150 | is.na(n_obs_Central), 0, 1),
         north = if_else(n_obs_North < 40 | is.na(n_obs_North), 0, 1)) 

# Channel Islands Separated Version
kelp_s     <- sp_kelp %>% filter(south == 1 & central == 0 & north == 0 & ci == 0) %>% pull(species_code)
kelp_sc    <- sp_kelp %>% filter(south == 1 & central == 1 & north == 0 & ci == 0) %>% pull(species_code)
kelp_n     <- sp_kelp %>% filter(south == 0 & central == 0 & north == 1 & ci == 0) %>% pull(species_code)

kelp_all   <- sp_kelp %>% filter(south == 1 & central == 1 & north == 1 & ci == 1) %>% pull(species_code)
kelp_ci    <- sp_kelp %>% filter(south == 0 & central == 0 & north == 0 & ci == 1) %>% pull(species_code)
kelp_c     <- sp_kelp %>% filter(south == 0 & central == 1 & north == 0 & ci == 0) %>% pull(species_code)
kelp_nc    <- sp_kelp %>% filter(south == 0 & central == 1 & north == 1 & ci == 0) %>% pull(species_code)
kelp_sci   <- sp_kelp %>% filter(south == 1 & central == 0 & north == 0 & ci == 1) %>% pull(species_code)
kelp_cci   <- sp_kelp %>% filter(south == 0 & central == 1 & north == 0 & ci == 1) %>% pull(species_code)
kelp_scci  <- sp_kelp %>% filter(south == 1 & central == 1 & north == 0 & ci == 1) %>% pull(species_code)
kelp_ncci  <- sp_kelp %>% filter(south == 0 & central == 1 & north == 1 & ci == 1) %>% pull(species_code)



# Fit models for each species --------------------------------------------------------------------
library(furrr)
library(parallel)

# Combine all species into a single list
species_list <- c(kelp_all, kelp_s, kelp_c, kelp_n, kelp_sc, kelp_nc) 
species_list <- c(kelp_all, kelp_s, kelp_c, kelp_n, kelp_sc, kelp_nc, kelp_ci, kelp_sci, kelp_cci, kelp_scci, kelp_ncci) 
species_list <- c("SPUL", "SMIN", "SMYS", "ELAT", "OPIC", "CPRI", "OELO", "SCAR", "SCAU", "EJAC")

# Define a function to process a single species - the 2-way version
run_model <- function(species) {
  # Determine the region
  region <- 
    if (species %in% kelp_all) c("Central", "North", "South", "N. Channel Islands") else
      if (species %in% kelp_s) c("South") else
        if (species %in% kelp_c) c("Central") else
          if (species %in% kelp_n) c("North") else
            if (species %in% kelp_sc) c("South", "Central") else
              if (species %in% kelp_nc) c("North", "Central") else
                if (species %in% kelp_ci) c("N. Channel Islands") else
                  if (species %in% kelp_sci) c("South", "N. Channel Islands") else
                    if (species %in% kelp_cci) c("Central", "N. Channel Islands") else
                      if (species %in% kelp_scci) c("South", "Central", "N. Channel Islands") else
                        if (species %in% kelp_ncci) c("North", "Central", "N. Channel Islands") else
                stop("Species not found in any region group")
  
  random_effects <- if(length(region) > 1) c("year", "region4", "affiliated_mpa") else c("year", "affiliated_mpa")
  
  # Run the habitat model
  results_df <- refine_habitat(
    species = species,
    response = "log_c_biomass",
    predictors_df = pred_kelp_2way, 
    random_effects = random_effects,
    data = data_kelp_subset,
    regions = region,
    path = "analyses/7habitat/output/2way-4region/kelp-reduced-rescaled"
  )
  
  return(results_df)
}

# Plan for parallel execution
num_cores <- min(length(species_list), detectCores()/3)  
plan(multisession, workers = num_cores)

# Run models in parallel
future_walk(species_list, run_model)


# Options that have been run so far and their locations:
# 2way-: Two way interactions only 
#   4region: run with n. channel islands as separate region
#   with-depth-comb: includes options for depth_mean + depth_cv in same model (otherwise are separate)
#   rmre: removes the random effect for MPA (still keeps region and year) b/c soaks up lots of variability
#   -reduced: drop sites with outliers in static habitat vars and drop sites where species infrequently observed (< 10% of years)
#   -rescaled: adjust scaling so static vars are scaled to site-level means and kelp is scaled within each year




