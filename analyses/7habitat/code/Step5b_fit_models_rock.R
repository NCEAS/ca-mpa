# Step5b Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the rocky reef models.

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

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% mutate(site_type = factor(site_type, levels = c("Reference", "MPA")))
pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_rock_int <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors_interactions.Rds"))
pred_rock_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors_2way.Rds"))


## Define Species Lists ------------------------------------------
sp_rock <- data_rock %>%
  filter(weight_kg > 0) %>%
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(weight_kg),
            total_count = sum(count),
            n_obs = n(), .groups = 'drop') %>%
  filter(n_obs > 100) %>% 
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
  mutate(south = if_else(n_obs_South < 40 | is.na(n_obs_South), 0, 1),
         central = if_else(n_obs_Central < 40 | is.na(n_obs_Central), 0, 1),
         north = if_else(n_obs_North < 40 | is.na(n_obs_North), 0, 1))


# Only model within the region they are prevalent in (defined as > 40 site-year observations)
rock_all <- sp_rock %>% filter(south == 1 & central == 1 & north == 1) %>% pull(species_code)
rock_s   <- sp_rock %>% filter(south == 1 & central == 0 & north == 0) %>% pull(species_code)
rock_c   <- sp_rock %>% filter(south == 0 & central == 1 & north == 0) %>% pull(species_code)
rock_n   <- sp_rock %>% filter(south == 0 & central == 0 & north == 1) %>% pull(species_code)
rock_sc  <- sp_rock %>% filter(south == 1 & central == 1 & north == 0) %>% pull(species_code)
rock_nc  <- sp_rock %>% filter(south == 0 & central == 1 & north == 1) %>% pull(species_code)


# Define subset for modeling (reduced number of columns) 
data_rock_subset <- data_rock %>%
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg, count, log_bpue_kg,
                all_of(pred_rock$predictor))

# Fit models for each species --------------------------------------------------------------------
library(furrr)
library(parallel)

# Combine all species into a single list
species_list <- c(rock_all, rock_s, rock_c, rock_n, rock_sc, rock_nc)

# Define a function to process a single species
run_model <- function(species) {
  # Determine the region
  region <- 
    if (species %in% rock_all) c("Central", "North", "South") else
      if (species %in% rock_s) c("South") else
        if (species %in% rock_c) c("Central") else
          if (species %in% rock_n) c("North") else
            if (species %in% rock_sc) c("South", "Central") else
              if (species %in% rock_nc) c("North", "Central") else
                stop("Species not found in any region group")
  
  # Run the habitat model
  results_df <- refine_habitat(
    species = species,
    response = "log_c_biomass",
    predictors_df = pred_rock_2way,
    random_effects = ifelse(length(region) > 1, 
                            c("year", "bioregion", "affiliated_mpa"), 
                            c("year", "affiliated_mpa")),
    data = data_rock_subset,
    regions = region,
    path = "analyses/7habitat/output/2way/rock"
  )
  
  return(results_df)
}

# Plan for parallel execution
num_cores <- min(length(species_list), detectCores()/3)  
plan(multisession, workers = num_cores)

# Run models in parallel
future_walk(species_list, run_model)


