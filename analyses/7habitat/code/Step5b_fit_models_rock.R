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

# Fit models for each species list -----------------------------------------------------

walk(rock_all, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_c_biomass",
                               predictors_df = pred_rock_int, 
                               random_effects = c("year", "bioregion", "affiliated_mpa"), 
                               data = data_rock_subset, 
                               regions = c("Central", "North", "South"),
                               path = "analyses/7habitat/output/rock/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 10))
})

walk(rock_s, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_c_biomass",
                               predictors_df = pred_rock_int, 
                               random_effects = c("year", "affiliated_mpa"),
                               data = data_rock_subset,
                               regions = c("South"), 
                               path = "analyses/7habitat/output/rock/south")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 10))
})

walk(rock_c, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_c_biomass",
                               predictors_df = pred_rock_int, 
                               random_effects = c("year", "affiliated_mpa"),
                               data = data_rock_subset,
                               regions = c("Central"), 
                               path = "analyses/7habitat/output/rock/central")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 10))
})

walk(rock_n, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_c_biomass",
                               predictors_df = pred_rock_int, 
                               random_effects = c("year", "affiliated_mpa"),
                               data = data_rock_subset,
                               regions = c("North"), 
                               path = "analyses/7habitat/output/rock/north")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 10))
})

walk(rock_nc, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_c_biomass",
                               predictors_df = pred_rock_int, 
                               random_effects = c("year", "bioregion", "affiliated_mpa"),
                               data = data_rock_subset,
                               regions = c("North", "Central"), 
                               path = "analyses/7habitat/output/rock/north_central")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 10))
})

walk(rock_sc, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_c_biomass",
                               predictors_df = pred_rock_int, 
                               random_effects = c("year", "bioregion", "affiliated_mpa"),
                               data = data_rock_subset,
                               regions = c("South", "Central"), 
                               path = "analyses/7habitat/output/rock/south_central")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 10))
})

