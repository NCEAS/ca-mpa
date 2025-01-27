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

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% mutate(site_type = factor(site_type, levels = c("Reference", "MPA")))
pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_kelp_int <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_interactions.Rds"))


# Define Kelp Species Lists --------------------------------------------------------------------------
sp_kelp <- data_kelp %>%
  filter(kg_per_m2 > 0) %>%
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(weight_kg),
            total_count = sum(count),
            n_obs = n(), .groups = 'drop') %>%
  filter(n_obs > 40) %>%
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
  mutate(south = if_else(n_obs_South < 40 | is.na(n_obs_South), 0, 1),
         central = if_else(n_obs_Central < 40 | is.na(n_obs_Central), 0, 1),
         north = if_else(n_obs_North < 40 | is.na(n_obs_North), 0, 1))

# Only model within the region they are prevalent in (defined as > 40 site-year observations)
kelp_all <- sp_kelp %>% filter(south == 1 & central == 1 & north == 1) %>% pull(species_code)
kelp_s   <- sp_kelp %>% filter(south == 1 & central == 0 & north == 0) %>% pull(species_code)
kelp_c   <- sp_kelp %>% filter(south == 0 & central == 1 & north == 0) %>% pull(species_code)
kelp_n   <- sp_kelp %>% filter(south == 0 & central == 0 & north == 1) %>% pull(species_code)
kelp_sc  <- sp_kelp %>% filter(south == 1 & central == 1 & north == 0) %>% pull(species_code)
kelp_nc  <- sp_kelp %>% filter(south == 0 & central == 1 & north == 1) %>% pull(species_code)


# Define subset for modeling (reduced number of columns)
data_kelp_subset <- data_kelp %>%
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg:count_per_m2, log_kg_per_m2,
                all_of(pred_kelp$predictor))

# Fit models for each species list --------------------------------------------------------------------

## All -------------
# walk(kelp_all, function(species) {
#   results_df <- refine_habitat(species = species,
#                                response = "log_c_biomass",
#                                predictors_df = pred_kelp_int,
#                                random_effects = c("year", "bioregion", "affiliated_mpa"),
#                                data = data_kelp_subset,
#                                regions = c("Central", "North", "South"),
#                                path = "analyses/7habitat/output/kelp/all_regions")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 10))
# })

## South -------------
# walk(kelp_s, function(species) {
#   results_df <- refine_habitat(species = species,
#                                response = "log_c_biomass",
#                                predictors_df = pred_kelp_int,
#                                random_effects = c("year", "affiliated_mpa"),
#                                data = data_kelp_subset,
#                                regions = c("South"),
#                                path = "analyses/7habitat/output/kelp/south")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 10))
# })

## Central -------------
# walk(kelp_c, function(species) {
#   results_df <- refine_habitat(species = species,
#                                response = "log_c_biomass",
#                                predictors_df = pred_kelp_int,
#                                random_effects = c("year", "affiliated_mpa"),
#                                data = data_kelp_subset,
#                                regions = c("Central"),
#                                path = "analyses/7habitat/output/kelp/central")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 10))
# })

## North -------------
# walk(kelp_n, function(species) {
#   results_df <- refine_habitat(species = species,
#                                response = "log_c_biomass",
#                                predictors_df = pred_kelp_int,
#                                random_effects = c("year", "affiliated_mpa"),
#                                data = data_kelp_subset,
#                                regions = c("North"),
#                                path = "analyses/7habitat/output/kelp/north")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 10))
# })


## North Central -------------
# walk(kelp_nc, function(species) {
#   results_df <- refine_habitat(species = species,
#                                response = "log_c_biomass",
#                                predictors_df = pred_kelp_int,
#                                random_effects = c("year", "bioregion", "affiliated_mpa"),
#                                data = data_kelp_subset,
#                                regions = c("North", "Central"),
#                                path = "analyses/7habitat/output/kelp/north_central")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 10))
# })


## South Central -------------
walk(kelp_sc, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_c_biomass",
                               predictors_df = pred_kelp_int,
                               random_effects = c("year", "bioregion", "affiliated_mpa"),
                               data = data_kelp_subset,
                               regions = c("South", "Central"),
                               path = "analyses/7habitat/output/kelp/south_central")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 10))
})
