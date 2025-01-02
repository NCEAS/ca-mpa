# Refine preferred habitat type for each species
# Cori Lopazanski
# Nov 2024

# This script builds and fits the models across the predictor lists generated in
# the previous step, including interactions with all possible habitat predictors.
# 1. Fit and save all of the candidate models
# 2. Extract the following focal models for each species:
#     -- Top models within 4 AICc of the model with the lowest AICc
#     -- The base model (site type * age at survey only)
#     -- The full models at each scale (all predictors with interactions)

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)
library(lmerTest)

rm(list = ls())
gc()


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

#data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) %>% mutate(site_type = factor(site_type, levels = c("Reference", "MPA")))
#data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% mutate(site_type = factor(site_type, levels = c("Reference", "MPA")))
data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% mutate(site_type = factor(site_type, levels = c("Reference", "MPA")))
 
#pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) 
#pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) 
pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds"))

#pred_kelp_int <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_interactions_add.Rds"))
pred_rock_int <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors_interactions_add.Rds"))
#pred_surf_int <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors_interactions_add.Rds"))

pred_rock_no_soft <- pred_rock_int %>% 
  filter(!str_detect(predictors, "soft") | type == "full") %>% 
  mutate(predictors = case_when(
    type == "full" ~ str_replace_all(predictors, "\\+ soft_bottom_500 \\* site_type ", ""), # Escape `+` and ensure spaces match
    TRUE ~ predictors
  ))

# Build Data  --------------------------------------------------------------------

## Kelp ------------------------------------------
# sp_kelp <- data_kelp %>%
#   filter(kg_per_m2 > 0) %>%
#   group_by(species_code, sciname, target_status, bioregion) %>%
#   summarize(total_biomass = sum(kg_per_m2),
#             total_count = sum(count_per_m2),
#             n_obs = n()) %>%
#   filter(n_obs > 40) %>% 
#   pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>% 
#   filter(!is.na(n_obs_North)) %>% 
#   filter(!is.na(n_obs_South))
# 
# data_kelp_subset <- data_kelp %>% 
#   dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
#                 species_code:target_status, assemblage_new, weight_kg:count_per_m2, log_kg_per_m2,
#                 all_of(pred_kelp$predictor)) %>% 
#   mutate(hard_bottom_25 = hard_bottom_0_30m_25,
#          hard_bottom_50 = hard_bottom_0_30m_50,
#          hard_bottom_100 = hard_bottom_0_30m_100,
#          hard_bottom_250 = hard_bottom_0_30m_250 + hard_bottom_30_100m_250,
#          hard_bottom_500 = hard_bottom_0_30m_500 + hard_bottom_30_100m_500,
#          soft_bottom_25 = soft_bottom_0_30m_25,
#          soft_bottom_50 = soft_bottom_0_30m_50,
#          soft_bottom_100 = soft_bottom_0_30m_100,
#          soft_bottom_250 = soft_bottom_0_30m_250 + soft_bottom_30_100m_250,
#          soft_bottom_500 = soft_bottom_0_30m_500 + soft_bottom_30_100m_500)

## Rock ------------------------------------------
sp_rock <- data_rock %>%
  filter(weight_kg > 0) %>%
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(weight_kg),
            total_count = sum(count),
            n_obs = n()) %>%
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
  filter(!is.na(n_obs_Central) & !is.na(n_obs_North) & !is.na(n_obs_South)) %>%
  filter(n_obs_South > 100)

data_rock_subset <- data_rock %>%
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg, count, log_bpue_kg,
                all_of(pred_rock$predictor)) %>%
  mutate(hard_bottom_25 = hard_bottom_0_30m_25 + hard_bottom_30_100m_25,
         hard_bottom_50 = hard_bottom_0_30m_50 + hard_bottom_30_100m_50,
         hard_bottom_100 = hard_bottom_0_30m_100+ hard_bottom_30_100m_100,
         hard_bottom_250 = hard_bottom_0_30m_250 + hard_bottom_30_100m_250,
         hard_bottom_500 = hard_bottom_0_30m_500 + hard_bottom_30_100m_500,
         soft_bottom_25 = soft_bottom_0_30m_25 + soft_bottom_30_100m_25,
         soft_bottom_50 = soft_bottom_0_30m_50 + soft_bottom_30_100m_50,
         soft_bottom_100 = soft_bottom_0_30m_100 + soft_bottom_30_100m_100,
         soft_bottom_250 = soft_bottom_0_30m_250 + soft_bottom_30_100m_250,
         soft_bottom_500 = soft_bottom_0_30m_500 + soft_bottom_30_100m_500)


## Surf ------------------------------------------

## Add here.

# Fit all habitat combinations --------------------------------------------------------------
refine_habitat <- function(species, response, predictors_df, random_effects, data, regions, path) {
  print(paste("Starting species: ", species))
  data_sp <- data %>% 
    filter(species_code == species) %>% 
    filter(bioregion %in% regions) %>% 
    mutate(across(where(is.numeric), scale)) # scale numeric predictors
  
  models <- list()
  
  models_df <- map_dfr(seq_len(nrow(predictors_df)), function(i) {
    predictors <- predictors_df$predictors[i]
    model_id <- predictors_df$model_id[i]
    scale <- predictors_df$scale[i]
    
    model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    warning_message <- NULL
    singular_status <- "Unknown"
    
    model <- suppressWarnings(
      tryCatch(
        {
          m <- lmer(model_formula, data = data_sp)
          singular_status <- if (isSingular(m)) "Singular fit" else "OK"
          m
        },
        warning = function(w) {
          warning_message <<- conditionMessage(w) # Capture the warning message
          return(NULL) # Return NULL for the model if a warning occurs
        },
        error = function(e) {
          warning_message <<- conditionMessage(e) # Capture the error message
          return(NULL) # Return NULL for the model if an error occurs
        }))
    
    models[[model_id]] <<- model
    
    data.frame(model_id = model_id,
               predictors = gsub("\\s*\\+\\s*", ", ", predictors),
               regions = paste(regions, collapse = ", "),
               random_effects = paste(random_effects, collapse = ", "),
               scale = scale,
               AICc = if (!is.null(model)) AICc(model) else NA,
               logLik = if (!is.null(model)) as.numeric(logLik(model)) else NA,
               n = if (!is.null(model)) nobs(model) else NA,
               n_sites = if (!is.null(model)) n_distinct(data_sp$site) else NA,
               n_mpas = if (!is.null(model)) n_distinct(data_sp$affiliated_mpa) else NA,
               singular_status = singular_status,
               warnings = ifelse(is.null(warning_message), "OK", warning_message))
  }) %>%
    mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>% 
    arrange(delta_AICc)
  
  # Extract the base model and full models for each scale
  core_model_names <- predictors_df %>%
    filter(type %in% c("base", "full")) %>%
    pull(model_id)
  
  # Extract the top models within deltaAICc of 4
  top_model_names <- models_df %>%
    filter(delta_AICc <= 4) %>%
    pull(model_id)
  
  # Extract the model objects for the core + top models
  models <- models[unique(c(top_model_names, core_model_names))]
  
  # Filter for reduced df with top, core, and full models
  models_df <- models_df %>%
    filter(model_id %in% c(top_model_names, core_model_names)) %>% 
    mutate(type = case_when(model_id %in% top_model_names ~ "top",
                            predictors == "site_type * age_at_survey" ~ "base",
                            model_id %in% core_model_names ~ "core"))
  
  # Save the subset
  saveRDS(list(models_df = models_df, models = models, data_sp = data_sp), 
          file = file.path(path, paste0(species, "_models.rds")))
  
  models_df
}


## Define parameters and run ---------------------------------------------------

# walk(unique(sp_kelp$species_code), function(species) { # Top 8 statewide species
#   results_df <- refine_habitat(species = species,
#                                response = "log_kg_per_m2",
#                                predictors_df = pred_kelp_int, # With interactions 
#                                random_effects = c("year", "bioregion", "affiliated_mpa"), # With MPA RE
#                                data = data_kelp_subset, # Scaled numeric predictors
#                                regions = c("Central", "North", "South"), # All regions
#                                path = "analyses/7habitat/output/kelp/all_regions/consolidated")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 10))
# })

walk(unique(sp_rock$species_code), function(species) { # Top 4 statewide species
  results_df <- refine_habitat(species = species,
                               response = "log_bpue_kg",
                               predictors_df = pred_rock_no_soft, # With interactions
                               random_effects = c("year", "bioregion", "affiliated_mpa"), # With MPA RE
                               data = data_rock_subset, # Scaled numeric predictors
                               regions = c("Central", "North", "South"), # All regions
                               path = "analyses/7habitat/output/rock/all_regions/no_soft")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 5))
})


# 
# 
# # Extract and save the top models and core models ---------------------------------------------------
# species <- "SMYS"
# path <- "analyses/7habitat/output/kelp/all_regions/w_depth"
# predictor_df <- pred_kelp_int
#   
# extract_models <- function(species, path, predictor_df){
#   # Read data containing all the models and the comparison df
#   data <- readRDS(file.path(path, paste0(species, "_models.rds"))) 
#   
#   # Extract the base model and full models for each scale
#   core_model_names <- predictor_df %>%
#     filter(type %in% c("base", "full")) %>%
#     pull(model_id)
#   
#   # Extract the top models within deltaAICc of 4
#   top_model_names <- data$models_df %>%
#     filter(delta_AICc <= 4) %>%
#     pull(model_id)
#   
#   # Extract the model objects for the core + top models
#   models <- data$models[unique(c(top_model_names, core_model_names))]
#   
#   # Filter for reduced df with top, core, and full models
#   models_df <- data$models_df %>%
#     filter(model_id %in% c(top_model_names, core_model_names)) %>% 
#     mutate(type = case_when(model_id %in% top_model_names ~ "top",
#                             predictors == "site_type * age_at_survey" ~ "base",
#                             model_id %in% core_model_names ~ "core"))
# 
#   # Save the subset
#   subset <- list(models_df = models_df, models = models, data_sp = data$data_sp)
#   saveRDS(subset, file = file.path(path, paste0(species, "_subset.rds")))
# }
# 
# walk(unique(sp_kelp$species_code), 
#      ~ extract_models(.x, 
#                       path = "analyses/7habitat/output/kelp/all_regions/w_depth", 
#                       predictor_df = pred_kelp_int))
# # 
# # walk(unique(sp_rock$species_code), 
# #      ~ extract_models(.x, 
# #                       path = "analyses/7habitat/output/rock/all_regions/interaction", 
# #                       predictor_df = pred_rock_int))
# # 
# # 
# # extract_models("PCLA", 
# #                path = "analyses/7habitat/output/kelp/all_regions/interaction", 
# #                predictor_df = pred_kelp_int)
