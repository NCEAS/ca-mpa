# Refine preferred habitat type for each species
# Cori Lopazanski
# Nov 2024

# This script separates the step that refines the preferred habitat type 
# and exports the model results and preferred habitat names to a dataframe
# that can be used in subsequent modeling steps.

library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)

rm(list = ls())
gc()


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) %>% mutate(site_type = factor(site_type, levels = c("Reference", "MPA")))
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% mutate(site_type = factor(site_type, levels = c("MPA", "Reference")))
data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% mutate(site_type = factor(site_type, levels = c("MPA", "Reference")))

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) #%>% map(~ .x[.x != "site_type * age_at_survey"])
pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) 
pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds"))

pred_kelp_int <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_interactions.Rds"))
pred_rock_int <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors_interactions.Rds"))
pred_surf_int <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors_interactions.Rds"))


# Build Data  --------------------------------------------------------------------

sp_kelp <- data_kelp %>%
  filter(kg_per_m2 > 0) %>%
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(kg_per_m2),
            total_count = sum(count_per_m2),
            n_obs = n()) %>%
  filter(n_obs > 40) %>% 
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>% 
  filter(!is.na(n_obs_North)) %>% 
  filter(!is.na(n_obs_South))

data_kelp_subset <- data_kelp %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg:count_per_m2, log_kg_per_m2,
                all_of(setdiff(unique(unlist(pred_kelp)), c("site_type * age_at_survey")))) 



# 1. Compare habitat combinations ----------------------------------------------
refine_habitat <- function(species, response, predictors_list, random_effects, data_subset, regions, save_path) {
  data_sp <- data_subset %>% 
    filter(species_code == species) %>% 
    filter(bioregion %in% regions) %>% 
    mutate(across(where(is.numeric), scale)) # scale numeric predictors
  
  models <- list()
  
  models_df <- map_dfr(predictors_list, function(predictors) {
    model_formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + "), "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    model <- lmer(model_formula, data = data_sp)
    models[[paste(predictors, collapse = ", ")]] <<- model
    
    data.frame(predictors = paste(predictors, collapse = ", "),
               regions = paste(regions, collapse = ", "),
               random_effects = paste(random_effects, collapse = ", "),
               AICc = AICc(model),
               logLik = logLik(model),
               n = nobs(model),
               n_sites = n_distinct(data_sp$site),
               n_mpas = n_distinct(data_sp$affiliated_mpa))}) %>%
    mutate(delta_AICc = AICc - min(AICc)) %>% 
    arrange(delta_AICc)

  saveRDS(list(models_df = models_df, models = models), 
          file = file.path(save_path, paste0(species, "_models.rds")))
  models_df
}


# 2. Load models and filter for positive habitat relationships ------------------------------------------------

filter_positive_models <- function(species, habitat_predictors, save_path) {
  data <- readRDS(file.path(save_path, paste0(species, "_models.rds")))
  models <- data$models
  
  positive_models_df <- map_dfr(names(models), function(model_name) {
    model <- models[[model_name]]
    coefs <- fixef(model)[names(fixef(model)) %in% habitat_predictors]
    
    if (length(coefs) > 0 && any(coefs > 0)) {
      data.frame(
        predictors = model_name,
        AICc = AICc(model),
        logLik = logLik(model),
        n = nobs(model),
        n_sites = data$models_df$n_sites[1],
        n_mpas = data$models_df$n_mpas[1]
      )
    } else {
      NULL  
    }
  }) %>%
    mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
    arrange(delta_AICc)
  
  # Save filtered positive models
  saveRDS(list(models_df = positive_models_df, 
               models = models[names(models) %in% positive_models_df$predictors]), 
         file = file.path(save_path, paste0(species, "_positive_models.rds")))

  positive_models_df
}


# 3. Load remaining models and compare ------------------------------------------------------------------------
analyze_top_models <- function(species, save_path) {
  data <- readRDS(file.path(save_path, paste0(species, "_models.rds")))
  
  top_model_names <- data$models_df %>% filter(delta_AICc <= 4) %>% pull(predictors)
  top_models <- data$models[top_model_names]

  if (length(top_models) > 1) {
    model_avg <- model.avg(top_models, fit = TRUE)
    coef_table <- data.frame(coefTable(model_avg)) %>%
      rownames_to_column("predictor") %>%
      dplyr::select(predictor, estimate = "Estimate") %>%
      mutate(predictor = str_replace(predictor, "typeMPA", "type"),
             importance = sw(model_avg)[predictor]) 
  } else {
    coef_table <- data.frame(estimate = fixef(top_models[[1]])) %>% 
      rownames_to_column("predictor") %>% 
      mutate(predictor = str_replace(predictor, "typeMPA", "type"),
             importance = 1)
  } 
  
  
  summary_df <- data.frame(coef_table) %>% 
    mutate(species = species,
           sign = sign(estimate),
           num_models = length(top_models)) %>% 
    filter(!predictor %in%c("(Intercept)")) %>% 
    dplyr::select(species, everything())
  
  return(summary_df)
}

# 4. Define parameters and run models ----------------------------------------------------

## Kelp ------------------------------------------------------------------------

### 1. Refine habitat ----------------------------------------------------------
# walk(sp_kelp$species_code, function(species) { # Top 8 statewide species
#   results_df <- refine_habitat(species = species,
#                                response = "log_kg_per_m2",
#                                predictors_list = pred_kelp, # No interactions 
#                                random_effects = c("year", "bioregion", "affiliated_mpa"), # With MPA RE
#                                data_subset = data_kelp_subset, # Scaled numeric predictors
#                                regions = c("North", "Central", "South"), # All regions
#                                save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 5))
# })

# walk(sp_kelp$species_code, function(species) { # Top 8 statewide species
#   results_df <- refine_habitat(species = species,
#                                response = "log_kg_per_m2",
#                                predictors_list = pred_kelp, # No interactions
#                                random_effects = c("year", "affiliated_mpa"), # With MPA RE
#                                data_subset = data_kelp_subset,
#                                regions = c("South"), # South only
#                                save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/south")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_df, 5))
# })

walk(sp_kelp$species_code, function(species) { # Top 8 statewide species
  results_df <- refine_habitat(species = species,
                               response = "log_kg_per_m2",
                               predictors_list = pred_kelp_int, # With interactions 
                               random_effects = c("year", "bioregion", "affiliated_mpa"), # With MPA RE
                               data_subset = data_kelp_subset, # Scaled numeric predictors
                               regions = c("Central", "North", "South"), # All regions
                               save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 5))
})



### 2. Filter positive models --------------------------------------------------

# Identify habitat predictors in data
# habitat_predictors <- grep("^(hard|soft|kelp)", names(data_kelp), value = TRUE)
# habitat_predictors_int <- unique(unlist(pred_kelp_int))
# 
# 
# walk(sp_kelp$species_code, function(species) { # Top 8 statewide species
#   results_pos <- filter_positive_models(species = species, 
#                                         habitat_predictors = habitat_predictors, # No interactions
#                                         save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_pos, 5))
# })
# 
# walk(sp_kelp$species_code, function(species) { # Top 8 statewide species
#   results_pos <- filter_positive_models(species = species, 
#                                         habitat_predictors = habitat_predictors_int, # With interactions
#                                         save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_pos, 5))
# })
# 
# walk(sp_kelp$species_code, function(species) { # Top 8 statewide species
#   results_pos <- filter_positive_models(species = species, 
#                                         habitat_predictors = habitat_predictors,
#                                         save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/south")
#   cat("\nTop 5 models for species:", species, "\n")
#   print(head(results_pos, 5))
# })

### 3. Average the top models --------------------------------------------------
consolidated_results <- map2(sp_kelp$species_code, 
                             "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction",
                             analyze_top_models) %>% list_rbind() 


species <- "ELAT"
save_path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction"
data <- readRDS(file.path(save_path, paste0(species, "_models.rds")))

top_model_names <- data$models_df %>% filter(delta_AICc <= 4) %>% pull(predictors)
top_models <- data$models[top_model_names]

if (length(top_models) > 1) {
  model_avg <- model.avg(top_models, fit = TRUE)
  coef_table <- data.frame(coefTable(model_avg)) %>%
    rownames_to_column("predictor") %>%
    dplyr::select(predictor, estimate = "Estimate") %>%
    mutate(predictor = str_replace(predictor, "typeMPA", "type"),
           importance = sw(model_avg)[predictor]) 
} else {
  coef_table <- data.frame(estimate = fixef(top_models[[1]])) %>% 
    rownames_to_column("predictor") %>% 
    mutate(predictor = str_replace(predictor, "typeMPA", "type"),
           importance = 1)
} 


summary_df <- data.frame(coef_table) %>% 
  mutate(species = species,
         sign = sign(estimate),
         num_models = length(top_models)) %>% 
  filter(!predictor %in%c("(Intercept)")) %>% 
  dplyr::select(species, everything())

# Extract signs and filter habitat predictors
habitat_predictors <- setdiff(names(predictor_signs), 
                              c("site_type", "age_at_survey", "site_typeMPA", "(Intercept)", "size_km2")) # still include site*age in case thats the only one

# Map expanded interaction names back to generic ones
predictor_importance_names <- names(predictor_importance) %>% 
  str_replace_all(., "site_typeMPA:", "site_type:") %>% 
  str_replace_all(., ":site_typeMPA", ":site_type")

# Create summary data frame
summary_df <- data.frame(
  species_code = species,
  predictor = habitat_predictors,
  importance_score = unname(predictor_importance[predictor_importance_names]),
  sign = unname(predictor_signs[predictor_importance_names]),
  num_models = length(top_models),
  row.names = NULL
)



saveRDS(consolidated_results, file.path("analyses/7habitat/output/refine_pref_habitat/kelp/all_regions", 
                                        "consolidated_results.Rds"))




# Surf zone ---------
sp_surf <- data_surf %>% 
  filter(kg_per_haul > 0) %>% 
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(kg_per_haul),
            total_count = sum(count),
            n_obs = n()) %>% 
  filter(total_count > 80) %>% 
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>% 
  filter(!is.na(n_obs_North)) %>% 
  filter(!is.na(n_obs_South))

data_surf_subset <- data_surf %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg:count_per_haul, log_kg_per_haul,
                all_of(setdiff(unique(unlist(pred_surf)), c("site_type", "age_at_survey")))) %>% 
  filter(!(species_code == "AARG" & kg_per_haul > 2.1))

walk(sp_surf$species_code, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_kg_per_haul",
                               predictors_list = pred_surf,
                               random_effects = c("year", "bioregion"),
                               data_subset = data_surf_subset,
                               save_path = "analyses/7habitat/output/refine_pref_habitat/surf/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 5))
})

# Rock
sp_rock <- data_rock %>% 
  filter(weight_kg > 0) %>% 
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(weight_kg),
            total_count = sum(count),
            n_obs = n()) %>% 
  filter(n_obs > 30) %>% 
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>% 
  filter(!is.na(n_obs_North)) %>% 
  filter(!is.na(n_obs_South))

data_rock_subset <- data_rock %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg, count, log_bpue_kg,
                all_of(setdiff(unique(unlist(pred_rock)), c("site_type", "age_at_survey")))) 

walk(unique(sp_rock$species_code), function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_bpue_kg",
                               predictors_list = pred_rock,
                               random_effects = c("year", "bioregion"),
                               data_subset = data_rock_subset,
                               save_path = "analyses/7habitat/output/refine_pref_habitat/rock/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 5))
})





walk(sp_surf$species_code, function(species) {
  results_pos <- filter_positive_models(species = species, 
                                        habitat_predictors = habitat_predictors,
                                        save_path = "analyses/7habitat/output/refine_pref_habitat/surf/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_pos, 5))
})

walk(unique(sp_rock$species_code), function(species) {
  results_pos <- filter_positive_models(species = species, 
                                        habitat_predictors = habitat_predictors,
                                        save_path = "analyses/7habitat/output/refine_pref_habitat/rock/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_pos, 5))
})



consolidated_results <- map2(sp_surf$species_code, 
                            "analyses/7habitat/output/refine_pref_habitat/surf/all_regions",
                            analyze_top_models) %>%
  list_rbind()

saveRDS(consolidated_results, file.path("analyses/7habitat/output/refine_pref_habitat/surf/all_regions", "consolidated_results.Rds"))


consolidated_results <- map2(unique(sp_rock$species_code), 
                             "analyses/7habitat/output/refine_pref_habitat/rock/all_regions",
                             analyze_top_models) %>% list_rbind() 

saveRDS(consolidated_results, file.path("analyses/7habitat/output/refine_pref_habitat/rock/all_regions", 
                                        "consolidated_results.Rds"))

## South ----
consolidated_results <- map2(sp_kelp$species_code, 
                             "analyses/7habitat/output/refine_pref_habitat/kelp/south",
                             analyze_top_models) %>% list_rbind() 

saveRDS(consolidated_results, file.path("analyses/7habitat/output/refine_pref_habitat/kelp/south", 
                                        "consolidated_results.Rds"))


