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
                all_of(pred_kelp$predictor))



# Fit all habitat combinations --------------------------------------------------------------
refine_habitat <- function(species, response, predictors_list, random_effects, data_subset, regions, save_path) {
  data_sp <- data_subset %>% 
    filter(species_code == species) %>% 
    filter(bioregion %in% regions) %>% 
    mutate(across(where(is.numeric), scale)) # scale numeric predictors
  
  models <- list()
  
  models_df <- map_dfr(predictors_list$predictors, function(predictors) {
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
        }
      )
    )
    
    models[[gsub("\\s*\\+\\s*", ", ", predictors)]] <<- model
    
    data.frame(predictors = gsub("\\s*\\+\\s*", ", ", predictors),
               regions = paste(regions, collapse = ", "),
               random_effects = paste(random_effects, collapse = ", "),
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
  
  saveRDS(list(models_df = models_df, models = models, data_sp = data_sp), 
          file = file.path(save_path, paste0(species, "_models.rds")))
  models_df
}

## Define parameters and run ---------------------------------------------------

walk(unique(sp_kelp$species_code), function(species) { # Top 8 statewide species
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



# Extract and save the top models and core models ---------------------------------------------------
extract_models <- function(species, path, predictor_list){
  # Read data containing all the models and the comparison df
  data <- readRDS(file.path(path, paste0(species, "_models.rds"))) 
  
  # Extract the base model and full models for each scale
  core_model_names <- predictor_list %>%
    filter(type %in% c("base", "full")) %>%
    mutate(predictors = gsub("\\s*\\+\\s*", ", ", predictors)) %>%
    pull(predictors)
  
  core_models <- data$models[core_model_names]
  
  # Extract the top models within deltaAICc of 4
  top_model_names <- data$models_df %>%
    filter(delta_AICc <= 4) %>%
    pull(predictors)
  
  top_models <- data$models[top_model_names]
  
  # Filter for reduced dataframe with top, core, and full models
  models_df <- data$models_df %>%
    filter(predictors %in% c(top_model_names, core_model_names)) %>% 
    mutate(type = case_when(predictors %in% top_model_names ~ "top",
                            predictors %in% core_model_names ~ "core",
                            predictors == "site_type * age_at_survey" ~ "base"))
  
  # Analyze the top models
  if (length(top_models) > 1) {
    model_avg <- model.avg(top_models, fit = TRUE)
    coef_table <- data.frame(coefTable(model_avg)) %>%
      rownames_to_column("predictor") %>%
      dplyr::select(predictor, estimate = "Estimate", se = "Std. Error") %>%
      mutate(predictor = str_replace(predictor, "typeMPA", "type"),
             importance = sw(model_avg)[predictor],
             conf.low = estimate - 1.96*se,
             conf.high = estimate + 1.96*se) 
  } else {
    coef_table <- data.frame(estimate = fixef(top_models[[1]])) %>% 
      rownames_to_column("predictor") %>% 
      mutate(predictor = str_replace(predictor, "typeMPA", "type"),
             importance = 1)
  } 
  
  summary_df <- data.frame(coef_table) %>% 
    mutate(species_code = species,
           sign = sign(estimate),
           num_models = length(top_models)) %>% 
    filter(!predictor %in%c("(Intercept)"))
  
  
  saveRDS(list(models_df = models_df, summary_df = summary_df,
               top_models = top_models, core_models = core_models,
               data_sp = data$data_sp),
          file = file.path(path, paste0(species, "_subset.rds")))
}

walk(unique(sp_kelp$species_code), 
     ~ extract_models(.x, 
                      path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction", 
                      predictor_list = pred_kelp_int))




# -------------------------------------------------------------------------------------------
# To Archive 
# -------------------------------------------------------------------------------------------


# Extract signs and filter habitat predictors
habitat_predictors <- setdiff(names(predictor_signs), 
                              c("site_type", "age_at_survey", "site_typeMPA", "(Intercept)", "size_km2")) # still include site*age in case thats the only one




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


