# 
# Cori 
# Dec 2024



library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)

rm(list = ls())
gc()


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) 
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds"))
data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds"))

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) #%>% map(~ .x[.x != "site_type * age_at_survey"])
pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) 
pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds"))


# 1. Compare habitat combinations ----------------------------------------------
refine_habitat <- function(species, response, predictors_list, random_effects, data_subset, save_path) {
  data_sp <- data_subset 
  
  models <- list()
  
  models_df <- map_dfr(predictors_list, function(predictors) {
    model_formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + "), "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    model <- lmer(model_formula, data = data_sp)
    models[[paste(predictors, collapse = ", ")]] <<- model
    
    data.frame(predictors = paste(predictors, collapse = ", "),
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

# 2. Define parameters and run -------------------------------------------------

# Kelp

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
                all_of(setdiff(unique(unlist(pred_kelp)), c("site_type", "age_at_survey")))) %>% 
  filter(genus == "Sebastes") %>% 
  filter(assemblage_new %in% c("Hard Bottom", "Hard Bottom Biotic")) %>% 
  filter(bioregion == "South") %>% 
  group_by(year, site, site_type, bioregion, affiliated_mpa, age_at_survey, genus, target_status) %>%
  group_by(across(all_of(setdiff(unique(unlist(pred_kelp)), c("site_type", "age_at_survey")))), .add = TRUE) %>% 
  summarize(
    kg_per_m2 = sum(kg_per_m2, na.rm = TRUE),
    log_kg_per_m2 = log(kg_per_m2 + 1),
    .groups = "drop"
  )

results_df <- refine_habitat(species = "Sebastes",
                             response = "log_kg_per_m2",
                             predictors_list = pred_kelp,
                             random_effects = c("year"),
                             data_subset = data_kelp_subset,
                             save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions")

# 3. Load models and filter for positive habitat relationships ------------------------------------------------

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
      NULL  # Skip models that don’t meet the positive criterion
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

# Identify habitat predictors in data
habitat_predictors <- grep("^(hard|soft)", names(data_kelp), value = TRUE)

results_pos <- filter_positive_models(species = "Sebastes", 
                                      habitat_predictors = habitat_predictors,
                                      save_path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions")



# 4. Load remaining models and compare ------------------------------------------------------------------------
analyze_top_models <- function(species, save_path) {
  data <- readRDS(file.path(save_path, paste0(species, "_positive_models.rds")))
  
  top_model_names <- data$models_df %>% filter(delta_AICc <= 5) %>% pull(predictors)
  top_models <- data$models[top_model_names]
  
  # Model averaging or single model extraction
  if (length(top_models) > 1) {
    model_avg <- model.avg(top_models, fit = TRUE)
    coef_table <- coefTable(model_avg)
    predictor_importance <- sw(model_avg)
  } else {
    coef_table <- fixef(top_models[[1]])
    predictor_importance <- setNames(rep(1, length(coef_table)), names(coef_table))
  }
  
  # Extract signs and filter habitat predictors
  predictor_signs <- sign(if (is.matrix(coef_table)) coef_table[, "Estimate"] else coef_table)
  habitat_predictors <- setdiff(names(predictor_signs), 
                                c("site_type", "age_at_survey", "site_typeReference", "(Intercept)", "size_km2",
                                  "age_at_survey:site_typeReference", "site_typeReference:age_at_survey"))
  
  # Create summary data frame
  summary_df <- data.frame(
    species_code = species,
    predictor = habitat_predictors,
    importance_score = unname(predictor_importance[habitat_predictors]),
    sign = unname(predictor_signs[habitat_predictors]),
    num_models = length(top_models),
    row.names = NULL
  )
  return(summary_df)
  
}

# Kelp - all regions:
consolidated_results <- map2("Sebastes", 
                             "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions",
                             analyze_top_models) %>% list_rbind() 

saveRDS(consolidated_results, file.path("analyses/7habitat/output/refine_pref_habitat/kelp/all_regions", "Sebastes_consolidated_results.Rds"))

# 5. Run comparisons

run_comparison <- function(species, data_subset, response, random_effects, path){
  print(paste("Species: ", species))
  
  consolidated_results <- consolidated_results
  
  preferred_habitat <- consolidated_results %>%
    filter(species_code == species) %>% 
    filter(importance_score >= 0.5) %>%
    filter(sign == 1) %>%
    pull(predictor)
  print(paste("Pref Habitat: ", preferred_habitat))
  
  preferred_scale <- str_sub(preferred_habitat, -3, -1) 
  all_habitat <- names(data_subset)[str_ends(names(data_subset), preferred_scale[1])]
  print(paste("Pref Scale: ", preferred_scale))
  
  data_sp <- data_subset %>% 
    mutate(pref_habitat = rowSums(across(all_of(preferred_habitat)), na.rm = TRUE))# %>% 
  #  mutate(across(where(is.numeric), scale))
  
  f1 <- as.formula(paste(response, " ~ site_type * age_at_survey  + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
  f2 <- as.formula(paste(response, " ~ site_type * age_at_survey + pref_habitat + ", paste0("(1 | ", random_effects, ")", collapse = " + ")))  
  f3 <- as.formula(paste(response, " ~ site_type * age_at_survey + site_type * pref_habitat + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
  f4 <- as.formula(paste(response, " ~ site_type * age_at_survey +", paste(all_habitat, collapse = " + "), " + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
  
  m1 <- lmer(f1, data = data_sp, REML = FALSE) 
  print("Model 1 Complete")
  m2 <- lmer(f2, data = data_sp, REML = FALSE)
  print("Model 2 Complete")
  m3 <- lmer(f3, data = data_sp, REML = FALSE)
  print("Model 3 Complete")
  m4 <- lmer(f4, data = data_sp, REML = FALSE)
  print("Model 4 Complete")
  model_comparison <- compare_performance(m1, m2, m3, m4)
  
  saveRDS(
    list(
      data_sp = data_sp,
      species = species,
      preferred_habitat = preferred_habitat,
      preferred_scale = preferred_scale,
      models = list(m1 = m1, m2 = m2, m3 = m3, m4 = m4),
      model_comparison = model_comparison
    ),
    file = file.path(path, paste0(species, "_model_comparison.rds"))
  )
  
  print(model_comparison)
  
}


comp <- run_comparison(species = "Sebastes",
                       data_subset = data_kelp_subset,
                       response = "log_kg_per_m2",
                       random_effects = c("year"),
                       path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions")

species <- "Sebastes"
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions"
data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))


broom.mixed::tidy(data$models$m1, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")

broom.mixed::tidy(data$models$m2, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")

broom.mixed::tidy(data$models$m3, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")

broom.mixed::tidy(data$models$m4, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")


