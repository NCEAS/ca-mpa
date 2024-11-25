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

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds"))
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds"))
data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds"))

# Generate predictors -----------------------------------------------------------
base_predictors <- c("site_type * age_at_survey", "size_km2") # Including interaction 

habitat_50  <- c("hard_bottom_biotic_0_30m_50",  # problem with 3 NAs for surf
                 "soft_bottom_biotic_0_30m_50",  # problem with 3 NAs for surf
                 "hard_bottom_0_30m_50",         # problem with 3 NAs for surf
                 "soft_bottom_0_30m_50")         # problem with 3 NAs for surf
habitat_100 <- c("hard_bottom_biotic_0_30m_100", 
                 "soft_bottom_biotic_0_30m_100", 
                 "hard_bottom_0_30m_100",   
                 "soft_bottom_0_30m_100")
habitat_250 <- c("hard_bottom_biotic_0_30m_250", 
                 "soft_bottom_biotic_0_30m_250",
                 "hard_bottom_0_30m_250",    
                 "soft_bottom_0_30m_250"#,
                 #"hard_bottom_30_100m_250",    # rm for surf zone
                # "soft_bottom_30_100m_250"     # rm for surf zone
                 )   
habitat_500 <- c("hard_bottom_biotic_0_30m_500",
                 "soft_bottom_biotic_0_30m_500",
                 "hard_bottom_0_30m_500", 
                 "soft_bottom_0_30m_500"#,      
              #   "hard_bottom_30_100m_500",    # rm for surf zone
               #  "soft_bottom_30_100m_500"     # rm for surf zone
                 )

predictors_list <- NULL
get_predictors <- function(habitat_buffer_list) {
  predictors <- list()
  
  for (r in 1:length(habitat_buffer_list)) {
    habitat_combinations <- combn(habitat_buffer_list, r, simplify = FALSE)
    
    for (combo in habitat_combinations) {
      predictors <- append(predictors, list(c(combo, base_predictors)))}}
  
  return(predictors)
}

predictors_list <- c(
 # get_predictors(habitat_50),
  get_predictors(habitat_100),
  get_predictors(habitat_250),
  get_predictors(habitat_500)
)

predictors_list <- unique(predictors_list)

num_predictors <- unique(unlist(predictors_list)) %>% setdiff(c("site_type", "site_type * age_at_survey", "size_km2")) # 

rm(get_predictors, habitat_50, habitat_100, habitat_250, habitat_500, base_predictors)


# 1. Compare habitat combinations ----------------------------------------------
refine_habitat <- function(species, response, random_effects, data_subset, save_path) {
  data_sp <- data_subset %>% filter(species_code == species)
  
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

# Kelp forest - all regions, including size as a covariate, these species: 
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
                all_of(num_predictors))

walk(sp_kelp$species_code, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_kg_per_m2",
                               random_effects = c("year", "bioregion"),
                               data_subset = data_kelp_subset,
                               save_path = "analyses/7habitat/output/refine_pref_habitat/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 5))
})

# Surf zone - all regions, including size as a covariate
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
                all_of(num_predictors)) %>% 
  filter(!(species_code == "AARG" & kg_per_haul > 2.1))

walk(sp_surf$species_code, function(species) {
  results_df <- refine_habitat(species = species,
                               response = "log_kg_per_haul",
                               random_effects = c("year", "bioregion"),
                               data_subset = data_surf_subset,
                               save_path = "analyses/7habitat/output/refine_pref_habitat/surf/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 5))
})

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
habitat_predictors <- grep("^(hard|soft)", names(data_kelp_subset), value = TRUE)

walk(sp_kelp$species_code, function(species) {
  results_pos <- filter_positive_models(species = species, 
                                        habitat_predictors = habitat_predictors,
                                        save_path = "analyses/7habitat/output/refine_pref_habitat/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_pos, 5))
})

walk(sp_surf$species_code, function(species) {
  results_pos <- filter_positive_models(species = species, 
                                        habitat_predictors = habitat_predictors,
                                        save_path = "analyses/7habitat/output/refine_pref_habitat/surf/all_regions")
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_pos, 5))
})

# 4. Load remaining models and compare ------------------------------------------------------------------------
analyze_top_models <- function(species) {
  data <- readRDS(file.path(save_path, paste0(species, "_positive_models.rds")))
  
  top_model_names <- data$models_df %>% filter(delta_AICc <= 4) %>% pull(predictors)
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

# Kelp - all regions incuding size:
save_path <- "analyses/7habitat/output/refine_pref_habitat/all_regions"
consolidated_results <- map(sp_kelp$species_code, analyze_top_models) %>% 
  list_rbind()

saveRDS(consolidated_results, file.path(save_path, "consolidated_results.Rds"))

# Surf - all regions including size:
save_path <- "analyses/7habitat/output/refine_pref_habitat/surf/all_regions"
consolidated_results <- map(sp_surf$species_code, analyze_top_models) %>%
  list_rbind()

saveRDS(consolidated_results, file.path(save_path, "consolidated_results.Rds"))



### --------------------------------------------------------------------------------------------------------------------

species <- "AARG"
data <- readRDS(file.path(save_path, paste0(species, "_positive_models.rds")))

test <- data_surf_subset %>% 
  dplyr::select(site, site_type, affiliated_mpa, bioregion, all_of(habitat_predictors)) %>% 
  distinct() %>% 
  pivot_longer(cols = hard_bottom_biotic_0_30m_50:soft_bottom_30_100m_500,
               names_to = "habitat", values_to = "value")

test_na <- test %>% 
  filter(is.na(hard_bottom_biotic_0_30m_50))
summary(testestsummary(test)
ggplot(data = test) +
  geom_point(aes(x = site, y = value, color = habitat))

# Test to make sure the above works:

species <- "SMAR"

data <- readRDS(file.path(save_path, paste0(species, "_positive_models.rds")))

top_model_names <- data$models_df %>% filter(delta_AICc <= 4) %>% pull(predictors)
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




# OLD --------------------------------------------------------------------------------------


# Let's explore the top models performance:
important_predictors <- consolidated_results %>%
  filter(importance_score >= 0.5) %>% 
  group_by(species_code) %>%
  summarize(top_predictors = list(predictor)) %>%
  deframe()  # Convert to named list by species


library(lme4)
library(ggplot2)

generate_diagnostic_plots <- function(species, data_subset, important_predictors, random_effects, save_path) {
  data_sp <- data_subset %>% 
    filter(species_code == species)
  
  predictors <- c(important_predictors[[species]], "age_at_survey", "site_type")

  model_formula <- as.formula(paste("kg_per_m2_adj", "~", paste(predictors, collapse = " + "), 
                                    "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
  
  model <- lmer(model_formula, data = data_sp)
  pdf_file <- file.path(save_path, paste0(species, "_diagnostics.pdf"))
  pdf(pdf_file, width = 8, height = 6)
  
  # 1. Residuals vs. Fitted
  plot_resid_fitted <- ggplot(data = data.frame(residuals = residuals(model), fitted = fitted(model)), 
                              aes(x = fitted, y = residuals)) +
    geom_point() +
    geom_hline(yintercept = 0, color = "red") +
    labs(title = paste("Residuals vs Fitted for", species))
  print(plot_resid_fitted)
  
  # 2. Q-Q Plot of Residuals
  plot_qq <- ggplot(data.frame(residuals = residuals(model)), aes(sample = residuals)) +
    stat_qq() + 
    stat_qq_line() +
    labs(title = paste("Q-Q Plot for Residuals -", species))
  print(plot_qq)
  
  # 3. Scale-Location (Spread-Location)
  plot_scale_location <- ggplot(data = data.frame(sqrt_abs_resid = sqrt(abs(residuals(model))), fitted = fitted(model)),
                                aes(x = fitted, y = sqrt_abs_resid)) +
    geom_point() +
    geom_smooth(se = FALSE, color = "blue") +
    labs(title = paste("Scale-Location Plot for", species), y = "sqrt(|Residuals|)")
  print(plot_scale_location)
  
  # Close PDF device
  dev.off()
  
  return(summary(model))  # Optionally return the model for further analysis if needed
}

generate_diagnostic_plots(species = "OYT", data_subset, important_predictors, random_effects = c("year", "bioregion"), save_path = save_path)


# Run the diagnostics for each species
map(species_list, ~ generate_diagnostic_plots(.x, data_subset, important_predictors, random_effects = c("year", "bioregion"), save_path = save_path))



# Create plots from the species lists
plot_species <- function(species){
  data_sp <- data_subset %>% 
    filter(species_code == species)
  
  b <- ggplot(data = data_sp) +
    geom_point(aes(x = affiliated_mpa, y = kg_per_m2, color = site_type), show.legend = F) +
    theme_minimal() +
    scale_color_manual(values = c("#ff7eb6", "#d4bbff")) +
    labs(x = "MPA",  y = "Biomass (kg per m2)", color = NULL) +
    coord_flip()+
    theme(axis.title = element_text(size = 12)) +
    facet_wrap(~bioregion)
  
  
  a <- ggplot(data = data_sp) +
    geom_histogram(aes(x = log_kg_per_m2)) +
    labs(title = species) +
    facet_wrap(~bioregion) 
  
  layout <- (a + ggtitle(species)) / ((b))
  layout
  
}

# plot_species("EJAC") # s/c
# plot_species("OCAL") # s/c
# plot_species("SATR") # s/c
# plot_species("PCLA") # s
# plot_species("CPRI") # s
# plot_species("SPUL") # s
# plot_species("CPUN") # s
# plot_species("SCAR") # all
# plot_species("SMIN") # all
plot_species("SCAU") # all
plot_species("OYT") # all
plot_species("SMYS") # all




# Try a gam
library(mgcv)

gam_model <- gam(kg_per_m2 ~ s(pref_habitat) + s(site_type, age_at_survey) +  s(year, bs = "re") + s(bioregion, bs = "re"),
                 family = Gamma(link = "log"), data = data_subset)



