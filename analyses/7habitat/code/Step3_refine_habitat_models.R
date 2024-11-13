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


# Read Data --------------------------------------------------------------------
# Proceed here directly from Step2_combine_tables.R
rm(list = setdiff(ls(), "data"))
gc()


# Generate predictors -----------------------------------------------------------
base_predictors <- c("site_type * age_at_survey")

habitat_50  <- c("hard_bottom_biotic_0_30m_50",  
                 "soft_bottom_biotic_0_30m_50", 
                 "hard_bottom_0_30m_50",       
                 "soft_bottom_0_30m_50")      
habitat_100 <- c("hard_bottom_biotic_0_30m_100", 
                 "soft_bottom_biotic_0_30m_100", 
                 "hard_bottom_0_30m_100",   
                 "soft_bottom_0_30m_100")
habitat_250 <- c("hard_bottom_biotic_0_30m_250", 
                 "soft_bottom_biotic_0_30m_250",
                 "hard_bottom_0_30m_250",    
                 "soft_bottom_0_30m_250",
                 "hard_bottom_30_100m_250",    
                 "soft_bottom_30_100m_250")   
habitat_500 <- c("hard_bottom_biotic_0_30m_500",
                 "soft_bottom_biotic_0_30m_500",
                 "hard_bottom_0_30m_500", 
                 "soft_bottom_0_30m_500",
                 "hard_bottom_30_100m_500", 
                 "soft_bottom_30_100m_500")

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
  get_predictors(habitat_50),
  get_predictors(habitat_100),
  get_predictors(habitat_250),
  get_predictors(habitat_500)
)

predictors_list <- unique(predictors_list)


# Compare all habitat combinations ----------------------------------------------
refine_habitat <- function(species) {
  data_sp <- data_subset %>%  filter(species_code == species)
  print(paste("Species: ", species))
  
  models <- list()
  models_df <- map_dfr(predictors_list, function(predictors) {
    model_formula <- as.formula(paste(response, "~", paste(predictors, collapse = " + "), "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    model <- lmer(model_formula, data = data_sp)
    models[[paste(predictors, collapse = ", ")]] <<- model
    
    data.frame(
      predictors = paste(predictors, collapse = ", "),
      AICc = AICc(model),
      logLik = logLik(model),
      n = nobs(model),
      n_sites = n_distinct(data_sp$site),
      n_mpas = n_distinct(data_sp$affiliated_mpa)
    )
  }) %>%
    mutate(delta_AICc = AICc - min(AICc)) %>% 
    arrange(delta_AICc)

  
  saveRDS(list(models_df = models_df, models = models), file = file.path(save_path, paste0(species, "_models.rds")))
  
  models_df
}




# Run for a handful of species

# Define response variable
response <- c("log_kg_per_m2")

# Define random effects
random_effects <- c("year", "bioregion")

# Get numeric predictors
num_predictors <- unique(unlist(predictors_list)) %>% setdiff(c("site_type", "bioregion"))

# Filter and preprocess data
data_subset <- data %>%
  filter(mpa_defacto_class == "smr") %>%
  filter(age_at_survey >= 0) ##%>% 
# mutate(across(all_of(num_predictors), scale))

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

test <- data %>% 
  filter(kg_per_m2 > 0) %>% 
  group_by(species_code, sciname, target_status, bioregion) %>% 
  summarize(total_biomass = sum(kg_per_m2),
            total_count = sum(count_per_m2),
            n_obs = n()) %>% 
  filter(n_obs > 50) %>% dplyr::select(species_code, sciname, target_status, bioregion, total_biomass) %>% 
  pivot_wider(names_from = "bioregion", values_from = "total_biomass") %>% 
  filter(North > 0 & Central > 0 & South > 0)


save_path <- "analyses/7habitat/output/refine_pref_habitat" 

species_list <- c("OYT", "SMYS", "SCAU")
species_list <- test$species_code

results <- refine_habitat(species = "OYT")

walk(species_list, function(species) {
  results_df <- refine_habitat(species = species)
  cat("\nTop 5 models for species:", species, "\n")
  print(head(results_df, 5))
})



# 2. Function to load models, select top ones, and analyze with model averaging
analyze_top_models <- function(species, file_path, delta_aicc_threshold = 4) {
  data <- readRDS(file.path(save_path, paste0(species, "_models.rds")))
  models_df <- data$models_df
  models <- data$models
  
  top_model_names <- models_df %>% filter(AICc <= min(AICc) + delta_aicc_threshold) %>% pull(predictors)
  top_models <- models[top_model_names]
  
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
  habitat_predictors <- setdiff(names(predictor_signs), c("site_type", "age_at_survey", "site_typeReference", "(Intercept)", "age_at_survey:site_typeReference", "site_typeReference:age_at_survey"))
  
  # Create summary data frame
  summary_df <- data.frame(
    species = species,
    predictor = habitat_predictors,
    importance_score = unname(predictor_importance[habitat_predictors]),
    sign = unname(predictor_signs[habitat_predictors]),
    num_models = length(top_models),
    row.names = NULL
  )
  
  return(summary_df)
}



analyze_top_models(species = "OYT")


analyze_multiple_species <- function(species_list, save_path, delta_aicc_threshold = 4) {
  map_dfr(species_list, function(species) {
    file_path <- file.path(save_path, paste0(species, "_models.rds"))
    analyze_top_models(species, file_path, delta_aicc_threshold)
  })
}


consolidated_results <- analyze_multiple_species(species_list, save_path)
consolidated_results


# Let's explore the top models performance:
important_predictors <- consolidated_results %>%
  filter(importance_score >= 0.5) %>% 
  group_by(species) %>%
  summarize(top_predictors = list(predictor)) %>%
  deframe()  # Convert to named list by species


library(lme4)
library(ggplot2)


generate_diagnostic_plots <- function(species, data_subset, important_predictors, random_effects, save_path) {
  data_sp <- data_subset %>% filter(species_code == species)
  predictors <- c(important_predictors[[species]], "age_at_survey", "site_type")
  
  model_formula <- as.formula(paste("log_kg_per_m2", "~", paste(predictors, collapse = " + "), 
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

# Run the diagnostics for each species
map(species_list, ~ generate_diagnostic_plots(.x, data_subset, important_predictors, random_effects = c("year", "bioregion"), save_path = save_path))





