# Cori Loapzanski
# August 2024

# About ------------------------------------------------------------------------

# Explore biomass (kg_per_m2) of fish species using mixed-effects models 
# Evaluate the influence of habitat characteristics on biomass
# Compare multiple models to identify the best-fitting model

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(tidymodels)
library(multilevelmod)
library(dplyr)
library(broom)
library(broom.mixed)
library(performance)
library(purrr)
library(MuMIn)
library(gtools)

# Read Data --------------------------------------------------------------------
# Proceed here directly from Step2_combine_tables.R
rm(list = setdiff(ls(), "data"))
gc()


# Generate predictors -----------------------------------------------------------
base_predictors <- c("site_type", "age_at_survey")

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
generate_predictors_list <- function(habitat_buffer_list) {
  predictors <- list()
  
  for (r in 1:length(habitat_buffer_list)) {
    habitat_combinations <- combn(habitat_buffer_list, r, simplify = FALSE)
    
    for (combo in habitat_combinations) {
     # predictors <- append(predictors, list(c("site_type", "age_at_survey")))
      #predictors <- append(predictors, list(combo))
      #predictors <- append(predictors, list(c(combo, "site_type")))
     # predictors <- append(predictors, list(c(combo, "age_at_survey")))
      predictors <- append(predictors, list(c(combo, base_predictors)))
    }
  }
  
  return(predictors)
}

predictors_list <- c(
  generate_predictors_list(habitat_50),
  generate_predictors_list(habitat_100),
  generate_predictors_list(habitat_250),
  generate_predictors_list(habitat_500)
)

predictors_list <- unique(predictors_list)


# 1. Specify mixed-effects model to refine habitat type ----------------------------

fit_model <- function(response, predictors, data) {
  full_formula <- as.formula(paste(response, "~", 
                                   paste(predictors, collapse = " + "), "+", 
                                   paste(random_effects, collapse = " + ")))
  
  recipe <- recipe(full_formula, data = data_subset) %>%
    step_log(all_outcomes(), base = 10, offset = 1) %>%
    step_scale(all_numeric_predictors())
  
  mixed_model_spec <- linear_reg() %>%
    set_engine("lmer")
  
  workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(mixed_model_spec, 
              formula = as.formula(paste(response, "~", 
                                         paste(predictors, collapse = " + "), "+",
                                         paste0("(1 | ", random_effects, ")", collapse = " + "))))
  
  fitted_model <- fit(workflow, data = data_subset)
  
  list(
    fitted_model = fitted_model,
    formula_str = as.character(full_formula),
    model_name = paste(response, paste(predictors, collapse = "_"), paste0("(1 | ", random_effects, ")", collapse = " + "), sep = "_")
  )
}

# Extract results  ----------------------------------------------------------------------
extract_results <- function(fit_result) {
  model <- extract_fit_parsnip(fit_result$fitted_model) %>% pluck("fit")
  fixed_effects <- tidy(model, effects = "fixed") %>%  
    mutate(effect_type = "fixed",
           p_value = p.value)
  
  random_effects <- tidy(model, effects = "ran_pars") %>% 
    mutate(effect_type = "random")
  
  effects <- bind_rows(fixed_effects, random_effects) %>%  
    mutate(model_name = fit_result$model_name,
           p_value = p.value)
  
  
  details <- tibble(
    model_name = fit_result$model_name,
    logLik = as.numeric(logLik(model)),
    AIC = AIC(model),
    AICc = as.numeric(AICc(model)),
    delta_AICc = AICc - min_AICc,
    delta_AIC = AIC - min_AIC,
    r_squared_marginal = if (!inherits(r_squared, "try-error")) r_squared[1, "R2m"] else NA,
    r_squared_conditional = if (!inherits(r_squared, "try-error")) r_squared[1, "R2c"] else NA)
  
  list(effects_table = effects,
       details_table = details)}

# Specify CI Models ----------------------------------------------

ci_model <- function(response, predictors, data) {
  full_formula <- as.formula(str_replace_all(
    paste(response, "~", 
          paste(predictors, collapse = " + "), "+", 
          paste(random_effects, collapse = " + ")), "\\*", "+"))
  
  recipe <- recipe(full_formula, data = data) %>%
    step_log(all_outcomes(), base = 10, offset = 1) %>% 
    step_scale(all_numeric_predictors())
  
  mixed_model_spec <- linear_reg() %>%
    set_engine("lmer")
  
  workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(mixed_model_spec, 
              formula = as.formula(paste(response, "~", 
                                         paste(predictors, collapse = " + "), "+",
                                         "(1| affiliated_mpa)")))
  
  fitted_model <- fit(workflow, data = data)
  
  list(
    fitted_model = fitted_model,
    formula_str = as.character(full_formula),
    model_name = paste(response, 
                       paste(predictors, collapse = "_"), 
                       "(1| affiliated_mpa)", sep = "_")
  )
}

# Specify combined models ----------------------------------------------
pref_habitat_model <- function(response, predictors, data){
  full_formula <- as.formula(str_replace_all(
    paste(response, "~", 
          paste(predictors, collapse = " + "), "+", 
          paste(random_effects, collapse = " + ")), "\\*", "+"))
  
  recipe <- recipe(full_formula, data = data) %>%
    step_log(all_outcomes(), base = 10, offset = 1) %>% 
    step_scale(all_numeric_predictors())
  
  mixed_model_spec <- linear_reg() %>%
    set_engine("lmer")
  
  workflow <- workflow() %>%
    add_recipe(recipe) %>%
    add_model(mixed_model_spec, 
              formula = as.formula(paste(response, "~", 
                                         paste(predictors, collapse = " + "), "+",
                                         paste0("(1 | ", random_effects, ")", collapse = " + "))))
  
  fitted_model <- fit(workflow, data = data)
  
  list(
    fitted_model = fitted_model,
    formula_str = as.character(full_formula),
    model_name = paste(response, 
                       paste(predictors, collapse = "_"),
                       paste0("(1 | ", random_effects, ")", collapse = "_"), sep = "_")
  )
}

# Run ----------------------------------------------------------------------------

# Define response variable
responses <- c("kg_per_m2")

# Define random effects
random_effects <- c("year", "region")

# Subset data to species of interest
data_subset <- data %>% 
  filter(mpa_defacto_class == "smr") %>% 
  filter(species_code == "OYT") %>% 
  filter(age_at_survey >= 0)

fitted_models <- purrr::map(predictors_list, ~fit_model(responses[[1]], .x, data_subset))


# Find minimum AICc
min_AICc <- map_dbl(fitted_models, 
                    ~ AICc(extract_fit_parsnip(.x$fitted_model) %>% pluck("fit"))) %>% min()

min_AIC <- map_dbl(fitted_models, 
                   ~ AIC(extract_fit_parsnip(.x$fitted_model) %>% pluck("fit"))) %>% min()

# Extract the results and combine into tables
results <- purrr::map(fitted_models, extract_results)
details <- purrr::map_dfr(results, "details_table") %>% 
  arrange(delta_AICc) %>% 
  slice(1:20)

effects <- purrr::map_dfr(results, "effects_table") %>% 
  filter(model_name %in% details$model_name)


## Examine and refine habitat ---------------------------------------------------
# Specify Preferred habitat 
pref_habitat <- c("hard_bottom_biotic_0_30m_250", "soft_bottom_biotic_0_30m_250")
#pref_habitat <- c("hard_bottom_0_30m_500")

# Define random effects
random_effects <- c("affiliated_mpa")

# Create combined habitat predictors
data_subset <- data_subset %>% 
  mutate(pref_habitat_m2 = rowSums(across(all_of(pref_habitat)))) 

# Create list of new predictors
ci_predictors <- list(
  c("site_type * age_at_survey"), # without habitat control
  c("site_type * age_at_survey", "pref_habitat_m2"), # with habitat control
  c("site_type * age_at_survey", "site_type * pref_habitat_m2")
)

# Fit habitat models
fitted_ci_models <- purrr::map(ci_predictors, ~ci_model(responses[[1]], .x, data = data_subset))

min_AICc <- map_dbl(fitted_ci_models, 
                    ~ AICc(extract_fit_parsnip(.x$fitted_model) %>% pluck("fit"))) %>% min()
min_AIC <- map_dbl(fitted_ci_models, 
                    ~ AIC(extract_fit_parsnip(.x$fitted_model) %>% pluck("fit"))) %>% min()

results_ci <- purrr::map(fitted_ci_models, extract_results)

effects_ci <- purrr::map_dfr(results_ci, "effects_table")

details_ci <- purrr::map_dfr(results_ci, "details_table")

names(fitted_ci_models) <- details_ci$model_name


# Combined predictors
pref_predictors <- list(
  c("pref_habitat_m2", "age_at_survey"), # habitat alone
  c("site_type * age_at_survey"),
  c("pref_habitat_m2 * site_type", "site_type * age_at_survey")
  ) # protection * habitat

fitted_pref_models <- purrr::map(pref_predictors, ~pref_habitat_model(responses[[1]], .x, data = data_subset))

min_AICc <- map_dbl(fitted_pref_models, 
                    ~ AICc(extract_fit_parsnip(.x$fitted_model) %>% pluck("fit"))) %>% min()
min_AIC <- map_dbl(fitted_pref_models, 
                   ~ AIC(extract_fit_parsnip(.x$fitted_model) %>% pluck("fit"))) %>% min()

results_pref <- purrr::map(fitted_pref_models, extract_results)

effects_pref <- purrr::map_dfr(results_pref, "effects_table")
details_pref <- purrr::map_dfr(results_pref, "details_table")


# Predict ----------------------------------------------------------------------------

# Create a function to generate predictions for each site_type
generate_predictions <- function(fitted_model, site_type, data, new_data_cols) {
  # Filter data for the specific site_type
  new_data <- data %>%
    filter(site_type == !!site_type) %>%
    mutate(site_type = site_type)
  
  # Generate predictions
  predictions <- predict(fitted_model, new_data)
  
  # Combine predictions with the new_data and select only the required columns
  predictions_df <- new_data %>%
    mutate(kg_per_m2_predict = predictions) %>%
    dplyr::select(any_of(c(pref_habitat, "pref_habitat_m2")), year, age_at_survey, affiliated_mpa,  site, site_type, kg_per_m2_predict)
  
  return(predictions_df)
}

# Apply the function across all fitted models and site types
pref_predictions <- purrr::map_dfr(fitted_pref_models, function(model) {
  model_fit <- extract_fit_parsnip(model$fitted_model) %>% pluck("fit")
  model_predictors <- str_replace_all(model$formula_str, paste(responses[[1]], "~"), "") %>%
    str_split(" \\+ ") %>%
    unlist() %>%
    str_trim()
  
  bind_rows(generate_predictions(model_fit, "MPA", data_subset, model_predictors), 
            generate_predictions(model_fit, "Reference", data_subset, model_predictors)) %>%
    mutate(model_name = model$model_name)
}) %>%
  select(pref_habitat_m2, year, age_at_survey,  affiliated_mpa, site, site_type, model_name, kg_per_m2_predict) %>% 
  mutate(model_label = factor(case_when(model_name == "kg_per_m2_pref_habitat_m2 * site_type_age_at_survey_(1 | year)_(1 | region)" ~ "Habitat * MPA/Ref",
                                        model_name == "kg_per_m2_site_type_age_at_survey_(1 | year)_(1 | region)" ~ "MPA/Ref Only",
                                        model_name == "kg_per_m2_pref_habitat_m2_site_type_age_at_survey_(1 | year)_(1 | region)" ~ "MPA/Ref + Habitat",
                                        model_name == "kg_per_m2_site_type * age_at_survey_pref_habitat_m2_(1 | year)_(1 | region)" ~ "MPA/Ref * Age + Habitat",
                                        model_name =="kg_per_m2_site_type * age_at_survey_(1 | year)_(1 | region)" ~  "MPA/Ref * Age",
                                        model_name == "kg_per_m2_pref_habitat_m2_age_at_survey_(1 | year)_(1 | region)"  ~ "Habitat Only",
                                        model_name == "kg_per_m2_pref_habitat_m2 * site_type *age_at_survey_(1 | year)_(1 | region)" ~ "Habitat * MPA/Ref * MPA Age"),
                        
                              levels = c("MPA/Ref Only", "Habitat Only", "MPA/Ref + Habitat", "MPA/Ref * Age", "MPA/Ref * Age + Habitat", "Habitat * MPA/Ref", "Habitat * MPA/Ref * MPA Age")))



ggplot(data = pref_predictions) +
  geom_smooth(aes(x = pref_habitat_m2, y = kg_per_m2_predict,
                  color = site_type), method = "glm") +
  scale_color_manual(values = c("#ff7eb6", "#d4bbff")) +
  # geom_point(aes(x = !!sym(pref_habitat), y = kg_per_m2_predict,
  #                 color = site_type)) +
  theme_minimal() + 
  labs(x = "Area of preferred habitat (km^2)",
       y = "Predicted biomass (kg per km^2)",
       color = "Site Type") +
  theme(strip.text = element_text(size = 10)) +
  facet_wrap(~model_name)

ggplot(data = pref_predictions %>% 
         filter(!is.na(model_label))) +
  geom_point(aes(x = age_at_survey, y = kg_per_m2_predict*1e6,
                  color = site_type)) +
  geom_smooth(aes(x = age_at_survey, y = kg_per_m2_predict*1e6,
                  color = site_type), method = "gam") +
  scale_color_manual(values = c("#ff7eb6", "#d4bbff")) +
  theme_minimal() + 
  labs(x = "Age of MPA",
       y = "Predicted biomass (kg per km^2)",
       color = "Site Type",
       title = "After only data (age_at_survey >= 0)") +
  theme(strip.text = element_text(size = 10)) +
  facet_wrap(~model_label)
