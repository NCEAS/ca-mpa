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

# library(tidyverse)
# library(lme4)
# library(MuMIn)
# library(dplyr)
# library(purrr)
# library(tidymodels)
# library(lmerTest)

# Testing:
# type = "target_status"
# focal_group = "targeted"
# biomass_variable = "weight_kg"
# biomass_variable = "kg_per_100m2"
# predictors_df = head(pred_rock_2way, 25)
# random_effects = c("region4/affiliated_mpa", "year")
# data = data_rock
# data = data_kelp
# regions = c("North", "Central", "N. Channel Islands", "South")
# path = "analyses/7habitat/output/targeted/test"


fit_habitat_models <- function(data_sp, focal_group, predictors_df, random_effects, path) {
  

  ## 2. Fit Models -----------------------------------------------------------------------
  models <- list()
  response <- "log_c_biomass"
  
  models_df <- map_dfr(seq_len(nrow(predictors_df)), function(i) {
    predictors <- predictors_df$predictors[i]
    model_id <- predictors_df$model_id[i]

    model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    warning_message <- NULL
    message_text <- NULL
    error_message <- NULL
    singular_status <- "Unknown"

    model <- suppressWarnings(
      tryCatch(
        {
          withCallingHandlers(
            { m <- lmer(model_formula, data = data_sp, REML = FALSE,
                        control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
              singular_status <- if (isSingular(m)) "Singular fit" else "OK"
              m
            },
            warning = function(w) {
              warning_message <<- conditionMessage(w) # Capture warning message
              invokeRestart("muffleWarning") # Suppress the warning display
            },
            message = function(msg) {
              message_text <<- conditionMessage(msg) # Capture message
              invokeRestart("muffleMessage") # Suppress message display
            }
          )
        },
        error = function(e) {
          error_message <<- conditionMessage(e) # Capture error message
          return(NULL) # Return NULL for the model on error
        }
      )
    )

    models[[model_id]] <<- model

    data.frame(focal_group = focal_group,
               model_id = model_id,
               predictors = gsub("\\s*\\+\\s*", " + ", predictors),
               response = response,
               regions = paste(regions, collapse = ", "),
               random_effects = paste(random_effects, collapse = ", "),
               AICc = if (!is.null(model)) AICc(model) else NA,
               logLik = if (!is.null(model)) as.numeric(logLik(model)) else NA,
               n = if (!is.null(model)) nobs(model) else NA,
               n_sites = if (!is.null(model)) n_distinct(data_sp$site) else NA,
               n_mpas = if (!is.null(model)) n_distinct(data_sp$affiliated_mpa) else NA,
               singular_status = singular_status,
               errors = ifelse(is.null(error_message), "OK", error_message),
               warnings = ifelse(is.null(warning_message), "OK", warning_message),
               messages = ifelse(is.null(message_text), "OK", message_text))
  }) %>%
    mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
    arrange(delta_AICc) %>% 
    mutate(across(c(singular_status, errors, warnings, messages), ~ if_else(.x == "OK", NA, .x))) %>% 
    unite("messages", c(singular_status, errors, warnings, messages), sep = ";", remove = T, na.rm = T) %>% 
    mutate(messages = if_else(messages == "", NA, messages))
  
  print("  Models complete. Starting extraction.")
  
  # Extract the base model and full models for each scale
  core_model_names <- predictors_df %>%
    filter(type == "base" | str_starts(type, "core")) %>%
    pull(model_id)

  # Extract the top models within deltaAICc of 10 
  top_model_names <- models_df %>%
    filter(delta_AICc <= 6) %>%
    pull(model_id)

  # Extract the model objects for the core + top models
  models <- models[unique(c(top_model_names, core_model_names))]

  # Define the top, core, and full models (but save entire df for debug)
  models_df <- models_df %>%
    mutate(type = case_when(model_id %in% top_model_names ~ "top",
                            predictors == "site_type * age_at_survey" ~ "base",
                            model_id %in% core_model_names ~ "core"))
  
  # Find those that did not converge or had other issues
  problems <- models_df %>% 
    filter(!is.na(messages))
  
  print(paste("  Problems: ", length(unique(problems$model_id))))

  # Save the subset
  saveRDS(list(models_df = models_df, models = models, data_sp = data_sp),
          file = file.path(path, "models", paste(habitat, focal_group, re_string, "models.rds", sep = "_")))
  
  print("  Saved.")
  
  

}


