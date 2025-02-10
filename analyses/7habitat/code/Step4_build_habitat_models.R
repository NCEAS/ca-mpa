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

# Fit all habitat combinations --------------------------------------------------------------

refine_habitat <- function(species, response, predictors_df, random_effects, data, regions, path) {
  print(paste("Starting species: ", species))
  data_sp <- data %>%
    filter(species_code == species, bioregion %in% regions) %>%
    mutate(bioregion = as.factor(bioregion),
           affiliated_mpa = as.factor(affiliated_mpa)) %>%
    # Scale all predictors
    mutate_at(vars(grep("^hard|soft|depth|kelp|age_at", names(.), value = TRUE)), scale)

  # Add a small constant, defined as the minimum value for that species
  if ("kg_per_m2" %in% colnames(data_sp)) {
    const <- min(data_sp$kg_per_m2[data_sp$kg_per_m2 > 0], na.rm = TRUE)
    data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_m2 + const))
  } else if ("weight_kg" %in% colnames(data_sp)) {
    const <- min(data_sp$weight_kg[data_sp$weight_kg > 0], na.rm = TRUE)
    data_sp <- data_sp %>% mutate(log_c_biomass = log(weight_kg + const))
  } else if ("kg_per_haul" %in% colnames(data_sp)) {
    const <- min(data_sp$kg_per_haul[data_sp$kg_per_haul > 0], na.rm = TRUE)
    data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_haul + const))
  } else {
    data_sp <- data_sp %>% mutate(log_c_biomass = NA_real_)
  }

  models <- list()

  models_df <- map_dfr(seq_len(nrow(predictors_df)), function(i) {
    predictors <- predictors_df$predictors[i]
    model_id <- predictors_df$model_id[i]

    model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    warning_message <- NULL
    message_text <- NULL
    error_message <- NULL
    singular_message <- NULL
    singular_status <- "Unknown"

    model <- suppressWarnings(
      tryCatch(
        {
          withCallingHandlers(
            { m <- lmer(model_formula, data = data_sp,
                        control = lmerControl(optCtrl = list(maxfun = 1e6)))
              # Check for singular fit if the model is successfully created
              singular_status <- tryCatch(
                {
                  if (isSingular(m)) "Singular fit" else "OK"
                },
                error = function(e) {
                  singular_message <<- paste("Error checking singularity:", conditionMessage(e))
                  "Unknown (Error in Singular Check)"
                }
              )
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

    data.frame(model_id = model_id,
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
               singular_message = ifelse(is.null(singular_message), "OK", singular_message),
               errors = ifelse(is.null(error_message), "OK", error_message),
               warnings = ifelse(is.null(warning_message), "OK", warning_message),
               messages = ifelse(is.null(message_text), "OK", message_text))
  }) %>%
    mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
    arrange(delta_AICc) %>% 
    mutate(across(c(singular_status, singular_message, errors, warnings, messages), ~ if_else(.x == "OK", NA, .x))) %>% 
    unite("messages", c(singular_status, singular_message, errors, warnings, messages), sep = ";", remove = T, na.rm = T) %>% 
    mutate(messages = if_else(messages == "", NA, messages))
  

  # Extract the base model and full models for each scale
  core_model_names <- predictors_df %>%
    filter(type == "base" | str_starts(type, "core")) %>%
    pull(model_id)

  # Extract the top models within deltaAICc of 4
  top_model_names <- models_df %>%
    filter(delta_AICc <= 4) %>%
    pull(model_id)

  # Extract the model objects for the core + top models
  models <- models[unique(c(top_model_names, core_model_names))]

  # Define the top, core, and full models (but save entire df for debug)
  models_df <- models_df %>%
    mutate(type = case_when(model_id %in% top_model_names ~ "top",
                            predictors == "site_type * age_at_survey" ~ "base",
                            model_id %in% core_model_names ~ "core"))

  # Save the subset
  saveRDS(list(models_df = models_df, models = models, data_sp = data_sp),
          file = file.path(path, paste0(species, "_models.rds")))

  models_df
}

# # Test species info
# species = "ELAT"
# response = "log_c_biomass"
# predictors_df = pred_kelp_max
# random_effects = c("year", "bioregion", "affiliated_mpa")
# data = data_kelp_subset
# regions = c("North", "Central", "South")
# path = "analyses/7habitat/output"

# # Test a dredge solution for the 3-way models ------
# 
# # Define helper function for reviewing 3-way interactions:
# test_interactions <- function(model) {
# 
#   summary_fit <- summary(model)
#   terms <- grep(".*:.*:.*", rownames(summary_fit$coefficients), value = TRUE)
# 
#   if (length(terms) > 0) {
#     three_way_pvals <- summary_fit$coefficients[terms, "Pr(>|t|)"]
#     significant_terms <- terms[three_way_pvals < 0.05]
# 
#     if (length(significant_terms) > 0) {
#       intx_test <- paste(significant_terms, collapse = "; ")
#     } else {
#       intx_test <- "NS"
#     }
#   } else {
#     intx_test <- "No 3-way interactions"
#   }
# 
#   return(intx_test)
# }
# 
# 
# refine_habitat_dredge <- function(species, response, predictors_df, random_effects, data, regions, path) {
#   print(paste("Starting species: ", species))
# 
#   data_sp <- data %>%
#     filter(species_code == species, bioregion %in% regions) %>%
#     mutate(year = as.factor(year),
#            bioregion = as.factor(bioregion),
#            affiliated_mpa = as.factor(affiliated_mpa)) %>%
#     # Scale all predictors
#     mutate_at(vars(grep("^hard|soft|depth|kelp|age_at", names(.), value = TRUE)), scale)
# 
#   # Add a small constant, defined as the minimum value for that species
#   if ("kg_per_m2" %in% colnames(data_sp)) {
#     const <- min(data_sp$kg_per_m2[data_sp$kg_per_m2 > 0], na.rm = TRUE)
#     data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_m2 + const))
#   } else if ("weight_kg" %in% colnames(data_sp)) {
#     const <- min(data_sp$weight_kg[data_sp$weight_kg > 0], na.rm = TRUE)
#     data_sp <- data_sp %>% mutate(log_c_biomass = log(weight_kg + const))
#   } else if ("kg_per_haul" %in% colnames(data_sp)) {
#     const <- min(data_sp$kg_per_haul[data_sp$kg_per_haul > 0], na.rm = TRUE)
#     data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_haul + const))
#   } else {
#     data_sp <- data_sp %>% mutate(log_c_biomass = NA_real_)
#   }
# 
#   models <- list()
#   models_df <- tibble()
#   top_models_list <- list()
# 
#   # Iterate over all models
#   for (i in seq_len(nrow(predictors_df))) {
#     predictors <- predictors_df$predictors[i]
#     model_id <- predictors_df$model_id[i]
#     model_list_type <- predictors_df$type[i]
# 
#     model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
#     warning_message <- NULL
#     message_text <- NULL
#     error_message <- NULL
#     singular_message <- NULL
#     singular_status <- "Unknown"
# 
#     model <- suppressWarnings(
#       tryCatch(
#         {
#           withCallingHandlers(
#             { m <- lmer(model_formula, data = data_sp,
#                         control = lmerControl(optCtrl = list(maxfun = 1e7)))
#             # Check for singular fit if the model is successfully created
#             singular_status <- tryCatch(
#               {
#                 if (isSingular(m)) "Singular fit" else "OK"
#               },
#               error = function(e) {
#                 singular_message <<- paste("Error checking singularity:", conditionMessage(e))
#                 "Unknown (Error in Singular Check)"
#               }
#             )
#             m
#             },
#             warning = function(w) {
#               warning_message <<- conditionMessage(w) # Capture warning message
#               invokeRestart("muffleWarning") # Suppress the warning display
#             },
#             message = function(msg) {
#               message_text <<- conditionMessage(msg) # Capture message
#               invokeRestart("muffleMessage") # Suppress message display
#             }
#           )
#         },
#         error = function(e) {
#           error_message <<- conditionMessage(e) # Capture error message
#           return(NULL) # Return NULL for the model on error
#         }
#       )
#     )
# 
#     # Store the model
#     models[[model_id]] <- model
# 
#     # Test the 3-way interactions
#     intx_test <- test_interactions(model)
# 
#     # Save model results
#     model_results <- data.frame(
#       model_id = model_id,
#       type = model_list_type,
#       intx_test = intx_test,
#       predictors = gsub("\\s*\\+\\s*", " + ", predictors),
#       response = response,
#       regions = paste(regions, collapse = ", "),
#       random_effects = paste(random_effects, collapse = ", "),
#       AICc = if (!is.null(model)) AICc(model) else NA,
#       n = if (!is.null(model)) nobs(model) else NA,
#       n_sites = if (!is.null(model)) n_distinct(data_sp$site) else NA,
#       n_mpas = if (!is.null(model)) n_distinct(data_sp$affiliated_mpa) else NA,
#       singular_status = singular_status,
#       singular_message = ifelse(is.null(singular_message), "OK", singular_message),
#       errors = ifelse(is.null(error_message), "OK", error_message),
#       warnings = ifelse(is.null(warning_message), "OK", warning_message),
#       messages = ifelse(is.null(message_text), "OK", message_text)
#     )
# 
#     models_df <- bind_rows(models_df, model_results)
# 
#     # # If the model is in the "max" list, run dredge()
#     # if (!is.null(model) && model_list_type == "max") {
#     #   print(paste("Running dredge for model:", model_id))
#     #   model_dredge <- dredge(model, rank = AICc)
#     #   best_submodels <- get.models(model_dredge, subset = delta < 4)
#     #
#     #   # Store the top submodels from this max model
#     #   top_models_list[[model_id]] <- best_submodels
#     # }
#   }
# 
#   print("  Models complete. Starting review.")
#   
#   max_models <- models_df %>% 
#     filter(type == "max" & !intx_test == "NS")
#   
#   
#   
# 
#   # Extract the best submodels from "max" models
#   top_models <- do.call(c, top_models_list)  # Flatten list of best models
#   top_models_df <- tibble(
#     model_id = names(top_models),
#     AICc = sapply(top_models, AICc)
#   ) %>%
#     arrange(AICc) %>%
#     mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
#     filter(delta_AICc <= 4)  # Keep models within ΔAICc ≤ 4
# 
#   # Merge with full models for saving
#   final_models <- models_df %>%
#     mutate(type = case_when(model_id %in% top_models_df$model_id ~ "top",
#                             predictors == "site_type * age_at_survey" ~ "base",
#                             list == "full" ~ "full"))
# 
#   # Extract final set of models
#   models <- models[names(models) %in% final_models$model_id]
# 
#   print("  Final models extracted. Saving results.")
# 
#   # Save output
#   saveRDS(list(models_df = final_models, models = models, data_sp = data_sp),
#           file = file.path(path, paste0(species, "_models.rds")))
# 
#   return(final_models)
# }
# 
# 
# 
