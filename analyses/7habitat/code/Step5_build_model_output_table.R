# Step 5: Analyze Model Results
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Analyze the focal models that were fit and extracted in the previous step.
# The extracted species_subset.Rds contains:
#   -- models_df: dataframe with summary of model results, including model_id, 
#         predictor string and other specifications used in model fitting, AICc
#         and other fit results (singular status, warnings), and the "type" of the
#         model (base, top, or core == full). *if base model was top model, == top
#   -- models: the model fit objects from above df, identified by the model_id
#   -- data_sp: the data used to fit the model

# 1. Process top models:
#     If there are multiple top models, calculate the model average
#         and extract the weighted estimates and importance scores for each predictor
#     If there is only one top model, use tidy to extract the estimates etc.
# 2. Process full and base model:
#     Extract the estimates and errors for each of the full models (at each scale) 
#     and the base model (site * age)
# 3. Combine results into a single dataframe and export

rm(list = ls())
gc()

library(tidyverse)
library(MuMIn) # for model averaging
library(broom.mixed) # for extracting fit info

# Helper Functions -------------------------------------------------------------

clean_terms <- function(df) {df %>%
    mutate(term_revised = str_remove_all(term, "MPA") %>% 
             str_remove_all("_annual_250|_annual_500|_annual_100|_annual_50|_annual_25") %>% 
             str_remove_all("_250|_500|_100|_25|_50") %>% 
             if_else(str_detect(., ":site_type"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .) %>% 
             factor(levels = c("(Intercept)",
                               "site_type:depth_mean",
                               "site_type:depth_sd",
                               "site_type:soft_bottom",
                               "site_type:hard_bottom",
                               "site_type:kelp",
                               "site_type:age_at_survey",
                               "site_type",
                               "age_at_survey",
                               "depth_mean",
                               "depth_sd",
                               "soft_bottom",
                               "hard_bottom",
                               "kelp")))}

add_significance <- function(df) {df %>%
    mutate(significance = factor(case_when(p_value < 0.001 ~ "***",
                                           p_value < 0.01 ~ "**",
                                           p_value < 0.05 ~ "*",
                                           is.na(p_value) ~ "NA",
                                           TRUE ~ "NS"), levels = c("***", "**", "*", "NS", "NA")))
}

# Analyze Focal Models ---------------------------------------------------------
species <- "SMYS"
path = "analyses/7habitat/output/kelp/all_regions/consolidated"
path = "analyses/7habitat/output/rock/all_regions/no_soft"
path = "analyses/7habitat/output/surf/central_south"
path = "analyses/7habitat/output/deep/all_regions"


analyze_models <- function(species, path){
  #Read data containing all the focal models 
  data <- readRDS(file.path(path, paste0(species, "_models.rds"))) 
  models_df <- data$models_df %>% 
    mutate(scale = case_when(model_id == "ST*A" ~ NA,
                             T ~ as.factor(str_extract(predictors, "\\d+")))) %>% 
    distinct() %>% 
    mutate(depth_type = case_when(str_detect(model_id, "DSD") ~ "depth_sd",
                                  str_detect(model_id, "DM") ~ "depth_mean")) %>% 
    filter(!is.na(type))

  # Process top models:
  # If there are multiple, get the model average. If not, get the results from the top one.
  if (sum(models_df$type == "top") > 1) {
    model_avg <- model.avg(data$models[models_df$type == "top"], fit = TRUE)
    coef_table <- data.frame(coefTable(model_avg)) %>%
      rownames_to_column("term") %>% 
      janitor::clean_names() %>% 
      dplyr::select(-df) %>%
      mutate(term       = str_replace(term, "typeMPA", "type"),
             conf_low   = estimate - 1.96 * std_error,
             conf_high  = estimate + 1.96 * std_error,
             importance = sw(model_avg)[term],
             p_value    = NA) 
  } else {
    coef_table <- tidy(data$models[models_df$type == "top"][[1]], conf.int = TRUE, effect = "fixed") %>%
      mutate(term = str_replace(term, "typeMPA", "type"),
             importance = 1) %>% 
      janitor::clean_names()
  } 
  
  # Create df with the results from the top models
  top_results <- coef_table %>%
    clean_terms() %>%
    add_significance() %>%
    mutate(num_top_models = sum(models_df$type == "top"),
           scale = paste(unique(models_df$scale[models_df$type == "top"]), collapse = ", "),           
           key = if_else(num_top_models == 1, "Top Model v. Base Model", "Top Models (Average) v. Base Model")) 
  
  # Add results for each of the individual top models (without averaging)
  if (sum(models_df$type == "top") > 1) {
    top_names <- models_df$model_id[models_df$type %in% c("top")]
    top_results_full <- lapply(top_names, function(model_id) {
      if (is.null(data$models[[model_id]])) {
        return(NULL)  # Skip processing and return NULL
      }
      tidy(data$models[[model_id]], conf.int = TRUE, effect = "fixed") %>%
        janitor::clean_names() %>% 
        clean_terms() %>%
        add_significance() %>%
        mutate(model_id = model_id)
    }) 
    
    top_results_full <- do.call(rbind, top_results_full) %>% 
      left_join(., models_df %>% dplyr::select(model_id, scale)) %>% 
      mutate(key = if_else(model_id == "ST*A", NA, "Top Model"))
    
  } else {
    top_results_full <- NULL
  }
  
  # Process core models
  core_names <- models_df$model_id[models_df$type %in% c("core", "base")]
  
  core_results <- lapply(core_names, function(model_id) {
    if (is.null(data$models[[model_id]])) {
      return(NULL)  # Skip processing and return NULL
    }
    tidy(data$models[[model_id]], conf.int = TRUE, effect = "fixed") %>%
      janitor::clean_names() %>% 
      clean_terms() %>%
      add_significance() %>%
      mutate(model_id = model_id)
  }) 
  
  # Combine core model results
  core_results <- do.call(rbind, core_results) %>% 
    left_join(., models_df %>% dplyr::select(model_id, scale)) %>% 
    mutate(key = if_else(model_id == "ST*A", NA, "Full Model"))
  
  # Combine all results
  all_results <- bind_rows(top_results, core_results, top_results_full) %>%
    mutate(species_code = species,
           scale = factor(scale, levels = c(25, 50, 100, 250, 500))) %>% 
    mutate(key = case_when(model_id == "ST*A" & unique(top_results$num_top_models) > 1 ~ "Top Models (Average) v. Base Model",
                           model_id == "ST*A" & unique(top_results$num_top_models) ~ "Top Model v. Base Model",
                           T~key),
           model_id = if_else(is.na(model_id) & num_top_models > 1, "Model Average", model_id)) %>% 
    dplyr::select(species_code, model_id, key, scale, term, term_revised, everything(), -effect)
  
  # Export 
  saveRDS(all_results, file = file.path(path, paste0(species, "_results.rds")))

  
}


# Run Analysis -----------------------------------------------------------------

# Kelp forest 
sp_list <- list.files(path = "analyses/7habitat/output/kelp/all_regions/consolidated") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

walk(sp_list, ~analyze_models(.x, path = "analyses/7habitat/output/kelp/all_regions/consolidated"))

# Rocky reef 
sp_list <- list.files(path = "analyses/7habitat/output/rock/all_regions/no_soft") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

walk(sp_list, ~analyze_models(.x, path = "analyses/7habitat/output/rock/all_regions/no_soft"))

# Surf 
sp_list <- list.files(path = "analyses/7habitat/output/surf/central_south") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

walk(sp_list, ~analyze_models(.x, path = "analyses/7habitat/output/surf/central_south"))

# Deep
sp_list <- list.files(path = "analyses/7habitat/output/deep/all_regions") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

walk(sp_list, ~analyze_models(.x, path = "analyses/7habitat/output/deep/all_regions"))




