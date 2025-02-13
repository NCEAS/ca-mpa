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
             if_else(str_detect(., ":age_at_survey$"), str_replace(., "^(.*):age_at_survey$", "age_at_survey:\\1"), .) %>% 
             if_else(str_detect(., ":site_type$"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .) %>% 
             if_else(str_detect(., ":site_type:"), str_replace(., "^(.*?):(site_type):(.*?)$", "\\2:\\3:\\1"), .) %>% 
             str_replace_all("site_type:(.*?):age_at_survey", "site_type:age_at_survey:\\1") %>% 
             factor(levels = c("(Intercept)",
                               "site_type:age_at_survey:depth_mean",
                               "site_type:age_at_survey:depth_sd",
                               "site_type:age_at_survey:depth_cv",
                               "site_type:age_at_survey:kelp",
                               "site_type:age_at_survey:hard_bottom",
                               "age_at_survey:depth_mean",
                               "age_at_survey:depth_sd",
                               "age_at_survey:depth_cv",
                               "age_at_survey:hard_bottom",
                               "age_at_survey:kelp",
                               "site_type:depth_mean",
                               "site_type:depth_sd",
                               "site_type:depth_cv",
                               "site_type:soft_bottom",
                               "site_type:hard_bottom",
                               "site_type:kelp",
                               "site_type:age_at_survey",
                               "site_type",
                               "age_at_survey",
                               "depth_mean",
                               "depth_sd",
                               "depth_cv",
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




# Analyze Focal Models: 3-way version -------------------------------------------

species <- "ELAT"
habitat <- "kelp"
path <- "analyses/7habitat/output/kelp"

analyze_models_3way <- function(species, path, habitat){
  print(paste("Species:", species))
  # Read predictor list (for the new types)
  pred_int <- readRDS(file.path(paste0("analyses/7habitat/intermediate_data/", habitat, "_predictors_interactions.Rds"))) %>% 
    rename(type_orig = type)
  
  # Read focal models 
  data <- readRDS(file.path(path, paste0(species, "_models.rds"))) 
  
  models_df <- data$models_df %>% 
    mutate(scale = case_when(model_id == "ST*A" ~ NA,
                             T ~ as.factor(str_extract(predictors, "\\d+")))) %>% 
    mutate(depth_type = case_when(str_detect(model_id, "DSD") ~ "depth_sd",
                                  str_detect(model_id, "DM") ~ "depth_mean",
                                  str_detect(model_id, "DCV") ~ "depth_cv")) %>% 
    left_join(., pred_int %>% dplyr::select(model_id, type_orig), by = "model_id") %>% 
    mutate(type = if_else(type == "core", NA_character_, type)) %>% 
    mutate(type = coalesce(type, type_orig)) %>% 
    filter(!is.na(type))
  
  if (sum(!is.na(unique(models_df$messages))) > 0){
    print(paste("  Messages:", unique(models_df$messages)))
  } else {
    print("  No messages.")
  }

  # Process top models:
  # Extract results from all top models 
  print(paste("  Top models:", sum(models_df$type == "top")))
  
  # Examine results for each of the individual top models (without averaging)
  if (sum(models_df$type == "top") > 1) {
    top_names <- models_df$model_id[models_df$type %in% c("top")]
    top_results_full <- lapply(top_names, function(model_id) {
      if (is.null(data$models[[model_id]])) { return(NULL) }
      tidy(data$models[[model_id]], conf.int = TRUE, effect = "fixed") %>%
        janitor::clean_names() %>% 
        clean_terms() %>%
        add_significance() %>%
        mutate(model_id = model_id)}) 
    
    top_results_full <- do.call(rbind, top_results_full) %>% 
      left_join(., models_df %>% dplyr::select(model_id, scale, delta_AICc), by = "model_id") %>% 
      mutate(key = if_else(model_id == "ST*A", NA, "Top Model"))
    
    # Examine interactions 
    # if (any(str_count(top_results_full$term_revised, ":") == 2)) {
    #   print("  X*ST*A detected.")
    #   return(NULL)
    #   if (all(top_results_full$significance[str_count(top_results_full$term_revised, ":") == 2] == "NS")) {
    #     print("  All 3-way interactions are non-significant.")
    #     top_names <- top_names[!grepl("\\*ST\\*A", top_names)]
    #   } else {
    #     print("  Some 3-way interactions are significant. Checking individual models.")
    #     
    #     # Identify models that contain only NS 3-way interactions
    #     models_to_remove <- top_results_full %>%
    #       filter(str_count(term_revised, ":") == 2) %>%
    #       group_by(model_id) %>%
    #       summarize(all_ns = all(significance == "NS"), .groups = "drop") %>%
    #       filter(all_ns) %>%
    #       pull(model_id)
    #     
    #     if (length(models_to_remove) > 0) {
    #       print(paste("    Removed models with only non-significant 3-way interactions: ", models_to_remove))
    #       
    #       # Remove those models from top_names
    #       top_names <- setdiff(top_names, models_to_remove)
    #       
    #     } else {
    #       print("  No models removed; at least one 3-way interaction is significant in each model.")
    #     }
    #   }
    # } else {
    #   print("  No 3-way interactions detected in term_revised.")
    # }
    
  } else {
    top_names <- models_df$model_id[models_df$type %in% c("top")]
    top_results_full <- NULL
  }
  
  
  # If there are multiple, get the model average. If not, get the results from the top one.
  if (length(top_names) > 1) {
    model_avg <- model.avg(data$models[models_df$model_id %in% top_names], fit = TRUE)
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
    mutate(num_top_models = length(top_names),
           scale = paste(unique(models_df$scale[models_df$type == "top"]), collapse = ", "),           
           key = if_else(num_top_models == 1, "Top Model v. Base Model", "Top Models (Average) v. Base Model")) 

  # Process core models
  core_names <- models_df$model_id[models_df$type %in% c("core")]
  
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
    left_join(., models_df %>% dplyr::select(model_id, scale, type), by = "model_id") %>% 
    mutate(key = if_else(model_id == "ST*A", NA, "Full Model"))
  
  # Combine all results
  all_results <- bind_rows(top_results, core_results, top_results_full) %>%
    mutate(species_code = species,
           scale = factor(scale, levels = c(25, 50, 100, 250, 500))) %>% 
    mutate(key = case_when(model_id == "ST*A" & unique(top_results$num_top_models) > 1 ~ "Top Models (Average) v. Base Model",
                           model_id == "ST*A" & unique(top_results$num_top_models) ~ "Top Model v. Base Model",
                           T~key),
           model_id = case_when(is.na(model_id) & num_top_models > 1 ~ "Model Average",
                                is.na(model_id) & num_top_models == 1 ~ "Top Model",
                                T~model_id)) %>% 
    dplyr::select(species_code, model_id, key, scale, term, term_revised, everything(), -effect) %>% 
    left_join(., models_df %>% dplyr::select(model_id, n_sites, n_mpas, type, depth_type), by = c("model_id", "type")) %>% 
    left_join(., data$data_sp %>% 
                distinct(species_code, sciname, genus, target_status, assemblage_new), by = "species_code")
  
  # Export 
  saveRDS(all_results, file = file.path(path, paste0(species, "_results.rds")))

  
}

# Analyze Focal Models: 2-way version -------------------------------------------

species <- "ADAV"
habitat <- "kelp"
path <- "analyses/7habitat/output/2way/kelp"

analyze_models_2way <- function(species, path, habitat){
  print(paste("Species:", species))
  # Read 2way predictor list
  pred <- readRDS(file.path(paste0("analyses/7habitat/intermediate_data/", habitat, "_predictors_2way.Rds"))) %>% 
    rename(type_orig = type)
  
  # Read focal models 
  data <- readRDS(file.path(path, paste0(species, "_models.rds"))) 
  
  models_df <- data$models_df %>% 
    mutate(scale = case_when(model_id == "ST*A" ~ NA,
                             T ~ as.factor(str_extract(predictors, "\\d+")))) %>% 
    mutate(depth_type = case_when(str_detect(model_id, "DSD") ~ "depth_sd",
                                  str_detect(model_id, "DM") ~ "depth_mean",
                                  str_detect(model_id, "DCV") ~ "depth_cv")) %>% 
    left_join(., pred %>% dplyr::select(model_id, type_orig), by = "model_id") %>% 
    mutate(type = if_else(type == "core", NA_character_, type)) %>% 
    mutate(type = coalesce(type, type_orig)) %>% 
    filter(!is.na(type))
  
  if (sum(!is.na(unique(models_df$messages))) > 0){
    print(paste("  Messages:", unique(models_df$messages)))
  } else {
    print("No messages.")
  }
  
  # Process top models:
  # Extract results from all top models 
  print(paste("  Top models:", sum(models_df$type == "top")))
  
  # Examine results for each of the individual top models (without averaging)
  if (sum(models_df$type == "top") > 1) {
    top_names <- models_df$model_id[models_df$type %in% c("top")]
    top_results_full <- lapply(top_names, function(model_id) {
      if (is.null(data$models[[model_id]])) { return(NULL) }
      tidy(data$models[[model_id]], conf.int = TRUE, effect = "fixed") %>%
        janitor::clean_names() %>% 
        clean_terms() %>%
        add_significance() %>%
        mutate(model_id = model_id)}) 
    
    top_results_full <- do.call(rbind, top_results_full) %>% 
      left_join(., models_df %>% dplyr::select(model_id, scale, delta_AICc), by = "model_id") %>% 
      mutate(key = if_else(model_id == "ST*A", NA, "Top Model"))
    
  } else {
    top_names <- models_df$model_id[models_df$type %in% c("top")]
    top_results_full <- NULL
  }
  
  
  # If there are multiple, get the model average. If not, get the results from the top one.
  if (length(top_names) > 1) {
    model_avg <- model.avg(data$models[models_df$model_id %in% top_names], fit = TRUE)
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
    mutate(num_top_models = length(top_names),
           scale = paste(unique(models_df$scale[models_df$type == "top"]), collapse = ", "),           
           key = if_else(num_top_models == 1, "Top Model v. Base Model", "Top Models (Average) v. Base Model")) 
  print(paste("  Top models:", unique(top_results$num_top_models)))
  
  # Fit the model with the predictors > 0.5
  if (length(top_names) > 1) {
    predictors <- top_results %>% 
      filter(importance > 0.5 & term != "(Intercept)") %>% 
      mutate(term = str_replace_all(term, ":", " * ")) %>% 
      pull(term) %>% 
      paste(., collapse = " + ")
    
    response <- unique(models_df$response)
    random_effects <- unique(models_df$random_effects)
    
    model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    
    m <- lmerTest::lmer(model_formula, data = data$data_sp,
                        control = lmerControl(optCtrl = list(maxfun = 1e7)))
    
    coef_table <- tidy(m, conf.int = TRUE, effect = "fixed") %>%
      mutate(term = str_replace(term, "typeMPA", "type"),
             importance = 1) %>% 
      janitor::clean_names() %>% 
      clean_terms() %>%
      add_significance() %>%
      mutate(num_top_models = length(top_names),
             scale = paste(unique(models_df$scale[models_df$type == "top"]), collapse = ", "),           
             key = if_else(num_top_models == 1, "Top Model v. Base Model", "Top Models (Average) v. Base Model")) 
    
    
  }
  
  # Process core models
  core_names <- models_df$model_id[models_df$type %in% c("core_2way", "core_3way", "base", "no_depth_2way", "no_depth_3way")]
  
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
    left_join(., models_df %>% dplyr::select(model_id, scale, type), by = "model_id") %>% 
    mutate(key = if_else(model_id == "ST*A", NA, "Full Model"))
  
  # Combine all results
  all_results <- bind_rows(top_results, core_results, top_results_full) %>%
    mutate(species_code = species,
           scale = factor(scale, levels = c(25, 50, 100, 250, 500))) %>% 
    mutate(key = case_when(model_id == "ST*A" & unique(top_results$num_top_models) > 1 ~ "Top Models (Average) v. Base Model",
                           model_id == "ST*A" & unique(top_results$num_top_models) ~ "Top Model v. Base Model",
                           T~key),
           model_id = case_when(is.na(model_id) & num_top_models > 1 ~ "Model Average",
                                is.na(model_id) & num_top_models == 1 ~ "Top Model",
                                T~model_id)) %>% 
    dplyr::select(species_code, model_id, key, scale, term, term_revised, everything(), -effect) %>% 
    left_join(., models_df %>% dplyr::select(model_id, n_sites, n_mpas, type, depth_type), by = c("model_id", "type")) %>% 
    left_join(., data$data_sp %>% 
                distinct(species_code, sciname, genus, target_status, assemblage_new), by = "species_code")
  
  # Export 
  saveRDS(all_results, file = file.path(path, paste0(species, "_results.rds")))
  
  
}


# Run Analysis -----------------------------------------------------------------

# Kelp forest 
species <- "SNEB"
habitat <- "kelp"
path <- "analyses/7habitat/output/kelp"
analyze_models(species = "SMIN", habitat = "kelp", path = "analyses/7habitat/output/kelp")

list.files(path = path, pattern = ".rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>%
  as.data.frame() %>% unique() %>% 
  filter(!(. %in% c("SNEB"))) %>%  # errors
  pull(.) %>%
  walk(., ~analyze_models(.x, path = path, habitat = "kelp"))

# Rocky reef 
path <- "analyses/7habitat/output/rock"

list.files(path = path, pattern = ".rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>%
  unique() %>%
  walk(., ~analyze_models(.x, path = path, habitat = "rock"))


## Surf 
path <- "analyses/7habitat/output/surf"

list.files(path = path, pattern = ".rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>%
  as.data.frame() %>%
  filter(!(. %in% c("LARM"))) %>% # needs review
  pull(.) %>%
  unique() %>%
  walk(., ~analyze_models(.x, path = path, habitat = "surf"))

## Deep
path <- "analyses/7habitat/output/deep"

list.files(path = path, pattern = ".rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>%
  unique() %>%
  walk(., ~analyze_models(.x, path = path, habitat = "surf"))


