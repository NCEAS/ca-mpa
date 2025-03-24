# Step 5: Analyze Model Results
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Analyze the focal models that were fit and extracted in the previous step.
# The extracted _models.Rds contains:
#   -- models_df: dataframe with summary of model results, including model_id, 
#         predictor string and other specifications used in model fitting, AICc
#         and other fit results (singular status, warnings), and the "type" of the
#         model (base, top, or core == full). *if base model was top model, == top
#   -- models: the model fit objects from above df, identified by the model_id
#   -- data_sp: the data used to fit the model

# 1. Process top models:
#     If there are multiple top models, calculate the model average
#         and extract the weighted estimates and importance scores for each predictor
#     If there is only one top model, we will still re-fit it with REML.
#
# 2. Process full and base model:
#     Extract the estimates and errors for each of the full models (at each scale) 
#     and the base model (site * age)
# 3. Combine results into a single dataframe and export

rm(list = ls())
gc()

library(tidyverse)
library(MuMIn) # for model averaging
library(broom.mixed) # for extracting fit info
library(lmerTest)
library(effects)
library(performance)
library(gt)
library(dplyr)
library(MuMIn)

source("analyses/7habitat/code/Step0_helper_functions.R")  


# Analyze Focal Models ----------------------------------------------------------------------------

# path <- "analyses/7habitat/output"
# habitat <- "rock_subset"
# habitat <- "kelp"
# focal_group <- "targeted"
# focal_group <- "SPUL"
# re_string <- "msy"
# delta_threshold <- 4
# results_file <- paste(habitat, focal_group, re_string, "models.rds", sep = "_")

model_selection <- function(results_file, delta_threshold, focal_group, habitat, re_string){
  
  print(paste(results_file))
  
  # Read focal models 
  models_df <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/models", results_file)) 
  
  top_models_df <- models_df %>% 
    filter(delta_AICc <= delta_threshold)
  
  if (model_type == "lmer"){
    top_models <- top_models_df %>% 
      mutate(model = map(formula, 
                         ~ lmer(as.formula(.x), data = data_sp, REML = FALSE,
                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))))
  }
  
  top_names <- top_models_df$model_id
  names(top_models$model) <- top_models$model_id
  print(paste("  Top models:", length(top_models_df$model_id)))
  
  if (length(top_names) > 1) {
    
    nesting_results <- data.frame(initial = length(top_models$model_id),
                                  nesting_rule = NA,
                                  LRT = NA,
                                  final = NA)
    
    # Check for nested models 
    model_set <- model.sel(top_models$model)
    nested <- nested(model_set)
    
    top_names <- top_names[nested == FALSE]
    top_models <- top_models$model[nested == FALSE]
    
    print(paste("  Nesting rule:", sum(nested)))
    
    nesting_results$nesting_rule <- sum(nested)
    
    # Apply LRT on any remaining nested models
    nested <- check_nested_models(top_models) 
    top_names <- nested$candidate_list$model
    top_models <- top_models[top_names]
    
    nesting_results$LRT <- sum(nested$nested_results$p > 0.05, na.rm = T)
    nesting_results$final <- length(top_names)
    print(paste("  LRT:", sum(nested$nested_results$p > 0.05, na.rm = T)))
    print(paste("    Top models:", length(top_names)))
    
    top_names
    
   } else {
    nesting_results <- "NA (only one model)"
   }
  
  # If there are multiple, get the model average. 
  # If there are not, get importance estimates from the top model.
  if (length(top_names) > 1) {
    model_avg <- model.avg(top_models, fit = TRUE)
    coef_table <- data.frame(coefTable(model_avg)) %>%
      rownames_to_column("term") %>% 
      filter(!term == "(Intercept)") %>% 
      janitor::clean_names() %>% 
      dplyr::select(-df) %>%
      mutate(term       = str_replace(term, "typeMPA", "type"),
             conf_low   = estimate - 1.96 * std_error,
             conf_high  = estimate + 1.96 * std_error,
             importance_abs_t = abs(estimate/std_error),
             importance_relative = importance_abs_t / max(importance_abs_t, na.rm = TRUE),  # Scale max = 1
             importance_sw = sw(model_avg)[term]) %>% 
      arrange(desc(importance_abs_t))
  } else {
    coef_table <- tidy(top_models[[1]], conf.int = TRUE, effect = "fixed") %>%
      mutate(term = str_replace(term, "typeMPA", "type"),
             importance_abs_t = abs(estimate/std.error),
             importance_relative = importance_abs_t / max(importance_abs_t, na.rm = TRUE),  # Scale max = 1
             importance_sw = 1) %>% 
      janitor::clean_names()
  } 
  
  # Create df with the results from the top models
  top_results <- coef_table %>%
    clean_terms() %>%
    mutate(key = if_else(length(top_names) > 1, "Top Models (Average)", "Top Model v. Base Model")) 
  
  # Get AIC weights
  aicc_table <- model.sel(top_models) %>% 
    as.data.frame() %>% 
    dplyr::select(delta, weight, df) %>% 
    rownames_to_column("Model") %>% 
    dplyr::select(Model, delta, weight, K = df) 
  
  # Get predictor importance table
  predictor_table <- top_results %>% 
    dplyr::select(term, term_revised, estimate:importance_sw) %>% 
    mutate(term = str_replace_all(term, "_", " ") %>% 
             str_to_sentence() %>% 
             str_replace("cv", "CV") %>% 
             str_replace("\\d+", paste0(str_extract(., "\\d+"), "m"))) %>% 
    mutate(CI = paste0("(", round(conf_low, 2), ", ", round(conf_high,2), ")")) %>% 
    dplyr::select(term, estimate, CI, importance_abs_t, importance_sw) %>% 
    arrange(desc(importance_abs_t)) %>% 
    mutate(rank = 1:nrow(.))
  
  # Create df with fit and selection details
  model_details <- top_models_df %>% 
    filter(model_id %in% top_names) %>% 
    mutate(focal_group = focal_group,
           n_sites = length(unique(data_sp$site)),
           n_mpas = length(unique(data_sp$affiliated_mpa))) %>% 
    bind_cols(nesting_results) 
  
  results <- list(aicc_table = aicc_table,
                  predictor_table = predictor_table,
                  top_results = top_results,
                  model_details = model_details)
  
  # Export the results
  saveRDS(results, file = file.path("~/ca-mpa/analyses/7habitat/output/results", 
                           paste(habitat, focal_group, re_string, "selection_results.rds", sep = "_")))
  
  # Export the data used for the models (will refer to this a bunch)
  saveRDS(data_sp, file = file.path("~/ca-mpa/analyses/7habitat/output/data", 
                                    paste(habitat, focal_group, re_string, "data.rds", sep = "_")))
  
}






















# Run          -----------------------------------------------------------------

file_list <- data.frame(results_file = list.files(path = "~/ca-mpa/analyses/7habitat/output/models", pattern = "models.rds")) %>% 
  mutate(info = str_remove_all(results_file, "_models.rds")) %>% 
  separate(info, into = c("habitat", "focal_group", "re_string"), sep = "_") %>% 
  filter(!(results_file %in% c("kelp_all_rmsy_models.rds",
                               "kelp_targeted_rmsy_models.rds",
                               "kelp_targeted_rmy_models.rds"))) # confirmed errors (unless rerun)

pmap(file_list, analyze_models_2way)
