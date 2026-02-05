# Analyze Model Results
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Analyze the focal models that were fit and extracted in the previous step.
# The extracted _models.Rds contains:
#   -- models_df: dataframe with summary of model results, including model_id, 
#         predictor string and other specifications used in model fitting, AICc
#         and other fit results (singular status, warnings).
#   -- data_sp: the data used to fit the model

# The files are stored here:
# -- /analyses/7habitat/output/model-set

library(tidyverse)
library(MuMIn) # for model averaging
library(broom.mixed) # for extracting fit info
library(lmerTest)
library(effects)
library(performance)
library(gt)

rm(list = ls())
gc()

source("analyses/7habitat/code/helper_functions.R")  

list2env(list(habitat = "kelp",
              re_string = "msy", 
              model_type = "lmer",
              delta_threshold = 2), envir = .GlobalEnv)

list2env(list(habitat = "rock", 
              re_string = "my", 
              model_type = "lmer",
              delta_threshold = 2), envir = .GlobalEnv)

list2env(list(habitat = "surf", 
              re_string = "m", 
              model_type = "lmer",
              delta_threshold = 2), envir = .GlobalEnv)

# Analyze Focal Models ----------------------------------------------------------------------------

model_selection <- function(results_file, delta_threshold, habitat, re_string, model_type){
  
  results_file <- paste(habitat,  re_string, "models.rds", sep = "_")
  print(paste(results_file))
  
  # Read model fit results and data_sp used
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/model-set", results_file)) 
  models_df <- results$models_df
  data_sp <- results$data_sp
  
  # Subset the models within the AICc threshold
  top_models_df <- models_df %>% 
    filter(delta_AICc <= delta_threshold | model_id == "ST*A") 

  print(paste("  Top models:", length(top_models_df$model_id)))
  
  # Refit the top models
  if (model_type == "lmer"){
    top_models <- top_models_df %>% 
      mutate(model = map(formula, 
                         ~ lmer(as.formula(.x), data = data_sp, REML = F,
                                control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))))
  }
  
  top_names <- top_models_df$model_id 
  names(top_models$model) <- top_models$model_id
  aicc_full <- model.sel(top_models$model)
  
  if (length(top_names) > 1) {
    nested <- evaluate_nested_models(top_models$model, delta_threshold = delta_threshold, alpha = 0.05)
    top_names <- if(length(nested$candidates) > 0) nested$candidates else top_names[1]
    top_models <- top_models$model[top_names]
    print(paste("      Top models:", length(top_names)))
    print(paste("      ", paste(top_names)))
   }
  
  nested_results_table <- nested$nested_results 

  # Get importance estimates from the top model.
  coef_table <- tidy(top_models[[1]], conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance_abs_t = abs(estimate/std.error),
           importance_relative = importance_abs_t / max(importance_abs_t, na.rm = TRUE),  # Scale max = 1
           importance_sw = 1) %>% 
    janitor::clean_names()
  
  # Create df with the results from the top models
  top_results <- coef_table %>%
    clean_terms() %>%
    mutate(key = if_else(length(top_names) > 1, "Top Models (Average)", "Top Model v. Base Model")) 
  
  # Create df with fit and selection details
  model_details <- top_models_df %>% 
    filter(model_id %in% top_names | model_id == "ST*A") %>% 
    mutate(focal_group = focal_group,
           n_sites = length(unique(data_sp$site)),
           n_mpas = length(unique(data_sp$affiliated_mpa)),
           response = str_trim(str_extract(formula, "^[^~]+")),
           random_effects = case_when(re_string == "rmy" ~ "region4/affiliated_mpa, year",
                                      re_string == "rmsy" ~ "region4/affiliated_mpa/site, year",
                                      re_string == "rm" ~ "region4/affiliated_mpa",
                                      re_string == "r" ~ "region4",
                                      re_string == "m" ~ "affiliated_mpa",
                                      re_string == "my" ~ "affiliated_mpa, year",
                                      re_string == "msy" ~ "affiliated_mpa/site, year",
                                      T~NA)) 
  
  # Create full AICC table from first fit
  aicc_table_full <- aicc_full %>% 
    as.data.frame() %>% 
    dplyr::select(delta, weight, df) %>% 
    rownames_to_column("Model") %>% 
    dplyr::select(Model, delta, weight, K = df) %>% 
    mutate(top = if_else(Model %in% top_names, TRUE, FALSE))
  
  results <- list(aicc_table_full = aicc_table_full,
                  top_results = top_results,
                  top_models = top_models,
                  model_details = model_details,
                  nested_results = nested_results_table)
  
  # Export the results
  saveRDS(results, file = file.path("~/ca-mpa/analyses/7habitat/output/results", 
                           paste(habitat, re_string, "selection_results.rds", sep = "_")))
  
  # Export the data used for the models (will refer to this a bunch)
  saveRDS(data_sp, file = file.path("~/ca-mpa/analyses/7habitat/output/data", 
                                    paste(habitat, re_string, "data.rds", sep = "_")))
  
}

### Filtered versions ---------------------------
model_selection(habitat = "rock", 
                re_string = "my", 
                model_type = "lmer",
                delta_threshold = 2)

model_selection(habitat = "kelp", 
                re_string = "msy", 
                model_type = "lmer",
                delta_threshold = 2)

model_selection(habitat = "surf", 
                re_string = "m", 
                model_type = "lmer",
                delta_threshold = 2)

