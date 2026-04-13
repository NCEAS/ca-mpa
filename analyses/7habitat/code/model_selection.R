# Analyze Model Results
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# list2env(list(habitat = "kelp",
#               re_string = "rmsy", 
#               model_type = "lmer",
#               delta_threshold = 2), envir = .GlobalEnv)
# 
# list2env(list(habitat = "rock",
#               re_string = "rmsy",
#               model_type = "lmer",
#               delta_threshold = 2), envir = .GlobalEnv)
# 
# list2env(list(habitat = "surf", 
#               re_string = "rm", 
#               delta_threshold = 2), envir = .GlobalEnv)


model_selection <- function(results_file, delta_threshold, habitat, re_string){
  
  library(tidyverse)
  library(MuMIn) # for model averaging
  library(broom.mixed) # for extracting fit info
  library(lmerTest)
  library(effects)
  library(performance)
  library(gt)
  
  source("analyses/7habitat/code/helper_functions.R")  
  
  
  # 1. Read model fit results and dataset (data_sp) used to run the models ------------------------------------
  results_file <- paste(habitat,  re_string, "models.rds", sep = "_")
  print(paste("Reading results file from output/model-set: ", results_file))
  
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/model-set", results_file)) 
  models_df <- results$models_df
  data_sp <- results$data_sp
  
  # 2. Subset for models within the AICc threshold ------------------------------------------------------------------------
  top_models_df <- models_df %>% 
    filter(delta_AICc <= delta_threshold | model_id == "ST*A") 
  
  print(paste("  Top models:", length(top_models_df$model_id))) # print # top models
  
  # 3. Refit the top models with REML = F for comparison ------------------------------------------------------------------
  print("  Refitting top models with REML = F")
  top_models <- top_models_df %>% 
    mutate(model = map2(formula, lmer_control, ~ {
      if (is.na(.y)) { 
        lmer(as.formula(.x), data = data_sp, REML = F)
      } else
        lmer(as.formula(.x), data = data_sp, REML = F, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
    }))
   
  top_names <- top_models_df$model_id 
  names(top_models$model) <- top_models$model_id # assign names using model ID
  
 
  # 3. Evaluate nested models with Richards 2008 rule -----------------==--------------------------------------------------
  if (length(top_names) > 1) {
    nested <- apply_nesting_rule(top_models$model)
    top_names <- if(length(nested) > 0) nested else top_names[1]
    print(paste("      Top models after removing nested models:", length(top_names)))
    print(paste("      ", paste(top_names)))
  }
  
  # Create full AICC table - use model.sel() to run AICc comparison on the model set of all within deltaAICc 2
  aicc_table_full <- model.sel(top_models$model) %>% 
    as.data.frame() %>% 
    dplyr::select(delta, weight, df) %>% 
    rownames_to_column("Model") %>% 
    dplyr::select(Model, delta, weight, K = df) %>% 
    mutate(top = if_else(Model %in% top_names, TRUE, FALSE))
  
  # Create top AICC table - use model.sel() to run AICc comparison on only those that remain after 
  # nesting rule plus the base model
  aicc_table_top <- model.sel(top_models$model[top_models$model_id %in% top_names | top_models$model_id == "ST*A"]) %>% 
    as.data.frame() %>% 
    dplyr::select(delta, weight, df) %>% 
    rownames_to_column("Model") %>% 
    dplyr::select(Model, delta, weight, K = df) %>% 
    mutate(top = if_else(Model == top_names[1], TRUE, FALSE))
  
  model_details <- top_models_df %>% filter(model_id == top_names[1] | model_id == "ST*A")  # pull model details

  # 5. Fit top model with REML = T ---------------------------------------------------------------------------------------
  top_formula <- as.formula(model_details$formula[1])   # top model is #1, base model is #2
  print(paste("  Fitting top model with REML = T:", model_details$formula[1]))
  
  m <- suppressMessages(suppressWarnings(lmer(top_formula, data = data_sp, REML = TRUE)))
 
  # If there are convergence issues, fit with BOBYQA optimizer:
  msgs <- m@optinfo$conv$lme4$messages
  
  if (!is.null(msgs) && !any(grepl("singular", msgs))) {
    print(paste("    Warning:", msgs))
    print(paste("    Refitting top model with BOBYQA optimizer."))
    m <- lmer(top_formula, data = data_sp, REML = TRUE, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e5)))
  }
  
  coef_table_top <- tidy(m, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>% 
    add_significance() %>%
    mutate(key = "Top Model")
  
  # 6. Fit base model with REML = T --------------------------------------------------------------------------------------
  base_formula <-  as.formula(model_details$formula[2])
  print(paste("  Fitting base model with REML = T:", model_details$formula[2]))
  
  m2 <- lmer(base_formula , data = data_sp, REML = TRUE)
  
  coef_table_base <- tidy(m2, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>%
    add_significance() %>%
    mutate(key = "Base Model")
  
   
  # 7. Combine top and base coef. tables  --------------------------------------------------------------------------------
  print("  Compiling outputs for export")
  all_results <- bind_rows(coef_table_top, coef_table_base) %>% 
    dplyr::select(term_revised, scale = term_scale, estimate, std_error, statistic, df, p_value, significance, key)
  
  
  # 8. Compile results for output
  models <- list(base = m2,
                 top = m)
  
  model_formulas <- list(model_formula_top = top_formula,
                         model_formula_base = base_formula)
  
  selection_results <- list(all_results = all_results, 
                            models = models, 
                            formulas = model_formulas,
                            aicc_table = aicc_table_top,
                            aicc_table_full = aicc_table_full)
  
   
  # Export the results
  print("  Saved selection_results to: output/results")
  saveRDS(selection_results, file = file.path("~/ca-mpa/analyses/7habitat/output/results", 
                                              paste(habitat, re_string, "selection_results.rds", sep = "_")))
  
  # Export the data used for the models (will refer to this a bunch)
  print("  Saved data_sp to: output/data")
  saveRDS(data_sp, file = file.path("~/ca-mpa/analyses/7habitat/output/data", 
                                    paste(habitat, re_string, "data.rds", sep = "_")))
  
  return(selection_results)
  
}

