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

source("analyses/7habitat/code/Step0_helper_functions.R")  

list2env(list(habitat = "kelp_filtered", 
              focal_group = "targeted",
              re_string = "msy", 
              model_type = "lmer",
              delta_threshold = 2), envir = .GlobalEnv)


# Analyze Focal Models ----------------------------------------------------------------------------

model_selection <- function(results_file, delta_threshold, focal_group, habitat, re_string, model_type){
  
  results_file <- paste(habitat, focal_group, re_string, "models.rds", sep = "_")
  print(paste(results_file))
  
  # Read model fit results and data_sp used
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/models", results_file))
  models_df <- results$models_df
  data_sp <- results$data_sp
  
  # Subset the models within the AICc threshold
  top_models_df <- models_df %>% 
    filter(delta_AICc <= delta_threshold | model_id == "ST*A") 
  
  # If any are singular, they are likely over-fitting (remove to avoid bias)
  if (sum(top_models_df$singular_status != "OK") > 0){
    print(paste("  Top models:", length(top_models_df$model_id)))
    print(paste("  Removed singular:", sum(top_models_df$singular_status != "OK")))
    
    top_models_df <- top_models_df %>% 
      filter(singular_status == "OK")
  }
  
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
    nested <- check_nested_models(top_models) 
    top_names <- nested$candidate_list$model
    top_models <- top_models$model[top_names]
    print(paste("      Top models:", length(top_names)))
    
   }
  
  nested_results_table <- nested$nested_results 

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
  
  print(paste("  ", aicc_table$Model[[1]]))
  
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
    filter(model_id %in% top_names | model_id == "ST*A") %>% 
    mutate(focal_group = focal_group,
           n_sites = length(unique(data_sp$site)),
           n_mpas = length(unique(data_sp$affiliated_mpa)),
           response = str_trim(str_extract(formula, "^[^~]+")),
           random_effects = case_when(re_string == "rmy" ~ "region4/affiliated_mpa, year",
                                      re_string == "rmsy" ~ "region4/affiliated_mpa/site, year",
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
  
  results <- list(aicc_table = aicc_table,
                  aicc_table_full = aicc_table_full,
                  predictor_table = predictor_table,
                  top_results = top_results,
                  model_details = model_details,
                  nested_results = nested_results_table)
  
  # Export the results
  saveRDS(results, file = file.path("~/ca-mpa/analyses/7habitat/output/results", 
                           paste(habitat, focal_group, re_string, "selection_results.rds", sep = "_")))
  
  # Export the data used for the models (will refer to this a bunch)
  saveRDS(data_sp, file = file.path("~/ca-mpa/analyses/7habitat/output/data", 
                                    paste(habitat, focal_group, re_string, "data.rds", sep = "_")))
  
}

### Filtered versions ---------------------------
model_selection(habitat = "rock_filtered", 
                focal_group = "targeted",
                re_string = "rmsy", 
                model_type = "lmer",
                delta_threshold = 2)
# 11 > 1
# H250+DM500+DCV500*ST+ST*A

model_selection(habitat = "kelp_filtered", 
                focal_group = "targeted",
                re_string = "msy", 
                model_type = "lmer",
                delta_threshold = 2)
# 12 > 1
# H50*ST+K100+DM500+DCV100+ST*A

model_selection(habitat = "surf_filtered", 
                focal_group = "targeted",
                re_string = "m", 
                model_type = "lmer",
                delta_threshold = 2)

# 19 > 2
# S25*ST+DCV250*ST+AV500+ST*A

## Shallow reef -------------------------------
model_selection(habitat = "rock", 
                focal_group = "targeted",
                re_string = "rmy", 
                model_type = "lmer",
                delta_threshold = 4)
# Top: 12, Nest: 8, LRT: 0, Final: 4             
# H100*ST+DM500*ST+DCV500*ST+AV100*ST+ST*A   

model_selection(habitat = "rock", 
                focal_group = "targeted",
                re_string = "rmsy", 
                model_type = "lmer",
                delta_threshold = 4)
# Top: 451, Nesting: 402, LRT: 50, Final: 9      
# H100+DM500+DCV500*ST+ST*A

model_selection(habitat = "rock", 
                focal_group = "targeted",
                re_string = "rmy", 
                model_type = "lmer",
                delta_threshold = 2)
# Top: 4, Nesting: 3, LRT: 04, Final: 1
# H100*ST+DM500*ST+DCV500*ST+AV100*ST+ST*A      >> Reducing delta doesn't change the outcome for RMY

model_selection(habitat = "rock", 
                focal_group = "targeted",
                re_string = "rmsy", 
                model_type = "lmer",
                delta_threshold = 2)
# Top: 45, Nesting: 29, LRT: 14, Final: 7       >> Reducing delta does change outcome for RMSY (more interactions and variables)
# H100*ST+DM250+DCV500*ST+ST*A                  >> averaging adds terms compared to top model

# Summary:
# - Including site as RE yields more candidate models, but fewer interactions
# - Lowering the delta from 4 to 2 does not change the final model for RMY, but does change the final model for RMSY.
# - 

## Kelp forest -------------------------------
# model_selection(habitat = "kelp", 
#                 focal_group = "targeted",
#                 re_string = "rmy", 
#                 model_type = "lmer",
#                 delta_threshold = 4)
# Top: 23 (All Singular)

model_selection(habitat = "kelp", 
                focal_group = "targeted",
                re_string = "my", 
                model_type = "lmer",
                delta_threshold = 4)
# Top: 23, Nesting: 8, LRT: 2, Final: 13
# H25*ST+K100*ST+DM250+DCV50*ST+AV250*ST+ST*A
# H50*ST+K50*ST+AV500*ST+DM100*ST+DCV100*ST+ST*A

model_selection(habitat = "kelp", 
                focal_group = "targeted",
                re_string = "msy", 
                model_type = "lmer",
                delta_threshold = 4)
# Top: 230, Nesting: 133, LRT: 2, Final: 91 
# H25*ST+K100+DM250+DCV50+AV500*ST+ST*A
# H25*ST+K100+DM250+DCV250*ST+AV500*ST+ST*A       >> adding site means fewer interactions

# What if we reduce the delta:
model_selection(habitat = "kelp", 
                focal_group = "targeted",
                re_string = "my", 
                model_type = "lmer",
                delta_threshold = 2)
# Top: 9, Nesting: 2, LRT: 1, Final: 6
# H25*ST+K100*ST+DM250+DCV50*ST+AV250*ST+ST*A
# H25*ST+K100*ST+DM250+DCV100*ST+AV500*ST+ST*A

model_selection(habitat = "kelp", 
                focal_group = "targeted",
                re_string = "msy", 
                model_type = "lmer",
                delta_threshold = 2)
# Top: 28, Nest: 11, LRT: 0 Final: 17
# H25*ST+K100+DM250+DCV50+AV500*ST+ST*A
# H25*ST+K100+DM500+DCV250*ST+AV500*ST+ST*A 

## Surf zone -------------------------------
model_selection(habitat = "surf", 
                focal_group = "targeted",
                re_string = "m", 
                model_type = "lmer",
                delta_threshold = 4)
# Top: 22, Nesting: 8, LRT: 0, Final: 14 (6 singular dropped)
# S50*ST+DM50*ST+AV250+ST*A
# site_type * depth_mean_50 + site_type * depth_cv_250 + site_type * soft_bottom_50 + site_type * age_at_survey + aquatic_vegetation_bed_500

model_selection(habitat = "surf", 
                focal_group = "targeted",
                re_string = "m", 
                model_type = "lmer",
                delta_threshold = 2)
# Top: 3, Final: 3 (1 singular dropped)
# S50*ST+DM50*ST+AV250+ST*A
# site_type * depth_mean_50 + site_type * soft_bottom_50 + site_type * age_at_survey + aquatic_vegetation_bed_250

# Summary: Higher delta means extra interaction (DCV250*ST)



data_sp$fitted <- fitted(m)
data_sp$residuals <- residuals(m)

ggplot(data = data_sp) +
  geom_point(aes(x = fitted, y = residuals, color = affiliated_mpa)) + 
  geom_smooth(aes(x = fitted, y = residuals), method = "loess") +
  theme_minimal() +
  labs(color = NULL)

ggplot(data = data_sp) +
  geom_point(aes(x = fitted, y = residuals, color = region4)) + 
  geom_smooth(aes(x = fitted, y = residuals), method = "loess") +
  theme_minimal() +
  labs(color = NULL)

hist(data_sp$biomass)
hist(data_sp$log_c_biomass)
check_outliers(m)
check_overdispersion(m)
