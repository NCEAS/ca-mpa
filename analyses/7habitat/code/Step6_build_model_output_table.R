# Step 5: Analyze Model Results
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Analyze the focal models that were fit and extracted in the previous step.
# The extracted species_models.Rds contains:
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
library(lmerTest)
library(effects)
library(performance)

source("analyses/7habitat/code/Step0_helper_functions.R")  # Load the function from the file



# Analyze Focal Models: 2-way version -------------------------------------------

path <- "analyses/7habitat/output"
habitat <- "kelp"
focal_group <- "all"
re_string <- "rmy"

analyze_models_2way(focal_group = "all",
                    path = "analyses/7habitat/output",
                    habitat = "kelp",
                    re_string = "my")

file_list <- data.frame(results_file = list.files(path = "~/ca-mpa/analyses/7habitat/output/models", pattern = "models.rds")) %>% 
  mutate(info = str_remove_all(results_file, "_models.rds")) %>% 
  separate(info, into = c("habitat", "focal_group", "re_string"), sep = "_")

pmap(file_list, analyze_models_2way)


analyze_models_2way <- function(results_file, focal_group, habitat, re_string){
  
  print(paste(results_file))
  
  # Read focal models 
  data <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/models", results_file)) 
  
  # Check for problems and stop if there are problems
  problems <- data$models_df %>% filter(!is.na(messages))
  
  if (length(problems$model_id > 0)) {
    print(paste("Problems: ", length(problems$model_id)))
    print(paste("   ", head(unique(problems$messages))))
    return(NULL)
  }
  
  models_df <- data$models_df %>% 
    mutate(scale = case_when(model_id == "ST*A" ~ NA, T ~ as.factor(str_extract(predictors, "\\d+")))) %>% 
    mutate(depth_type = case_when(str_detect(model_id, "DM") & str_detect(model_id, "DCV") ~ "depth_mean_and_cv",
                                  str_detect(model_id, "DM") ~ "depth_mean",
                                  str_detect(model_id, "DCV") ~ "depth_cv")) %>% 
    filter(!is.na(type)) 
  
  # Process top models:
  # Extract results from all top models 
  print(paste("  Top models:", sum(models_df$type == "top")))
  
  if (sum(models_df$type == "top") > 1) {
    top_names <- models_df$model_id[models_df$type %in% c("top")]
    top_models <- data$models[top_names]
    
  } else {
    top_names <- models_df$model_id[models_df$type %in% c("top")]
    # top_results_full <- NULL
  }

  # If there are multiple, get the model average. If not, get the results from the top one.
  if (length(top_names) > 1) {
    model_avg <- model.avg(top_models, fit = TRUE)
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
           key = if_else(num_top_models == 1, "Top Model v. Base Model", "Top Models (Average)")) 
  
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
    left_join(., models_df %>% dplyr::select(model_id, scale, type), by = "model_id") %>% 
    mutate(key = if_else(model_id == "ST*A", NA, "Full Model"))
  
  # Fit the model with the predictors > 0.5
  if (sum(models_df$type == "top") > 1) {
    predictors <- top_results %>% 
      filter(importance >= 0.5 & term != "(Intercept)") %>% 
      mutate(term = str_replace_all(term, ":", " * ")) %>% 
      pull(term) %>% 
      paste(., collapse = " + ")
    
    data_sp <- data$data_sp %>% as.data.frame()
    response <- unique(models_df$response)
    random_effects <- unique(unlist(strsplit(models_df$random_effects, ", ")))
    
    model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    
    m <- lmerTest::lmer(model_formula, data = data_sp, control = lmerControl(optCtrl = list(maxfun = 1e7)))
    
    coef_table <- tidy(m, conf.int = TRUE, effect = "fixed") %>%
      mutate(term = str_replace(term, "typeMPA", "type"),
             importance = 1) %>% 
      janitor::clean_names() %>% 
      clean_terms() %>%
      add_significance() %>%
      mutate(num_top_models = length(top_names),
             key = "Top Model v. Base Model")
    
    # Create df with the results from the top models
    refit_results <- coef_table %>%
      clean_terms() %>%
      add_significance() 
    
    predictor_list <- predictors %>% 
      str_replace_all(., " \\* ", "\\*") %>% 
      str_replace_all(., " \\+ ", ", ") %>% 
      str_remove_all(" site_type,") %>% 
      str_remove_all(" age_at_survey\\,") %>% 
      str_remove_all(" age_at_survey\\*site_type")
    
    print(paste("  Predictors:", predictor_list))
    
  } else {
    top_model_id <- models_df$model_id[models_df$type == "top"]
    m <- data$models[[top_model_id]]
    refit_results <- NULL
    print(paste(" Predictors:", models_df$model_id[models_df$type == "top"]))
  }
  

  # Calculate the effects 
  assign("data_sp", data$data_sp, envir = .GlobalEnv)
  effects_list_top <- allEffects(m, data = data$data_sp, xlevels = 50, partial.residuals = TRUE)
  effects_list_base <- allEffects(data$models$`ST*A`, data = data$data_sp, xlevels = 50, partial.residuals = TRUE)
  
  # Add the refitted model to the models output
  models <- list(base = data$models$`ST*A`, 
                 top = m,
                 effects_list_top = effects_list_top,
                 effects_list_base = effects_list_base)
  
  # Combine all results
  all_results <- bind_rows(top_results, core_results, refit_results) %>%
    mutate(species_code = focal_group) %>% 
    mutate(key = case_when(model_id == "ST*A" & unique(top_results$num_top_models) > 1 ~ "Top Models (Average) ",
                           model_id == "ST*A" & unique(top_results$num_top_models) ~ "Top Model v. Base Model",
                           T~key),
           model_id = case_when(is.na(model_id) & num_top_models > 1 ~ "Model Average",
                                is.na(model_id) & num_top_models == 1 ~ "Top Model",
                                T~model_id)) %>% 
    dplyr::select(species_code, model_id, key, scale, term, term_revised, everything(), -effect) %>% 
    left_join(., models_df %>% dplyr::select(model_id, n_sites, n_mpas, type, depth_type, regions), by = c("model_id", "type")) #%>% 
   # left_join(., data$data_sp %>% 
   #             distinct(species_code, sciname, genus, target_status, assemblage_new), by = "species_code")
  
  # Export 
  saveRDS(list(results = all_results, models = models), 
          file = file.path("~/ca-mpa/analyses/7habitat/output/results", paste(habitat, focal_group, re_string, "results.rds", sep = "_")))
  
}


# Run          -----------------------------------------------------------------
