# Next Piece
# Cori Lopazanski

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R")  # Load the function from the file


path <- "analyses/7habitat/output"
habitat <- "rock_subset"
focal_group <- "targeted"
re_string <- "rmy"
delta_threshold <- 2
results_file <- paste(habitat, focal_group, re_string, "selection_results.rds", sep = "_")
data_file <-  paste(habitat, focal_group, re_string, "data.rds", sep = "_")
  
process_final_models <- function(results_file){
  
  print(paste(results_file))
  
  # Read the model selection results
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", results_file)) 
  names(results)
  
  # Read the data used to fit the models
  data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", data_file)) 
  
  # 1. Refit the top model with REML
  top_names <- unlist(results$model_details$top_names)
  top_results <- results$top_results # for the predictors from model avg
  
  # Set some overarching variables
  response <- results$model_details$response
  random_effects <- unique(unlist(strsplit(results$model_details$random_effects, ", ")))
  
  # Select the most important term (main effect or intx) for each variable group
  selected_terms <- top_results %>%
    filter(!term == "(Intercept)") %>% 
    separate(term, into = c("var1", "var2"), sep = ":", remove = FALSE, fill = "left") %>%
    group_by(term_revised) %>%
    slice_max(order_by = importance_relative, n = 1) %>%
    ungroup() 
  
  # Add the main effects back in, if they are part of an interaction and not already selected
  predictors <- selected_terms %>% 
    bind_rows(top_results %>%
                filter(!str_detect(term, ":")) %>%  
                filter(term %in% selected_terms$var1 & !term %in% selected_terms$term)) %>%
    distinct() %>% 
    group_by(term_revised) %>% 
    filter(n() == 1 | term %in% selected_terms$var1) %>%  # Keep only if it's the only term or part of an interaction
    ungroup() %>% 
    mutate(term = str_replace_all(term, ":", " * ")) %>% 
    pull(term) %>% 
    paste(., collapse = " + ")
  
  # Refit the top model with REML
  model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
  
  m <- lmer(model_formula, data = data_sp, REML = TRUE,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
  
  coef_table <- tidy(m, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>%
    add_significance() %>%
    mutate(key = "Top Model v. Base Model")
  
  print(paste("  Predictors:",
              predictors %>% 
                str_replace_all(., " \\* ", "\\*") %>% 
                str_replace_all(., " \\+ ", ", ") %>% 
                str_remove_all(" site_type,") %>% 
                str_remove_all(" age_at_survey\\,") %>% 
                str_remove_all(" age_at_survey\\*site_type") %>% 
                str_replace_all(",,", ",")))
  
  # Drop the interactions
  predictors_simple <- selected_terms %>% 
    bind_rows(top_results %>%
                filter(!str_detect(term, ":")) %>%  
                filter(term %in% selected_terms$var1 & !term %in% selected_terms$term)) %>%
    distinct() %>% 
    group_by(term_revised) %>% 
    filter(n() == 1 | term %in% selected_terms$var1) %>%  # Keep only if it's the only term or part of an interaction
    ungroup() %>% 
    mutate(term = str_replace_all(term, ":", " * ")) %>% 
    filter(!str_detect(term, "\\*") | str_detect(term, "age_at_survey")) %>% 
    pull(term) %>% 
    paste(., collapse = " + ")
  
  # Fit the model without the interactions
  model_formula_simple <- as.formula(paste(response, "~", predictors_simple, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
  
  m2 <- lmer(model_formula_simple, data = data_sp, REML = TRUE,
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
  
  coef_table2 <- tidy(m2, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>%
    add_significance() %>%
    mutate(key = "Simple Model")
  
  # Fit the base model
  model_formula_base <- as.formula(paste(response, "~ site_type * age_at_survey +", paste0("(1 | ", random_effects, ")", collapse = " + ")))
  
  m3 <- lmer(model_formula_base, data = data_sp, REML = TRUE,
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
  
  coef_table3 <- tidy(m3, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>%
    add_significance() %>%
    mutate(key = "Top Model vs. Base Model")

  
  # 2. Refit the core models with REML
  # -- This is where we would add that back in if someone wants the forest plots
  # -- Currently this won't work because we need the formulas from the last stage
  # -- So would go back to Step6 and extract formulas for each of the core/base models
  
  # core_names <- unlist(results$model_details$core_names)
  
  # core_results <- lapply(core_names, function(model_id) {
  #   tidy(data$models[[model_id]], conf.int = TRUE, effect = "fixed") %>%
  #     janitor::clean_names() %>% 
  #     clean_terms() %>%
  #     add_significance() %>%
  #     mutate(model_id = model_id)
  # }) 
  
  # Combine core model results
  # core_results <- do.call(rbind, core_results) %>% 
  #   left_join(., models_df %>% dplyr::select(model_id, scale, type), by = "model_id") %>% 
  #   mutate(key = if_else(model_id == "ST*A", NA, "Full Model"))
  
  
  # Calculate the effects 
  # assign("data_sp", data$data_sp, envir = .GlobalEnv)
  effects_list_top <- allEffects(m, data = data_sp, xlevels = 50, partial.residuals = TRUE)
  effects_list_base <- allEffects(m3, data = data_sp, xlevels = 50, partial.residuals = TRUE)
  effects_list_simple <- allEffects(m2, data = data_sp, xlevels = 50, partial.residuals = TRUE)
  
  plot(effects_list_top, multiline = T, confint = list(style = 'auto'))
  plot(effects_list_base, multiline = T, confint = list(style = 'auto'))
  plot(effects_list_simple, multiline = T, confint = list(style = 'auto'))
  
  # Add the refitted models and effects to the final output
  models <- list(base = m3,
                 simple = m2,
                 top = m,
                 effects_list_top = effects_list_top,
                 effects_list_base = effects_list_base,
                 effects_list_simple = effects_list_simple)
  
  model_formulas <- list(model_formula_top = model_formula,
                         model_formula_base = model_formula_base,
                         model_formula_simple = model_formula_simple)
  
  # Combine the coefficient table results
  all_results <- bind_rows(coef_table, coef_table2, coef_table3)
  
  # Export 
  saveRDS(list(results = all_results, models = models, formulas = model_formulas), 
          file = file.path("~/ca-mpa/analyses/7habitat/output/effects", paste(habitat, focal_group, re_string, "effects.rds", sep = "_")))
  
}


# OLD STUFF:
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


