# Next Piece
# Cori Lopazanski

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R")  # Load the function from the file

list2env(list(habitat = "rock_filtered",
              focal_group = "targeted",
              re_string = "rmsy"), envir = .GlobalEnv)

list2env(list(habitat = "kelp_filtered",
              focal_group = "targeted",
              re_string = "msy"), envir = .GlobalEnv)

list2env(list(habitat = "surf_filtered",
              focal_group = "targeted",
              re_string = "m"), envir = .GlobalEnv)
  
process_final_models <- function(habitat, focal_group, re_string){
  
  results_file <- paste(habitat, focal_group, re_string, "selection_results.rds", sep = "_")
  data_file <-  paste(habitat, focal_group, re_string, "data.rds", sep = "_")
  print(paste(results_file))
  
  # Read the model selection results
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", results_file)) 
  names(results)
  
  # Read the data used to fit the models
  data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", data_file)) 
  
  # 1. Refit the top model with REML
  response <- unique(results$model_details$response)
  random_effects <- unlist(str_split(unique(results$model_details$random_effects), ", "))
  top_formula <- as.formula(results$model_details$formula[1])   # Just use the top model
  
  m <- lmer(top_formula, data = data_sp, REML = TRUE,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
  
  coef_table <- tidy(m, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>%
    add_significance() %>%
    mutate(key = "Top Model")
  
  # Drop the interactions
  predictors_simple <- coef_table %>%
    filter(!str_detect(term, ":") | term == "site_type:age_at_survey") %>% 
    filter(term != "(Intercept)") %>% 
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
    mutate(key = "Base Model")

  
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
  assign("data_sp", data_sp, envir = .GlobalEnv)
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
                 effects_list_simple = effects_list_simple,
                 effects_list_base = effects_list_base)
  
  model_formulas <- list(model_formula_top = top_formula,
                         model_formula_simple = model_formula_simple,
                         model_formula_base = model_formula_base)
  
  # Combine the coefficient table results
  all_results <- bind_rows(coef_table, coef_table2, coef_table3) %>% 
    dplyr::select(term_revised, scale = term_scale, estimate, std_error, statistic, df, p_value, significance, key)
  
  gt_table <- all_results %>% 
    filter(term_revised != "(Intercept)") %>% 
    mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>%
    mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>%
    mutate(term_revised = str_replace_all(term_revised, "_", " ") %>% str_to_sentence()) %>% 
    mutate(term_revised = if_else(term_revised == "Aquatic vegetation bed", "Max bioitic extent", term_revised)) %>% 
    gt() %>%
    tab_header(title = paste0("Model results: ", str_remove(str_to_sentence(habitat), "_filtered"), ", ", focal_group, " fish biomass")) %>% 
    cols_label(term_revised = "Term",
               scale = "Scale (m)",
               estimate = "Estimate",
               std_error = "Std. Error",
               statistic = "Statistic",
               df = "df",
               p_value = "p-value",
               significance = "") %>%
    fmt_number(columns = c(estimate, std_error, statistic),  decimals = 3) %>%
    fmt_number(columns = c(df),  decimals = 0) %>%
    sub_missing(columns = everything(), missing_text = "") %>%
    tab_options(heading.align = "left") %>%
    cols_align(align = "center", columns = everything()) %>% 
    tab_row_group(label = "Base Model", rows = key == "Base Model") %>%
    tab_row_group(label = "Simple Model", rows = key == "Simple Model") %>%
    tab_row_group(label = "Top Model", rows = key == "Top Model") %>% 
    cols_hide(key) %>% 
    tab_source_note(source_note = paste0("Random effects: ", str_replace_all(paste(random_effects, collapse = ", "), "_", " "))) %>% 
    tab_style(style = cell_text(font = "Arial", size = px(12)), 
              locations = cells_source_notes()) %>%
    tab_style(style = cell_text(font = "Arial", size = px(12)), 
              locations = cells_body(columns = everything())) %>%
    tab_style(style = cell_text(font = "Arial", size = px(12)), 
              locations = cells_row_groups()) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = c(p_value, significance), rows = significance %in% c("***", "**", "*") )) %>%
    tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold"), 
              locations = cells_column_labels(columns = everything())) %>%
    tab_style(style = cell_text(font = "Arial", size = px(13), weight = "bold"),
              locations = cells_title(groups = "title")) %>% 
    tab_options(table.width = pct(70)) %>% 
    tab_options(data_row.padding = px(6),
                row_group.padding = px(6))
  
  gt_table
  gtsave(gt_table, paste("tableSX", habitat, re_string, "fit.png", sep = "_"),  vwidth = 1000, vheight = 1000)
  
  # Export 
  saveRDS(list(results = all_results, models = models, formulas = model_formulas), 
          file = file.path("~/ca-mpa/analyses/7habitat/output/effects", paste(habitat, focal_group, re_string, "effects.rds", sep = "_")))
  
  #r2_nakagawa(m)
  #r2_nakagawa(m3)
  
  # dev_exp <- function(mod1, mod2) {
  #   1 - (deviance(mod1) / deviance(mod2))
  # }
  # 
  # dev_exp(m, m3)
  
}

process_final_models(habitat = "surf_filtered",
                     focal_group = "targeted",
                     re_string = "m")
process_final_models(habitat = "rock_filtered",
                     focal_group = "targeted",
                     re_string = "rmsy")
process_final_models(habitat = "kelp_filtered",
                     focal_group = "targeted",
                     re_string = "msy")

process_final_models(habitat = "surf",
                     focal_group = "targeted",
                     re_string = "m")

process_final_models(habitat = "kelp",
                     focal_group = "targeted",
                     re_string = "msy")

process_final_models(habitat = "kelp",
                     focal_group = "targeted",
                     re_string = "my")

process_final_models(habitat = "rock",
                     focal_group = "targeted",
                     re_string = "rmsy")

process_final_models(habitat = "rock",
                     focal_group = "targeted",
                     re_string = "rmy")
