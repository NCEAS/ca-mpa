# Next Piece
# Cori Lopazanski

library(tidyverse)
library(MuMIn) # for model averaging
library(broom.mixed) # for extracting fit info
library(lmerTest)
library(effects)
library(performance)
library(gt)

rm(list = ls())
gc()

source("analyses/7habitat/code/helper_functions.R")  # Load the function from the file

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"

list2env(list(habitat = "rock",
              re_string = "my"), envir = .GlobalEnv)

list2env(list(habitat = "kelp",
              re_string = "msy"), envir = .GlobalEnv)

list2env(list(habitat = "surf",
              re_string = "m"), envir = .GlobalEnv)
  
process_final_models <- function(habitat, focal_group, re_string){
  # Read the model selection results
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", 
                               paste(habitat, re_string, "selection_results.rds", sep = "_"))) 

  # Read the data used to fit the models
  data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", 
                               paste(habitat, re_string, "data.rds", sep = "_"))) 
  
  # 1. Refit the top model with REML
  response <- unique(results$model_details$response)
  random_effects <- unlist(str_split(unique(results$model_details$random_effects), ", "))
  top_formula <- as.formula(results$model_details$formula[1])   # top model is #1, base model is #2
  
  m <- lmer(top_formula, data = data_sp, REML = TRUE,
            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
  
  coef_table <- tidy(m, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>% 
    add_significance() %>%
    mutate(key = "Top Model")

  # Fit the base model
  model_formula_base <- as.formula(paste(response, "~ site_type * age_at_survey + region4 + ", paste0("(1 | ", random_effects, ")", collapse = " + ")))
  
  m2 <- lmer(model_formula_base, data = data_sp, REML = TRUE,
             control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))
  
  coef_table2 <- tidy(m2, conf.int = TRUE, effect = "fixed") %>%
    mutate(term = str_replace(term, "typeMPA", "type"),
           importance = 1) %>% 
    janitor::clean_names() %>% 
    clean_terms() %>%
    add_significance() %>%
    mutate(key = "Base Model")

  # Calculate the effects 
  # For 3-way interactions, we do this in the next step to help with plotting
  #assign("data_sp", data_sp, envir = .GlobalEnv)
  # effects_list_top <- allEffects(m, data = data_sp, xlevels = 50, partial.residuals = TRUE)
  #effects_list_base <- allEffects(m3, data = data_sp, xlevels = 50, partial.residuals = TRUE)
  #effects_list_simple <- allEffects(m2, data = data_sp, xlevels = 50, partial.residuals = TRUE)
  
  # plot(effects_list_top, multiline = T, confint = list(style = 'auto'))
  # plot(effects_list_base, multiline = T, confint = list(style = 'auto'))
  # plot(effects_list_simple, multiline = T, confint = list(style = 'auto'))
  
  # Add the refitted models and effects to the final output
  models <- list(base = m2,
                 top = m)

  
  model_formulas <- list(model_formula_top = top_formula,
                         model_formula_base = model_formula_base)
  
  # Combine the coefficient table results
  all_results <- bind_rows(coef_table, coef_table2) %>% 
    dplyr::select(term_revised, scale = term_scale, estimate, std_error, statistic, df, p_value, significance, key)
  
  # Rename habitat
  habitat_label <- case_when(habitat == "rock" ~ "Shallow reef",
                             habitat == "kelp" ~ "Kelp forest",
                             habitat == "surf" ~ "Surf zone")
  
  gt_table <- all_results %>% 
    filter(term_revised != "(Intercept)") %>% 
    mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>%
    mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>%
    gt() %>%
    tab_header(title = paste0("Model results: ", str_to_sentence(habitat_label), ", targeted fish biomass")) %>% 
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
    tab_row_group(label = "Top Model", rows = key == "Top Model") %>% 
    cols_hide(key) %>% 
    tab_source_note(source_note = paste0("Random effects: ", 
                                         str_replace_all(paste(random_effects, collapse = ", "), "_", " ") %>% 
                                           str_replace_all("affiliated mpa", "MPA") %>% 
                                           str_replace_all("site", "Site"))) %>% 
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
  gtsave(gt_table, file.path(fig.dir, paste("tableSX", habitat, re_string, "fit.png", sep = "-")),  vwidth = 1000, vheight = 1000)
  
  # Export 
  saveRDS(list(results = all_results, models = models, formulas = model_formulas), 
          file = file.path("~/ca-mpa/analyses/7habitat/output/effects", paste(habitat, re_string, "effects.rds", sep = "_")))
  
}

process_final_models(habitat = "surf",
                     re_string = "m")

process_final_models(habitat = "rock",
                     re_string = "my")

process_final_models(habitat = "kelp",
                     re_string = "msy")



