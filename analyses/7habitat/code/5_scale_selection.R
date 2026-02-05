# Univariate scale selection

library(lme4)
library(MuMIn)
library(dplyr)

select_scales <- function(data, pred_list, intx.terms, response, random_effects) {

  # Select top scales for each predictor
  scale_inputs <- list(
    hard_bottom = pred_list$predictor[str_detect(pred_list$predictor, "hard_bottom")],
    soft_bottom = pred_list$predictor[str_detect(pred_list$predictor, "soft_bottom")],
    depth_mean = pred_list$predictor[str_detect(pred_list$predictor, "depth_mean")],
    depth_cv = pred_list$predictor[str_detect(pred_list$predictor, "depth_cv")],
    depth_sd = pred_list$predictor[str_detect(pred_list$predictor, "depth_sd")],
    kelp_annual = pred_list$predictor[str_detect(pred_list$predictor, "kelp_annual")],
    aquatic_vegetation = pred_list$predictor[str_detect(pred_list$predictor, "aquatic_vegetation")],
    tri = pred_list$predictor[str_detect(pred_list$predictor, "tri")],
    slope_sd = pred_list$predictor[str_detect(pred_list$predictor, "slope_sd")],
    relief = pred_list$predictor[str_detect(pred_list$predictor, "relief")]) %>% compact()
  
  fixed <- "site_type * age_at_survey"
  
  all_results <- purrr::map_dfr(names(scale_inputs), function(var_class) {
    habitat_vars <- scale_inputs[[var_class]]
    
    models <- lapply(habitat_vars, function(var) {
      formula_str <- paste(response, "~", fixed, "+", 
                           var, intx.terms, " + ", paste0("(1 | ", random_effects, ")", collapse = " + ")) 
      lmer(as.formula(formula_str), data = data,
           control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)), REML = FALSE)
     })
    names(models) <- habitat_vars
    model_tbl <- tibble(Model = names(models), model_obj = models)
    
    aicc_table <- model.sel(models) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Model") %>%
      mutate(Feature = var_class) %>%
      dplyr::select(Feature, Model, df, logLik, AICc, delta, weight) %>%
      left_join(model_tbl, by = "Model")
    
    aicc_table$Converged <- map_lgl(models, ~ is.null(.x@optinfo$conv$lme4$messages))[aicc_table$Model] # for gauss

    return(aicc_table)
    
  })
  
  formatted_table <- all_results %>% 
    dplyr::select(!c(Converged, model_obj)) %>% 
    mutate(Model = str_to_sentence(str_replace_all(Model, "_", " ")),
           Feature = str_to_sentence(str_replace_all(Feature, "_", " "))) %>% 
    mutate(Model = str_replace_all(Model, "Aquatic vegetation bed", "Max biotic extent") %>% 
             str_replace_all("cv", "CV") %>% 
             str_replace_all("sd", "SD") %>% 
             str_replace_all("relief", "Vertical relief") %>% 
             str_replace_all("Tri_mean", "TRI")) %>% 
    mutate(Feature = str_replace_all(Feature,"Aquatic vegetation", "Max biotic extent") %>% 
             str_replace_all("cv", "CV") %>% 
             str_replace_all("sd", "SD") %>% 
             str_replace_all("relief", "Vertical relief") %>% 
             str_replace_all("Tri_mean", "TRI")) %>% 
    gt(groupname_col = "Feature") %>% 
    cols_label(delta = "ΔAICc",
               weight = "AICc Weight") %>% 
    fmt_number(columns = c(delta, weight), decimals = 3) %>% 
    tab_options(table.width = pct(60), heading.align = "left") %>% 
    tab_style(style = cell_text(font = "Arial", size = px(12)), 
              locations = cells_body(columns = everything())) %>% 
    tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold"), 
              locations = cells_column_labels(columns = everything())) %>% 
    tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold", color = "black"),
              locations = cells_row_groups()) %>% 
    tab_options(data_row.padding = px(4),
                row_group.padding = px(6)) %>% 
    tab_style(
      style = cell_fill(color = "lightgray"),
      locations = cells_body(rows = delta == 0)
    )
  
  return(list(results = all_results,
              formatted_table = formatted_table))
  
}
