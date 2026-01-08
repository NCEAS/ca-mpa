# Step 0 Build Helper Functions
# Moved here Feb 2025
# Cori Lopazanski

# Helper Functions -------------------------------------------------------------

select_scales <- function(data, pred_list, intx.terms, response, random_effects) {
  library(lme4)
  library(MuMIn)
  library(dplyr)
  
  # Select top scales for each predictor
  scale_inputs <- list(
    hard_bottom = pred_list$predictor[str_detect(pred_list$predictor, "hard_bottom")],
    soft_bottom = pred_list$predictor[str_detect(pred_list$predictor, "soft_bottom")],
    depth_mean = pred_list$predictor[str_detect(pred_list$predictor, "depth_mean")],
    depth_cv = pred_list$predictor[str_detect(pred_list$predictor, "depth_cv")],
    kelp_annual = pred_list$predictor[str_detect(pred_list$predictor, "kelp_annual")],
    aquatic_vegetation = pred_list$predictor[str_detect(pred_list$predictor, "aquatic_vegetation")],
    tri = pred_list$predictor[str_detect(pred_list$predictor, "tri")],
    slope_sd = pred_list$predictor[str_detect(pred_list$predictor, "slope_sd")]) %>% compact()
  
  fixed <- "site_type * age_at_survey"
  
  all_results <- purrr::map_dfr(names(scale_inputs), function(var_class) {
    habitat_vars <- scale_inputs[[var_class]]
    
    models <- lapply(habitat_vars, function(var) {
      formula_str <- paste(response, "~", fixed, "+", 
                           var, intx.terms, " + ", paste0("(1 | ", random_effects, ")", collapse = " + ")) 
      # lmer(as.formula(formula_str), data = data, 
      #      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)), REML = FALSE)
      glmmTMB(as.formula(formula_str), data = data,
              family = tweedie(link = "log"),
              control = glmmTMBControl(optCtrl = list(iter.max = 1e8, eval.max = 1e8)),
              REML = FALSE)
    })
    names(models) <- habitat_vars
    model_tbl <- tibble(Model = names(models), model_obj = models)
    
    aicc_table <- model.sel(models) %>%
      as.data.frame() %>%
      tibble::rownames_to_column("Model") %>%
      mutate(Feature = var_class) %>%
      dplyr::select(Feature, Model, df, logLik, AICc, delta, weight) %>%
      left_join(model_tbl, by = "Model")
    
   # aicc_table$Converged <- map_lgl(models, ~ is.null(.x@optinfo$conv$lme4$messages))[aicc_table$Model] # for gauss
    aicc_table$Converged <- map_lgl(models, ~ isTRUE(.x$sdr$pdHess))[aicc_table$Model]
    
    return(aicc_table)
    
  })
  
  formatted_table <- all_results %>% 
    dplyr::select(!c(Converged, model_obj)) %>% 
    mutate(Model = str_to_sentence(str_replace_all(Model, "_", " ")),
           Feature = str_to_sentence(str_replace_all(Feature, "_", " "))) %>% 
    mutate(Model = str_replace_all(Model, "Aquatic vegetation bed", "Max biotic extent") %>% 
             str_replace_all("cv", "CV") %>% 
             str_replace_all("Tri_mean", "TRI")) %>% 
    mutate(Feature = str_replace_all(Feature,"Aquatic vegetation", "Max biotic extent") %>% 
             str_replace_all("cv", "CV") %>% 
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


clean_terms <- function(df) {df %>%
    mutate(term = str_remove_all(term, "MPA") %>% 
             if_else(str_detect(., ":site_type$"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .)) %>% 
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
                               "site_type:aquatic_vegetation_bed",
                               "site_type:age_at_survey",
                               "site_type",
                               "age_at_survey",
                               "depth_mean",
                               "depth_sd",
                               "depth_cv",
                               "soft_bottom",
                               "hard_bottom",
                               "kelp",
                               "aquatic_vegetation_bed"))) %>% 
    mutate(term_scale = str_extract(term, "\\d+"))}

add_significance <- function(df) {df %>%
    mutate(significance = factor(case_when(p_value < 0.001 ~ "***",
                                           p_value < 0.01 ~ "**",
                                           p_value < 0.05 ~ "*",
                                           is.na(p_value) ~ "NA",
                                           TRUE ~ "NS"), levels = c("***", "**", "*", "NS", "NA")))
}

evaluate_nested_models <- function(models, delta_threshold, alpha) {
  library(MuMIn)
  library(dplyr)
  
  model_set <- model.sel(models)
  model_set <- model_set[model_set$delta <= delta_threshold, , drop = FALSE]
  selected_names  <- rownames(model_set)
  selected_models <- models[selected_names]
  
  nested_list <- nested(model_set, indices = "rownames")
  
  nested_results <- tibble::tibble(
    larger  = character(),
    smaller = character(),
    p       = numeric()
  )
  
  for (larger in names(nested_list)) {
    for (smaller in nested_list[[larger]]) {
      p_val <- anova(
        selected_models[[larger]],
        selected_models[[smaller]]
      )$`Pr(>Chisq)`[2]
      
      nested_results <- nested_results %>%
        add_row(larger = larger, smaller = smaller, p = p_val)
    }
  }
  
  decision <- nested_results %>%
    group_by(larger) %>%
    summarize(drop_larger = any(p >= alpha),
              keep_smaller = list(smaller[p >= alpha]),
              drop_smaller = list(smaller[p < alpha]))
  
  candidates <- decision %>%
    filter(!drop_larger) %>% # keep larger models that were significantly better fit
    pull(larger) %>% 
    union(unlist(decision$keep_smaller)) %>% # add the smaller models that were just as good as their nested larger model
    setdiff(., unlist(decision$drop_smaller)) %>% # drop the smaller models where at least one larger model was significantly better
    setdiff(., unlist(decision$larger[decision$drop_larger])) # drop the models where at least one smaller model was significantly better
  
  
  list(
    candidates = candidates,
    nested_results   = nested_results,
    decision = decision
  )
}

predictors_to_model_id <- function(predictor_df){
  predictor_df <- predictor_df %>% 
    mutate(model_id = 
           str_replace_all(predictors, "hard_bottom_(\\d+)", "H\\1") %>% 
           str_replace_all("soft_bottom_(\\d+)", "S\\1") %>% 
           str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
           str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
           str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
           str_replace_all("depth_cv_(\\d+)", "DCV\\1") %>% 
           str_replace_all("site_type", "ST") %>%
           str_replace_all("age_at_survey", "A") %>% 
           str_replace_all("aquatic_vegetation_bed_(\\d+)", "AV\\1") %>% 
           str_replace_all("\\s+", "")) 
  return(predictor_df)
}

generate_simple_3way <- function(pred_top) {
  
  pred <- pred_top %>% 
    mutate(intx = paste(predictor, "* site_type"),
           intx2 = paste(predictor, "* age_at_survey"),
           intx3 = paste(predictor, "* site_type * age_at_survey"))
  
  pred_3way <- expand.grid(hard = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "hard")], 
                                    pred$intx[str_detect(pred$predictor, "hard")],
                                    pred$intx2[str_detect(pred$predictor, "hard")],
                                    pred$intx3[str_detect(pred$predictor, "hard")]),
                           kelp = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "kelp")], 
                                    pred$intx[str_detect(pred$predictor, "kelp")],
                                    pred$intx2[str_detect(pred$predictor, "kelp")],
                                    pred$intx3[str_detect(pred$predictor, "kelp")]),
                           depm = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "depth_mean")], 
                                    pred$intx[str_detect(pred$predictor, "depth_mean")],
                                    pred$intx2[str_detect(pred$predictor, "depth_mean")],
                                    pred$intx3[str_detect(pred$predictor, "depth_mean")]),
                           depc = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "depth_cv")], 
                                    pred$intx[str_detect(pred$predictor, "depth_cv")],
                                    pred$intx2[str_detect(pred$predictor, "depth_cv")],
                                    pred$intx3[str_detect(pred$predictor, "depth_cv")]), 
                           trim = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "tri_mean")], 
                                    pred$intx[str_detect(pred$predictor, "tri_mean")],
                                    pred$intx2[str_detect(pred$predictor, "tri_mean")],
                                    pred$intx3[str_detect(pred$predictor, "tri_mean")]),
                           slsd = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "slope_sd")], 
                                    pred$intx[str_detect(pred$predictor, "slope_sd")],
                                    pred$intx2[str_detect(pred$predictor, "slope_sd")],
                                    pred$intx3[str_detect(pred$predictor, "slope_sd")]),
                           stringsAsFactors = F) %>% 
    mutate(base = "site_type * age_at_survey") %>% 
    unite("predictors", c(hard, kelp, depm, depc, trim, slsd, base), sep = " + ", na.rm = TRUE, remove = FALSE) %>% 
    mutate(model_id = 
             str_replace_all(predictors, "hard_bottom_(\\d+)", "H\\1") %>% 
             str_replace_all("soft_bottom_(\\d+)", "S\\1") %>% 
             str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
             str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
             str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
             str_replace_all("depth_cv_(\\d+)", "DCV\\1") %>% 
             str_replace_all("tri_mean_(\\d+)", "TRI\\1") %>% 
             str_replace_all("slope_sd_(\\d+)", "SSD\\1") %>% 
             str_replace_all("site_type", "ST") %>%
             str_replace_all("age_at_survey", "A") %>% 
             str_replace_all("aquatic_vegetation_bed_(\\d+)", "AV\\1") %>% 
             str_replace_all("\\s+", "")) 
  
  return(pred_3way)
  
}

generate_surf_3way <- function(pred_top) {
  
  pred <- pred_top %>% 
    mutate(intx = paste(predictor, "* site_type"),
           intx2 = paste(predictor, "* age_at_survey"),
           intx3 = paste(predictor, "* site_type * age_at_survey"))
  
  pred_3way <- expand.grid(hard = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "hard")], 
                                    pred$intx[str_detect(pred$predictor, "hard")],
                                    pred$intx2[str_detect(pred$predictor, "hard")],
                                    pred$intx3[str_detect(pred$predictor, "hard")]),
                           soft = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "soft")], 
                                    pred$intx[str_detect(pred$predictor, "soft")],
                                    pred$intx2[str_detect(pred$predictor, "soft")],
                                    pred$intx3[str_detect(pred$predictor, "soft")]),
                           kelp = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "kelp")], 
                                    pred$intx[str_detect(pred$predictor, "kelp")],
                                    pred$intx2[str_detect(pred$predictor, "kelp")],
                                    pred$intx3[str_detect(pred$predictor, "kelp")]),
                           depm = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "depth_mean")], 
                                    pred$intx[str_detect(pred$predictor, "depth_mean")],
                                    pred$intx2[str_detect(pred$predictor, "depth_mean")],
                                    pred$intx3[str_detect(pred$predictor, "depth_mean")]),
                           depc = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "depth_cv")], 
                                    pred$intx[str_detect(pred$predictor, "depth_cv")],
                                    pred$intx2[str_detect(pred$predictor, "depth_cv")],
                                    pred$intx3[str_detect(pred$predictor, "depth_cv")]),
                           trim = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "tri_mean")], 
                                    pred$intx[str_detect(pred$predictor, "tri_mean")],
                                    pred$intx2[str_detect(pred$predictor, "tri_mean")],
                                    pred$intx3[str_detect(pred$predictor, "tri_mean")]),
                           slsd = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "slope_sd")], 
                                    pred$intx[str_detect(pred$predictor, "slope_sd")],
                                    pred$intx2[str_detect(pred$predictor, "slope_sd")],
                                    pred$intx3[str_detect(pred$predictor, "slope_sd")]),
                           aquv = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "aquatic")], 
                                    pred$intx[str_detect(pred$predictor, "aquatic")],
                                    pred$intx2[str_detect(pred$predictor, "aquatic")],
                                    pred$intx3[str_detect(pred$predictor, "aquatic")]), stringsAsFactors = F) %>% 
    mutate(base = "site_type * age_at_survey") %>% 
    filter(!(!is.na(hard) & !is.na(soft))) %>% 
    unite("predictors", c(hard, soft, kelp, depm, depc, trim, slsd, aquv, base), sep = " + ", na.rm = TRUE, remove = FALSE) %>% 
    mutate(model_id = 
             str_replace_all(predictors, "hard_bottom_(\\d+)", "H\\1") %>% 
             str_replace_all("soft_bottom_(\\d+)", "S\\1") %>% 
             str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
             str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
             str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
             str_replace_all("depth_cv_(\\d+)", "DCV\\1") %>% 
             str_replace_all("tri_mean_(\\d+)", "TRI\\1") %>% 
             str_replace_all("slope_sd_(\\d+)", "SSD\\1") %>% 
             str_replace_all("site_type", "ST") %>%
             str_replace_all("age_at_survey", "A") %>% 
             str_replace_all("aquatic_vegetation_bed_(\\d+)", "AV\\1") %>% 
             str_replace_all("\\s+", "")) 
  
  return(pred_3way)
  
}
