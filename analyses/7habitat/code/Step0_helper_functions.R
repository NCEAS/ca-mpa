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
    aquatic_vegetation = pred_list$predictor[str_detect(pred_list$predictor, "aquatic_vegetation")]
  ) %>% compact()
  
  fixed <- "site_type * age_at_survey"
  
  all_results <- purrr::map_dfr(names(scale_inputs), function(var_class) {
    habitat_vars <- scale_inputs[[var_class]]
    
    models <- lapply(habitat_vars, function(var) {
      formula_str <- paste(response, "~", fixed, "+", 
                           var, " * ", intx.terms, " + ", paste0("(1 | ", random_effects, ")", collapse = " + ")) 
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
    
    aicc_table$Converged <- map_lgl(models, ~ is.null(.x@optinfo$conv$lme4$messages))[aicc_table$Model]
    
    return(aicc_table)
    
  })
  
  formatted_table <- all_results %>% 
    dplyr::select(!c(Converged, model_obj)) %>% 
    mutate(Model = str_to_sentence(str_replace_all(Model, "_", " ")),
           Feature = str_to_sentence(str_replace_all(Feature, "_", " "))) %>% 
    mutate(Model = str_replace_all(Model, "Aquatic vegetation bed", "Max biotic extent") %>% 
             str_replace_all("cv", "CV")) %>% 
    mutate(Feature = str_replace_all(Feature,"Aquatic vegetation", "Max biotic extent") %>% 
             str_replace_all("cv", "CV")) %>% 
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



check_nested_models <- function(top_models) {
  
  candidate_list <- data.frame(model = NULL)
  nested_results <- data.frame(model = NULL, nested = NULL, p = NULL)
  drop_list <- data.frame(model = NULL)
  top_models <- top_models$model
  
  # Convert models to a model.selection object
  model_set <- model.sel(top_models) %>% arrange(delta)
  
  # Check whether they are nested
  nested <- nested(model_set, indices = "rownames")
  nested_lengths <- sapply(nested, length)
  
  # Output is a list with one object per model
  for (i in 1:length(nested)) {
    current_model <- names(nested)[i]
    nested_status <- nested_lengths[current_model]
  
    if (nested_status == 0) {
      candidate_list <- bind_rows(candidate_list, data.frame(model = current_model))
      results <- data.frame(model = current_model)
      nested_results <- bind_rows(nested_results, results)
      
    } else {
      
      nested_models <- nested[[i]]
      
      for (j in 1:length(nested_models)) {
        nested_model <- nested_models[j]
        lrt <- anova(top_models[[current_model]], top_models[[nested_model]])
        p <- lrt$`Pr(>Chisq)`[2]
        
        if (p >= 0.05) {
          candidate_list <- bind_rows(candidate_list, data.frame(model = nested_model))
          drop_list <- bind_rows(drop_list, data.frame(model = current_model))
        } else {
          candidate_list <- bind_rows(candidate_list, data.frame(model = current_model))
          drop_list <- bind_rows(drop_list, data.frame(model = nested_model))
        }
        
        results <- data.frame(model = current_model, nested = nested_model, p = p)
        nested_results <- bind_rows(nested_results, results)
        
        }
      }
  }
  
  candidate_list <- candidate_list %>% 
    filter(!model %in% drop_list$model) %>% 
    distinct()
  
  return(list(candidate_list = candidate_list, nested_results = nested_results))
}



# check_nested_models <- function(top_models) {
#   
#   nested_results <- data.frame(model = character(),
#                                nested = character(),
#                                p = numeric(),
#                                stringsAsFactors = FALSE)
#   
#   model_set <- model.sel(top_models$model) %>% arrange(delta)
#   nested_list <- nested(model_set, indices = "rownames")
#   
#   all_models <- names(top_models$model)
#   drop_list <- c()
#   
#   for (i in seq_along(nested_list)) {
#     current_model <- names(nested_list)[i]
#     nested_models <- nested_list[[i]]
#     
#     if (length(nested_models) == 0) next
#     
#     for (nested_model in nested_models) {
#       lrt <- anova(top_models$model[[current_model]], top_models$model[[nested_model]])
#       p <- lrt$`Pr(>Chisq)`[2]
#       
#       nested_results <- bind_rows(nested_results, data.frame(
#         model = current_model,
#         nested = nested_model,
#         p = p
#       ))
#     }
#   }
#   
#   # Identify models to drop
#   for (model in all_models) {
#     nested_comparisons <- nested_results %>% filter(model == !!model)
#     if (nrow(nested_comparisons) > 0) {
#       if (any(nested_comparisons$p >= 0.05)) {
#         drop_list <- c(drop_list, model)
#       }
#     }
#   }
#   
#   candidate_list <- setdiff(all_models, drop_list)
#   
#   return(list(
#     candidate_list = candidate_list,
#     drop_list = drop_list,
#     nested_results = nested_results
#   ))
# }


# Include only 2-way interactions  ----
get_2way_list <- function(predictors_df, habitat){
  
  predictors_df <- predictors_df %>% 
    filter(pred_group %in% c("all", "combined")) %>% 
    filter(!str_detect(predictor, "depth_sd")) %>% 
    mutate(predictor2 = paste0(predictor, " * site_type"))
  
  K25_absent <- sum(predictors_df$predictor == "kelp_annual_25") == 0
  DCV25_absent <- sum(predictors_df$predictor == "depth_cv_25") == 0
  
  hard_vars  <- predictors_df %>% filter(str_detect(predictor, "hard")) %>% pull(predictor)
  soft_vars  <- predictors_df %>% filter(str_detect(predictor, "soft")) %>% pull(predictor)
  kelp_vars  <- predictors_df %>% filter(str_detect(predictor, "kelp")) %>% pull(predictor)
  depth_vars <- predictors_df %>% filter(str_detect(predictor, "depth")) %>% pull(predictor)
  aqua_vars  <- predictors_df %>% filter(str_detect(predictor, "aquatic")) %>% pull(predictor)
  
  hard_intx <- predictors_df %>% filter(str_detect(predictor, "hard")) %>% pull(predictor2)
  soft_intx <- predictors_df %>% filter(str_detect(predictor, "soft")) %>% pull(predictor2)
  kelp_intx <- predictors_df %>% filter(str_detect(predictor, "kelp")) %>% pull(predictor2)
  depth_intx <- predictors_df %>% filter(str_detect(predictor, "depth")) %>% pull(predictor2)
  aqua_intx  <- predictors_df %>% filter(str_detect(predictor, "aquatic")) %>% pull(predictor2)
  
  # Version with both depths matching scales
  # depth_comb <- predictors_df %>% filter(str_detect(predictor, "depth")) %>% 
  #   group_by(scale) %>%
  #   summarize(d1 = paste(predictor, collapse = " + "),
  #             d2 = paste(predictor2, collapse = " + "),
  #             d3 = paste(predictor[1], predictor2[2], sep = " + ", collapse = " + "),
  #             d4 = paste(predictor2[1], predictor[2], sep = " + ", collapse = " + ")) %>%
  #   pivot_longer(cols = d1:d4, values_to = "predictor") %>% pull(predictor)
  
  # Version with both depths all scales
  depth_comb <- expand.grid(depth_mean = c(depth_vars[str_detect(depth_vars, "depth_mean")], depth_intx[str_detect(depth_intx, "depth_mean")]),
                            depth_cv = c(depth_vars[str_detect(depth_vars, "depth_cv")], depth_intx[str_detect(depth_intx, "depth_cv")])) %>%
    unite("predictors", c(depth_mean, depth_cv), sep = " + ") %>% pull(predictors)
  
  # Generate all models (all combinations of H, K, and D at any scale)
  if (habitat == "surf"){
    pred_list <-  expand.grid(hard  = c(NA, hard_vars, hard_intx, soft_vars, soft_intx),
                              kelp  = c(NA, kelp_vars, kelp_intx),
                              depth = c(NA, depth_vars, depth_intx, depth_comb), # add depth_comb here if multiple depths
                              aqua = c(NA, aqua_vars, aqua_intx),
                              stringsAsFactors = FALSE) 
  } else {
    pred_list <-  expand.grid(hard  = c(NA, hard_vars, hard_intx),
                              kelp  = c(NA, kelp_vars, kelp_intx),
                              depth = c(NA, depth_vars, depth_intx, depth_comb), # add depth_comb here if multiple depths
                              aqua = c(NA, aqua_vars, aqua_intx),
                              stringsAsFactors = FALSE)
  }
  
  pred_list <-  pred_list %>% 
    mutate(hard_scale  = str_extract(hard, "\\d+"),
           kelp_scale  = str_extract(kelp, "\\d+"),
           aqua_scale = str_extract(aqua, "\\d+"),
           depth_scale = str_extract(depth, "\\d+"),
           depth_scale2 = str_extract_all(depth, "\\d+") %>% map_chr(~ .x[2] %||% NA)
    ) %>% 
    mutate(base_terms = "site_type * age_at_survey") %>% 
    unite("predictors", c(hard, kelp, depth, aqua, base_terms), sep = " + ", na.rm = TRUE, remove = FALSE) %>% 
    mutate(type = case_when(hard_scale == kelp_scale & kelp_scale == depth_scale & depth_scale == aqua_scale &
                              str_detect(hard, "site") & str_detect(kelp, "site") & str_count(depth, "site") == 2 & depth_scale == depth_scale2 & str_detect(aqua, "site") ~ "core",
                            predictors == "site_type * age_at_survey" ~ "base",
                            T~NA)) %>% 
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
             str_replace_all("\\s+", "")) %>% 
    dplyr::select(predictors, type, model_id, hard_scale, kelp_scale, depth_mean_scale = depth_scale, depth_cv_scale = depth_scale2, aqua_scale) %>% 
    mutate(type = if_else(K25_absent & str_detect(model_id, stringr::fixed("H25*ST+DM25*ST+DCV25*ST+AV25*ST+ST*A")), "core", type)) %>% 
    mutate(type = if_else(DCV25_absent & str_detect(model_id, stringr::fixed("H25*ST+K25*ST+DM25*ST+AV25*ST+ST*A")), "core", type))
  
  return(pred_list)
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
                                       pred$intx3[str_detect(pred$predictor, "depth_cv")]), stringsAsFactors = F) %>% 
    mutate(base = "site_type * age_at_survey") %>% 
    unite("predictors", c(hard, kelp, depm, depc, base), sep = " + ", na.rm = TRUE, remove = FALSE) %>% 
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
  
  return(pred_3way)
  
}
