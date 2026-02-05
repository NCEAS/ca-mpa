# Step 0 Build Helper Functions
# Moved here Feb 2025
# Cori Lopazanski

# Helper Functions -------------------------------------------------------------

clean_terms <- function(df) {
  
  term_levels <- c("region - North",
                   "region - South",
                   "region - N. Channel Islands",
                   "region - Central",
                   "(Intercept)",
                   "site_type:age_at_survey:depth_mean",
                   "site_type:age_at_survey:depth_sd",
                   "site_type:age_at_survey:depth_cv",
                   "site_type:age_at_survey:slope_sd",
                   "site_type:age_at_survey:relief",
                   "site_type:age_at_survey:kelp",
                   "site_type:age_at_survey:hard_bottom",
                   "age_at_survey:depth_mean",
                   "age_at_survey:depth_sd",
                   "age_at_survey:depth_cv",
                   "age_at_survey:slope_sd",
                   "age_at_survey:relief",
                   "age_at_survey:hard_bottom",
                   "age_at_survey:kelp",
                   "site_type:depth_mean",
                   "site_type:depth_sd",
                   "site_type:depth_cv",
                   "site_type:slope_sd",
                   "site_type:relief",
                   "site_type:soft_bottom",
                   "site_type:hard_bottom",
                   "site_type:kelp",
                   "site_type:max_biotic_extent",
                   "site_type:age_at_survey",
                   "site_type",
                   "age_at_survey",
                   "depth_mean",
                   "depth_sd",
                   "depth_cv",
                   "slope_sd",
                   "relief",
                   "soft_bottom",
                   "hard_bottom",
                   "kelp",
                   "max_biotic_extent")
  
  df %>%
    #   mutate(term = str_remove_all(term, "MPA") %>% 
    #            if_else(str_detect(., ":site_type$"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .)) %>% 
    mutate(term_revised = str_remove_all(term, "MPA") %>% 
             str_remove_all("_500|_400|_300|_250|_200|_150|_100|_50|_25|_annual") %>% 
             if_else(str_detect(., ":age_at_survey$"), str_replace(., "^(.*):age_at_survey$", "age_at_survey:\\1"), .) %>% 
             if_else(str_detect(., ":site_type$"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .) %>% 
             if_else(str_detect(., ":site_type:"), str_replace(., "^(.*?):(site_type):(.*?)$", "\\2:\\3:\\1"), .) %>% 
             str_replace_all("site_type:(.*?):age_at_survey", "site_type:age_at_survey:\\1") %>% 
             str_replace_all("4", " - ") %>% 
             str_replace_all("aquatic_vegetation_bed", "max_biotic_extent") %>% 
             str_replace_all("_", " ") %>% 
             str_to_title() %>% 
             str_replace_all(":", " x ") %>% 
             str_replace_all(regex("depth cv", ignore_case = TRUE), "Depth CV") %>% 
             str_replace_all(regex("slope sd", ignore_case = TRUE), "Slope SD") %>% 
             str_replace_all(regex("site type", ignore_case = TRUE), "Protected Status") %>% 
             str_replace_all(regex("age at survey", ignore_case = TRUE), "MPA Age") %>%
             str_replace_all("hard", "Hard") %>% 
             str_replace_all("kelp", "Kelp") %>% 
             str_replace_all("soft", "Soft") %>% 
             str_replace_all("depth", "Depth")) %>% 
    mutate(term_scale = if_else(!str_detect(term, "region"), str_extract(term, "\\d+"), NA)) 
}

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

condense_terms <- function(predictors){
  predictors <- 
    str_replace_all(predictors, "hard_bottom_(\\d+)", "H\\1") %>% 
    str_replace_all("soft_bottom_(\\d+)", "S\\1") %>% 
    str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
    str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
    str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
    str_replace_all("depth_cv_(\\d+)", "DCV\\1") %>% 
    str_replace_all("tri_mean_(\\d+)", "TRI\\1") %>% 
    str_replace_all("slope_sd_(\\d+)", "SSD\\1") %>% 
    str_replace_all("relief_(\\d+)", "VR\\1") %>% 
    str_replace_all("site_type", "ST") %>%
    str_replace_all("age_at_survey", "A") %>% 
    str_replace_all("aquatic_vegetation_bed_(\\d+)", "AV\\1") %>% 
    str_replace_all("\\s+", "")
  return(predictors)
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
                           deps = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "depth_sd")], 
                                    pred$intx[str_detect(pred$predictor, "depth_sd")],
                                    pred$intx2[str_detect(pred$predictor, "depth_sd")],
                                    pred$intx3[str_detect(pred$predictor, "depth_sd")]), 
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
                           reli = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "relief")], 
                                    pred$intx[str_detect(pred$predictor, "relief")],
                                    pred$intx2[str_detect(pred$predictor, "relief")],
                                    pred$intx3[str_detect(pred$predictor, "relief")]),
                           stringsAsFactors = F) %>% 
    mutate(base = "site_type * age_at_survey") %>% 
    unite("predictors", c(hard, kelp, depm, depc, deps, trim, slsd, reli, base), sep = " + ", na.rm = TRUE, remove = FALSE) %>% 
    mutate(model_id = condense_terms(predictors))
  
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
                           deps = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "depth_sd")], 
                                    pred$intx[str_detect(pred$predictor, "depth_sd")],
                                    pred$intx2[str_detect(pred$predictor, "depth_sd")],
                                    pred$intx3[str_detect(pred$predictor, "depth_sd")]), 
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
                           reli = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "relief")], 
                                    pred$intx[str_detect(pred$predictor, "relief")],
                                    pred$intx2[str_detect(pred$predictor, "relief")],
                                    pred$intx3[str_detect(pred$predictor, "relief")]),
                           aquv = c(NA, 
                                    pred$predictor[str_detect(pred$predictor, "aquatic")], 
                                    pred$intx[str_detect(pred$predictor, "aquatic")],
                                    pred$intx2[str_detect(pred$predictor, "aquatic")],
                                    pred$intx3[str_detect(pred$predictor, "aquatic")]), stringsAsFactors = F) %>% 
    mutate(base = "site_type * age_at_survey") %>% 
    filter(!(!is.na(hard) & !is.na(soft))) %>% 
    unite("predictors", c(hard, soft, kelp, depm, depc, deps, trim, slsd, reli, aquv, base), sep = " + ", na.rm = TRUE, remove = FALSE) %>% 
    mutate(model_id = condense_terms(predictors))
  
  return(pred_3way)
  
}

create_re_string <- function(random_effects){
  re_string <- data.frame(random_effects = random_effects) %>% 
    mutate(re_string = str_replace_all(random_effects, "region4", "r") %>% 
             str_replace_all(., "affiliated_mpa", "m") %>% 
             str_replace_all(., "site", "s") %>% 
             str_replace_all(., "year", "y") %>% 
             str_replace_all(., "/", "")) 
  
  re_string <- paste(re_string$re_string, collapse = "")
  
  return(re_string)
}
