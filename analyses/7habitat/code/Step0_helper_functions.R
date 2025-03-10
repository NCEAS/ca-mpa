# Step 0 Build Helper Functions
# Moved here Feb 2025
# Cori Lopazanski

# Helper Functions -------------------------------------------------------------

clean_terms <- function(df) {df %>%
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
                               "site_type:age_at_survey",
                               "site_type",
                               "age_at_survey",
                               "depth_mean",
                               "depth_sd",
                               "depth_cv",
                               "soft_bottom",
                               "hard_bottom",
                               "kelp")))}

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
