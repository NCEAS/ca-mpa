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


check_nested_models <- function(models, model_names) {
  # helper to check nesting: returns a string if nested, NA otherwise
  nested_status <- function(mod1, mod2) {
    t1 <- attr(terms(mod1), "term.labels")
    t2 <- attr(terms(mod2), "term.labels")
    if (all(t1 %in% t2)) return("mod1_nested_in_mod2")
    if (all(t2 %in% t1)) return("mod2_nested_in_mod1")
    NA_character_
  }
  
  # Compare every unique pair of models
  comparisons <- combn(model_names, 2, simplify = FALSE) %>% 
    map_df(function(pair) {
      mod1 <- models[[pair[1]]]
      mod2 <- models[[pair[2]]]
      nest_status <- nested_status(mod1, mod2)
      nested_flag <- !is.na(nest_status)
      p_val <- NA_real_
      decision <- "not_nested"
      
      if (nested_flag) {
        test <- anova(mod1, mod2)
        p_val <- test$`Pr(>Chisq)`[2]
        # If the difference is not significant, keep both; if significant, keep the better one (lower AIC)
        if (is.na(p_val) || p_val > 0.05) {
          decision <- "keep_both"
        } else {
          decision <- if (AIC(mod1) < AIC(mod2)) paste(pair[1])
          else paste(pair[2])
        }
      }
      
      tibble(
        model1 = pair[1],
        model2 = pair[2],
        nested = nested_flag,
        p_val = p_val,
        decision = decision
      )
    })
  
  comparisons
}