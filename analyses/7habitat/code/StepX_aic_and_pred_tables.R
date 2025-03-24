# Separate making the tables to a diffeerent script 


# Get AIC weights
aicc_table <- model.sel(top_models) %>% 
  as.data.frame() %>% 
  dplyr::select(delta, weight, df) %>% 
  rownames_to_column("Model") %>% 
  dplyr::select(Model, delta, weight, K = df) %>% 
  gt() %>% 
  cols_label(delta = "ΔAICc",
             weight = "AICc Weight") %>% 
  fmt_number(columns = c(delta, weight), decimals = 3) %>% 
  tab_options(table.width = pct(80), heading.align = "left") %>% 
  tab_style(style = cell_text(font = "Arial", size = px(14)), 
            locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(14), weight = "bold"), 
            locations = cells_column_labels(columns = everything()))

aicc_table

# Get predictor importance table
predictor_table <- top_results %>% 
  dplyr::select(term, term_revised, estimate:importance_sw) %>% 
  mutate(term = str_replace_all(term, "_", " ") %>% 
           str_to_sentence() %>% 
           str_replace("cv", "CV") %>% 
           str_replace("\\d+", paste0(str_extract(., "\\d+"), "m"))) %>% 
  mutate(CI = paste0("(", round(conf_low, 2), ", ", round(conf_high,2), ")")) %>% 
  dplyr::select(term, estimate, CI, importance_abs_t, importance_sw) %>% 
  arrange(desc(importance_abs_t)) %>% 
  mutate(rank = 1:nrow(.))# %>% 
# gt() %>%
# cols_label(term = "Term",
#            estimate = "Estimate",
#            CI = "95% CI",
#            importance_abs_t = "Absolute t-statistic",
#            importance_sw = "AICc Weight (SW)",
#            rank = "Rank") %>%
# tab_header(title = paste0("Predictor importance: ", str_to_sentence(habitat), ", ", focal_group, " fish biomass")) %>%
# fmt_number(columns = c(estimate, importance_abs_t, importance_sw),
#            decimals = 3) %>% 
# tab_options(heading.align = "left") %>%
# cols_align(align = "center", columns = everything()) %>% 
# cols_align(align = "left", columns = c("term")) %>% 
# tab_style(style = cell_text(font = "Arial", size = px(12)), 
#           locations = cells_body(columns = everything())
# ) %>%
# tab_style(
#   style = cell_text(font = "Arial", size = px(12), weight = "bold"), 
#   locations = cells_column_labels(columns = everything())
# ) %>%
# tab_style(
#   style = cell_text(font = "Arial", size = px(13), weight = "bold"),
#   locations = cells_title(groups = "title")
# ) 

predictor_table  