# Separate making the tables to a diffeerent script 

library(gt)
library(tidyverse)

list2env(list(habitat = "rock_filtered",
              focal_group = "targeted",
              re_string = "rmsy"), envir = .GlobalEnv)

get_tables <- function(habitat, focal_group, re_string){
  
  results_file <- paste(habitat, focal_group, re_string, "selection_results.rds", sep = "_")
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", results_file)) 
  
  return(list(aicc_table = results$aicc_table,
              aicc_table_full = results$aicc_table_full,
              predictor_table = results$predictor_table))
}

rock <- get_tables("rock_filtered", "targeted", "rmsy")
kelp <- get_tables("kelp_filtered", "targeted", "msy")
surf <- get_tables("surf_filtered", "targeted", "m")


rock_aicc <- rock$aicc_table_full
kelp_aicc <- kelp$aicc_table_full
surf_aicc <- surf$aicc_table_full

combined_aicc <- bind_rows(
  rock = rock_aicc,
  kelp = kelp_aicc,
  surf = surf_aicc,
  .id = "Ecosystem"
) %>% mutate(Ecosystem = case_when(Ecosystem == "rock" ~ "Shallow reef",
                                   Ecosystem == "kelp" ~ "Kelp forest",
                                   Ecosystem == "surf" ~ "Surf zone")) %>% 
  mutate(Model = str_replace(Model, "AV", "MB"))

aicc_table <- combined_aicc %>% 
  filter(delta <= 2 | Model == "ST*A") %>% 
  gt(groupname_col = "Ecosystem") %>% 
  cols_label(delta = "ΔAICc",
             weight = "AICc Weight") %>% 
  fmt_number(columns = c(delta, weight), decimals = 3) %>% 
  tab_source_note(
    source_note = "H = hard bottom, S = soft bottom, ST = site type, DM = depth mean, DCV = depth coefficient of variation, K = annual kelp canopy cover, MB = maximum biotic extent, A = MPA age"
  ) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(12)), 
            locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold"), 
            locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold", color = "black"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(9)), 
            locations = cells_source_notes()) %>% 
  tab_options(data_row.padding = px(4),
             row_group.padding = px(6)) %>% 
  tab_style(
    style = cell_fill(color = "lightgray"),
    locations = cells_body(rows = top == TRUE)
  ) %>%   cols_hide("top") %>% 
  tab_options(table.width = pct(60), heading.align = "left") 
  

aicc_table
gtsave(aicc_table, "table1-aicc.png",  vwidth = 900, vheight = 1200)


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
            locations = cells_column_labels(columns = everything())) %>% 

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
  mutate(rank = 1:nrow(.)) %>% 
  gt() %>%
  cols_label(term = "Term",
             estimate = "Estimate",
             CI = "95% CI",
             importance_abs_t = "Absolute t-statistic",
             importance_sw = "AICc Weight (SW)",
             rank = "Rank") %>%
  tab_header(title = paste0("Predictor importance: ", str_to_sentence(habitat), ", ", focal_group, " fish biomass")) %>%
  fmt_number(columns = c(estimate, importance_abs_t, importance_sw),
             decimals = 3) %>%
  tab_options(heading.align = "left") %>%
  cols_align(align = "center", columns = everything()) %>%
  cols_align(align = "left", columns = c("term")) %>%
  tab_style(style = cell_text(font = "Arial", size = px(12)),
            locations = cells_body(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(font = "Arial", size = px(12), weight = "bold"),
    locations = cells_column_labels(columns = everything())
  ) %>%
  tab_style(
    style = cell_text(font = "Arial", size = px(13), weight = "bold"),
    locations = cells_title(groups = "title")
  )

# Get models for R2 comparison table
get_models <- function(habitat, focal_group, re_string){
  
  effects_file <- paste(habitat, focal_group, re_string, "effects.rds", sep = "_")
  effects <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects", effects_file)) 
  
  return(list(models = effects$models,
              results = effects$results))
}

rock <- get_models("rock_filtered", "targeted", "rmsy")
kelp <- get_models("kelp_filtered", "targeted", "msy")
surf <- get_models("surf_filtered", "targeted", "m")


rock <- rock$models
kelp <- kelp$models
surf <- surf$models

model_lists <- list(rock = rock, kelp = kelp, surf = surf)

model_long <- map2_dfr(
  model_lists,
  names(model_lists),
  ~ tibble(
    Ecosystem = .y,
    Model = c("top", "base"),
    Marginal_R2 = map_dbl(c(.x$top, .x$base), ~ r2_nakagawa(.x)$R2_marginal),
    Conditional_R2 = map_dbl(c(.x$top, .x$base), ~ r2_nakagawa(.x)$R2_conditional)
  )
)

model_long

r2 <- model_long %>% 
  mutate(Ecosystem = case_when(Ecosystem == "rock" ~ "Shallow reef",
                               Ecosystem == "kelp" ~ "Kelp forest",
                               Ecosystem == "surf" ~ "Surf zone"),
         Model = if_else(Model == "top", "Top model", "Base model")) %>% 
  gt(groupname_col = "Ecosystem") %>% 
  fmt_number(columns = c("Marginal_R2", "Conditional_R2"), decimals = 3) %>% 
  cols_label( Marginal_R2 = html("Marginal R&sup2;"),
              Conditional_R2 = html("Conditional R&sup2;")) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(12)), 
            locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold"), 
            locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold", color = "black"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(9)), 
            locations = cells_source_notes()) %>% 
  tab_options(data_row.padding = px(4),
              row_group.padding = px(6)) %>% 
  tab_options(table.width = pct(30), heading.align = "left") 

r2
gtsave(r2, "tableSX-r2.png",  vwidth = 1200, vheight = 1200)



# Function to extract variance components
extract_re_variance <- function(model, ecosystem, model_type) {
  vc <- as.data.frame(VarCorr(model))
  total_var <- sum(vc$vcov)
  
  vc %>%
    mutate(
      Ecosystem = ecosystem,
      Model = model_type,
      Group = grp,
      Variance = vcov,
      Percent = 100 * vcov / total_var
    ) %>%
    select(Ecosystem, Model, Group, Variance, Percent)
}

# Combined model lists
model_lists <- list(`Shallow reef` = rock, `Kelp forest` = kelp, `Surf zone` = surf)

# Extract variance contributions for top and base models
re_variance <- map2_dfr(model_lists, names(model_lists), function(models, eco) {
  bind_rows(
    extract_re_variance(models$top,  ecosystem = eco, model_type = "top"),
    extract_re_variance(models$base, ecosystem = eco, model_type = "base")
  )
})

re_variance %>% 
  gt(groupname_col = "Ecosystem") %>% 
  fmt_number(columns = c("Variance", "Percent"), decimals = 3)
