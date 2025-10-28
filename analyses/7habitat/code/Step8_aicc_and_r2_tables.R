# Make AICc, R2, and Nested Tables
# Cori Lopazanski
# Feb 2025


# Setup ------------------------------------------------------------------------

library(gt)
library(performance)
library(tidyverse)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R")  

fig.dir <- "~/ca-mpa/analyses/7habitat/figures/3way-figures"


# Load Tables ------------------------------------------------------------------

get_tables <- function(habitat, focal_group, re_string){
  
  results_file <- paste(habitat, focal_group, re_string, "selection_results.rds", sep = "_")
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results/3way", results_file)) 
  
  return(list(aicc_table = results$aicc_table,
              aicc_table_full = results$aicc_table_full,
              predictor_table = results$predictor_table,
              nested_results = results$nested_results))
}

rock <- get_tables("rock_filtered", "targeted", "rmy")
kelp <- get_tables("kelp_filtered", "targeted", "my")
surf <- get_tables("surf_filtered", "targeted", "m")

rock_nest <- rock$nested_results %>% mutate(larger = str_remove_all(larger, "\\+ST\\*A")) %>% mutate(smaller = str_remove_all(smaller, "\\+ST\\*A"))
kelp_nest <- kelp$nested_results %>% mutate(larger = str_remove_all(larger, "\\+ST\\*A")) %>% mutate(smaller = str_remove_all(smaller, "\\+ST\\*A"))
surf_nest <- surf$nested_results

rock_aicc <- rock$aicc_table_full %>% mutate(Model = str_remove_all(Model, "\\+ST\\*A"))
kelp_aicc <- kelp$aicc_table_full %>% mutate(Model = str_remove_all(Model, "\\+ST\\*A"))
surf_aicc <- surf$aicc_table_full


# Build AICc Table -------------------------------------------------------------

combined_aicc <- bind_rows(rock = rock_aicc,
                           kelp = kelp_aicc,
                           surf = surf_aicc, .id = "Ecosystem") %>% 
  mutate(Ecosystem = case_when(Ecosystem == "rock" ~ "Shallow reef",
                               Ecosystem == "kelp" ~ "Kelp forest",
                               Ecosystem == "surf" ~ "Surf zone")) %>% 
  mutate(Model = str_replace(Model, "AV", "MB") %>% 
           str_replace_all("ST", "P") %>% 
           str_replace_all("\\+", " + ")) 

aicc_table <- combined_aicc %>% 
  filter(delta <= 2 | Model == "P*A") %>% 
  gt(groupname_col = "Ecosystem") %>% 
  cols_label(delta = "ΔAICc",
             weight = "AICc Weight") %>% 
  fmt_number(columns = c(delta, weight), decimals = 3) %>% 
  tab_source_note(source_note = "H = hard bottom, S = soft bottom, P = protected status, DM = depth mean, DCV = depth coefficient of variation, K = annual kelp canopy cover, MB = maximum biotic extent, A = MPA age") %>% 
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
  tab_style(style = cell_fill(color = "lightgray"),
            locations = cells_body(rows = top == TRUE)) %>%   
  cols_hide("top") %>% 
  tab_options(table.width = pct(60), heading.align = "left") 
  

aicc_table

gtsave(aicc_table, file.path(fig.dir, "table1-aicc.png"),  vwidth = 900, vheight = 1200)


aicc_table <- combined_aicc %>% 
  filter(delta <= 2 | Model == "P*A") %>% 
  gt(groupname_col = "Ecosystem") %>% 
  cols_label(delta = "ΔAICc",
             weight = "AICc Weight") %>% 
  fmt_number(columns = c(delta, weight), decimals = 3) %>% 
  tab_options(table.font.names = "Times New Roman",
    table.font.size = px(10),
    data_row.padding = px(4),
    row_group.padding = px(6),
    heading.align = "left",
    table.width = pct(60)) %>% 
  tab_style( style = cell_text(font = "Times New Roman", size = px(12)),
    locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
    locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
    locations = cells_row_groups()) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_body(rows = top == TRUE)) %>%   
  cols_hide("top") %>% 
  tab_source_note(
    source_note = paste0(
      "Notes: Models shown are within ΔAICc ≤ 2 of the top-ranked model or are the base model without any habitat variables (P*A). The final competitive model for each ecosystem after applying the nesting rule and likelihood ratio tests is bolded. ",
      "Abbreviations: H = hard bottom; S = soft bottom; P = protected status; DM = depth mean; DCV = depth coefficient of variation; K = annual kelp canopy cover; MB = maximum biotic extent; A = MPA age."
    )
  ) 

aicc_table

# Build Nested Table ------------------------------------------------------------
combined_nest <- bind_rows(rock = rock_nest,
                           kelp = kelp_nest,
                           surf = surf_nest,
                           .id = "Ecosystem") %>% 
  mutate(Ecosystem = case_when(Ecosystem == "rock" ~ "Shallow reef",
                               Ecosystem == "kelp" ~ "Kelp forest",
                               Ecosystem == "surf" ~ "Surf zone")) %>% 
  rename(Model = larger, Nested = smaller, p_value = p) %>% 
  filter(!Model == "ST*A") %>% 
  filter(!(Nested == "ST*A" & Ecosystem != "Surf zone")) %>% 
  mutate(Model = str_replace(Model, "AV", "MB") %>% 
           str_replace_all("ST", "P") %>% 
           str_replace_all("\\+", " + ")) %>% 
  mutate(Nested = str_replace(Nested, "AV", "MB") %>% 
           str_replace_all("ST", "P") %>% 
           str_replace_all("\\+", " + ")) %>% 
  add_significance() %>% 
  mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>%
  mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>%
  mutate(Model = str_replace(Model, "AV", "MB")) %>% 
  gt(groupname_col = "Ecosystem") %>% 
  cols_label(p_value = "p-value",
             significance = "") %>% 
  sub_missing(columns = everything(), missing_text = "") %>% 
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


combined_nest
gtsave(combined_nest, file.path(fig.dir, "tableSX_nest_results.png"),  vwidth = 900, vheight = 1200)



# Get models for R2 comparison table
get_models <- function(habitat, focal_group, re_string){
  
  effects_file <- paste(habitat, focal_group, re_string, "effects.rds", sep = "_")
  effects <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", effects_file)) 
  
  return(list(models = effects$models,
              results = effects$results))
}

rock <- get_models("rock_filtered", "targeted", "rmy")
kelp <- get_models("kelp_filtered", "targeted", "my")
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
gtsave(r2, file.path(fig.dir, "table2-r2.png"),  vwidth = 1200, vheight = 1200)

r2_difference <- model_long %>%
 dplyr::select(-c(Conditional_R2)) %>% 
  pivot_wider(names_from = Model, values_from = Marginal_R2) %>%
  mutate(Marginal_R2_diff = (top - base)*100) %>%
  select(Ecosystem, Marginal_R2_diff)
r2_difference



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


# Build Combined AICc and R2 Table (EcoApps) ----

# # create a small lookup from model_long (map short ecosystem names to the long names used in combined_aicc)
r2_lookup <- model_long %>%
  mutate(Ecosystem = case_when(
    Ecosystem == "rock" ~ "Shallow reef",
    Ecosystem == "kelp" ~ "Kelp forest",
    Ecosystem == "surf" ~ "Surf zone",
    TRUE ~ Ecosystem
  ),
  role = Model) %>%
  select(Ecosystem, role, Marginal_R2, Conditional_R2)

# tag combined_aicc rows that correspond to the 'top' and 'base' models, join the R2 values, and drop the helper
aicc_with_r2 <- combined_aicc %>%
  mutate(role = case_when(
    top == TRUE         ~ "top",
    Model == "P*A"      ~ "base",
    TRUE                ~ NA_character_
  )) %>%
  left_join(r2_lookup, by = c("Ecosystem", "role")) %>%
  select(-role)

# render with gt (adds the two R2 columns)
aicc_table_with_r2 <- aicc_with_r2 %>%
  filter(delta <= 2 | Model == "P*A") %>%
  gt(groupname_col = "Ecosystem") %>%
  cols_label(delta = "ΔAICc",
    weight = "AICc weight",
    Marginal_R2 = html("Marginal R&sup2;"),
    Conditional_R2 = html("Conditional R&sup2;")
  ) %>%
  fmt_number(columns = c(delta, weight, Marginal_R2, Conditional_R2), decimals = 3) %>% 
  fmt_missing(columns = everything(), missing_text = "") %>% 
  tab_options(table.font.names = "Times New Roman",
              table.font.size = px(10),
              data_row.padding = px(4),
              row_group.padding = px(6),
              heading.align = "left",
              table.width = pct(60)) %>% 
  tab_style( style = cell_text(font = "Times New Roman", size = px(12)),
             locations = cells_body(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_column_labels(columns = everything())) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_row_groups()) %>% 
  tab_style(style = cell_text(font = "Times New Roman", size = px(12), weight = "bold"),
            locations = cells_body(rows = top == TRUE)) %>%   
  cols_hide("top") %>% 
  tab_source_note(source_note = paste0(
      "Notes: Models shown are within ΔAICc ≤ 2 of the top-ranked model or are the base model without any habitat variables (P*A). The final competitive model for each ecosystem after applying the nesting rule and likelihood ratio tests is bolded. ",
      "Abbreviations: H = hard bottom; S = soft bottom; P = protected status; DM = depth mean; DCV = depth coefficient of variation; K = annual kelp canopy cover; MB = maximum biotic extent; A = MPA age.")) 

aicc_table_with_r2
