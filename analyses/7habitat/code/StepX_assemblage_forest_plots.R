# Build forest plots with full models
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Setup ------------------------------------------------------------------------
library(tidyverse)
library(gt)
library(patchwork)
library(performance)
library(effects)

rm(list = ls())
gc()


# Begin ------------------------------------------------------------------------

# Set my theme
my_theme <- theme(
  plot.title = element_text(size = 10, face = "bold"),
  plot.subtitle = element_text(size = 8),
  axis.title = element_text(size = 8),
  axis.text = element_text(size = 8),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8),
  plot.caption = element_text(size = 8),
  strip.text = element_text(size = 7, face = "bold"),
  panel.background = element_rect(fill = "white", color = NA),  
  plot.background = element_rect(fill = "white", color = NA)
)

path <- "analyses/7habitat/output"
habitat <- "rock"
focal_group <- "targeted"
re_string <- "rmy"
results_file <- paste(habitat, focal_group, re_string, "results.rds", sep = "_")

make_forest_plots <- function(results_file, habitat, focal_group, re_string){
  print(paste(results_file))
  # Read the data
  data <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", paste(results_file)))$results %>% 
    filter(term != "(Intercept)") %>%  
    mutate(scale = if_else(key != "Full Model", as.factor(str_extract(term, "\\d+")), as.factor(scale)),
           importance_sw_type = case_when(is.na(importance_sw) | importance_sw > 0.5 ~ "Greater than 0.5",
                                       !is.na(importance_sw) & importance_sw < 0.5 ~ "Less than 0.5"),
           type = case_when(model_id == "Model Average" ~ "average", 
                            model_id == "Top Model" ~ "top",
                            TRUE ~ type)) %>% 
    mutate(key = case_when(type == "base" ~ "Top Model v. Base Model",  T~key) %>% 
             factor(levels = c( "Full Model", "Top Models (Average)", "Top Model v. Base Model"))) %>% 
    mutate(scale = case_when(type == "base" ~ "Base",
                             is.na(scale) ~ "None",
                             TRUE ~ as.character(scale)) %>% 
             factor(levels = c(25, 50, 100, 250, 500, "None", "Base"))) 
  
  data_plot <- data %>% 
    filter(type %in% c("average", "top", "base", "core"))
  
  forest <- ggplot(data_plot, 
                   aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, linetype = importance_sw_type, color = scale),
                   height = 0.3, position = position_dodge(width = 0.6))+
    geom_point(position = position_dodge(width = 0.6), size = 2) +  
    scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
    scale_color_manual(values = c("#440154","#3b528b", "#21908d", "#5dc863", "#D7C51B", "grey60", "black"), # Adjusted manually
                       guide = guide_legend(order = 1)) +
    facet_wrap(~key) +
    theme_minimal() +
    labs(x = "Estimate (scaled)", 
         y = NULL, 
         color = "Scale", 
         pch = "Significance", 
         linetype = "Importance") +
    my_theme
  forest
  
  ggsave(forest, filename = paste(habitat, focal_group, re_string, "forest.png", sep = "_"),
         path = "~/ca-mpa/analyses/7habitat/output/forest", width = 8, height = 5, dpi = 300, units = "in")
  
  # Create table to export
  gt_table <- data_plot %>%
    filter(model_id == "Top Model" | key == "Top Model v. Base Model") %>%
    dplyr::select(term_revised, scale, estimate, std_error, statistic, df, p_value, significance) %>%
    mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>%
    mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>%
    mutate(scale = if_else(scale == "None", NA_character_, scale)) %>% 
    gt() %>%
    tab_header(title = paste0("Model results: ", str_to_sentence(habitat), ", ", focal_group, " fish biomass")) %>% 
    cols_label(term_revised = "Term",
               scale = "Scale (m)",
               estimate = "Estimate",
               std_error = "Std. Error",
               statistic = "Statistic",
               df = "df",
               p_value = "p-value",
               significance = "") %>%
    fmt_number(columns = c(estimate, std_error, statistic),
               decimals = 3) %>%
    fmt_number(columns = c(df),
               decimals = 0) %>%
    sub_missing(columns = everything(), missing_text = "") %>%
    tab_options(heading.align = "left") %>%
    cols_align(align = "center", columns = everything()) %>% 
    tab_row_group(label = "Base Model", rows = scale == "Base") %>%
    tab_row_group(label = "Top Model", rows = scale != "Base" | is.na(scale)) %>% 
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
    tab_options(table.width = pct(90))
  
  gt_table
  
  gtsave(gt_table, filename = paste(habitat, focal_group, re_string, "table.docx", sep = "_"),
         path = "~/ca-mpa/analyses/7habitat/output/table", width = 4, height = 3, dpi = 300, units = "in")
  
  # Create predictor importance table
  predictor_table <- data %>% 
    filter(key == "Top Models (Average)") %>% 
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
               estimate = "Std. Estimate",
               CI = "95% CI",
               importance_abs_t = "Absolute t-statistic",
               importance_sw = "AICc Weight (SW)",
               rank = "Rank") %>%
    tab_header(title = paste0("Model-averaged predictor importance: ", str_to_sentence(habitat), ", ", focal_group, " fish biomass")) %>%
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
    ) %>% 
    tab_options(table.width = pct(90))
  predictor_table  
  gtsave(predictor_table, filename = paste(habitat, focal_group, re_string, "predictor_table.docx", sep = "_"),
         path = "~/ca-mpa/analyses/7habitat/output/table",  width = 4, height = 3, dpi = 300, units = "in")
  
   
  # Barplot ?
  bar_plot <- data_plot %>% 
    filter(term_revised == "site_type:age_at_survey" & key == "Top Model v. Base Model") %>% 
    mutate(model_id = case_when(model_id == "Model Average" ~ "Top Model",
                                model_id == "ST*A" ~ "Base Model"))
  
  ggplot(bar_plot, aes(x = model_id, y = estimate)) +
    geom_point(size = 3) +  # Coefficients as points
    geom_errorbar(aes(ymin = conf_low, ymax = conf_high), width = 0.2) +  # CI bars
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray") +  # Reference line at 0
    theme_minimal() +
    labs(x = NULL, y = "Estimate")
}



# Create and save figures -------------------------------------------------------------

file_list <- data.frame(results_file = list.files(path = "~/ca-mpa/analyses/7habitat/output/results")) %>% 
  mutate(info = str_remove_all(results_file, "_results.rds")) %>% 
  separate(info, into = c("habitat", "focal_group", "re_string"), sep = "_")

pmap(file_list, make_forest_plots)



## Species Versions

## Kelp ----
path <- "analyses/7habitat/output/2way-4region/kelp-reduced"

list.files(path = path, pattern = "_results.rds") %>%
  str_remove_all("_models.rds|_results.rds") %>% 
  unique() %>% 
  walk(., make_forest_plots, path = path, habitat = "kelp")


## Rocky reef ----
path <- "analyses/7habitat/output/2way/rock"

list.files(path = path, pattern = "_results.rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% 
  unique() %>% 
  map(., make_forest_plots, path = path, habitat = "rock")


## Surf ---
path <- "/Users/lopazanski/Desktop/output/surf"

list.files(path = path, pattern = "_results.rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% 
  unique() %>% 
  map(., make_forest_plots, path = path, habitat = "surf")

## Deep ---
path <- "/Users/lopazanski/Desktop/output/deep"

list.files(path = path, pattern = "_results.rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% 
  unique() %>% 
  map(., make_forest_plots, path = path, habitat = "deep")

