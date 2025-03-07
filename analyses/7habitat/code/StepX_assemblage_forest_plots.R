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

focal_group <- "targeted"
habitat <- "Shallow reef"
path <- "analyses/7habitat/output/rock/region-mpa-year"


# habitat <- "kelp"
# path <- "analyses/7habitat/output/drop-outliers/mpa-year"
#path <- "analyses/7habitat/output/drop-outliers/site-year"
#path <- "analyses/7habitat/output/drop-outliers/nested.ms"


make_forest_plots <- function(focal_group, path, habitat){
  print(paste("Group:", focal_group))
  # Read the data
  data <- readRDS(file.path(path, paste0(focal_group, "_results.rds")))$results %>% 
    filter(term != "(Intercept)") %>%  
    mutate(scale = if_else(key != "Full Model", as.factor(str_extract(term, "\\d+")), as.factor(scale)),
           importance_type = case_when(is.na(importance) | importance > 0.5 ~ "Greater than 0.5",
                                       !is.na(importance) & importance < 0.5 ~ "Less than 0.5"),
           type = case_when(model_id == "Model Average" ~ "average", 
                            model_id == "Top Model" ~ "top",
                            TRUE ~ type)) %>% 
    mutate(key = case_when(type == "base" ~ "Top Model v. Base Model",  T~key) %>% 
             factor(levels = c( "Full Model", "Top Models (Average)", "Top Model v. Base Model"))) %>% 
    mutate(scale = case_when(type == "base" ~ "Base",
                             is.na(scale) ~ "None",
                             TRUE ~ as.character(scale)) %>% 
             factor(levels = c(25, 50, 100, 250, 500, "None", "Base"))) %>% 
    mutate(depth_core = if_else(type == "core" & (str_detect(model_id, "DM25\\*ST\\+DCV25\\*ST") |
                                                   str_detect(model_id, "DM50\\*ST\\+DCV50\\*ST") |
                                                   str_detect(model_id, "DM100\\*ST\\+DCV100\\*ST") |
                                                   str_detect(model_id, "DM250\\*ST\\+DCV250\\*ST") | 
                                                   str_detect(model_id, "DM500\\*ST\\+DCV500\\*ST")), "core", NA))
  
  # Determine top model type
  top <- data %>% 
    filter(key %in% c("Top Model v. Base Model", "Top Models (Average)")) %>% 
    #filter(importance >= 0.5) %>% 
    mutate(top_depth = case_when(str_detect(term_revised, "_cv") ~ "depth_cv",
                                 str_detect(term_revised, "_mean") ~ "depth_mean",
                                 T~NA_character_)) %>% 
    mutate(top_sub = case_when(str_detect(term_revised, "hard") ~ "hard_bottom",
                               str_detect(term_revised, "soft") ~ "soft_bottom",
                               T~NA_character_)) %>% 
    dplyr::select(species_code, term_revised, top_depth, top_sub)
  
  top_depth <- unique(top$top_depth[!is.na(top$top_depth)])
  
  top_depth_intx <- top %>% 
    filter(str_detect(term_revised, ":") & !is.na(top_depth)) %>% 
    distinct(top_depth) %>% 
    pull(top_depth)
  
  if (length(top_depth) > 0) {
  data_plot <- data %>% 
    filter(type %in% c("average", "top", "base", "core")) %>% 
    filter(!(type == "core" & is.na(depth_core)))
  } else {
    data_plot <- data %>% 
      filter(type %in% c("average", "top", "base", "core")) %>% 
      filter(!(type == "core" & depth_type != "depth_cv"))
  }
  
  forest <- ggplot(data_plot, 
                   aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, linetype = importance_type, color = scale),
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
         linetype = "Importance",
         title = paste0(str_to_sentence(focal_group), "\n", str_to_sentence(habitat), "\n", "Top Models: ", paste(unique(data$num_top_models)))) +
    my_theme
  forest
  
  ggsave(forest, filename = paste(focal_group, snakecase::to_snake_case(habitat), "forest.png", sep = "_"),
         path = path, width = 10, height = 5, dpi = 300, units = "in")
  
  # Create table to export
  gt_table <- data_plot %>%
    filter(model_id == "Top Model" | key == "Top Model v. Base Model") %>%
    dplyr::select(term_revised, scale, estimate, std_error, statistic, df, p_value, significance) %>%
    mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>%
    mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>%
    mutate(scale = if_else(scale == "None", NA_character_, scale)) %>% 
    gt() %>%
    tab_header(title = html(paste0("<b><i>", str_to_sentence(focal_group), " - ", str_to_sentence(habitat), "</i></b>"))) %>%
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
    sub_missing(columns = everything(), missing_text = "") %>%
    tab_options(heading.align = "left") %>%
    cols_align(align = "center", columns = everything()) %>%
    tab_style(style = cell_text(weight = "bold"),
              locations = cells_body(columns = c(p_value, significance), rows = significance %in% c("***", "**", "*") )) %>%
    tab_options(table.font.size = "small", table.width = pct(100)) %>% 
    tab_row_group(label = "Base Model", rows = scale == "Base") %>%
    tab_row_group(label = "Top Model", rows = scale != "Base" | is.na(scale))
  gt_table
  gtsave(gt_table, filename = paste(focal_group, snakecase::to_snake_case(habitat), "table.docx", sep = "_"),
         path = path, width = 4, height = 3, dpi = 300, units = "in")

}



# Create and save figures -------------------------------------------------------------

focal_groups <- c("targeted", "nontargeted", "all")

habitat_paths <- list(
  "Shallow rocky reef" = c("analyses/7habitat/output/rock/mpa-year", 
                           "analyses/7habitat/output/rock/region-mpa-year"),
  "Kelp forest" = c("analyses/7habitat/output/kelp/region-mpa-year", 
                    "analyses/7habitat/output/kelp/mpa-year", 
                    "analyses/7habitat/output/kelp/region-mpa-year-drop", 
                    "analyses/7habitat/output/kelp/mpa-year-drop"))

# Iterate efficiently over all combinations
purrr::walk2(
  rep(names(habitat_paths), lengths(habitat_paths)),  
  unlist(habitat_paths),
  ~ purrr::walk(focal_groups, \(fg) make_forest_plots(focal_group = fg, path = .y, habitat = .x))
)



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

