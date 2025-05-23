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

species <- "SMYS"
habitat <- "kelp"
path <- "analyses/7habitat/output/2way-4region/kelp-reduced-rescaled"


make_forest_plots <- function(species, path, habitat){
  print(paste("Species:", species))
  # Read the data
  data <- readRDS(file.path(path, paste0(species, "_results.rds")))$results %>% 
    filter(term != "(Intercept)") %>%  
    mutate(scale = if_else(key != "Full Model", as.factor(str_extract(term, "\\d+")), as.factor(scale)),
           importance_type = case_when(is.na(importance) | importance > 0.5 ~ "Greater than 0.5",
                                       !is.na(importance) & importance < 0.5 ~ "Less than 0.5"),
           type = case_when(model_id == "Model Average" ~ "average", 
                            model_id == "Top Model" ~ "top",
                            TRUE ~ type),
           assemblage_new = str_to_sentence(assemblage_new)) %>% 
    mutate(key = case_when(type == "base" ~ "Top Model v. Base Model",  T~key) %>% 
             factor(levels = c( "Full Model", "Top Models (Average)", "Top Model v. Base Model"))) %>% 
    mutate(scale = case_when(type == "base" ~ "Base",
                             is.na(scale) ~ "None",
                             TRUE ~ as.character(scale)) %>% 
             factor(levels = c(25, 50, 100, 250, 500, "None", "Base")))
  
  
  # Extract species info
  sciname <- unique(data$sciname)
  assemblage <- unique(data$assemblage_new)
  target_status <- unique(data$target_status)
  regions <- unique(data$regions[!is.na(data$regions)])
  
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
    filter(!(type == "core" & depth_type != top_depth))
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
    scale_color_manual(values = c("#3b528b", "#21908d", "#5dc863", "#D7C51B", "grey60", "black"), # Adjusted manually
                       guide = guide_legend(order = 1)) +
    facet_wrap(~key) +
    theme_minimal() +
    labs(x = "Estimate (scaled)", 
         y = NULL, 
         color = "Scale", 
         pch = "Significance", 
         linetype = "Importance",
         title = paste0(sciname, "\n", target_status, "\n", assemblage, "\n", regions)) +
    my_theme
  forest
  
  ggsave(forest, filename = paste0(species, "_forest.png"),
         path = path, width = 10, height = 5, dpi = 300, units = "in")
  print("  Forest complete.")
  
  # Create table to export
  # gt_table <- data_plot %>% 
  #   filter(model_id == "Top Model" | key == "Refit Top Model (Importance > 0.5)") %>% 
  #   dplyr::select(term_revised, scale, estimate, std_error, statistic, df, p_value, significance) %>% 
  #   mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>% 
  #   mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>% 
  #   gt() %>%
  #   tab_header(title = html(paste0("<b><i>", sciname, "</i></b>", "<br>", target_status, "<br>", assemblage))) %>% 
  #   cols_label(term_revised = "Term",
  #              scale = "Scale (m)",
  #              estimate = "Estimate",
  #              std_error = "Std. Error",
  #              statistic = "Statistic",
  #              df = "df",
  #              p_value = "p-value",
  #              significance = "") %>%
  #   fmt_number(columns = c(estimate, std_error, statistic),
  #              decimals = 3) %>%
  #   sub_missing(columns = everything(), missing_text = "") %>%
  #   tab_options(heading.align = "left") %>% 
  #   cols_align(align = "center", columns = everything()) %>%
  #   tab_style(style = cell_text(weight = "bold"),
  #             locations = cells_body(columns = c(p_value, significance), rows = significance %in% c("***", "**", "*") )) %>%
  #   tab_options(table.font.size = "small", table.width = pct(100))

  # gtsave(gt_table, filename = paste0(species, "_table.docx"),
  #        path = file.path(fig.dir, habitat), width = 4, height = 3, dpi = 300, units = "in")
  #print("  Table complete.")

}



# Create and save figures -------------------------------------------------------------

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

