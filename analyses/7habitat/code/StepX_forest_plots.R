# Build forest plots with full models
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Setup ------------------------------------------------------------------------
library(tidyverse)

rm(list = ls())
gc()

fig.dir <- "analyses/7habitat/figures/forest"

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
  strip.text = element_text(size = 8, face = "bold"),
  panel.background = element_rect(fill = "white", color = NA),  
  plot.background = element_rect(fill = "white", color = NA)
)

species <- "CPRI"
habitat <- "kelp"
path <- "analyses/7habitat/output/kelp"

make_forest_plots <- function(species, path, habitat){
  print(paste("Species:", species))
  # Read the data
  data <- readRDS(file.path(path, paste0(species, "_results.rds"))) %>% 
    mutate(scale = if_else(model_id != "Full Model", as.factor(str_extract(term, "\\d+")), as.factor(scale))) %>% 
    mutate(scale = factor(scale, levels = c(25, 50, 100, 250, 500, NA))) %>% 
    mutate(importance_type = case_when((is.na(importance) | importance > 0.5) ~ "Greater than 0.5",
                                       (!is.na(importance) & importance < 0.5) ~ "Less than 0.5")) %>% 
    filter(!term == "(Intercept)") %>% 
    mutate(type = case_when(model_id == "Model Average" ~ "average", 
                            model_id == "Top Model" ~ "top",
                            T~type)) %>% 
    mutate(assemblage_new = str_to_sentence(assemblage_new))
  
  # Extract species info
  sciname <- unique(data$sciname)
  assemblage <- unique(data$assemblage_new)
  target_status <- unique(data$target_status)
  
  # Determine top model type
  top <- data %>% 
    filter(key %in% c("Top Model v. Base Model", "Top Models (Average) v. Base Model")) %>% 
    filter(importance >= 0.5) %>% 
    mutate(intx_type = if_else(str_detect(term_revised,  ".*:.*:.*"), "3way", "2way")) %>% 
    mutate(top_depth = case_when(str_detect(term_revised, "_cv") ~ "depth_cv",
                                  str_detect(term_revised, "_sd") ~ "depth_sd",
                                  str_detect(term_revised, "_mean") ~ "depth_mean",
                                  T~NA)) %>% 
    dplyr::select(species_code, term_revised, intx_type, top_depth)
  
  top_depth <- unique(top$top_depth[!is.na(top$top_depth)])
  core_type <- case_when(
    length(top_depth) > 0 & sum(top$intx_type == "3way") > 0 ~ "core_3way",
    length(top_depth) > 0 & sum(top$intx_type == "3way") == 0 ~ "core_2way",
    length(top_depth) == 0 & sum(top$intx_type == "3way") > 0 ~ "no_depth_3way",
    length(top_depth) == 0 & sum(top$intx_type == "3way") == 0 ~ "no_depth_2way")
  
  # Plot models
  if (length(top_depth) > 0) {
    data_plot <- data %>% 
      filter(type %in% c("average", "top", "base", core_type)) %>% 
      filter(!(!type == core_type) & (depth_type == top_depth) | is.na(depth_type))
  } else {
    data_plot <- data %>% 
      filter(type %in% c("average", "top", "base", core_type))
    
    print("No depth.")
  }

  ggplot(data_plot, 
         aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, linetype = importance_type, color = scale),
                   height = 0.4, position = position_dodge(width = 0.8))+
    geom_point(position = position_dodge(width = 0.8), size = 2) +  
    scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
    scale_color_manual(values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), # Adjusted manually
                       guide = guide_legend(order = 1)) +
    facet_wrap(~key) +
    theme_minimal() +
    labs(x = "Estimate (scaled)", 
         y = NULL, 
         color = "Scale", 
         pch = "Significance", 
         linetype = "Importance",
         title = paste0(sciname, "\n", target_status, "\n", assemblage)) +
    my_theme
  
  ggsave(filename = paste0(species, "_forest.png"),
         path = file.path(fig.dir, habitat), width = 8, height = 5, dpi = 300, units = "in")
  
  # Create table to export
  data_plot %>% 
    filter(model_id == "Top Model") %>% 
    dplyr::select(term_revised, scale, estimate, std_error, statistic, df, p_value, significance) %>% 
    mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>% 
    mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>% 
    gt() %>%
    tab_header(title = html(paste0("<b><i>", sciname, "</i></b>", "<br>", target_status, "<br>", assemblage))) %>% 
    cols_label(term_revised = "Term",
               scale = "Scale",
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
    tab_options(table.font.size = "small", table.width = pct(100))
}

# Create and save figures -------------------------------------------------------------

## Kelp ----
path <- "/Users/lopazanski/Desktop/output/kelp"

list.files(path = path, pattern = "_results.rds") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% 
  unique() %>% 
  map(., make_forest_plots, path = path, habitat = "kelp")

## Rocky reef ----
path <- "/Users/lopazanski/Desktop/output/rock"

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
