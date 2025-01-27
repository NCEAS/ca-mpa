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


species <- "VER"
habitat <- "rock"
path <- "/Users/lopazanski/Desktop/output/rock/all_regions"


make_forest_plots <- function(species, path, habitat){
  # Read the data
  data <- readRDS(file.path(path, paste0(species, "_results.rds"))) %>% 
    mutate(scale = case_when(is.na(scale) & model_id == "ST*A" ~ NA,
                             is.na(scale) ~ as.factor(str_extract(term, "\\d+")),
                             T~scale)) %>% 
    mutate(scale = factor(scale, levels = c(25, 50, 100, 250, 500, NA))) %>% 
    mutate(importance_type = case_when((is.na(importance) | importance > 0.5) ~ "Greater than 0.5",
                                       (!is.na(importance) & importance < 0.5) ~ "Less than 0.5")) %>% 
    filter(!term == "(Intercept)") %>% 
    mutate(type = case_when(model_id == "Model Average" ~ "average", T~type))
  
  # 1. Plot 3-way interaction models
  data_sd <- data %>% 
    filter(type != "core_2way") %>% # Drop the 2-way full models
    filter(!(type == "core_3way" & depth_type %in% c("depth_mean", "depth_cv")))

  data_mean <- data %>% 
    filter(type != "core_2way") %>% # Drop the 2-way full models
    filter(!(type == "core_3way" & depth_type %in% c("depth_sd", "depth_cv")))
  
  data_cv <- data %>% 
    filter(type != "core_2way") %>% # Drop the 2-way full models
    filter(!(type == "core_3way" & depth_type %in% c("depth_mean", "depth_sd")))
  
  ggplot(data_cv %>% filter(!type == "top"),
         aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, linetype = importance_type, color = scale),
                   height = 0.4, position = position_dodge(width = 0.8))+
    geom_point(position = position_dodge(width = 0.8), size = 2) +  
    scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
    scale_color_manual(
      values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), # Adjusted manually
      guide = guide_legend(order = 1)
    ) +
    facet_wrap(~key) +
    theme_minimal() +
    labs(x = "Estimate (scaled)", 
         y = NULL, 
         color = "Scale", 
         pch = "Significance", 
         linetype = "Importance",
         title = paste(species)) +
    my_theme
  
  ggsave(filename = paste0(species, "_forest_depth_cv.png"),
         path = file.path(fig.dir, habitat), width = 8, height = 5, dpi = 300, units = "in")
  
  ggplot(data_mean %>% filter(!type == "top"),
         aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, linetype = importance_type, color = scale),
                   height = 0.4, position = position_dodge(width = 0.8))+
    geom_point(position = position_dodge(width = 0.8), size = 2) +  
    scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
    scale_color_manual(
      values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), # Adjusted manually
      guide = guide_legend(order = 1)
    ) +
    facet_wrap(~key) +
    theme_minimal() +
    labs(x = "Estimate (Scaled)", 
         y = NULL, 
         color = "Scale", 
         pch = "Significance", 
         linetype = "Importance",
         title = paste(species)) +
    my_theme
  
  ggsave(filename = paste0(species, "_forest_depth_mean.png"),
         path = file.path(fig.dir, habitat), width = 8, height = 5, dpi = 300, units = "in")
  
  ggplot(data_sd %>% filter(!type == "top"),
         aes(x = estimate, y = term_revised, color = scale, pch = significance)) +
    geom_vline(aes(xintercept = 0), linetype = "dashed") +
    geom_errorbarh(aes(xmin = conf_low, xmax = conf_high, linetype = importance_type, color = scale),
                   height = 0.4, position = position_dodge(width = 0.8))+
    geom_point(position = position_dodge(width = 0.8), size = 2) +  
    scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
    scale_color_manual(
      values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), # Adjusted manually
      guide = guide_legend(order = 1)
    ) +
    facet_wrap(~key) +
    theme_minimal() +
    labs(x = "Estimate (Scaled)", 
         y = NULL, 
         color = "Scale", 
         pch = "Significance", 
         linetype = "Importance",
         title = paste(species)) +
    my_theme
  
  ggsave(filename = paste0(species, "_forest_depth_sd.png"),
         path = file.path(fig.dir, habitat), width = 8, height = 5, dpi = 300, units = "in")

}

# Create and save figures -------------------------------------------------------------

## Kelp ----
path <- "/Users/lopazanski/Desktop/output/kelp/south_central"

kelp_list <- list.files(path = path) %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

map(kelp_list, make_forest_plots, path = path, habitat = "kelp")

## Rocky reef ----
path <- "/Users/lopazanski/Desktop/output/rock/south"

list.files(path = path) %>%
  str_remove_all(., "_models.rds|_results.rds") %>% 
  unique() %>% 
  map(., make_forest_plots, path = path, habitat = "rock")


## Surf ---
surf_list <- list.files(path = "analyses/7habitat/output/surf/central_south") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

map(surf_list, make_forest_plots, path = "analyses/7habitat/output/surf/central_south", habitat = "surf")

## Deep ---
deep_list <- list.files(path = "analyses/7habitat/output/deep/all_regions") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

map(deep_list, make_forest_plots, path = "analyses/7habitat/output/deep/all_regions", habitat = "deep")


