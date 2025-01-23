# Build forest plots with full models
# Cori Lopazanski
# lopazanski@bren.ucsb.edu
# Dec 2024

# Setup ------------------------------------------------------------------------
library(tidyverse)

rm(list = ls())
gc()

fig.dir <- "analses/7habitat/figures/forest"

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

kelp_list <- list.files(path = "analyses/7habitat/output/kelp/all_regions/log_c_unscaled") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

pred_kelp_int <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_interactions.Rds"))

species <- "ELAT"
path <- "analyses/7habitat/output/kelp/all_regions/log_c_scaled"
habitat <- "kelp"

make_forest_plots <- function(species, path, habitat){
  # Read the data
  data <- readRDS(file.path(path, paste0(species, "_results.rds"))) %>% 
    mutate(scale = case_when(is.na(scale) & model_id == "ST*A" ~ NA,
                             is.na(scale) ~ as.factor(str_extract(term, "\\d+")),
                             T~scale)) %>% 
    mutate(scale = factor(scale, levels = c(25, 50, 100, 250, 500, NA))) %>% 
    mutate(importance_type = case_when((is.na(importance) | importance > 0.5) ~ "Greater than 0.5",
                                       (!is.na(importance) & importance < 0.5) ~ "Less than 0.5")) %>% 
    left_join(pred_kelp_int) %>% 
    filter(!(key == "Full Model" & is.na(type) & is.na(type3)))
  
  data_sd <- data %>% 
    # Plot the 3-way models
    filter(!is.na(type3) | !key == "Full Model") %>% 
    filter(str_detect(model_id, "DSD") | !key == "Full Model") %>% 
    filter(!term == "(Intercept)") %>% 
    mutate(term_revised = str_remove_all(term, "MPA") %>% 
             str_remove_all("_annual_250|_annual_500|_annual_100|_annual_50|_annual_25") %>% 
             str_remove_all("_250|_500|_100|_25|_50") %>% 
             if_else(str_detect(., ":age_at_survey$"), str_replace(., "^(.*):age_at_survey$", "age_at_survey:\\1"), .) %>% 
             if_else(str_detect(., ":site_type$"), str_replace(., "^(.*):site_type$", "site_type:\\1"), .) %>% 
             if_else(str_detect(., ":site_type:"), str_replace(., "^(.*?):(site_type):(.*?)$", "\\2:\\3:\\1"), .) %>% 
             factor(levels = c("(Intercept)",
                               "site_type:age_at_survey:depth_mean",
                               "site_type:age_at_survey:depth_sd",
                               "site_type:age_at_survey:kelp",
                               "site_type:age_at_survey:hard_bottom",
                               "age_at_survey:depth_mean",
                               "age_at_survey:depth_sd",
                               "age_at_survey:hard_bottom",
                               "age_at_survey:kelp",
                               "site_type:depth_mean",
                               "site_type:depth_sd",
                               "site_type:soft_bottom",
                               "site_type:hard_bottom",
                               "site_type:kelp",
                               "site_type:age_at_survey",
                               "site_type",
                               "age_at_survey",
                               "depth_mean",
                               "depth_sd",
                               "soft_bottom",
                               "hard_bottom",
                               "kelp")))
  
  data_mean <- data %>% filter(str_detect(model_id, "DM") | !key == "Full Model") %>% 
    filter(!term == "(Intercept)")
  
  ggplot(data_sd,
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
  
  ggsave(filename = paste0(species, "_forest_depth_sd.png"),
         path = file.path(fig.dir, habitat), width = 8, height = 5, dpi = 300, units = "in")
  
  ggplot(data_mean %>% filter(!term == "(Intercept)"), 
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
}

# Create and save figures -------------------------------------------------------------

## Kelp ----

kelp_list <- list.files(path = "analyses/7habitat/output/kelp/all_regions/log_c_scaled") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

map(kelp_list, make_forest_plots, path = "analyses/7habitat/output/kelp/all_regions/log_c_scaled", habitat = "kelp")

## Rocky reef ----
rock_list <- list.files(path = "analyses/7habitat/output/rock/all_regions/log_c_scaled") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

map(rock_list, make_forest_plots, path = "analyses/7habitat/output/rock/all_regions/log_c_scaled", habitat = "rock")

## Surf ---
surf_list <- list.files(path = "analyses/7habitat/output/surf/central_south") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

map(surf_list, make_forest_plots, path = "analyses/7habitat/output/surf/central_south", habitat = "surf")

## Deep ---
deep_list <- list.files(path = "analyses/7habitat/output/deep/all_regions") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

map(deep_list, make_forest_plots, path = "analyses/7habitat/output/deep/all_regions", habitat = "deep")


