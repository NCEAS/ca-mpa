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

# Set example species and path
species <- "ELAT"
species <- "OELO"
species <- "OPIC"
species <- "OYT"
species <- "SCAR"
species <- "SMIN"
species <- "SMEL"
species <- "SMYS"

path <- "analyses/7habitat/output/kelp/all_regions/consolidated"


# species <- "BLU"
# species <- "CPR"
# species <- "GPR"
# species <- "VER"
# path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions/interaction"

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

# Define species list
sp_list <- list.files(path = "analyses/7habitat/output/kelp/all_regions/consolidated") %>%
  str_remove_all(., "_models.rds|_results.rds") %>% unique()

lapply(sp_list, function(species){
  # Read the data
  data <- readRDS(file.path(path, paste0(species, "_results.rds"))) %>% 
    mutate(scale = case_when(is.na(scale) & model_id == "ST*A" ~ NA,
                             is.na(scale) ~ as.factor(str_extract(term, "\\d+")),
                             T~scale)) %>% 
    mutate(scale = factor(scale, levels = c(25, 50, 100, 250, 500, NA))) %>% 
    mutate(importance_type = case_when((is.na(importance) | importance > 0.5) ~ "Greater than 0.5",
                                       (!is.na(importance) & importance < 0.5) ~ "Less than 0.5"))
  
  data_sd <- data %>% 
    filter(str_detect(model_id, "DSD") | !key == "Full Model") %>% 
    filter(!term == "(Intercept)")
  
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
    labs(x = "Estimate (Scaled)", 
         y = NULL, 
         color = "Scale", 
         pch = "Significance", 
         linetype = "Importance",
         title = paste(species)) +
    my_theme
  
  ggsave(filename = paste0(species, "_forest_depth_sd.png"),
         path = fig.dir, width = 8, height = 5, dpi = 300, units = "in")
  
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
         title = paste(species)) +
    my_theme
  
  ggsave(filename = paste0(species, "_forest_depth_mean.png"),
         path = fig.dir, width = 8, height = 5, dpi = 300, units = "in")
})








# Make one combined species plot?

# # List all files with '_results' in their name
# result_files <- list.files(path, pattern = "_results\\.rds$", full.names = TRUE)
# 
# # Read and combine all the data
# all_data <- result_files %>%
#   map_dfr(readRDS)
# 
# species_plot <- all_data %>% 
#   filter(!term == "(Intercept)") %>% 
#   filter(!key == "Full Model") %>% 
#   filter(is.na(model_id) | model_id == "Model Average") %>% 
#   filter(is.na(importance) | importance > 0.5)
# 
# ggplot(species_plot, 
#        aes(x = estimate, y = term_revised, color = species_code, pch = significance)) +
#   geom_vline(aes(xintercept = 0), linetype = "dashed") +
#   geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), linetype = "solid",
#                  height = 0.2, position = position_dodge(width = 0.8)) +
#   geom_point(position = position_dodge(width = 0.8), size = 2) +  
#   scale_shape_manual(values = c("***" = 16, "**" = 17, "*" = 15, "NA" = NA, "NS" = 3)) +
#  # scale_color_manual(
# #    values = c("#440154", "#3b528b", "#21908d", "#5dc863", "#D7C51B"), # Adjusted manually
# #    guide = guide_legend(order = 1)
# #  ) +
#   theme_minimal() +
#   labs(x = "Estimate (Scaled)", 
#        y = NULL, 
#        color = "Scale", 
#        pch = "Significance") +
#   theme(axis.text.y = element_text(size = 10))
