# Step X Effects Plots
# Cori Lopazanski
# February 2025 


# Setup ------------------------------------------------------------------------
library(tidyverse)
library(gt)
library(patchwork)
library(performance)
library(effects)

rm(list = ls())
gc()

fig.dir <- "analyses/7habitat/figures"

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

species <- "PCLA"
habitat <- "kelp"
path <- "analyses/7habitat/output/2way/kelp"

make_effects_plots <- function(species, path, habitat){
  print(paste("Species:", species))
  
  # 1. Read the top model and base model
  top_models <- readRDS(file.path(path, paste0(species, "_top.rds")))
  
  # 2. Read data for fitting models (used to calculate effects)
  data_models <- readRDS(file.path(path, paste0(species, "_models.rds")))
  data_sp <- data_models$data_sp
  
  # 3. Extract species information
  sciname <- unique(data_sp$sciname)
  assemblage <- unique(data_sp$assemblage_new)
  target_status <- unique(data_sp$target_status)
  const <- min(data_sp$kg_per_m2[data_sp$kg_per_m2 > 0], na.rm = TRUE)
  
  # 4. Evaluate diagnostics for the top model
  # diag <- plot(check_model(top_models$top, residual_type = "simulated", check = c("ncv", "qq", "normality", "vif")))
  # ggsave(diag, filename = paste0(species, "_diagnostic.png"),
  #        path = file.path(fig.dir, habitat), width = 10, height = 6, dpi = 300, units = "in")
  # print("  Diagnostic complete.")
  
  # 5. Calculate and plot the predictor effects 
  assign("data", data_models, envir = environment())
  effects_list <- allEffects(top_models$top, xlevels = 50)
  
  effect_plots <- lapply(seq_along(effects_list), function(i) {
    effects_data <- as.data.frame(effects_list[[i]])
    x_var <- colnames(effects_data)[which(colnames(effects_data) != "site_type")[1]]
    
    show_legend <- (i == length(effects_list))  # Show legend only on the last plot
    
    if (sum(str_detect(colnames(effects_data), "site_type")) > 0) {
      ggplot(effects_data, aes(x = !!sym(x_var))) +
        geom_smooth(aes(y = exp(fit) - const, color = site_type), show.legend = show_legend, method = "loess", formula = y ~ x) +
        geom_ribbon(aes(ymin = exp(lower) - const, ymax = exp(upper) - const, fill = site_type), alpha = 0.2, show.legend = show_legend) +
        scale_color_manual(values = c("#7e67f8", "#e5188b")) +
        scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
        labs(x = x_var, 
             y = "Biomass (kg per m2)",
             color = NULL, fill = NULL) +
        theme_minimal() +
        theme(legend.position = ifelse(show_legend, "right", "none")) +
        my_theme
    } else {
      ggplot(effects_data, aes(x = !!sym(x_var))) +
        geom_smooth(aes(y = exp(fit) - const), color = "black", show.legend = FALSE, method = "loess", formula = y ~ x) +
        geom_ribbon(aes(ymin = exp(lower) - const, ymax = exp(upper) - const), alpha = 0.2, show.legend = FALSE) +
        labs(x = x_var, 
             y = "Biomass (kg per m2)") +
        theme_minimal() +
        my_theme
    }
  })
  
  # Combine all effect plots and export
  wrap_plots(effect_plots, ncol = length(effect_plots)) + 
    plot_annotation(
      title = paste0(sciname, "\n", target_status, "\n", assemblage),
      theme = theme(plot.title = element_text(size = 10))  # Adjust size as needed
    )
  
  ggsave(filename = paste0(species, "_effects.png"),
         path = file.path(fig.dir, habitat), width = 10, height = 3, dpi = 300, units = "in")
  
  print("  Effects complete.")
  
}

path <- "analyses/7habitat/output/2way/kelp"

make_effects_plots(species = "SMIN", path = path, habitat = "kelp")

list.files(path = path, pattern = "_results.rds") %>%
  str_remove_all("_models.rds|_results.rds|_top.rds") %>% 
  unique() %>% 
  walk(., make_effects_plots, path = path, habitat = "kelp")

# Test out other predictor effects plots


# models <- readRDS(file.path(path, paste0(species, "_top.rds")))
# data <- readRDS(file.path(path, paste0(species, "_models.rds"))) 
# data_sp <- data$data_sp
# const <- min(data_sp$kg_per_m2[data_sp$kg_per_m2 > 0], na.rm = TRUE)
# intx_types <- data_plot %>% 
#   filter(key == "Refit Top Model (Predictor Importance > 0.5)" | model_id == "Top Model") %>% 
#   mutate(term = str_remove_all(term, ":site_type|site_type:")) %>% 
#   filter(!term == "site_type") %>% 
#   pull(term) %>% unique()
# 
# pred_effects <- predictorEffects(models$top, as.formula(paste("~", paste(intx_types, collapse = " + "))), partial.residuals = T)
# 
# plot(pred_effects,
#      lattice = list(key.args = list(cex.title = 0)),
#      axes = list(grid = TRUE, y = list(transform = function(x) exp(x) - const, lab = "Biomass (kg/m2)")),
#      lines = list(multiline = TRUE), 
#      confint = list(style = "auto"), 
#      main = NULL, rows = 1, cols = length(intx_types))
# 
# plot(pred_effects,
#      lattice = list(key.args = list(cex.title = 0)),
#      axes = list(grid = TRUE),
#      confint = list(style = "auto"), 
#      main = FALSE)




