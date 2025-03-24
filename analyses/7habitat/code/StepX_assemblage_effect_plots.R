# Step X Effects Plots
# Cori Lopazanski
# February 2025 


# Setup ------------------------------------------------------------------------
library(tidyverse)
library(gt)
library(patchwork)
library(performance)
library(effects)
library(grid)


rm(list = ls())
gc()

# Begin ------------------------------------------------------------------------

my_theme <- theme(
  plot.title = element_text(size = 10, face = "bold"),
  plot.subtitle = element_text(size = 8),
  axis.title = element_text(size = 8),
  axis.text.x = element_text(size = 8, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 8),
  legend.title = element_text(size = 8),
  legend.text = element_text(size = 8),
  plot.caption = element_text(size = 8),
  strip.text = element_text(size = 7, face = "bold"),
  panel.background = element_rect(fill = "white", color = NA),  
  plot.background = element_rect(fill = "white", color = NA)
)

path <- "analyses/7habitat/output"
habitat <- "rock_subset"
focal_group <- "targeted"
re_string <- "rmy"
results_file <- paste(habitat, focal_group, re_string, "effects.rds", sep = "_")
data_file <- paste(habitat, focal_group, re_string, "data.rds", sep = "_")


make_effects_plots <- function(focal_group, path, habitat){
  
  print(paste0(habitat, ", ", focal_group))
  
  effects <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects", results_file)) 
  data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", data_file)) 

  effects_list <- effects$models$effects_list_top
  
  effect_plots <- lapply(seq_along(effects_list), function(i) {
    effects_data <- as.data.frame(effects_list[[i]])
    x_var <- colnames(effects_data)[which(colnames(effects_data) != "site_type")[1]]
    const <- min(data_sp$biomass[data_sp$biomass > 0], na.rm = TRUE)
    
    # Reverse scaling if available
    center <- attr(data_sp[[x_var]], "scaled:center")
    scale_ <- attr(data_sp[[x_var]], "scaled:scale")
    
    if (!is.null(center) && !is.null(scale_)) {
      effects_data[[x_var]] <- effects_data[[x_var]] * scale_ + center
    }
    
    x_var_label <- x_var %>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cv", "CV") %>% 
      str_replace("\\d+", paste0(str_extract(., "\\d+"), "m"))
    
    show_legend <- (i == length(effects_list))  # Show legend only on the last plot

    if (sum(str_detect(colnames(effects_data), "site_type")) > 0) {
      ggplot(effects_data, aes(x = !!sym(x_var))) +
       geom_ribbon(aes(ymin = exp(lower) - const, ymax = exp(upper) - const, fill = site_type),
                   alpha = 0.2, show.legend = show_legend, stat = "identity") +
       geom_line(aes(y = exp(fit) - const, color = site_type), show.legend = show_legend, stat = "identity") +
        scale_color_manual(values = c("#7e67f8", "#e5188b")) +
        scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        labs(x = x_var_label,
             y = "Biomass (kg per 100m2)",
             color = NULL, fill = NULL) +
        theme_minimal() +
        theme(legend.position = ifelse(show_legend, "right", "none"),
              axis.title.y = if (i != 1) element_blank() else element_text()) +
        my_theme
      
      
    } else {
      ggplot(effects_data, aes(x = !!sym(x_var))) +
        geom_smooth(aes(y = exp(fit) - const), color = "black", show.legend = FALSE, method = "loess", formula = y ~ x) +
        geom_ribbon(aes(ymin = exp(lower) - const, ymax = exp(upper) - const), alpha = 0.2, show.legend = FALSE) +
        scale_y_continuous(limits = c(0, NA), expand = c(0, 0)) +
        labs(x = x_var_label,
             y = "Biomass (kg per unit effort)")+
        theme_minimal() +
        theme(axis.title.y = if (i != 1) element_blank() else element_text()) +
        my_theme
    }
  })
  
  # Combine all effect plots and export
  wrap_plots(effect_plots, ncol = length(effect_plots)) + 
    plot_annotation(
     # title = paste0(str_to_sentence(focal_group), "\n", str_to_sentence(habitat)),
      theme = theme(plot.title = element_text(size = 10, face = "bold"),
                    plot.subtitle = element_text(size = 8),
                    axis.title = element_text(size = 8),
                    axis.text = element_text(size = 8),
                    legend.title = element_text(size = 8),
                    legend.text = element_text(size = 8),
                    plot.caption = element_text(size = 8),
                    strip.text = element_text(size = 7, face = "bold"),
                    panel.background = element_rect(fill = "white", color = NA),  
                    plot.background = element_rect(fill = "white", color = NA)))
  
  # ggsave(filename = paste(focal_group, snakecase::to_snake_case(habitat), "effects.png", sep = "_"),
  #        path = path, width = 8, height = 3, dpi = 300, units = "in")
  # 
  png(paste0(path, "/", focal_group, "_", snakecase::to_snake_case(habitat), "_residuals.png"), width = 5000, height = 1000, res = 300)
  
  plot(effects_list, 
       confint = list(style = "auto"), 
       partial.residual = list(smooth = TRUE, span = 0.75, lty = "dashed", col="#E69F00", smooth.col="#E69F00", 
                                cex = 0.1, pch = 19, col = ""),  # Reduce cex for less dominant points
       lattice = list(strip = list(cex = 0.8)),
       axes = list(x = list(rotate = 45, cex = 0.8)),
       main = NULL# rows = 2, cols = length(effects_list)
       )  
  
  grid.text(paste(focal_group, "-", habitat), x = 0.02, y = 0.95, just = "left", gp = gpar(fontsize = 10, fontface = "bold"))

  dev.off()
  
}



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
  ~ purrr::walk(focal_groups, \(fg) make_effects_plots(focal_group = fg, path = .y, habitat = .x))
)






# OLD STUFF

# 1. Read the top model and base model
top_models <-  readRDS(file.path("~/ca-mpa/analyses/7habitat/output/results", paste(results_file)))$models

# 2. Read data for fitting models (used to calculate effects)
data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/models", paste(habitat, focal_group, re_string, "models.rds", sep = "_")))$data_sp
#models_df <- readRDS(file.path(path, paste0(focal_group, "_models.rds")))$models_df

# 4. Evaluate diagnostics for the top model
par(mfrow = c(1, 2))
plot(residuals(top_models$top) ~ fitted(top_models$top), main = attr(top_models$top, "name"))
abline(h = 0, col = "red", lty = 2)
lines(lowess(fitted(top_models$top), residuals(top_models$top)), col = "blue", lwd = 2)

qqnorm(residuals(top_models$top), main = paste(attr(top_models$top, "name")), cex.main = 0.8); qqline(residuals(top_models$top))

diag <- plot(check_model(top_models$top, residual_type = "simulated", check = c("ncv", "qq")))
plot(fitted(top_models$top), resid(top_models$top))
plot(top_models$top)

data_sp$fitted <- fitted(top_models$top)
data_sp$residuals <- residuals(top_models$top)

ggplot(data = data_sp) +
  geom_point(aes(x = fitted, y = residuals, color = affiliated_mpa)) + 
  geom_smooth(aes(x = fitted, y = residuals), method = "loess") +
  theme_minimal() +
  labs(color = NULL)

ggplot(data = data_sp) +
  geom_point(aes(x = fitted, y = residuals, color = region4)) + 
  geom_smooth(aes(x = fitted, y = residuals), method = "loess") +
  theme_minimal() +
  labs(color = NULL)

ggplot(data_sp, aes(sample = residuals)) +
  stat_qq() +
  stat_qq_line() +
  #    labs(title = "QQ Plot of Residuals by MPA") +
  theme_minimal()

outliers <- check_outliers(top_models$top)
plot(outliers)
# 5. Calculate and plot the predictor effects 









path <- "analyses/7habitat/output/2way-4region-rmre/kelp-reduced"

make_effects_plots(focal_group = "OPIC", path = path, habitat = "kelp")

list.files(path = path, pattern = "_results.rds") %>%
  str_remove_all("_models.rds|_results.rds|_effects.rds") %>% 
  unique() %>% 
  walk(., make_effects_plots, path = path, habitat = "kelp") %>% 
  walk(., make_forest_plots, path = path, habitat = "kelp")

# Test
ggplot(data_sp) + 
  geom_point(aes(x = age_at_survey, y = kelp_annual_100, color = site_type)) +
  geom_smooth(aes(x = age_at_survey, y = kelp_annual_100, color = site_type)) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  theme_minimal() 

# models <- readRDS(file.path(path, paste0(focal_group, "_results.rds")))@
# data <- readRDS(file.path(path, paste0(focal_group, "_models.rds")))
# data_sp <- data$data_sp
# const <- min(data_sp$kg_per_m2[data_sp$kg_per_m2 > 0], na.rm = TRUE)

check_outliers(data_sp$kelp_annual_100)

intx_types <- data_plot %>%
  filter(key == "Refit Top Model (Predictor Importance > 0.5)" | model_id == "Top Model") %>%
  mutate(term = str_remove_all(term, ":site_type|site_type:")) %>%
  filter(!term == "site_type") %>%
  pull(term) %>% unique()

predictors <- data.frame(term = names(fixef(top_models$top))) %>% 
  mutate(term = str_remove_all(term, ":site_type|site_type:")) %>%
  filter(!term %in% c("(Intercept)", "site_type"))

pred_effects <- predictorEffects(top_models$top, partial.residuals = T)
plot(pred_effects[setdiff(names(pred_effects), "site_type")])

plot(pred_effects[setdiff(names(pred_effects), "site_type")],
     lattice = list(key.args = list(cex.title = 0)),
     residuals.pch = 19, residuals.cex = 0.3,
     axes = list(grid = TRUE, y = list(transform = function(x) exp(x) - const, lab = "Biomass (kg/m2)")),
     lines = list(multiline = TRUE),
     confint = list(style = "auto"),
     main = NULL, rows = 1)

plot(pred_effects[setdiff(names(pred_effects), "site_type")],
     id = list(n = 5), residuals.pch = 19, residuals.cex = 0.2, main = NULL, rows = 1)

plot(allEffects(top_models$top, ~ hard_bottom_500 + age_at_survey, partial.residuals = TRUE), 
     id = list(n = 1), residuals.pch = 19, residuals.cex = 0.2)
