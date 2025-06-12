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
library(ggeffects)


rm(list = ls())
gc()

# Begin ------------------------------------------------------------------------

fig.dir <- "~/ca-mpa/analyses/7habitat/figures/3way-figures"

my_theme <- theme_minimal(base_family = "Arial") + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, unit='cm'),
        plot.caption = element_text(size = 8),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 8, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA),
        legend.position = "top",
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

mpa_colors <- c("Reference" = "#6d55aa", "MPA" = "#c42119")

list2env(list(habitat = "surf_filtered", 
              focal_group = "targeted",
              re_string = "m"), envir = .GlobalEnv)


auto_xlevels <- function(focal_model, data_sp,
                         probs    = c(0, 0.25, 0.50, 0.75, 0.95),
                         digits   = 4) {
  terms <- attr(terms(focal_model), "term.labels")
  vars  <- unique(unlist(strsplit(terms, ":")))
  num   <- vars[sapply(data_sp[vars], is.numeric)]
  
  xlvls <- lapply(num, function(v) {
    qs <- round(quantile(data_sp[[v]], probs = probs, na.rm = TRUE), digits)
    qs[!duplicated(qs)]
  
    grd <- round(seq(min(data_sp[[v]], na.rm = TRUE),
                     max(data_sp[[v]], na.rm = TRUE),
                     length.out = 50), digits)
    grd <- setdiff(grd, qs)
    all <- c(qs, grd)
    
    all
  })
  
  names(xlvls) <- num
  xlvls
}


make_effects_plots <- function(habitat, focal_group, re_string){
  
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects", "3way", paste(habitat, focal_group, re_string, "effects.rds", sep = "_")))
  data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "3way", paste(habitat, focal_group, re_string, "data.rds", sep = "_")))
  
  focal_model <- results$models$top
  xlevels_auto  <- auto_xlevels(focal_model, data_sp)
  
  assign("data_sp", data_sp, envir = .GlobalEnv)
  effects_list  <- allEffects(focal_model, partial.residuals = F, xlevels = xlevels_auto)
  effects_list <- effects_list[names(effects_list) != "site_type:age_at_survey"]  
  const <- min(data_sp$biomass[data_sp$biomass > 0], na.rm = TRUE)
  
  effect_plots <- list() 
  
  for (i in seq_along(effects_list)) {
    # Extract the effects dataframe and transform
    effects_data <- as.data.frame(effects_list[[i]]) %>% 
      mutate(fit = exp(fit) - const,
             lower = exp(lower) - const,
             upper = exp(upper) - const) %>% distinct()
    
    # Pull habitat variable and its associated percentiles
    hab_var <- setdiff(names(effects_data), c("site_type", "age_at_survey"))[1]
    
    hab_list <- enframe(xlevels_auto[[hab_var]], name = "pct", value = hab_var) %>%
      mutate(pct = fct_reorder(pct, .data[[hab_var]]))
    
    effects_data <- left_join(effects_data, hab_list)
    
    # Reverse scaling for the habitat variable
    if (sum(str_detect(colnames(effects_data), "kelp")) == 0){
      var_center <- attr(data_sp[[hab_var]], "scaled:center")
      var_scale <- attr(data_sp[[hab_var]], "scaled:scale")
      effects_data[[hab_var]] <- effects_data[[hab_var]] * var_scale + var_center
    }
  
    # Reverse scaling for age
    age_center <- attr(data_sp[["age_at_survey"]], "scaled:center")
    age_scale  <- attr(data_sp[["age_at_survey"]], "scaled:scale")
    
    if (sum(str_detect(colnames(effects_data), "age")) == 1){
      effects_data[["age_at_survey"]] <- round(effects_data[["age_at_survey"]] * age_scale + age_center, 0)
    }
    
    
    x_var_label <- hab_var %>% 
      str_replace_all("_", " ") %>% 
      str_to_sentence() %>% 
      str_replace("cv", "CV") %>% 
      str_replace("\\d+", paste0(str_extract(., "\\d+"), "m")) %>% 
      str_replace("Aquatic vegetation bed", "Max biotic extent")
    
    y_var_label <- case_when(habitat %in% c("rock", "rock_filtered") ~ "Biomass (kg per unit effort)",
                             habitat %in% c("surf", "surf_filtered") ~ "Biomass (kg per haul)",
                             T~NA)
      
    key <- paste0(as.integer("site_type" %in% names(effects_data)), as.integer("age_at_survey" %in% names(effects_data)))
    
    effect_plots[[paste0(habitat, "_", names(effects_list)[i])]] <- 
      switch(key,
             "00" = {
               ggplot(effects_data, aes(x = !!sym(hab_var), y = fit)) +
                 geom_smooth(color = "black", method = "loess", formula = y ~ x, se = FALSE) +
                 geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
                 scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
                 labs(x = x_var_label, 
                      y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label) +
                 my_theme
             },
             "10" = {
               ggplot(effects_data, aes(x = !!sym(hab_var), y = fit, fill = site_type)) +
                 geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
                 geom_smooth(aes(color = site_type), method = "loess", se = FALSE) +
                 scale_color_manual(values = mpa_colors) +
                 scale_fill_manual(values = mpa_colors) +
                 coord_cartesian(ylim = c(0, NA), expand = F) +
                 labs(x = x_var_label, 
                      y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label, 
                      color = NULL, fill = NULL) +
                 my_theme
             },
             "11" = {
               ggplot(effects_data %>% filter(str_detect(pct, "25|50|75|95")),
                      aes(x = age_at_survey, y = fit, fill = site_type)) +
                 geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
                 geom_smooth(aes(color = site_type), method = "loess", se = FALSE) +
                 facet_wrap(~pct, nrow = 1) +
                 scale_color_manual(values = mpa_colors) +
                 scale_fill_manual(values = mpa_colors) +
                 scale_x_continuous(limits = c(0, 20), expand = c(0,0)) +
                 scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
                 labs(x = "MPA Age (years)", 
                      y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label,
                      color = NULL, fill = NULL) +
                 my_theme
             },
             stop("Unknown key")
      )
    
    if (key == "11"){
      p2 <- ggplot(data = effects_data %>% filter(age_at_survey %in% c(0, 5, 10, 15)), 
                   aes(x = !!sym(hab_var), y = fit, fill = site_type)) +
        geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2) +
        geom_smooth(aes(color = site_type), method = "loess", se = F) +
        facet_wrap(~age_at_survey, nrow = 1) +
        scale_color_manual(values = mpa_colors) +
        scale_fill_manual(values = mpa_colors) +
        scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
        labs(x = x_var_label,
             y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label,
             color = NULL, fill = NULL) +
        my_theme
      
      effect_plots[[paste0(habitat, "_", names(effects_list)[i], "_v2")]] <- p2
    }
  }
  
  # Get predicted biomass over age for both MPA and reference, averaged over habitat
  eff_age <- ggpredict(focal_model, terms = c("age_at_survey [n = 50]", "site_type"))
  
  # Plot the average predicted biomass trajectories
  age <- ggplot(eff_age, aes(x = x * age_scale + age_center, y = exp(predicted) - const, color = group)) +
    geom_line(size = 1.2) +
    geom_ribbon(aes(ymin = exp(conf.low) - const, ymax = exp(conf.high) - const, fill = group), alpha = 0.2, color = NA) +
    scale_color_manual(values = mpa_colors) +
    scale_fill_manual(values = mpa_colors) +
    coord_cartesian(xlim = if (habitat %in% c("surf", "surf_filtered")) c(NA, NA) else c(0, 20), 
                               ylim = c(0, NA), expand = F) + 
    labs(
      x = "MPA Age (years)",
      y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label,
      color =  NULL, fill = NULL) + my_theme

  effect_plots[[paste0(habitat, "_overall")]] <- age
  
  saveRDS(effect_plots,
          file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", paste(habitat, focal_group, re_string, "effects_plots.rds", sep = "_")))
  
  return(effect_plots)
  
}


# This will both save the plots (via functions above) and keep them in the environment
kelp <- make_effects_plots("kelp_filtered", "targeted", "my")
rock <- make_effects_plots("rock_filtered", "targeted", "rmy")
surf <- make_effects_plots("surf_filtered", "targeted", "m")


# Combine all three into one plot with panels
#rock <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", "rock_filtered_targeted_rmsy_effects_plots.rds"))
#kelp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", "kelp_filtered_targeted_msy_effects_plots.rds"))
#surf <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", "surf_filtered_targeted_m_effects_plots.rds"))

rm(data_sp, focal_group, habitat, re_string)

all_plots <- c(rock, kelp, surf)
names(all_plots)  

age_plots <- all_plots[str_detect(names(all_plots), "_overall")]
wrap_plots(age_plots, ncol = 1)

titles <- c("Shallow reef", "Kelp forest", "Surf zone")

age_plots_named <- Map(function(p, title) {
    p + ggtitle(title) +
    theme(axis.text = element_text(size = 8),
          axis.text.x = element_text(size = 8, angle = 0, hjust = 0.5),
          axis.text.y =  element_text(size = 8, angle = 0, hjust = 0.5),
          axis.title = element_text(size = 10),
          legend.text = element_text(size = 8))
    
}, age_plots, titles)

(guide_area() / wrap_plots(age_plots_named, axis_titles = "collect_x", ncol = 1)) + 
  plot_layout(guides = "collect", heights = unit(c(0.4, 1), c("cm", "null"))) + 
  theme(legend.position = "top")


ggsave("fig1-age-effects.png", 
       width = 3, height = 6, dpi = 600, units = "in")


habitat_plots <- all_plots[!str_detect(names(all_plots), "overall")]

names(habitat_plots)
wrap_plots(habitat_plots)

rock_plots <- habitat_plots[str_detect(names(habitat_plots), "rock")]
kelp_plots <- habitat_plots[str_detect(names(habitat_plots), "kelp")]
surf_plots <- habitat_plots[str_detect(names(habitat_plots), "surf")]
rock_plots[[1]] <- rock_plots[[1]] + ggtitle("Shallow reef")
kelp_plots[[1]] <- kelp_plots[[1]] + ggtitle("Kelp forest")
surf_plots[[1]] <- surf_plots[[1]] + ggtitle("Surf zone")

# Remove y-axis labels for all but first in each group
rock_plots[-1] <- lapply(rock_plots[-1], \(p) p + theme(axis.title.y = element_blank()))
kelp_plots[-1] <- lapply(kelp_plots[-1], \(p) p + theme(axis.title.y = element_blank()))
surf_plots[-1] <- lapply(surf_plots[-1], \(p) p + theme(axis.title.y = element_blank()))


final_plot <- wrap_plots(
  wrap_plots(rock_plots, ncol = length(rock_plots)),
  wrap_plots(kelp_plots, ncol = length(kelp_plots)),
  wrap_plots(surf_plots, ncol = length(surf_plots)),
  ncol = 1
) + plot_layout(guides = "collect") & 
  theme(legend.position = "bottom")

final_plot

ggsave("fig2-habitat-effects.png", 
       width = 8, height = 8, dpi = 600, units = "in")

# layout <- "
# ABC##
# DEGHI
# J#K#L
# "
# 
# wrap_plots(
#   A = habitat_plots[["rock_depth_mean_250"]],
#   B = habitat_plots[["rock_depth_cv_500"]],
#   C = habitat_plots[["rock_hard_bottom_100"]],
#   D = habitat_plots[["kelp_depth_mean_250"]],
#   E = habitat_plots[["kelp_depth_cv_50"]],
#   G = habitat_plots[["kelp_hard_bottom_25"]],
#   H = habitat_plots[["kelp_kelp_annual_100"]],
#   I = habitat_plots[["kelp_aquatic_vegetation_bed_500"]],
#   J = habitat_plots[["surf_depth_mean_50"]],
#   K = habitat_plots[["surf_soft_bottom_50"]],
#   L = habitat_plots[["surf_aquatic_vegetation_bed_250"]],
#   `#` = plot_spacer()
# ) + plot_layout(design = layout)

