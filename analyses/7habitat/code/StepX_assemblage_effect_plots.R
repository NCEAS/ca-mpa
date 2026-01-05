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

fig.dir <- "~/ca-mpa/analyses/7habitat/figures"

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

list2env(list(habitat = "surf", 
              focal_group = "targeted",
              re_string = "rm"), envir = .GlobalEnv)


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
  
  results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects", paste(habitat, focal_group, re_string, "effects.rds", sep = "_")))
  data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", paste(habitat, focal_group, re_string, "data.rds", sep = "_")))
  
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
    
    effects_data <- droplevels(effects_data)
    
    # Pull habitat variable and its associated percentiles
    hab_var <- setdiff(names(effects_data), c("site_type", "age_at_survey"))[1]
    
    hab_list <- enframe(xlevels_auto[[hab_var]], name = "pct", value = hab_var) %>%
      mutate(pct = fct_reorder(pct, .data[[hab_var]]))
    
    effects_data <- left_join(effects_data, hab_list)
    
    # Reverse scaling for the habitat variable
    var_center <- attr(data_sp[[hab_var]], "scaled:center")
    var_scale <- attr(data_sp[[hab_var]], "scaled:scale")
    
    
    if (sum(str_detect(colnames(effects_data), "kelp")) == 0){
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
                 geom_smooth(aes(color = site_type), linewidth = 1.2, method = "loess", se = FALSE) +
                 scale_color_manual(values = mpa_colors) +
                 scale_fill_manual(values = mpa_colors) +
                 coord_cartesian(ylim = c(0, NA), expand = F) +
                 labs(x = x_var_label, 
                      y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label, 
                      color = NULL, fill = NULL) +
                 my_theme
             },
             "11" = {
               ggplot(effects_data %>%
                        filter(str_detect(pct, "25|50|75|95")) %>%
                        group_by(age_at_survey, site_type, pct) %>%
                        summarise(fit   = mean(fit),
                                  lower = mean(lower),
                                  upper = mean(upper),
                                  .groups = "drop"),
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
        geom_smooth(aes(color = site_type), method = "loess", se = F, linewidth = 1.2) +
        facet_wrap(~age_at_survey, nrow = 1) +
        scale_color_manual(values = mpa_colors) +
        scale_fill_manual(values = mpa_colors) +
        scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
        labs(x = x_var_label,
             y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label,
             color = NULL, fill = NULL) +
        my_theme
      
      effect_plots[[paste0(habitat, "_", names(effects_list)[i], "_v2")]] <- p2
      
      eff_hab <- ggpredict(focal_model, terms = c(paste(hab_var, "[n = 100]"), "site_type")) %>% 
        as.data.frame() %>% 
        filter(between(x, min(data_sp[hab_var]), max(data_sp[hab_var]))) %>% 
        mutate(x = x*var_scale+var_center) %>% 
        rename(site_type = group) 
        
      p3 <- ggplot(eff_hab, 
                   aes(x = x, y = exp(predicted) - const, fill = site_type)) +
        geom_ribbon(aes(ymin = exp(conf.low) - const, ymax = exp(conf.high) - const), alpha = 0.2) +
        geom_smooth(aes(color = site_type), se = F, linewidth = 1.2) +
        scale_color_manual(values = mpa_colors) +
        scale_fill_manual(values = mpa_colors) +
        coord_cartesian(ylim = c(0, NA), expand = F) +
        labs(
          x = x_var_label,
          y = if (habitat %in% c("kelp", "kelp_filtered")) expression("Biomass (kg per 100 m"^2*")") else y_var_label,
          color =  NULL, fill = NULL) + 
        my_theme
      
      effect_plots[[paste0(habitat, "_", names(effects_list)[i], "_habitat")]] <- p3
    }
  }
  
  # Get predicted biomass over age for both MPA and reference, averaged over habitat
  eff_age <- ggpredict(focal_model, terms = c("age_at_survey [n = 50]", "site_type"))
  
  # Plot the average predicted biomass trajectories
  age <- ggplot(eff_age, aes(x = x * age_scale + age_center, y = exp(predicted) - const, color = group)) +
    geom_line(linewidth = 1.2) +
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
          file.path("~/ca-mpa/analyses/7habitat/output/effects", paste(habitat, focal_group, re_string, "effects_plots.rds", sep = "_")))
  
  return(effect_plots)
  
}

(guide_area() / wrap_plots(effect_plots[1:length(effect_plots)], axis_titles = "collect_x", ncol = 1)) + 
  plot_layout(guides = "collect", heights = unit(c(0.4, 1), c("cm", "null"))) + 
  theme(legend.position = "top")


# This will both save the plots (via functions above) and keep them in the environment
kelp <- make_effects_plots("kelp_filtered", "targeted", "my")
rock <- make_effects_plots("rock_filtered", "targeted", "rmy")
surf <- make_effects_plots("surf", "targeted", "m")


# Combine all three into one plot with panels
rock <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", "rock_filtered_targeted_rmsy_effects_plots.rds"))
kelp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", "kelp_filtered_targeted_msy_effects_plots.rds"))
surf <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects/3way", "surf_filtered_targeted_m_effects_plots.rds"))

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
          axis.title.x = element_text(size = 10),
          axis.title.y = element_text(size = 10, hjust = 0.5, vjust = 0),
          legend.text = element_text(size = 8))
    
}, age_plots, titles)

(guide_area() / wrap_plots(age_plots_named, axis_titles = "collect_x", ncol = 1)) + 
  plot_layout(guides = "collect", heights = unit(c(0.4, 1), c("cm", "null"))) + 
  theme(legend.position = "top")


ggsave(file.path(fig.dir, "fig3-age-effects.png"), 
       width = 3, height = 6, dpi = 600, units = "in")



habitat_plots <- all_plots[!str_detect(names(all_plots), "overall|v2|v3") & (str_count(names(all_plots), ":") == 1 | str_detect(names(all_plots), "habitat"))]

names(habitat_plots)
wrap_plots(habitat_plots)

rock_plots <- habitat_plots[str_detect(names(habitat_plots), "rock")]
kelp_plots <- habitat_plots[str_detect(names(habitat_plots), "kelp")]
surf_plots <- habitat_plots[str_detect(names(habitat_plots), "surf")]

rock_plots[[1]] <- rock_plots[[1]] + ggtitle("Shallow reef") + 
  labs(x = bquote(.(gsub(" [0-9]+m$", "", rock_plots[[1]]$labels$x)) ~ "("*m^2*")")) + 
  annotate("text", x = -Inf, y = Inf, label = "**", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 4)+ 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 0))

rock_plots[[2]] <- rock_plots[[2]] + 
  labs(x = "Depth variability (CV)",
       y = NULL) + 
  annotate("text", x = -Inf, y = Inf, label = "**", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 4)+ 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 0))

kelp_plots[[1]] <- kelp_plots[[1]] + ggtitle("Kelp forest") + 
  labs(x = "Kelp (± SD from annual mean)") +
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 0,  margin = margin(0,0,0,0)))

kelp_plots[[2]] <- kelp_plots[[2]] +
  labs(x = "Depth variability (CV)") + 
  annotate("text", x = -Inf, y = Inf, label = "**", hjust = -0.5, vjust = 1.5, fontface = "bold", size = 4)+ 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 0, margin = margin(0,0,0,0)))

surf_plots[[1]] <- surf_plots[[1]] + ggtitle("Surf zone") + 
  labs(x = bquote(.(gsub(" [0-9]+m$", "", surf_plots[[1]]$labels$x)) ~ "("*m^2*")")) + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 0))

surf_plots[[2]] <- surf_plots[[2]] +
  labs(x = "Depth variability (CV)") + 
  theme(axis.title.x = element_text(size = 10),
        axis.title.y = element_text(size = 10, vjust = 0))


# Remove y-axis labels for all but first in each group
#rock_plots[-1] <- lapply(rock_plots[-1], \(p) p + theme(axis.title.y = element_blank()))
#kelp_plots[-1] <- lapply(kelp_plots[-1], \(p) p + theme(axis.title.y = element_blank()))
#surf_plots[-1] <- lapply(surf_plots[-1], \(p) p + theme(axis.title.y = element_blank()))


habitat_plots <- c(rock_plots, kelp_plots, surf_plots)

(guide_area() / wrap_plots(habitat_plots, axis_titles = "collect_y", ncol = 2)) + 
  plot_layout(guides = "collect", heights = unit(c(0.4, 1), c("cm", "null"))) + 
  theme(legend.position = "top")

ggsave(file.path(fig.dir, "fig4-habitat-effects.png"), 
       width = 6, height = 7, dpi = 600, units = "in")



three_plots <- all_plots[str_count(names(all_plots), ":") == 2 & !str_detect(names(all_plots), "_v2|habitat")]
names(three_plots)

three_plots[[1]] <- three_plots[[1]] + ggtitle("Shallow reef: hard bottom") + theme(axis.title.x = element_text(size = 10),
                                                                                    axis.title.y = element_text(size = 10, vjust = 0))
three_plots[[2]] <- three_plots[[2]] + ggtitle("Shallow reef: structural complexity") + theme(axis.title.x = element_text(size = 10),
                                                                                              axis.title.y = element_text(size = 10, vjust = 0))
three_plots[[3]] <- three_plots[[3]] + ggtitle("Kelp forest: structural complexity") + theme(axis.title.x = element_text(size = 10),
                                                                                             axis.title.y = element_text(size = 10, vjust = 0))


(guide_area() / wrap_plots(three_plots , axis_titles = "collect_x", ncol = 1)) + 
  plot_layout(guides = "collect", heights = unit(c(0.4, 1), c("cm", "null"))) + 
  theme(legend.position = "top")

ggsave(file.path(fig.dir, "fig5-habitat-threeway-effects.png"), 
       width = 6, height = 7, dpi = 600, units = "in")
