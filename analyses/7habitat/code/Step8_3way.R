

rm(list = ls())
gc()

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
        plot.background = element_rect(fill = "white", color = NA))

mpa_colors <- c("Reference" = "#6d55aa", "MPA" = "#c42119")


list2env(list(habitat = "rock", # _filtered
              focal_group = "targeted",
              re_string = "rmy"), envir = .GlobalEnv)


results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects", "3way", paste(habitat, focal_group, re_string, "effects.rds", sep = "_")))
data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "3way", paste(habitat, focal_group, re_string, "data.rds", sep = "_")))
       
focal_model <- results$models$top

# Make the plot for the SI that shows more levels 
effects_list <- allEffects(focal_model, partial.residuals = T, 
                           xlevels = list(depth_cv_500 = round(quantile(data_sp$depth_cv_500, probs = seq(0, 1, by = 0.05)), digits = 3), 
                                          hard_bottom_500 = round(quantile(data_sp$hard_bottom_500, probs = seq(0, 1, by = 0.05)), digits = 3), 
                                          age_at_survey = round(quantile(data_sp$age_at_survey, probs = seq(0, 1, by = 0.05)), digits = 3))) 


const <- min(data_sp$biomass[data_sp$biomass > 0], na.rm = TRUE)

# Reverse scaling if available
cv_center <- attr(data_sp[["depth_cv_500"]], "scaled:center")
cv_scale  <- attr(data_sp[["depth_cv_500"]], "scaled:scale")
age_center <- attr(data_sp[["age_at_survey"]], "scaled:center")
age_scale  <- attr(data_sp[["age_at_survey"]], "scaled:scale")
hard_center <- attr(data_sp[["hard_bottom_500"]], "scaled:center")
hard_scale  <- attr(data_sp[["hard_bottom_500"]], "scaled:scale")
depth_center <- attr(data_sp[["depth_mean_500"]], "scaled:center")
depth_scale  <- attr(data_sp[["depth_mean_500"]], "scaled:scale")

effects_cv <- data.frame(effects_list$`depth_cv_500:site_type:age_at_survey`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         depth_cv_500 = round(depth_cv_500 * cv_scale + cv_center, digits = 2),
         age_at_survey = round(age_at_survey * age_scale + age_center, digits = 1)) 


dcv_vals <- sort(unique(effects_cv$depth_cv_500))

effects_cv <- effects_cv %>% 
  mutate(dcv = fct_reorder(paste0(seq(0, 100, by = 5)[match(depth_cv_500, dcv_vals)], "% (DCV = ", round(depth_cv_500, 1), ")"), depth_cv_500)) 
  

ggplot(data = effects_cv %>% 
         filter(str_detect(dcv, "25|50|75|95")), 
       aes(x = age_at_survey, y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = 'identity') +
  geom_smooth(aes(color = site_type), method = "loess") +
  facet_wrap(~dcv, nrow = 1) +
  labs(x = "MPA Age (years)",
       y = "Biomass (kg per unit effort)", color = NULL, fill = NULL) +
  scale_color_manual(values = mpa_colors) + 
  scale_fill_manual(values = mpa_colors) + # c("#7e67f8", "#e5188b")
  scale_x_continuous(limits = c(0, 20), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 33.2), expand = c(0,0)) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

ggsave(file.path(fig.dir, "habitat-reef-dcv.png"), width = 6, height = 3, units = "in", dpi = 600)

# For the 2-way interaction with hard bottom:
effects_hard <- data.frame(effects_list$`site_type:hard_bottom500`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         hard_bottom_500 = round(hard_bottom_500 * hard_scale + hard_center, digits = 2)) 

ggplot(data = effects_hard, aes(x = hard_bottom_500)) +
  geom_smooth(aes(y = fit, color= site_type), se = F) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type), alpha = 0.2, stat ='identity')

# For the 3-way interaction with hard bottom:
effects_hard <- data.frame(effects_list$`hard_bottom_500:site_type:age_at_survey`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         hard_bottom_500 = round(hard_bottom_500 * hard_scale + hard_center, digits = 2),
         age_at_survey = round(age_at_survey * age_scale + age_center, digits = 1)) 

hard_vals <- sort(unique(effects_hard$hard_bottom_500))

effects_hard <- effects_hard %>% 
  mutate(hard = fct_reorder(paste0(seq(0, 100, by = 5)[match(hard_bottom_500, hard_vals)], "% (Hard = ", round(hard_bottom_500, 1), ")"), hard_bottom_500))

ggplot(data = effects_hard %>% 
         filter(str_detect(hard, "^25|^50|^75|^95")), 
       aes(x = age_at_survey, y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = 'identity') +
  geom_smooth(aes(color = site_type), method = "loess") +
  facet_wrap(~hard, nrow = 1) +
  labs(x = "MPA Age (years)",
       y = "Biomass (kg per unit effort)", color = NULL, fill = NULL) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
 # scale_x_continuous(limits = c(0, 20), expand = c(0,0)) + 
 # scale_y_continuous(limits = c(0, 33.2), expand = c(0,0)) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

ggsave(file.path(fig.dir, "habitat-reef-hard.png"), width = 6, height = 3, units = "in", dpi = 600)


ggplot(data = effects_hard %>% 
         mutate(age = round(age_at_survey, digits = 0)) %>% 
         filter(age %in% c(0, 5, 10)) %>% 
         mutate(age = fct_reorder(paste0(age, " years"), age)), 
       aes(x = hard_bottom_500/1e6, y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = 'identity') +
  geom_smooth(aes(color = site_type), method = "loess") +
  facet_wrap(~age, nrow = 1) +
  labs(x = expression("Hard bottom (km"^2*")"),
       y = "Biomass (kg per unit effort)", color = NULL, fill = NULL) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
  coord_cartesian(xlim = c(0, 0.8), ylim = c(0, 15), expand = F) +
#  scale_x_continuous(limits = c(0, 0.8), expand = c(0,0)) + 
#  scale_y_continuous(limits = c(0, 20), expand = c(0,0)) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(2, "lines"),
        legend.text = element_text(size = 8),
        legend.key.size = unit(0.8, "lines"))

ggsave(file.path(fig.dir, "habitat-reef-hard2.png"), width = 6, height = 3, units = "in", dpi = 600)

# Plot just the depth relationship
eff_depth <- data.frame(effects_list$depth_mean_500) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         depth_mean_500 = round(depth_mean_500 * depth_scale + depth_center, digits = 2)) 

ggplot(eff_depth, aes(x = depth_mean_500, y =  fit)) +
  geom_line(size = 1.2, show.legend = F) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, color = NA, show.legend = F) +
  coord_cartesian(xlim = c(10, 48), ylim = c(0,10), expand = F) + 
  labs(
    x = "Average depth (m)",
    y = "Biomass (kg per unit effort)",
    color =  NULL,
    fill = NULL) + 
  my_theme +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig.dir, "habitat-rock-depth.png"), width = 2.75, height = 2.5, units = "in", dpi = 600)


# Get predicted biomass over age for both MPA and reference, averaged over habitat
eff_age <- predict_response(focal_model, terms = c("age_at_survey", "site_type"), margin = "empirical", type = "fixed")

# Plot the average predicted biomass trajectories
ggplot(eff_age, aes(x = x * age_scale + age_center, y = exp(predicted) - const, color = group)) +
  geom_line(size = 1.2, show.legend = F) +
  geom_ribbon(aes(ymin = exp(conf.low) - const, ymax = exp(conf.high) - const, fill = group), alpha = 0.2, color = NA, show.legend = F) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
 # scale_y_continuous(breaks = seq(0, 10, by = 2.5)) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, NA), expand = F) + 
  labs(
    x = "MPA Age (years)",
    y = "Biomass (kg per unit effort)",
    color =  NULL,
    fill = NULL) + my_theme +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig.dir, "habitat-rock-overall.png"), width = 2.75, height = 2.5, units = "in", dpi = 600)




m <- lmer(log_c_biomass ~ depth_cv_500 * site_type * age_at_survey + depth_mean_500 + (1|region4/affiliated_mpa) + (1|year), data = data_sp)

# # Calculate slope difference (MPA - Reference) at each habitat level
hard_vals <- unique(data_sp$hard_bottom_500[data_sp$site_type == "MPA"])

# Generate depth CV grid across observed range
hard_vals_grid <- seq(min(data_sp$hard_bottom_500, na.rm = TRUE),
                       max(data_sp$hard_bottom_500, na.rm = TRUE), 
                       length.out = 20)

# Calculate slope difference (MPA - Reference) at each depth CV level
slope_diffs <- map_dfr(hard_vals_grid, ~{
  emtrends(focal_model,
           specs = "site_type",
           var = "age_at_survey",
           at = list(hard_bottom_500 = .x)) %>%
    contrast(method = "revpairwise") %>%
    as.data.frame() %>%
    mutate(hard_bottom_500 = .x)
})

# Calculate percent change per year from log-scale estimate
slope_diffs2 <- slope_diffs %>% 
  mutate(est_pct = (exp(estimate) - 1) * 100,
         lower_pct = (exp(estimate - 1.96 * SE) - 1) * 100,
         upper_pct = (exp(estimate + 1.96 * SE) - 1) * 100) %>% 
#  mutate(dcv_mpa = if_else(depth_cv_250 %in% c(data_sp$depth_cv_250[data_sp$site_type == "MPA"]), 1, 0)) %>% 
  mutate(hard_bottom_500 = hard_bottom_500 * hard_scale + hard_center) %>% 
  mutate(model = "With habitat")


# Calculate slope difference from simple model
base_diff <- emtrends(results$models$base,
                      specs = "site_type",
                      var = "age_at_survey") %>%
  contrast(method = "revpairwise") %>%
  as.data.frame() %>%
  mutate(bb = NA,
         model = "Without habitat",
         est_pct = (exp(estimate) - 1) * 100,
         lower_pct = (exp(estimate - 1.96 * SE) - 1) * 100,
         upper_pct = (exp(estimate + 1.96 * SE) - 1) * 100)

# Plot contrasts with ribbon and transformed values to show percent change
rock <- ggplot(slope_diffs2, aes(x = hard_bottom_500/1e6, y = est_pct)) +
  # Uniform-effect model (95% CI)
  geom_rect(data = base_diff, inherit.aes = FALSE,
            aes(xmin = 0, xmax = max(slope_diffs2$hard_bottom_500)/1e6,
                ymin = lower_pct, ymax = upper_pct,
                fill = model, linetype = model)) +
  geom_hline(data = base_diff, aes(yintercept = est_pct, linetype = model, color = model), size = 0.8) +
  # Reference line for "No difference" with label + arrow
#  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  annotate("text", x = 1.5, y = -2, label = "No difference", hjust = 0, size = 3.5) +
  annotate("curve",
           x = 1.45, y = -2,       # starting point (tip of "No")
           xend = 1, yend = -0.05,    # endpoint (reference line)
           curvature = -0.3,         # positive = curve right, negative = left
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = "black",
           linewidth = 0.4) +
  # Baseline-informed model (95% CI)
  geom_ribbon(aes(ymin = lower_pct, ymax = upper_pct, fill = model), alpha = 0.2) +
  geom_line(aes(color = model, linetype = model), linewidth = 0.8) +
 # geom_point(data = slope_diffs2 %>% filter(dcv_mpa == 1), aes(color = model, fill = model)) +
  labs(
    x = "Hard Bottom",
    y = "MPA effect (% change in biomass per year)",
    color = NULL, fill = NULL, linetype = NULL
  ) +
  scale_color_manual(values = c("With habitat" = "#084594", "Without habitat" = "black")) +
  scale_fill_manual(values = c("With habitat" = "#084594", "Without habitat" = "grey80")) +
  scale_linetype_manual(values = c("With habitat" = "solid", "Without habitat" = "22")) +
  coord_cartesian(xlim = c(0, 0.78), ylim = c(0, 75), expand = F) +
  scale_x_continuous(breaks = seq(0, 0.8, by = 0.15)) +
  my_theme +
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.8),
        panel.grid.minor = element_blank())

rock
ggsave(file.path(fig.dir, "habitat-rock-hard-mpaeffect.png"), 
       plot = rock, width = 4.5, height = 3.5, units = "in", dpi = 600)










# Kelp forest

rm(list = setdiff(ls(), c("my_theme", "fig.dir", "mpa_colors")))


list2env(list(habitat = "kelp",
              focal_group = "targeted",
              re_string = "my"), envir = .GlobalEnv)

results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects", "3way", paste(habitat, focal_group, re_string, "effects.rds", sep = "_")))
data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "3way", paste(habitat, focal_group, re_string, "data.rds", sep = "_")))

focal_model <- results$models$top

# Make the plot for the SI that shows more levels 
effects_list_full <- allEffects(focal_model, partial.residuals = T, 
                                xlevels = list(depth_cv_100 = round(quantile(data_sp$depth_cv_100, probs = seq(0, 1, by = 0.05)), digits = 3), 
                                               age_at_survey = round(quantile(data_sp$age_at_survey, probs = seq(0, 1, by = 0.05)), digits = 3))) 


const <- min(data_sp$biomass[data_sp$biomass > 0], na.rm = TRUE)
cv_center <- attr(data_sp[["depth_cv_500"]], "scaled:center")
cv_scale  <- attr(data_sp[["depth_cv_500"]], "scaled:scale")
age_center <- attr(data_sp[["age_at_survey"]], "scaled:center")
age_scale  <- attr(data_sp[["age_at_survey"]], "scaled:scale")

effects_cv <- data.frame(effects_list_full$`site_type:depth_cv_100:age_at_survey`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         depth_cv_100 = round(depth_cv_100 * cv_scale + cv_center, digits = 2),
         age_at_survey = round(age_at_survey * age_scale + age_center, digits = 1)) 

dcv_vals <- sort(unique(effects_cv$depth_cv_100))

effects_cv2 <- effects_cv %>% 
  mutate(dcv = fct_reorder(paste0(seq(0, 100, by = 5)[match(depth_cv_100, dcv_vals)], "% (DCV = ", round(depth_cv_100, 1), ")"), depth_cv_100)) 


ggplot(data = effects_cv2 %>% 
         filter(str_detect(dcv, "25|50|75|95")), 
       aes(x = age_at_survey, y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = 'identity') +
  geom_smooth(aes(color = site_type), method = "loess") +
  facet_wrap(~dcv, nrow = 1) +
  labs(x = "MPA Age (years)",
       y = "Biomass (kg per unit effort)", color = NULL, fill = NULL) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
  scale_x_continuous(limits = c(0, 20), expand = c(0,0)) + 
 # scale_y_continuous(limits = c(0, 33.2), expand = c(0,0)) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

ggsave(file.path(fig.dir, "habitat-kelp-dcv.png"), width = 6, height = 3, units = "in", dpi = 600)


ggplot(data = effects_cv %>% 
         filter(age_at_survey %in% c(0, 5, 10, 15)), 
       aes(x = depth_cv_100, y = fit, color = site_type, fill = site_type)) +
  geom_smooth() +
  geom_ribbon(aes(ymin = lower, ymax = upper), color = NA, alpha = 0.2)+
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
  labs(color = NULL, fill = NULL,
       x = "Structural complexity", y = expression("Biomass (kg per 100m"^2*")")) +
  facet_wrap(~age_at_survey, nrow = 1) + 
  coord_cartesian(xlim = c(40, 80), ylim = c(0, 10), expand = F) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.2, "lines"),
        legend.key.size = unit(1, "lines"))

ggsave(file.path(fig.dir, "habitat-kelp-dcv2.png"), width = 7.5, height = 3, units = "in", dpi = 600)


plot(allEffects(focal_model), multiline = T)
library(ggeffects)


# Bin depth complexity
data_sp %>%
  mutate(dcv_bin = cut(depth_cv_100, breaks = quantile(depth_cv_100, probs = seq(0, 1, 0.2), na.rm = TRUE))) %>%
  group_by(dcv_bin) %>%
  summarise(mean_kelp = mean(kelp_annual_100, na.rm = TRUE),
            mean_depth = mean(depth_mean_500, na.rm = TRUE),
            n = n()) %>%
  ggplot(aes(x = dcv_bin, y = mean_kelp)) +
  geom_col() +
  labs(title = "Average kelp cover across structural complexity bins",
       x = "Depth CV bin", y = "Mean annual kelp cover")

ggplot(data_sp, aes(x = depth_cv_100, y = kelp_annual_100)) +
  geom_point(alpha = 0.2) +
  geom_smooth(method = "loess", se = FALSE, color = "blue") +
  labs(title = "Relationship between structural complexity and kelp cover")


expression("Biomass (kg per 100m"^2*")")


# Get predicted biomass over age for both MPA and reference, averaged over habitat
eff_age <- ggpredict(focal_model, terms = c("age_at_survey", "site_type"))

# Plot the average predicted biomass trajectories
ggplot(eff_age, aes(x = x * age_scale + age_center, y = exp(predicted) - const, color = group)) +
  geom_line(size = 1.2, show.legend = F) +
  geom_ribbon(aes(ymin = exp(conf.low) - const, ymax = exp(conf.high) - const, fill = group), alpha = 0.2, color = NA, show.legend = F) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
  scale_y_continuous(breaks = seq(0, 10, by = 2.5)) +
  coord_cartesian(xlim = c(0, 20), ylim = c(0, NA), expand = F) + 
  labs(
    x = "MPA Age (years)",
    y = expression("Biomass (kg per 100m"^2*")"),
    color =  NULL,
    fill = NULL, title = "Kelp forest") + my_theme +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig.dir, "habitat-kelp-overall.png"), width = 2.75, height = 2.5, units = "in", dpi = 600)

eff_hard <- ggpredict(focal_model, terms = c("hard_bottom_50", "site_type"))
hard_center <- attr(data_sp[["hard_bottom_50"]], "scaled:center")
hard_scale  <- attr(data_sp[["hard_bottom_50"]], "scaled:scale")
ggplot(eff_hard, aes(x = x * hard_scale + hard_center, y = exp(predicted) - const, color = group)) +
  geom_line(size = 1.2, show.legend = F) +
  geom_ribbon(aes(ymin = exp(conf.low) - const, ymax = exp(conf.high) - const, fill = group), alpha = 0.2, color = NA, show.legend = F) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
 scale_y_continuous(breaks = seq(2, 8, by = 2)) +
  coord_cartesian(xlim = c(0, 8000), ylim = c(2, NA), expand = F) + 
  labs(
    x = expression("Hard bottom (m"^2*")"),
    y = expression("Biomass (kg per 100m"^2*")"),
    color =  NULL,
    fill = NULL, title = "Kelp forest") + my_theme +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(fig.dir, "habitat-kelp-hard.png"), width = 2.75, height = 2.5, units = "in", dpi = 600)


# Calculate slope difference (MPA - Reference) at each habitat level
dc_vals <- unique(data_sp$depth_cv_100[data_sp$site_type == "MPA"])

# Generate depth CV grid across observed range
dcv_vals_grid <- c(seq(min(data_sp$depth_cv_250, na.rm = TRUE),
                     max(data_sp$depth_cv_250, na.rm = TRUE), 
                     length.out = 20), dc_vals)

# Calculate slope difference (MPA - Reference) at each depth CV level
slope_diffs <- map_dfr(dcv_vals_grid, ~{
  emtrends(m,
           specs = "site_type",
           var = "age_at_survey",
           at = list(depth_cv_250 = .x)) %>%
    contrast(method = "revpairwise") %>%
    as.data.frame() %>%
    mutate(depth_cv_250 = .x)
})

# Calculate percent change per year from log-scale estimate
slope_diffs2 <- slope_diffs %>% 
  mutate(est_pct = (exp(estimate) - 1) * 100,
         lower_pct = (exp(estimate - 1.96 * SE) - 1) * 100,
         upper_pct = (exp(estimate + 1.96 * SE) - 1) * 100) %>% 
  mutate(dcv_mpa = if_else(depth_cv_250 %in% c(data_sp$depth_cv_250[data_sp$site_type == "MPA"]), 1, 0)) %>% 
  mutate(depth_cv_250 = depth_cv_250 * cv_scale + cv_center) %>% 
  mutate(model = "With habitat")


# Calculate slope difference from simple model
base_diff <- emtrends(results$models$base,
                      specs = "site_type",
                      var = "age_at_survey") %>%
  contrast(method = "revpairwise") %>%
  as.data.frame() %>%
  mutate(bb = NA,
         model = "Without habitat",
         est_pct = (exp(estimate) - 1) * 100,
         lower_pct = (exp(estimate - 1.96 * SE) - 1) * 100,
         upper_pct = (exp(estimate + 1.96 * SE) - 1) * 100)

# Plot contrasts with ribbon and transformed values to show percent change
kelp_cv <- ggplot(slope_diffs2, aes(x = depth_cv_250, y = est_pct)) +
  # Uniform-effect model (95% CI)
  geom_rect(data = base_diff, inherit.aes = FALSE,
            aes(xmin = 0, xmax = max(slope_diffs2$depth_cv_250),
                ymin = lower_pct, ymax = upper_pct,
                fill = model, linetype = model)) +
  geom_hline(data = base_diff, aes(yintercept = est_pct, linetype = model, color = model), size = 0.8) +
  # Reference line for "No difference" with label + arrow
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  annotate("text", x = 1.5, y = -2, label = "No difference", hjust = 0, size = 3.5) +
  annotate("curve",
           x = 1.45, y = -2,       # starting point (tip of "No")
           xend = 1, yend = -0.05,    # endpoint (reference line)
           curvature = -0.3,         # positive = curve right, negative = left
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = "black",
           linewidth = 0.4) +
  # Baseline-informed model (95% CI)
  geom_ribbon(aes(ymin = lower_pct, ymax = upper_pct, fill = model), alpha = 0.2) +
  geom_line(aes(color = model, linetype = model), linewidth = 0.8) +
  geom_point(data = slope_diffs2 %>% filter(dcv_mpa == 1), aes(color = model, fill = model)) +
  labs(
    x = "Depth CV",
    y = "MPA effect (% change in biomass per year)",
    color = NULL, fill = NULL, linetype = NULL
  ) +
  scale_color_manual(values = c("With habitat" = "#084594", "Without habitat" = "black")) +
  scale_fill_manual(values = c("With habitat" = "#084594", "Without habitat" = "grey80")) +
  scale_linetype_manual(values = c("With habitat" = "solid", "Without habitat" = "22")) +
  coord_cartesian(xlim = c(25, 90), ylim = c(-25, 75), expand = F) +
  scale_x_continuous(breaks = seq(25, 90, by = 10)) +
  my_theme +
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.8),
        panel.grid.minor = element_blank())

kelp_cv
ggsave(file.path(fig.dir, "habitat-kelp-cv-mpaeffect.png"), 
       plot = kelp_cv, width = 4.5, height = 3.5, units = "in", dpi = 600)





# Surf zone ----------------------------------------------------------------------------

list2env(list(habitat = "surf_filtered",
              focal_group = "targeted",
              re_string = "m"), envir = .GlobalEnv)


# Load necessary packages
library(emmeans)
library(lme4)

results <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/effects", "3way", paste(habitat, focal_group, re_string, "effects.rds", sep = "_")))
data_sp <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/data", "3way", paste(habitat, focal_group, re_string, "data.rds", sep = "_")))

focal_model <- results$models$top


# Estimate overall MPA effect (average across habitat and MPA age)
emm_habitat <- emmeans(focal_model, pairwise ~ site_type, type = "response")
summary(emm_habitat)

emm_base <- emmeans(results$models$base, pairwise ~ site_type, type = "response")
summary(emm_base)



# Make the plot for the SI that shows more levels 
effects_list_full <- allEffects(focal_model, partial.residuals = T, 
                                xlevels = list(age_at_survey = round(quantile(data_sp$age_at_survey, probs = seq(0, 1, by = 0.01)), digits = 3))) 


const <- min(data_sp$biomass[data_sp$biomass > 0], na.rm = TRUE)
cv_center <- attr(data_sp[["depth_cv_500"]], "scaled:center")
cv_scale  <- attr(data_sp[["depth_cv_500"]], "scaled:scale")
age_center <- attr(data_sp[["age_at_survey"]], "scaled:center")
age_scale  <- attr(data_sp[["age_at_survey"]], "scaled:scale")
soft_center <- attr(data_sp[["soft_bottom_25"]], "scaled:center")
soft_scale  <- attr(data_sp[["soft_bottom_25"]], "scaled:scale")

eff<- data.frame(effects_list_full$`site_type:age_at_survey`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         age_at_survey = round(age_at_survey * age_scale + age_center, digits = 1)) 

ggplot(data = eff, aes(x = age_at_survey, y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = 'identity', show.legend = F) +
  geom_smooth(aes(color = site_type), method = "loess", show.legend = F) +
  labs(x = "MPA Age (years)",
       y = "Biomass (kg per haul)", color = NULL, fill = NULL) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
 # scale_x_continuous(limits = c(0, 20), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, NA), expand = c(0,0)) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

ggsave(file.path(fig.dir, "habitat-surf-overall.png"),  width = 2.75, height = 2.5, units = "in", dpi = 600)

eff2<- data.frame(effects_list_full$`soft_bottom_25:site_type`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         soft_bottom_25 = soft_bottom_25 * soft_scale + soft_center) 


ggplot(data = eff2, aes(x = soft_bottom_25, y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = 'identity', show.legend = F) +
  geom_smooth(aes(color = site_type), method = "loess", show.legend = F) +
  labs(x = expression("Amount of soft bottom (m"^2*")"),
       y = "Biomass (kg per haul)", color = NULL, fill = NULL) +
  scale_color_manual(values = mpa_colors) +
  scale_fill_manual(values = mpa_colors) +
  scale_x_continuous(limits = c(100, NA), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 3.5), expand = c(0,0)) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1, "lines"),
        legend.key.size = unit(1, "lines"))

ggsave(file.path(fig.dir, "habitat-surf-soft.png"),  width = 2.75, height = 2.5, units = "in", dpi = 600)
