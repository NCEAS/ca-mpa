# Step 5. Fit Targeted Fish Model
# Cori Lopazanski
# Feb 2025

# Set up --------------------------------------------------------------------------
rm(list = ls())
gc()

#source("analyses/7habitat/code/Step4_build_habitat_models.R")  # Load the function from the file

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

# Read Data --------------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_kelp_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_2way.Rds"))

# Load data and relevant variables
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg:count_per_m2, 
                all_of(pred_kelp$predictor)) 

# Build Data --------------------------------------------------------------------------

# Summarize across all targeted fishes
data1 <- data_kelp %>%
  group_by(year, site, site_type, bioregion, region4, affiliated_mpa, age_at_survey,
           target_status, across(matches("^hard|soft|depth|kelp"))) %>%
  summarize(kg_per_100m2 = sum(kg_per_m2, na.rm = T)*100,
            count_per_100m2 = sum(count_per_m2, na.rm = T)*100, .groups = 'drop') %>%
  filter(target_status == "Targeted")

# Summarize across genuses
# data1 <- data_kelp %>% 
#   group_by(year, site, site_type, bioregion, region4, affiliated_mpa, age_at_survey, genus, across(matches("^hard|soft|depth|kelp"))) %>% 
#   summarize(kg_per_100m2 = sum(kg_per_m2, na.rm = T)*100,
#             count_per_100m2 = sum(count_per_m2, na.rm = T)*100, .groups = 'drop') %>% 
#   filter(genus == "Sebastes") #%>% 
# #  filter(region4 == "Central")

# Identify sites where species are infrequently observed
zero_site <- data1 %>%
  group_by(site) %>% 
  summarize(prop_zero = mean(kg_per_100m2 == 0)) %>% 
  filter(prop_zero > 0.9) # drop sites where observed < 10% of years

# Check MPA/Ref balance of remaining sites 
zero_site_balance <- data1 %>% 
  filter(!site %in% zero_site$site) %>% 
  distinct(site, site_type, affiliated_mpa, year) %>% 
  group_by(affiliated_mpa, site_type) %>% 
  summarize(n_site_year = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = n_site_year) %>% 
  filter(is.na(MPA) | is.na(Reference))

data2 <- data1 %>% 
  filter(!site %in% zero_site$site) %>% 
  filter(!affiliated_mpa %in% zero_site_balance$affiliated_mpa)

# Scale the static variables at the site-level (e.g. don't weight based on obs. frequency)
site_static <- data2 %>% 
  distinct(site, across(all_of(grep("^hard|soft|depth", names(.), value = TRUE)))) %>%
  mutate_at(vars(grep("^hard|soft|depth", names(.), value = TRUE)), scale)

# Remove sites with extreme values in static vars (depth and hard bottom)
extreme_site <- site_static %>% 
  pivot_longer(cols = depth_cv_100:hard_bottom_500, names_to = "variable", values_to = "value") %>% 
  filter(!between(value, -3.5, 3.5)) %>% 
  pivot_wider(names_from = variable, values_from = value)

# Check balance of remaining sites (ensure still MPA/Ref pairs)
extreme_site_balance <- data2 %>% 
  filter(!site %in% extreme_site$site) %>% 
  distinct(site, site_type, affiliated_mpa, year) %>% 
  group_by(affiliated_mpa, site_type) %>% 
  summarize(n_site_year = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = n_site_year) %>% 
  filter(is.na(MPA) | is.na(Reference))

data3 <- data2 %>%
  filter(!site %in% extreme_site$site) %>% 
  filter(!affiliated_mpa %in% extreme_site_balance$affiliated_mpa) %>% 
  # Drop un-scaled static variables
  dplyr::select(!c(grep("^hard|soft|depth", names(.), value = TRUE))) %>% 
  # Join the scaled static variables
  left_join(site_static, by = "site") %>% 
  # Scale age
  mutate_at(vars(grep("^age", names(.), value = TRUE)), scale) %>% 
  # Scale kelp within each year (so relative to annual average instead of across all years)
  group_by(year) %>%
  mutate_at(vars(grep("^kelp", names(.), value = TRUE)), scale) 

# Save final output for the model
const <- if_else(min(data3$kg_per_100m2) > 0, 0, min(data3$kg_per_100m2[data3$kg_per_100m2 > 0]))

data_sp <- data3 %>% 
  mutate(log_biomass = log(kg_per_100m2 + const))
  
rm(list = setdiff(ls(), c("data_sp", "my_theme", "pred_kelp_2way")))



# Base Model --------------------------------------------------------------------------

base_model <- lmer(log_biomass ~ site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year), 
                   data = data_sp)

summary(base_model)  
#plot(base_model)  
#plot(allEffects(base_model, partial.residuals = T), residuals.pch = 19, residuals.cex = 0.2)  

# Habitat Model --------------------------------------------------------------------------

## Test appropriate RE structure -----
# 1. Full model at intermediate scale (250m)
habitat250_rms <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                     kelp_annual_250 * site_type + 
                     depth_mean_250 * site_type + depth_cv_250 * site_type + 
                     site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                   data = data_sp) 

VarCorr(habitat250_rms)  # Inspect variance estimates
# Singular fit with zero variance for mpa:region

# 2. Given zero variance for mpa:region, test whether region improves fit
habitat250_ms <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                     kelp_annual_250 * site_type + 
                     depth_mean_250 * site_type + depth_cv_250 * site_type + 
                     site_type * age_at_survey + (1|affiliated_mpa/site) + (1|year),
                   data = data_sp) 

VarCorr(habitat250_ms) 
performance::icc(habitat250_ms, by_group = T) 
anova(habitat250_ms, habitat250_rms) # p > 0.05 so likely OK to drop region; top is mpa/site + year

# 3. Confirm site as RE
habitat250_m <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                      data = data_sp) 

VarCorr(habitat250_m) 
performance::icc(habitat250_m, by_group = T) 
anova(habitat250_m, habitat250_ms) # p < 0.05 so site is useful; top is mpa/site + year

# 4. Confirm MPA as RE
habitat250_s <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                         kelp_annual_250 * site_type + 
                         depth_mean_250 * site_type + depth_cv_250 * site_type + 
                         site_type * age_at_survey + (1|site) + (1|year),
                       data = data_sp) 
VarCorr(habitat250_s) 
performance::icc(habitat250_s, by_group = T)  
anova(habitat250_s, habitat250_ms) # p = 1, so variation is pretty low for MPA; top is site + year

# 5. Confirm year as RE
habitat250_m2 <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1|site),
                      data = data_sp) 

anova(habitat250_m2, habitat250_s) # p < 0.0 so year is useful
# Conclusion: (1|affiliated_mpa/site) + (1|year)

# 6. Examine if age effect varies by MPA (while including above RE)
habitat250_m3 <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1 + age_at_survey | affiliated_mpa) + (1 | site:affiliated_mpa) + (1 | year), 
                      data = data_sp) 

VarCorr(habitat250_m3) 
performance::icc(habitat250_m3, by_group = T)  # Intraclass correlation to check variance partitioning
anova(habitat250_m3, habitat250_ms) # p > 0.05 so does not significantly improve fit

## Confirm appropriate RE structure ----



# 1. Full model at intermediate scale (250m)
habitat_rms <- lmer(log_biomass ~ hard_bottom_50 * site_type + kelp_annual_50 + depth_mean_500 * site_type + site_type * age_at_survey + 
                         (1|region4/affiliated_mpa/site) + (1|year),
                       data = data_sp) 

VarCorr(habitat_rms)  # Inspect variance estimates
# Singular fit with zero variance for mpa:region

# 2. Given zero variance for mpa:region, test whether region improves fit
habitat_ms <- lmer(log_biomass ~ hard_bottom_50 * site_type + kelp_annual_50 + depth_mean_500 * site_type + site_type * age_at_survey + 
                        (1|affiliated_mpa/site) + (1|year),
                      data = data_sp) 

VarCorr(habitat_ms) 
performance::icc(habitat_ms, by_group = T) 
anova(habitat_ms, habitat_rms) # p > 0.05 so likely OK to drop region; top is mpa/site + year

# 3. Confirm site as RE
habitat_m <- lmer(log_biomass ~ hard_bottom_50 * site_type + kelp_annual_50 + depth_mean_500 * site_type + site_type * age_at_survey + 
                       (1|affiliated_mpa) + (1|year),
                     data = data_sp) 

VarCorr(habitat_m) 
performance::icc(habitat_m, by_group = T) 
anova(habitat_m, habitat_ms) # p < 0.05 so site is useful; top is mpa/site + year

# 4. Confirm MPA as RE
habitat_s <- lmer(log_biomass ~ hard_bottom_50 * site_type + kelp_annual_50 + depth_mean_500 * site_type + site_type * age_at_survey + 
                    (1|site) + (1|year),
                     data = data_sp) 
VarCorr(habitat_s) 
performance::icc(habitat_s, by_group = T)  
anova(habitat_s, habitat_ms) # p = 1, so variation is pretty low for MPA; top is site + year

# 5. Confirm year as RE
habitat_m2 <- lmer(log_biomass ~ hard_bottom_50 * site_type + kelp_annual_50 + depth_mean_500 * site_type + site_type * age_at_survey + 
                     (1|site),
                      data = data_sp) 

anova(habitat_m2, habitat_s) # p < 0.0 so year is useful
# Conclusion: (1|site) + (1|year)

habitat_m3 <- lmer(log_biomass ~ hard_bottom_50 * site_type + kelp_annual_50 + depth_mean_500 * site_type + site_type * age_at_survey + 
                     (1|affiliated_mpa) + (1|year),
                   data = data_sp) 
VarCorr(habitat_m3)
anova(habitat_m3, habitat_s) 

# note - also explored (1 + age_at_survey | affiliated_mpa:site_type) and it was statistically better but obviously more complex and likely not useful
# 
# random_slopes <- ranef(habitat250_m3)$affiliated_mpa
# colnames(random_slopes) <- c("Intercept", "Slope")
# random_slopes$affiliated_mpa <- rownames(random_slopes)
# 
# random_slopes <- random_slopes %>% 
#   left_join(data_sp %>% ungroup %>% distinct(affiliated_mpa, bioregion, region4)) %>% 
#   mutate(MPA = fct_reorder(affiliated_mpa, Slope))
# 
# ggplot(random_slopes, aes(x = MPA, y = Slope)) +
#   geom_point(aes(color = region4)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   theme_minimal() +
#   labs(title = "Variation in MPA Age Slopes", x = "MPA", y = "Slope for MPA_Age") +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))

compare_performance(habitat250_rms, habitat250_ms, habitat250_m, habitat250_m2, habitat250_m3)

# Confirm: Keep the random intercept model with site nested within MPA + year (_ms)

# Side Idea: Biomass at Baseline Question ----------

mpa_baseline <- data_sp %>%
  group_by(affiliated_mpa, site_type) %>%
  summarize(baseline_biomass = mean(log_biomass[age_at_survey == min(age_at_survey)], na.rm = TRUE))

data_sp <- left_join(data_sp, mpa_baseline, by = c("affiliated_mpa", "site_type"))

model_baseline <- lmer(log_biomass ~hard_bottom_50 * site_type + kelp_annual_50+ depth_mean_500 *site_type+
                         site_type * age_at_survey + 
                         baseline_biomass * age_at_survey + 
                         (1 | affiliated_mpa) + (1 | year), 
                       data = data_sp)

summary(model_baseline)
plot(model_baseline)

cor(data_sp[, c("baseline_biomass", "hard_bottom_250", "kelp_annual_250", "depth_mean_250", "depth_cv_250")], use = "pairwise.complete.obs")

data_sp <- data_sp %>%
  mutate(biomass_category = ifelse(baseline_biomass > median(baseline_biomass, na.rm = TRUE), "High (Baseline > Median)", "Low (Baseline < Median)"))

ggplot(data_sp, aes(x = age_at_survey, y = log_biomass, color = biomass_category)) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "MPA Age",
       y = "Log Biomass",
       color = paste0("MPA Baseline Biomass")) + facet_wrap(~site_type)


habitat500 <- lmer(log_biomass ~ hard_bottom_500 * site_type +
                     kelp_annual_500 * site_type + 
                     depth_mean_500 * site_type + depth_cv_500 * site_type + 
                     site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                   data = data_sp)

habitat500 <- lmer(log_biomass ~ hard_bottom_500 * site_type + 
                     kelp_annual_500 * site_type + 
                     depth_mean_500 * site_type + depth_cv_500 * site_type + 
                site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
             data = data_sp)

habitat250 <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                     kelp_annual_250 * site_type + 
                     depth_mean_250 * site_type + depth_cv_250 * site_type + 
                     site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                   data = data_sp)

habitat100 <- lmer(log_biomass ~ hard_bottom_100 * site_type + 
                     kelp_annual_100 * site_type + 
                     depth_mean_100 * site_type + depth_cv_100 * site_type + 
                     site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                   data = data_sp)

habitat50 <- lmer(log_biomass ~ hard_bottom_50 * site_type + 
                    kelp_annual_50 * site_type + 
                    depth_mean_50 * site_type + depth_cv_50 * site_type + 
                     site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                   data = data_sp)

habitat_refined <-  lmer(log_biomass ~ hard_bottom_100 * site_type + 
                           depth_mean_100*site_type +
                           site_type * age_at_survey + (1|affiliated_mpa) + (1|year), 
                         data = data_sp)

summary(habitat500) # K, DM, STA
summary(habitat250) # H, K, DM, STA
summary(habitat100) # H*, DM*, STA, all individual
summary(habitat50)
summary(habitat_refined)




check_model(habitat100,residual_type = "simulated", check = c("ncv", "qq", "normality", "vif"))
effects_list <- allEffects(habitat_refined, partial.residuals = T)

effects_list <- allEffects(habitat100, partial.residuals = T)
effects_list <- allEffects(base_model, partial.residuals = T)
effects_list <- allEffects(model_baseline, xlevels = 100)

effect_plots <- lapply(seq_along(effects_list), function(i) {
  effects_data <- as.data.frame(effects_list[[i]])
  x_var <- colnames(effects_data)[which(colnames(effects_data) != "site_type")[1]]
  const <-  min(data3$kg_per_100m2[data3$kg_per_100m2 > 0])*10
  
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
      labs(x = x_var_label,
           y = "Biomass (kg per 100m2)",
           color = NULL, fill = NULL) +
      theme_minimal() +
      theme(legend.position = ifelse(show_legend, "right", "none"),
            axis.title.y = if (i != 1) element_blank() else element_text()) +
      my_theme
    
  } else if (sum(str_detect(colnames(effects_data), "baseline_biomass")) > 0)  {
  effects_baseline <- effects_data %>%
    mutate(baseline_cat = cut(baseline_biomass, breaks = quantile(baseline_biomass, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), 
                              labels = c("Low", "Medium", "High"), include.lowest = TRUE))
  
  ggplot(effects_baseline, aes(x = !!sym(x_var), y = exp(fit) - const)) +
    geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Low"), 
                aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, ymax = exp(predict(loess(lower~age_at_survey))) - const, fill = baseline_cat), stat = "identity", alpha = 0.2) +
    geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Medium"), 
                aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, ymax = exp(predict(loess(lower~age_at_survey))) - const, fill = baseline_cat), stat = "identity", alpha = 0.2) +
    geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "High"), 
                aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, ymax = exp(predict(loess(lower~age_at_survey))) - const, fill = baseline_cat), stat = "identity", alpha = 0.2) +
    geom_smooth(aes(color = baseline_cat), se = F) +
    facet_wrap(~ baseline_cat) +
    labs(x = "Age at survey", y = "Biomass (kg per 100m2)", color = "Baseline Biomass Type", fill = "Baseline Biomass Type") +
    theme_minimal() +
    my_theme
  
  }
  else {
    ggplot(effects_data, aes(x = !!sym(x_var))) +
      geom_line(aes(y = exp(fit) - const), color = "black", show.legend = FALSE) +
      geom_ribbon(aes(ymin = exp(lower)- const, ymax = exp(upper)- const), alpha = 0.2, show.legend = FALSE) +
      labs(x = x_var_label,
           y = "Biomass (kg per 100m2)")+
      theme_minimal() +
      theme(axis.title.y = if (i != 1) element_blank() else element_text()) +
      my_theme
  }
  
})

# Combine all effect plots and export
wrap_plots(effect_plots, ncol = length(effect_plots)) + 
  plot_annotation(
    #title = paste0(sciname, "\n", target_status, "\n", assemblage, "\n", regions),
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

ggplot(data = data_sp) +
  geom_jitter(aes(x = age_at_survey, y = kg_per_100m2, color = site_type)) + 
  facet_wrap(~affiliated_mpa)

check_outliers(data_sp$kg_per_100m2)

plot(effects_list, partial.residuals = TRUE, residuals.cex = 0.4, residuals.pch = 19, 
     confint = list(style = "auto"), 
     axes = list(x = list(rotate = 45, cex = 0.8)),
     lattice = list(strip = list(cex = 0.8)),
     main = NULL, rows = 1, cols = length(effects_list))  

effects_baseline <- effects_list$`age_at_survey:baseline_biomass` %>% as.data.frame()



ggplot(effects_baseline, aes(x = age_at_survey)) +
  geom_smooth(aes(y = exp(upper) - const, color = baseline_cat, fill = baseline_cat), method = "loess", alpha = 0.2) +  
  geom_smooth(aes(y = exp(lower) - const,color = baseline_cat, fill = baseline_cat), method = "loess", alpha = 0.2) +  
  geom_smooth(aes(y = exp(fit) - const, color = baseline_cat), method = "loess", size = 1.2) +  # Main line
  facet_wrap(~ baseline_cat) +
  labs(x = "MPA Age", y = "Biomass (kg per 100m2)", color = "Baseline Biomass Type", fill = "Baseline Biomass Type") +
  theme_minimal() +
  my_theme

