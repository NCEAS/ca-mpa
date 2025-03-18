# Step 5. Fit Targeted Fish Model
# Cori Lopazanski
# Feb 2025

library(tidyverse)
library(lmerTest)
library(performance)
library(patchwork)


# Set up --------------------------------------------------------------------------
rm(list = ls())
gc()


my_theme <- theme(
  plot.title = element_text(size = 10, face = "bold"),
  plot.subtitle = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 10, angle = 45, hjust = 1),
  axis.text.y = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 10),
  plot.caption = element_text(size = 8),
  strip.text = element_text(size = 10, face = "bold"),
  panel.background = element_rect(fill = "white", color = NA),  
  plot.background = element_rect(fill = "white", color = NA)
)

source("analyses/7habitat/code/Step4a_prep_focal_data.R")

# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))

# Define subset for modeling (reduced number of columns)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:site_type, lat_dd, bioregion:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, 
                all_of(pred_kelp$predictor))  %>% 
  mutate(region5 = if_else(affiliated_mpa %in% c("blue cavern onshore smca", "farnsworth onshore smca", "long point smr"), "S. Channel Islands", region4)) %>% 
  filter(!(site %in% c("SCAI_SHIP_ROCK", "POINT_CABRILLO_2", "ANACAPA_EAST_ISLE_W"))) %>% # depth criteria not met
  mutate(kg_per_100m2 = kg_per_m2*100)

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg,
                all_of(pred_rock$predictor)) %>% 
  # Remove sites that do not fit criteria
  filter(site != "SW14")  # depth is 8m; minimum 10m

# Prep data
kelp_sp <- prep_focal_data(
  type = "target_status",
  focal_group = "targeted",
  drop_outliers = "no",
  biomass_variable = "kg_per_100m2",
  data = data_kelp,
  regions = c("North", "Central", "N. Channel Islands", "South")
  )

kelp2 <- data2

rock_sp <- prep_focal_data(
  type = "target_status",
  focal_group = "targeted",
  drop_outliers = "no",
  biomass_variable = "weight_kg",
  data = data_rock,
  regions = c("North", "Central", "N. Channel Islands", "South")
)

rock2 <- data2

# data_sp is the aggregated and scaled version
# data2 is the aggregated but NOT scaled version

# Calculate baseline biomass ----------
kelp_center <- attr(kelp_sp$age_at_survey, "scaled:center")
kelp_scale <- attr(kelp_sp$age_at_survey, "scaled:scale")

kelp_baseline <- kelp_sp %>%
  group_by(affiliated_mpa, site_type) %>%
  summarize(baseline_biomass = mean(biomass[age_at_survey == min(age_at_survey)], na.rm = TRUE),
            min_age = round(min(age_at_survey) * kelp_scale + kelp_center, 0)) %>% 
  mutate(mpa_type_median = median(baseline_biomass)) %>% ungroup() 

kelp_sp2 <- kelp_sp %>%
  left_join(kelp_baseline, by = c("affiliated_mpa", "site_type")) %>% 
  filter(min_age < 2) %>% 
  mutate(baseline_category2 = if_else(baseline_biomass > median(baseline_biomass), "High", "Low")) %>% 
  mutate(baseline_category3 = cut(baseline_biomass,
                                 breaks = quantile(baseline_biomass, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE),
                                 labels = c("Low", "Mid", "High"),
                                 include.lowest = TRUE)) %>% 
  mutate(baseline_category4 = case_when(baseline_biomass > mpa_type_median & site_type == "MPA" ~ "MPA > Reference", 
                                        baseline_biomass < mpa_type_median & site_type == "Reference" ~ "MPA > Reference",
                                        baseline_biomass < mpa_type_median & site_type == "MPA" ~ "Reference > MPA", 
                                        baseline_biomass > mpa_type_median & site_type == "Reference" ~ "Reference > MPA"))

test <- kelp_sp2 %>% 
  distinct(site, site_type, affiliated_mpa, baseline_biomass, mpa_type_median, baseline_category4) 

ggplot(data = kelp_sp2, 
       aes(x = age_at_survey * kelp_scale + kelp_center, 
           y = biomass, color = baseline_category2, fill = baseline_category2)) +
  #geom_jitter(alpha = 0.2, size = 1) +
  geom_smooth(method = 'lm') +
  labs(x = "MPA age",
       y = "Biomass (kg per 100m2)", color = NULL, fill = NULL) +
  theme_minimal() + 
  facet_wrap(~site_type)

ggplot(data = kelp_sp2, 
       aes(x = age_at_survey * kelp_scale + kelp_center, 
           y = biomass, color = site_type, fill = site_type)) +
#  geom_jitter(alpha = 0.2, size = 1) +
  geom_smooth(method = 'lm') +
  labs(x = "MPA age",
       y = "Biomass (kg per 100m2)", color = NULL, fill = NULL) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  theme_minimal() + 
  facet_wrap(~baseline_category4)

kelp_baseline_wide <- kelp_baseline %>% 
  dplyr::select(affiliated_mpa, site_type, min_age) %>% 
  pivot_wider(names_from = "site_type", values_from = "min_age") # 13 MPAs measured in year 0 or 1


# Test FYM  --------------------------------------------------------------------------

base_model <- lmer(log_c_biomass ~ site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year), 
                   data = kelp_sp2)

summary(base_model)  



plot(check_outliers(base_model, ID = "site"))
plot(base_model)  
plot(allEffects(base_model, partial.residuals = T), residuals.pch = 19, residuals.cex = 0.2)  


base_biomass <- lmer(log_c_biomass ~ baseline_biomass * site_type * age_at_survey  + 
                       (1|affiliated_mpa/site) + (1|year), 
                     data = kelp_sp2)

summary(base_biomass)
plot(allEffects(base_biomass), x.var = "age_at_survey", multiline = T, confint = list(style = "auto"))



base_biomass2 <- lmer(log_c_biomass ~ site_type * age_at_survey + baseline_biomass * age_at_survey + 
                        (1|region4/affiliated_mpa/site) + (1|year), 
                      data = kelp_sp2)

summary(base_biomass2)

kelp_sp2 <- kelp_sp2 %>% 
  mutate(baseline_category3 = relevel(baseline_category3, ref = "High"))

base_biomass3 <- lmer(log_c_biomass ~ site_type * age_at_survey * baseline_category2 + 
                        (1|affiliated_mpa/site) + (1|year), 
                      data = kelp_sp2)

summary(base_biomass3)



base_biomass_habitat <- lmer(log_c_biomass ~ hard_bottom_25 * site_type + kelp_annual_50 + 
                               depth_mean_250 + depth_cv_100 + 
                               site_type * baseline_biomass * age_at_survey + 
                               (1 | affiliated_mpa) + (1 | year), 
                             data = kelp_sp2)

summary(base_biomass_habitat)



# Habitat Model --------------------------------------------------------------------------

## Test appropriate RE structure -----
# 1. Full model at intermediate scale (250m)
habitat250_rms <- lmer(log_c_biomass ~ hard_bottom_250 * site_type + 
                     kelp_annual_250 * site_type + 
                     depth_mean_250 * site_type + depth_cv_250 * site_type + size_km2 +
                     site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                   data = data_sp) 

VarCorr(habitat250_rms)  # Inspect variance estimates
performance::icc(habitat250_rms, by_group = T) # Pretty low for region

visreg(habitat250_rms, "age_at_survey", by = "site_type", overlay = TRUE)

plot(ggpredict(habitat250_rms, terms = "age_at_survey"))


# 2. Given low variance for mpa:region, test whether region improves fit
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
anova(habitat250_s, habitat250_ms) 
# p depends on whether extreme sites are included
# if they are dropped = p > 0.05
# if they are not dropped = p < 0.05

# 5. Confirm year as RE
habitat250_m2 <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1|site),
                      data = data_sp) 

anova(habitat250_m2, habitat250_s) # p < 0.0 so year is useful

# Conclusion: (1|affiliated_mpa/site) + (1|year)

# 6. Examine if age effect varies by MPA (while including above RE)
# habitat250_m3 <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
#                         kelp_annual_250 * site_type + 
#                         depth_mean_250 * site_type + depth_cv_250 * site_type + 
#                         site_type * age_at_survey + (1 + age_at_survey | affiliated_mpa) + (1 | site:affiliated_mpa) + (1 | year), 
#                       data = data_sp) 
# 
# VarCorr(habitat250_m3) 
# performance::icc(habitat250_m3, by_group = T)  # Intraclass correlation to check variance partitioning
# anova(habitat250_m3, habitat250_ms) # p > 0.05 so does not significantly improve fit

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




# Confirm: Keep the random intercept model with site nested within MPA + year (_ms)




base_model <- lmer(log_c_biomass ~ site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year), 
                   data = data_sp2)


summary(base_model)

base_biomass <- lmer(log_c_biomass ~ site_type * baseline_biomass * age_at_survey + 
                       (1|region4/affiliated_mpa/site) + (1|year), 
                     data = data_sp2)

summary(base_biomass)
plot(allEffects(base_biomass))



base_biomass_habitat <- lmer(log_c_biomass ~ hard_bottom_25 * site_type + kelp_annual_50 + 
                               depth_mean_250 + depth_cv_100 * site_type + 
                               site_type * age_at_survey + 
                               baseline_biomass * age_at_survey + 
                               (1 | affiliated_mpa) + (1 | year), 
                             data = data_sp2)

summary(base_biomass_habitat)

plot(baseline_model)
#cor(data_sp3[, c("baseline_biomass", "hard_bottom_250", "kelp_annual_250", "depth_mean_250", "depth_cv_250")], use = "pairwise.complete.obs")

data_sp3 <- data_sp2 %>%
  mutate(biomass_category = ifelse(baseline_biomass > median(baseline_biomass, na.rm = TRUE), "High (Baseline > Median)", "Low (Baseline < Median)")) %>% 
  mutate(age_at_survey = age_at_survey*attr(data_sp$age_at_survey, "scaled:scale") + attr(data_sp$age_at_survey, "scaled:center")) %>% 
  mutate(baseline_cat = cut(baseline_biomass, breaks = quantile(baseline_biomass, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), 
                            labels = c("Low", "Medium", "High"), include.lowest = TRUE))

ggplot(data_sp3, aes(x = age_at_survey, y = exp(log_c_biomass), color = biomass_category, fill = biomass_category)) +
  #geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "MPA age",
       y = "Biomass (kg per 100m2)",
       color = "Baseline biomass",
       fill = "Baseline biomass") + 
  facet_wrap(~site_type)




habitat500 <- lmer(log_biomass ~ hard_bottom_500 * site_type + 
                     kelp_annual_500 * site_type + 
                     depth_mean_500 * site_type + depth_cv_500 * site_type + 
                site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
             data = data_sp)

habitat250 <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                     kelp_annual_250 * site_type + 
                     depth_mean_250 * site_type + depth_cv_250 * site_type + 
                     site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                   data = data_sp)

habitat100 <- lmer(log_biomass ~ hard_bottom_100 * site_type + 
                     kelp_annual_100 * site_type + 
                     depth_mean_100 * site_type + depth_cv_100 * site_type + 
                     site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                   data = data_sp)

habitat50 <- lmer(log_biomass ~ hard_bottom_50 * site_type + 
                    kelp_annual_50 * site_type + 
                    depth_mean_50 * site_type + depth_cv_50 * site_type + 
                     site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                   data = data_sp)

habitat25 <- lmer(log_biomass ~ hard_bottom_25 * site_type + 
                    kelp_annual_25 * site_type + 
                    depth_mean_25 * site_type + depth_cv_25 * site_type + 
                    site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                  data = data_sp)

habitat_refined <-  lmer(log_biomass ~ hard_bottom_100 * site_type + 
                           depth_mean_100*site_type +
                           site_type * age_at_survey + (1|affiliated_mpa) + (1|year), 
                         data = data_sp)

summary(habitat500) # K, DM, ~STA      If drop site RE: +DCV*, -STA
summary(habitat250) # K, DM, ~STA                       +DCV* +H*, -STA
summary(habitat100) # K, DM, ~STA, H, ~H*               +DCV* +H*, - STA
summary(habitat50)  # K, DM,  STA, H, H*                      +H*, -STA
summary(habitat25)  # K, DM,  STA, H, H*                      +H*, -STA
summary(habitat_refined)

check_model(habitat100,residual_type = "simulated", check = c("ncv", "qq", "normality", "vif"))
effects_list <- allEffects(habitat_refined, partial.residuals = T)

effects_list <- allEffects(habitat100, partial.residuals = T)
effects_list <- allEffects(base_model, partial.residuals = T)
effects_list <- allEffects(baseline_model, xlevels = 50, partial.residuals = T)
effects_list <- allEffects(std_model, xlevels = 50, partial.residuals = T)
effects_list <- allEffects(base_biomass, xlevels = 50, partial.residuals = T)
effects_list <- allEffects(base_biomass2, xlevels = 50, partial.residuals = T)

effects_list <- allEffects(base_biomass_habitat, xlevels = 50, partial.residuals = T)

effect_plots <- lapply(seq_along(effects_list), function(i) {
  data_sp <- kelp_sp2
  effects_data <- as.data.frame(effects_list[[i]])
  x_var <- colnames(effects_data)[which(colnames(effects_data) != "site_type")[1]]
  const <-  min(data_sp$log_c_biomass[data_sp$log_c_biomass > 0])

  # Reverse scaling if available
  center <- attr(data_sp[[x_var]], "scaled:center")
  scale  <- attr(data_sp[[x_var]], "scaled:scale")
  
  if (!is.null(center) && !is.null(scale)) {
    effects_data[[x_var]] <- effects_data[[x_var]] * scale + center
  }
  
  x_var_label <- x_var %>% 
    str_replace_all("_", " ") %>% 
    str_to_sentence() %>% 
    str_replace("cv", "CV") %>% 
    str_replace("\\d+", paste0(str_extract(., "\\d+"), "m"))
  
  show_legend <-(i == length(effects_list)-1)  # Show legend only on the last plot
  
  if (sum(str_detect(colnames(effects_data), "site_type")) > 0) {
    ggplot(effects_data, aes(x = !!sym(x_var))) +
      geom_ribbon(aes(ymin = exp(lower) - const, ymax = exp(upper) - const, fill = site_type),
                  alpha = 0.2, show.legend = show_legend, stat = "identity") +
      geom_line(aes(y = exp(fit) - const, color = site_type), show.legend = show_legend, stat = "identity") +
      scale_color_manual(values = c("#7e67f8", "#e5188b")) +
      scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
      scale_y_continuous(limits = c(0, NA))+
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
                              labels = c("Low", "Medium", "High"), include.lowest = TRUE)) %>%
    group_by(baseline_cat) %>%
    mutate(loess_upper = predict(loess(upper ~ age_at_survey)),
           loess_lower = predict(loess(lower ~ age_at_survey)))
  
  ggplot(effects_baseline, aes(x = !!sym(x_var), y = exp(fit) - const)) +
    geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Low"), 
                aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                    ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                    fill = baseline_cat), stat = "identity", alpha = 0.2) +
    geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Medium"), 
                aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                    ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                    fill = baseline_cat), stat = "identity", alpha = 0.2) +
    geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "High"), 
                aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                    ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                    fill = baseline_cat), stat = "identity", alpha = 0.2) +
    scale_y_continuous(limits = c(0, NA))+
    geom_smooth(aes(color = baseline_cat), se = F) +
    facet_wrap(~baseline_cat) +
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
      scale_y_continuous(limits = c(0, NA))+
      theme_minimal() +
      theme(axis.title.y = if (i != 1) element_blank() else element_text()) +
      my_theme
  }
  
})

# Combine all effect plots and export
wrap_plots(effect_plots, ncol = 2 #length(effect_plots)/2
           ) + 
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


effects_baseline <- effects_data %>%
  mutate(baseline_cat = cut(baseline_biomass, breaks = quantile(baseline_biomass, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), 
                            labels = c("Low", "Medium", "High"), include.lowest = TRUE)) 

ggplot(effects_baseline, aes(x = !!sym(x_var), y = exp(fit) - const)) +
  geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Low" & site_type == "Reference"), 
              aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                  ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                  fill = site_type), stat = "identity", alpha = 0.2) +
  geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Low" & site_type == "MPA"), 
              aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                  ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                  fill = site_type), stat = "identity", alpha = 0.2) +
  geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Medium" & site_type == "Reference"), 
              aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                  ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                  fill = site_type), stat = "identity", alpha = 0.2) +
  geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "Medium" & site_type == "MPA"), 
              aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                  ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                  fill = site_type), stat = "identity", alpha = 0.2) +
  geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "High"& site_type == "Reference"),
              aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                  ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                  fill = site_type), stat = "identity", alpha = 0.2) +
  geom_ribbon(data = effects_baseline %>% filter(baseline_cat == "High"& site_type == "MPA"),
              aes(ymin = exp(predict(loess(upper ~ age_at_survey))) - const, 
                  ymax = exp(predict(loess(lower ~ age_at_survey))) - const, 
                  fill = site_type), stat = "identity", alpha = 0.2) +
  scale_y_continuous(limits = c(0, NA))+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  geom_smooth(aes(color = site_type), se = F) +
  facet_wrap(~baseline_cat) +
  labs(x = "Age at survey", y = "Biomass (kg per 100m2)", color = NULL, fill = NULL) +
  theme_minimal() +
  my_theme



flibrary(tidyverse)
library(lme4)
library(lmerTest)
library(performance)
library(MuMIn)


# Try adding a three-way interaction to see if it's significant?
path <- "analyses/7habitat/output"
habitat <- "kelp"
focal_group <- "targeted"
re_string <- "my"
results_file <- paste(habitat, focal_group, re_string, "models.rds", sep = "_")

data <- readRDS(file.path("~/ca-mpa/analyses/7habitat/output/models", results_file)) 

data_sp <- data$data_sp 


# Define full model with all interactions up to 3-way
full_model <- lmer(
  log_c_biomass ~ (depth_cv_100 + hard_bottom_25 + kelp_annual_50 + 
                     depth_mean_250 + age_at_survey + site_type)^2 + 
    (1 | affiliated_mpa) + (1 | year),
  na.action = "na.fail",
  data = data_sp,
  REML = FALSE  # Use ML for model comparison
)

model_set <- dredge(full_model, evaluate = FALSE,
                    fixed = ~ site_type * age_at_survey)  # Generates model combinations but doesn’t fit them


# Dredge model, keeping site_type and age_at_survey in all models
dredge_results <- dredge(full_model, 
                         fixed = ~ site_type * age_at_survey, 
                         extra = c("R^2", "AICc"))  # Include AICc and R² for ranking

VarCorr(m)



sst_anomaly <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/envr_anomalies_at_mpas.Rds")

sst_anomaly2 <- sst_anomaly %>% 
  group_by(group, mpa_name, mpa_designation, year) %>% 
  summarize(sst_annual_baseline = mean(sst_annual_baseline, na.rm = T),
            sst_annual_obs = mean(sst_annual_obs, na.rm = T),
            sst_monthly_anom = mean(sst_monthly_anom, na.rm = T), .groups = 'drop')

sst_plot <- sst_anomaly2 %>% 
  filter(group == "kelp") %>% 
  filter(mpa_name %in% data_sp$affiliated_mpa) %>% 
  mutate(site_type = case_when(mpa_designation == "ref" ~ "Reference", T~"MPA")) %>% 
  rename(affiliated_mpa = mpa_name) %>% 
  mutate(year = factor(year))

data_sp2 <- data_sp %>% 
  left_join(sst_plot) %>% 
  filter(!str_detect(affiliated_mpa, "matlahuayl smr")) %>% 
  filter(year != '2023')

m3 <- lmer(log_c_biomass ~ depth_cv_250 * site_type + 
             hard_bottom_250  + 
             kelp_annual_250  + 
             depth_mean_250 + 
             sst_annual_obs + 
             site_type * age_at_survey  + 
             (1 | site) + (1 | year),
           na.action = "na.fail",
           data = data_sp2,
           REML = FALSE)

summary(m3)



data_sp$residuals <- residuals(m)

# Use k-means clustering to group MPAs
set.seed(123)  # Ensures reproducibility
clusters <- kmeans(data_sp$residuals, centers = 3)  # Adjust number of clusters as needed

# Add cluster labels to data
data_sp$cluster <- as.factor(clusters$cluster)

# Visualize clusters
ggplot(data_sp, aes(x = age_at_survey, y = residuals, color = cluster)) +
  geom_point(size = 3) +
  theme_minimal() +
  labs(y = "Residual Effect")

mpa_habitat <- data_sp %>% 
  dplyr::select(year, site, site_type, bioregion, region4, affiliated_mpa, kelp_annual_100:soft_bottom_500, -biomass) %>% 
  distinct() %>% 
  group_by(site, site_type, bioregion, region4, affiliated_mpa, across(starts_with("^(hard|soft|depth)"))) %>% 
  summarize(across(where(is.numeric), mean, na.rm = T), .groups = 'drop') %>% 
  group_by(affiliated_mpa, bioregion, region4) %>% 
  summarize(across(where(is.numeric), mean, na.rm = T), .groups = 'drop')


mpa_effects <- ranef(m)$affiliated_mpa

mpa_effects <- rownames_to_column(mpa_effects, "affiliated_mpa")

mpa_effects <- left_join(mpa_effects, mpa_habitat, by = "affiliated_mpa")

mpa_effects <- mpa_effects %>% dplyr::select(-bioregion, -region4)

mpa_corr <- mpa_effects %>% 
  correlate() %>% 
  rearrange()

rplot(mpa_corr %>%  shave()) +
  theme(axis.text.x = element_text(angle = 60, hjust = 1))

mpa_effects_long <- mpa_effects %>%
  pivot_longer(cols = -c(affiliated_mpa, `(Intercept)`), 
               names_to = "habitat_variable", values_to = "habitat_value")

# Plot scatterplots for each habitat variable vs. random effect
ggplot(mpa_effects_long, aes(x = habitat_value, y = `(Intercept)`)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = FALSE, color = "blue", linetype = "dashed") +
  facet_wrap(~habitat_variable, scales = "free_x") +
  theme_minimal() +
  labs(y = "MPA Random Effect (Intercept)", x = "Habitat Variable",
       title = "Relationship Between MPA Effects and Habitat Variables")


cor_results <- cor(mpa_effects[-1], use = "pairwise.complete.obs")  # Exclude MPA name column
plot(cor_results)

# Compute correlations
mpa_corr_values <- mpa_effects %>%
  select(-affiliated_mpa, -all_of(starts_with("soft")), -all_of(starts_with("depth_sd"))) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("habitat_variable") %>%
  filter(habitat_variable != "(Intercept)") %>%  # Keep only habitat correlations
  arrange(desc(abs(`(Intercept)`)))  # Sort by absolute correlation strength

# Plot correlations
ggplot(mpa_corr_values, aes(x = reorder(habitat_variable, abs(`(Intercept)`)), y = `(Intercept)`)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal() +
  labs(y = "Correlation with MPA Random Effect", x = "Habitat Variable",
       title = "Correlation Between Habitat Variables and MPA Effects")

