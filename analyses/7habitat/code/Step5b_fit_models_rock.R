# Step5b Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the rocky reef models.

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)
library(lmerTest)
library(furrr)
library(parallel)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step4a_prep_focal_data.R")  # Load the function from the file
source("analyses/7habitat/code/Step4b_build_habitat_models.R")  # Load the function from the file


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_rock_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors_2way.Rds"))

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg,
                all_of(pred_rock$predictor)) %>% 
  # Remove sites that do not fit criteria
  filter(site != "SW14")  # depth is 8m; minimum 10m

# Fit Assemblage Models --------------------------------------------------------

# Plan for parallel execution
group_list <- c("targeted")
num_cores <- min(length(group_list), detectCores()/3)  
plan(multisession, workers = num_cores)

# Provide some of the global variables
habitat <- "rock_post2017"
re_string <- "rmy"
random_effects <- c("region4/affiliated_mpa", "year")
regions = c("North", "Central", "N. Channel Islands", "South")
focal_group <- "targeted"
print(paste0("Starting: ", habitat))
print(paste0("RE Structure: ", paste(random_effects, collapse = ", ")))

run_model <- function(focal_group){
  
  data_sp <- prep_focal_data(
    type = "target_status",
    focal_group = focal_group,
    drop_outliers = "no",
    biomass_variable = "weight_kg",
    data = data_rock,
    regions = c("North", "Central", "N. Channel Islands", "South")
    )
  
  fit_habitat_models(
    data = data_sp,
    predictors_df = pred_rock_2way,
    random_effects = random_effects,
    path = "analyses/7habitat/output"
  )
  
}

# Run models in parallel
future_walk(group_list, run_model)





# Base Model --------------------------------------------------------------------------

base_model <- lmer(log_c_biomass ~ site_type * age_at_survey + age_at_survey * region4 + (1|affiliated_mpa/site) + (1|year), 
                   data = data_sp)

summary(base_model)  

#plot(check_outliers(base_model, ID = "site"))
#plot(base_model)  
#plot(allEffects(base_model, partial.residuals = T), residuals.pch = 19, residuals.cex = 0.2)  


m <- lmer(log_c_biomass ~ hard_bottom_100 * site_type + 
                           depth_cv_500 * site_type + 
                           depth_mean_500 + age_at_survey * region4 + 
                           site_type * age_at_survey + (1|affiliated_mpa/site) + (1|year),
                         data = data_sp) 

summary(m)

ggplot(data_sp, aes(x = age_at_survey, y = biomass, color = region4)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm") +
  facet_wrap(~ site_type) +
  theme_minimal()


mpa_baseline <- data_sp %>%
  group_by(affiliated_mpa, site_type) %>%
  summarize(baseline_biomass = mean(biomass[age_at_survey == min(age_at_survey)], na.rm = TRUE), .groups = 'drop')

m2 <- lmer(log_c_biomass ~ hard_bottom_100 * site_type + 
            depth_cv_500 * site_type + 
            depth_mean_500 + baseline_biomass * age_at_survey +  age_at_survey + region4 + 
            site_type * age_at_survey + (1|affiliated_mpa/site) + (1|year),
          data = data_sp2) 

model.sel(m, m2)
summary(m2)

# Habitat Model --------------------------------------------------------------------------

## Test appropriate RE structure -----
# 1. Full model at intermediate scale (250m)
habitat250_rms <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                         kelp_annual_250 * site_type + 
                         depth_mean_250 * site_type + depth_cv_250 * site_type + 
                         site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                       data = data_sp) 

VarCorr(habitat250_rms)  # Inspect variance estimates
performance::icc(habitat250_rms, by_group = T) # Region is fairly high, year seems low

# 2. See whether site RE is needed; hopefully not b/c is major source of variation in habitat
habitat250_rm <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1|region4/affiliated_mpa) + (1|year),
                      data = data_sp) 

VarCorr(habitat250_rm) 
performance::icc(habitat250_rm, by_group = T) 
anova(habitat250_rm, habitat250_rms) # p < 0.05 so we need site for rock... but soaks lots of variation

# 3. Confirm MPA as RE
habitat250_r <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                       kelp_annual_250 * site_type + 
                       depth_mean_250 * site_type + depth_cv_250 * site_type + 
                       site_type * age_at_survey + (1|region4) + (1|year),
                     data = data_sp) 

VarCorr(habitat250_r) 
performance::icc(habitat250_r, by_group = T) 
anova(habitat250_r, habitat250_rm) # p < 0.05 so MPA is useful

# 4. Confirm year as RE
habitat250_rm2 <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                       kelp_annual_250 * site_type + 
                       depth_mean_250 * site_type + depth_cv_250 * site_type + 
                       site_type * age_at_survey + (1|region4/affiliated_mpa),
                     data = data_sp) 
VarCorr(habitat250_rm2) 
performance::icc(habitat250_rm2, by_group = T)  
anova(habitat250_rm2, habitat250_rm) 

# 5. Confirm region as RE
habitat250_m <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                         kelp_annual_250 * site_type + 
                         depth_mean_250 * site_type + depth_cv_250 * site_type + 
                         site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                       data = data_sp)  
VarCorr(habitat250_m) 
performance::icc(habitat250_m, by_group = T)  
anova(habitat250_m, habitat250_rm)  # close, so this could be okay (p = 0.03)
anova(habitat250_rm, habitat250_ms)

# Lower AIC with region/MPA than with MPA/site, site likely soaks up too much variation

# Confirm Model -------------------------------------------------------------------------

mpa_baseline <- data_sp %>%
  group_by(affiliated_mpa, site_type) %>%
  summarize(baseline_biomass = mean(weight_kg[age_at_survey == min(age_at_survey)], na.rm = TRUE))

data_sp2 <- left_join(data_sp, mpa_baseline, by = c("affiliated_mpa", "site_type"))

m <- lmer(log_biomass ~ hard_bottom_50 * site_type + 
            depth_mean_25 + depth_cv_500 * site_type + 
            site_type * age_at_survey + (1|region4/affiliated_mpa) + (1|year),
          data = data_sp2) 

mb <- lmer(log_biomass ~ hard_bottom_50 * site_type + 
            depth_mean_25 + depth_cv_500 * site_type + 
             baseline_biomass * age_at_survey + 
            site_type * age_at_survey + (1|region4/affiliated_mpa) + (1|year),
          data = data_sp2) 

summary(m)
summary(mb)


data_sp3 <- data_sp2 %>% ungroup() %>% 
  mutate(biomass_category = ifelse(baseline_biomass > median(baseline_biomass, na.rm = TRUE), "High (Baseline > Median)", "Low (Baseline < Median)")) %>% 
  mutate(age_at_survey = age_at_survey*attr(data_sp$age_at_survey, "scaled:scale") + attr(data_sp$age_at_survey, "scaled:center")) %>% 
  mutate(baseline_cat = cut(baseline_biomass, breaks = quantile(baseline_biomass, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), 
                            labels = c("Low", "Medium", "High"), include.lowest = TRUE))

ggplot(data_sp3, aes(x = age_at_survey, y = exp(log_biomass), color = biomass_category, fill = biomass_category)) +
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "MPA age",
       y = "Biomass (kg per 100m2)",
       color = "Baseline biomass",
       fill = "Baseline biomass") + 
  facet_wrap(~site_type)

plot(allEffects(mb, partial.residuals = T))


# Try out some plots:
data_plot <- data1 %>% 
  dplyr::select(year, site, site_type, bioregion, region4, affiliated_mpa, age_at_survey, target_status, weight_kg, everything()) %>% 
  pivot_longer(cols = depth_cv_100:soft_bottom_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "depth_sd|soft")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+")),
         habitat_type = factor(str_remove(habitat_variable, "_\\d+"))) %>% 
  arrange(habitat_type, scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable)))

data_scaled <- data3 %>% 
  dplyr::select(year, site, site_type, bioregion, region4, affiliated_mpa, age_at_survey, target_status, weight_kg, everything()) %>% 
  pivot_longer(cols = kelp_annual_100:hard_bottom_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "depth_sd|soft")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+")),
         habitat_type = factor(str_remove(habitat_variable, "_\\d+"))) %>% 
  arrange(habitat_type, scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable)))


ggplot(data = data_plot, aes(x = value, y = weight_kg, color = site_type, fill = site_type)) +
  geom_point(alpha = 0.2, size = 0.5)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "Value of habitat characteristic",
       y = "Biomass (kg) per unit effort",
       color = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable, scales = "free")

# Show kelp scaled
ggplot(data = data_scaled %>% filter(str_detect(habitat_variable, "kelp")), aes(x = value, y = weight_kg, color = site_type, fill = site_type)) +
  geom_point(alpha = 0.2, size = 0.5)+
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "Value of habitat characteristic",
       y = "Weight (kg)",
       color = NULL, fill = NULL) +
  theme_minimal() + 
  facet_wrap(~habitat_variable, scales = "free", ncol = 4)

ggplot(data = data1, aes(x = year, y = weight_kg, color = site_type, fill = site_type)) +
  geom_jitter(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "Year",
       y = "Weight (kg)",
       color = NULL, fill = NULL) +
  theme_minimal()


a <- ggplot(data = data1, aes(x = age_at_survey, y = weight_kg, color = site_type, fill = site_type)) +
  geom_jitter(alpha = 0.1, size = 0.5, show.legend = F) +
  geom_smooth(method = "loess", show.legend = F)+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "MPA Age",
       y = "Weight (kg)",
       color = NULL, fill = NULL,
       title = "A") +
  theme_minimal() 

b <- ggplot(data = data1, aes(x = age_at_survey, y = weight_kg, color = site_type, fill = site_type)) +
  geom_jitter(alpha = 0.1, size = 0.5) +
  geom_smooth(method = "loess")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "MPA Age",
       y = "Weight (kg)",
       color = NULL, fill = NULL,
       title = "B") +
  theme_minimal() +
  facet_wrap(~region4, ncol = 1)

library(patchwork)
a+b

## Define Species Lists ------------------------------------------
sp_rock <- data_rock %>%
  filter(weight_kg > 0) %>%
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(weight_kg),
            total_count = sum(count),
            n_obs = n(), .groups = 'drop') %>%
  filter(n_obs > 100) %>% 
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
  mutate(south = if_else(n_obs_South < 40 | is.na(n_obs_South), 0, 1),
         central = if_else(n_obs_Central < 40 | is.na(n_obs_Central), 0, 1),
         north = if_else(n_obs_North < 40 | is.na(n_obs_North), 0, 1))


# Only model within the region they are prevalent in (defined as > 40 site-year observations)
rock_all <- sp_rock %>% filter(south == 1 & central == 1 & north == 1) %>% pull(species_code)
rock_s   <- sp_rock %>% filter(south == 1 & central == 0 & north == 0) %>% pull(species_code)
rock_c   <- sp_rock %>% filter(south == 0 & central == 1 & north == 0) %>% pull(species_code)
rock_n   <- sp_rock %>% filter(south == 0 & central == 0 & north == 1) %>% pull(species_code)
rock_sc  <- sp_rock %>% filter(south == 1 & central == 1 & north == 0) %>% pull(species_code)
rock_nc  <- sp_rock %>% filter(south == 0 & central == 1 & north == 1) %>% pull(species_code)


# Define subset for modeling (reduced number of columns) 
data_rock_subset <- data_rock %>%
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg, count, log_bpue_kg,
                all_of(pred_rock$predictor))

# Fit models for each species --------------------------------------------------------------------
library(furrr)
library(parallel)

# Combine all species into a single list
species_list <- c(rock_all, rock_s, rock_c, rock_n, rock_sc, rock_nc)

# Define a function to process a single species
run_model <- function(species) {
  # Determine the region
  region <- 
    if (species %in% rock_all) c("Central", "North", "South") else
      if (species %in% rock_s) c("South") else
        if (species %in% rock_c) c("Central") else
          if (species %in% rock_n) c("North") else
            if (species %in% rock_sc) c("South", "Central") else
              if (species %in% rock_nc) c("North", "Central") else
                stop("Species not found in any region group")
  
  # Run the habitat model
  results_df <- refine_habitat(
    species = species,
    response = "log_c_biomass",
    predictors_df = pred_rock_2way,
    random_effects = ifelse(length(region) > 1, 
                            c("year", "bioregion", "affiliated_mpa"), 
                            c("year", "affiliated_mpa")),
    data = data_rock_subset,
    regions = region,
    path = "analyses/7habitat/output/2way/rock"
  )
  
  return(results_df)
}

# Plan for parallel execution
num_cores <- min(length(species_list), detectCores()/3)  
plan(multisession, workers = num_cores)

# Run models in parallel
future_walk(species_list, run_model)


