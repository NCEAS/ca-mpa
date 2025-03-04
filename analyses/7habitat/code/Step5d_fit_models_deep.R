# Step5d Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the deep reef models.

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)
library(lmerTest)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step4_build_habitat_models.R")  # Load the function from the file


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
#ltm.dir <- "/Users/lopazanski/Desktop/ltm/update_2024"

pred_deep <- readRDS(file.path("analyses/7habitat/intermediate_data/deep_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_deep_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/deep_predictors_2way.Rds"))

data_deep <- readRDS(file.path(ltm.dir, "combine_tables/deep_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, kg_per_m2,
                all_of(pred_deep$predictor)) %>% 
  mutate(kg_per_100m2 = kg_per_m2*100) %>% 
  filter(!site == "2016-128-MPA") # super deep

# Summarize across all targeted fishes
data1 <- data_deep %>%
  group_by(year, site, site_type, region4, bioregion, affiliated_mpa, age_at_survey, 
           target_status, across(matches("^hard|soft|depth|kelp"))) %>%
  summarize(kg_per_100m2 = sum(kg_per_100m2, na.rm = T), .groups = 'drop') %>% 
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
  filter(prop_zero > 0.9) # drop sites where observed < 10% of years; no zero sites for deep

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
  dplyr::select(!all_of(grep("^depth_sd", names(.), value = TRUE))) %>% 
  distinct(site, across(all_of(grep("^hard|depth", names(.), value = TRUE)))) %>% 
  mutate_at(vars(grep("^hard|depth", names(.), value = TRUE)), scale)

# Remove sites with extreme values in static vars (depth and hard bottom)
extreme_site <- site_static %>%
  pivot_longer(cols = depth_cv_100:hard_bottom_500, names_to = "variable", values_to = "value") %>%
  filter(!between(value, -3.29, 3.29)) %>%
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
  #filter(!site %in% extreme_site$site) %>% # keep for now; reasonable
  #filter(!affiliated_mpa %in% extreme_site_balance$affiliated_mpa) %>% 
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

rm(list = setdiff(ls(), c("data_sp", "ltm.dir")))

## Test appropriate RE structure -----
# 1. Not enough structure to do region/MPA/site:
habitat250_rm <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                         depth_cv_250 * site_type + depth_mean_250 * site_type + 
                         site_type * age_at_survey + (1|region4/affiliated_mpa) + (1|year),
                       data = data_sp) 


VarCorr(habitat250_rm)

# 2. See if region is necessary
habitat250_m <- lmer(log_biomass ~ hard_bottom_250 * site_type + 
                        depth_cv_250 * site_type + depth_mean_250 * site_type + 
                        site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                      data = data_sp) 

VarCorr(habitat250_m)
anova(habitat250_m, habitat250_rm) # p = 0.08 so likely okay to drop region


# 3. Build the general model

m <- lmer(log_biomass ~ hard_bottom_500  +  depth_mean_50 + 
             site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
           data = data_sp) 
summary(m)
plot(allEffects(m, partial.residuals = T))

hist(data_sp$hard_bottom_500)

# 4. Build the baseline model

mpa_baseline <- data_sp %>%
  group_by(affiliated_mpa, site_type) %>%
  summarize(baseline_biomass = mean(log_biomass[age_at_survey == min(age_at_survey)], na.rm = TRUE))

data_sp2 <- left_join(data_sp, mpa_baseline, by = c("affiliated_mpa", "site_type"))


baseline_model <- lmer(log_biomass ~ hard_bottom_50 * site_type + 
                         depth_mean_250 +
                         site_type * age_at_survey + 
                         baseline_biomass * age_at_survey + 
                         (1 | affiliated_mpa) + (1 | year), 
                       data = data_sp2)

summary(baseline_model)

plot(baseline_model)

data_sp3 <- data_sp2 %>%
  mutate(biomass_category = ifelse(baseline_biomass > median(baseline_biomass, na.rm = TRUE), "High (Baseline > Median)", "Low (Baseline < Median)")) %>% 
  mutate(age_at_survey = age_at_survey*attr(data_sp$age_at_survey, "scaled:scale") + attr(data_sp$age_at_survey, "scaled:center")) %>% 
  mutate(baseline_cat = cut(baseline_biomass, breaks = quantile(baseline_biomass, probs = c(0, 0.33, 0.66, 1), na.rm = TRUE), 
                            labels = c("Low", "Medium", "High"), include.lowest = TRUE))

ggplot(data_sp3, aes(x = age_at_survey, y = exp(log_biomass), color = biomass_category, fill = biomass_category)) +
  #geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "MPA age",
       y = "Biomass (kg per 100m2)",
       color = "Baseline biomass",
       fill = "Baseline biomass") + 
  facet_wrap(~site_type)




## Define Species Lists ------------------------------------------
sp_deep <- data_deep %>%
  filter(weight_kg > 0) %>%
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(weight_kg),
            total_count = sum(count),
            n_obs = n(), .groups = 'drop') %>%
  filter(n_obs > 40) %>% 
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
  mutate(south = if_else(n_obs_South < 40 | is.na(n_obs_South), 0, 1),
         central = if_else(n_obs_Central < 40 | is.na(n_obs_Central), 0, 1),
         north = if_else(n_obs_North < 40 | is.na(n_obs_North), 0, 1))


# Only model within the region they are prevalent in (defined as > 40 site-year observations)
deep_all <- sp_deep %>% filter(south == 1 & central == 1 & north == 1) %>% pull(species_code)
deep_s   <- sp_deep %>% filter(south == 1 & central == 0 & north == 0) %>% pull(species_code)
deep_c   <- sp_deep %>% filter(south == 0 & central == 1 & north == 0) %>% pull(species_code)
deep_n   <- sp_deep %>% filter(south == 0 & central == 0 & north == 1) %>% pull(species_code)
deep_sc  <- sp_deep %>% filter(south == 1 & central == 1 & north == 0) %>% pull(species_code)
deep_nc  <- sp_deep %>% filter(south == 0 & central == 1 & north == 1) %>% pull(species_code)


# Define subset for modeling (reduced number of columns) 
data_deep_subset <- data_deep %>%
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg, count, log_bpue_kg,
                all_of(pred_deep$predictor))

# Fit models for each species --------------------------------------------------------------------
library(furrr)
library(parallel)

# Combine all species into a single list
species_list <- c(deep_all, deep_s, deep_c, deep_n, deep_sc, deep_nc)

# Define a function to process a single species
run_model <- function(species) {
  # Determine the region
  region <- 
    if (species %in% deep_all) c("Central", "North", "South") else
      if (species %in% deep_s) c("South") else
        if (species %in% deep_c) c("Central") else
          if (species %in% deep_n) c("North") else
            if (species %in% deep_sc) c("South", "Central") else
              if (species %in% deep_nc) c("North", "Central") else
                stop("Species not found in any region group")
  
  # Run the habitat model
  results_df <- refine_habitat(
    species = species,
    response = "log_c_biomass",
    predictors_df = pred_deep_int,
    random_effects = ifelse(length(region) > 1, 
                            c("year", "bioregion", "affiliated_mpa"), 
                            c("year", "affiliated_mpa")),
    data = data_deep_subset,
    regions = region,
    path = "analyses/7habitat/output/deep"
  )
  
  return(results_df)
}

# Plan for parallel execution
num_cores <- min(length(species_list), detectCores()/3)  
plan(multisession, workers = num_cores)

# Run models in parallel
future_walk(species_list, run_model)


