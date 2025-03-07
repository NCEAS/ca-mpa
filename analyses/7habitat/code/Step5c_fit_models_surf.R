# Step5c Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the surf zone models.

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

pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_surf_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors_2way.Rds"))

# Define subset for modeling (reduced number of columns)
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,species_code:target_status, 
                assemblage_new, weight_kg, count, kg_per_haul, count_per_haul, 
                all_of(pred_surf$predictor), matches("^aquatic_vegetation_bed"))


# Build Data --------------------------------------------------------------------------

# Summarize across all targeted fishes
data1 <- data_surf %>%
  group_by(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, age_at_survey,
           target_status, across(matches("^hard|soft|depth|kelp|aquatic"))) %>%
  summarize(kg_per_haul = sum(kg_per_haul), .groups = 'drop') %>%
  filter(target_status == "Targeted")

surf_patterns <- data1 %>% 
  pivot_longer(cols = depth_cv_100:aquatic_vegetation_bed_25, names_to = "variable", values_to = "value")

ggplot(data = surf_patterns, aes(x = value, y = kg_per_haul, color = site_type, fill = site_type)) + 
  geom_point(alpha = 0.2) + 
  geom_smooth(method = "lm") + 
  facet_wrap(~variable, scales = "free")

# Scale the static variables at the site-level (e.g. don't weight based on obs. frequency)
site_static <- data1 %>% 
  dplyr::select(!all_of(grep("^depth_sd", names(.), value = TRUE))) %>% 
  distinct(site, across(all_of(grep("^hard|soft|depth|aquatic", names(.), value = TRUE)))) %>% 
  mutate_at(vars(grep("^hard|soft|depth|aquatic", names(.), value = TRUE)), scale)

# # Remove sites with extreme values in static vars (depth and hard bottom)
extreme_site <- site_static %>%
  pivot_longer(cols = depth_cv_100:aquatic_vegetation_bed_25, names_to = "variable", values_to = "value") %>%
  filter(!between(value, -3, 3)) %>%
  pivot_wider(names_from = variable, values_from = value)
# 
# # Check balance of remaining sites (ensure still MPA/Ref pairs)
# extreme_site_balance <- data2 %>%
#   filter(!site %in% extreme_site$site) %>%
#   distinct(site, site_type, affiliated_mpa, year) %>%
#   group_by(affiliated_mpa, site_type) %>%
#   summarize(n_site_year = n(), .groups = 'drop') %>%
#   pivot_wider(names_from = site_type, values_from = n_site_year) %>%
#   filter(is.na(MPA) | is.na(Reference))

data2 <- data1 %>%
  # filter(!site %in% extreme_site$site) %>% 
  # filter(!affiliated_mpa %in% extreme_site_balance$affiliated_mpa) %>% 
  # Drop un-scaled static variables
  dplyr::select(!c(grep("^hard|soft|depth|aquatic", names(.), value = TRUE))) %>% 
  # Join the scaled static variables
  left_join(site_static, by = "site") %>% 
  # Scale age
  mutate_at(vars(grep("^age", names(.), value = TRUE)), scale) %>% 
  # Scale kelp within each year (so relative to annual average instead of across all years)
  group_by(year) %>%
  mutate_at(vars(grep("^kelp", names(.), value = TRUE)), scale) 

# Save final output for the model
const <- if_else(min(data2$kg_per_haul) > 0, 0, min(data2$kg_per_haul[data2$kg_per_haul > 0]))

data_sp <- data2 %>% 
  mutate(log_biomass = log(kg_per_haul + const))

# rm(list = setdiff(ls(), c("data_sp", "my_theme", "pred_kelp_2way")))

# Habitat Model --------------------------------------------------------------------------

## Test appropriate RE structure -----
# 1. Full model at intermediate scale (250m)
habitat250_rms <- lmer(log_biomass ~ soft_bottom_250 * site_type + 
                         kelp_annual_250 * site_type + 
                         depth_mean_250 * site_type + depth_cv_250 * site_type + 
                         site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                       data = data_sp) 

VarCorr(habitat250_rms)  # Inspect variance estimates
performance::icc(habitat250_rms, by_group = T) # Pretty low for site; makes sense because only one per type

# 2. Given low variance for site, test whether it improves fit
habitat250_rm <- lmer(log_biomass ~ soft_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1|region4/affiliated_mpa) + (1|year),
                      data = data_sp) 

VarCorr(habitat250_rm) 
performance::icc(habitat250_rm, by_group = T) 
anova(habitat250_rm, habitat250_rms) # p > 0.05 so likely OK to drop site; top is region/MPA

# 3. Given low variance for MPA:region, see about whether region is needed
habitat250_m <- lmer(log_biomass ~ soft_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1|affiliated_mpa) + (1|year),
                      data = data_sp) 

VarCorr(habitat250_m) # odd, now variance for year is zero; guessing region is useful
anova(habitat250_m, habitat250_rm) # keep region

# 4. Test whether year improves fit
habitat250_rm2 <- lmer(log_biomass ~ soft_bottom_250 * site_type + 
                        kelp_annual_250 * site_type + 
                        depth_mean_250 * site_type + depth_cv_250 * site_type + 
                        site_type * age_at_survey + (1|region4/affiliated_mpa),
                      data = data_sp) 

VarCorr(habitat250_rm2) 
performance::icc(habitat250_rm, by_group = T) 
anova(habitat250_rm2, habitat250_rm) # p > 0.05 so likely OK to drop year 

# Examine best model for general structure

std_model <- lmer(log_biomass ~ soft_bottom_25 + 
                    depth_mean_250 * site_type +
                    site_type * age_at_survey + (1 | affiliated_mpa), 
                  data = data_sp)

summary(std_model)
plot(allEffects(std_model))
performance::check_model(std_model)

## Define Species Lists ------------------------------------------
sp_surf <- data_surf %>%
  filter(weight_kg > 0) %>%
  group_by(species_code, sciname, target_status, bioregion) %>%
  summarize(total_biomass = sum(weight_kg),
            total_count = sum(count),
            n_obs = n(), .groups = 'drop') %>%
  filter(total_count > 30) %>% 
  pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
  mutate(south = if_else(n_obs_South < 5 | is.na(n_obs_South), 0, 1),
         central = if_else(n_obs_Central < 5 | is.na(n_obs_Central), 0, 1),
         north = if_else(n_obs_North < 5 | is.na(n_obs_North), 0, 1))


# Only model within the region they are prevalent in (defined as > 40 site-year observations)
surf_all <- sp_surf %>% filter(south == 1 & central == 1 & north == 1) %>% pull(species_code)
surf_s   <- sp_surf %>% filter(south == 1 & central == 0 & north == 0) %>% pull(species_code)
surf_c   <- sp_surf %>% filter(south == 0 & central == 1 & north == 0) %>% pull(species_code)
surf_n   <- sp_surf %>% filter(south == 0 & central == 0 & north == 1) %>% pull(species_code)
surf_sc  <- sp_surf %>% filter(south == 1 & central == 1 & north == 0) %>% pull(species_code)
surf_nc  <- sp_surf %>% filter(south == 0 & central == 1 & north == 1) %>% pull(species_code)


# Define subset for modeling (reduced number of columns) 
data_surf_subset <- data_surf %>%
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg, count, kg_per_haul, count_per_haul, log_kg_per_haul,
                all_of(pred_surf$predictor))

# Fit models for each species --------------------------------------------------------------------
library(furrr)
library(parallel)

# Combine all species into a single list
species_list <- c(surf_all, surf_s, surf_c, surf_n, surf_sc, surf_nc)

# Define a function to process a single species
run_model <- function(species) {
  # Determine the region
  region <- 
    if (species %in% surf_all) c("Central", "North", "South") else
      if (species %in% surf_s) c("South") else
        if (species %in% surf_c) c("Central") else
          if (species %in% surf_n) c("North") else
            if (species %in% surf_sc) c("South", "Central") else
              if (species %in% surf_nc) c("North", "Central") else
                stop("Species not found in any region group")
  
  # Run the habitat model
  results_df <- refine_habitat(
    species = species,
    response = "log_c_biomass",
    predictors_df = pred_surf_int,
    random_effects = ifelse(length(region) > 1, 
                            c("year", "bioregion", "affiliated_mpa"), 
                            c("year", "affiliated_mpa")),
    data = data_surf_subset,
    regions = region,
    path = "analyses/7habitat/output/surf"
  )
  
  return(results_df)
}

# Plan for parallel execution
num_cores <- min(length(species_list), detectCores()/3)  
plan(multisession, workers = num_cores)

# Run models in parallel
future_walk(species_list, run_model)


