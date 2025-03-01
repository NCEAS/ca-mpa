# Test model options, cont.

# This script will fit the kelp forest models.

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)
library(lmerTest)
library(glmmTMB)
library(MASS)
library(performance)
library(effects)


rm(list = ls())
gc()

#source("analyses/7habitat/code/Step4_build_habitat_models.R")  # Load the function from the file


# Read Data --------------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_kelp_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_2way.Rds"))

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg:count_per_m2, 
                all_of(pred_kelp$predictor)) 

# Set Focal Details ------------------------------------------------------------------
species <-"SMYS"
regions <- c("Central", "North", "N. Channel Islands")

# Prep Data --------------------------------------------------------------------------

# Filter to the species and regions of interest, convert RE to factors
data1 <- data_kelp %>%
  filter(species_code == species) %>% 
  filter(region4 %in% regions) %>% 
  mutate(year = as.factor(year),
         bioregion = as.factor(bioregion),
         region4 = as.factor(region4),
         affiliated_mpa = as.factor(affiliated_mpa))

# Identify sites where species are infrequently observed
zero_site <- data1 %>%
  group_by(site) %>% 
  summarize(prop_zero = mean(kg_per_m2 == 0)) %>% 
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
  filter(!between(value, -3, 3)) %>% 
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
  mutate_at(vars(grep("^age", names(.), value = TRUE)), scale)

# Scale the kelp within each year (so it's relative to the annual average instead of across all years)
data4 <- data3 %>%
  group_by(year) %>%
  mutate_at(vars(grep("^kelp", names(.), value = TRUE)), scale) %>% ungroup()

# Save final output for the model
data_sp <- data4 %>% 
  # Scale up and round to nearest integer for modeling
  mutate(count_per_100m2 = round(count_per_m2*100)) %>% 
  dplyr::select(year:count_per_m2, count_per_100m2, everything())


# Add a small constant, defined as the minimum value for that species
if ("kg_per_m2" %in% colnames(data_sp)) {
  const <- min(data_sp$kg_per_m2[data_sp$kg_per_m2 > 0], na.rm = TRUE)
  data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_m2*100 + const*100)) # kg per 100m2
} else if ("weight_kg" %in% colnames(data_sp)) {
  const <- min(data_sp$weight_kg[data_sp$weight_kg > 0], na.rm = TRUE)
  data_sp <- data_sp %>% mutate(log_c_biomass = log(weight_kg + const)) # bpue
} else if ("kg_per_haul" %in% colnames(data_sp)) {
  const <- min(data_sp$kg_per_haul[data_sp$kg_per_haul > 0], na.rm = TRUE)
  data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_haul + const)) # bpue
} else {
  data_sp <- data_sp %>% mutate(log_c_biomass = NA_real_)
}


# Create a binary indicator: 1 for zero count, 0 for positive count
data_sp$zero <- ifelse(data_sp$count_per_100m2 == 0, 0, 1)

# Subset the data for positive counts only
data_positive <- subset(data_sp, count_per_100m2 > 0) %>% 
  mutate(log_count_per_100m2 = log(count_per_100m2))

# Define Modata_sp# Define Model ----------------------------------------------------------

response <- "count_per_100m2"
predictors_zero  <- "hard_bottom_250 * site_type + site_type * age_at_survey"
predictors_count <- "hard_bottom_250 * site_type + depth_mean_250 * site_type + depth_cv_250 * site_type + site_type * age_at_survey"

random_effects <- c("affiliated_mpa", "region4", "year")

form.zero  <- as.formula(paste("zero", "~", predictors_zero, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.count <- as.formula(paste(response, "~", predictors_count, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.zi    <- as.formula(paste("~", predictors_zero, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.log   <- as.formula(paste("log_count_per_100m2", "~", predictors_count, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))

# Fit the binary logistic regression (zero vs. positive)
logit_model <- glmer(formula = form.zero,
                     data = data_sp,
                     family = binomial, control = glmerControl(optimizer = "bobyqa"))

# Fit the count model on the positive counts (using negative binomial)
count_model <- glmmTMB(form.count, data = data_positive, family = nbinom2)
gamma_model <- glmmTMB(form.count, data = data_positive, family = Gamma(link = "log"), na.action = na.pass)

# Fit the zero-inflated hurdle model
zi_model <- glmmTMB(form.count, ziformula = form.zi, data = data_sp, family = nbinom2(link = "log"), na.action = na.pass)

summary(logit_model)
summary(count_model)
summary(zi_model)
summary(gamma_model)

check_model(logit_model)
check_model(count_model)
check_model(gamma_model)
check_model(zi_model)
check_model(log_model, residual_type = "simulated", check = c("ncv", "qq", "normality", "vif"))

plot(allEffects(zi_model, partial.residuals = T), confint = T, residuals.pch = 19, residuals.cex = 0.2)
plot(allEffects(count_model, partial.residuals = T),confint = T)
plot(allEffects(logit_model, partial.residuals = T), confint = T)
plot(allEffects(gamma_model, partial.residuals = T), confint = T)

plot(simulateResiduals(gamma_model))
plot(simulateResiduals(zi_model))
plot(simulateResiduals(count_model))

data_sp$fitted <- fitted(zi_model)
data_sp$residuals <- residuals(zi_model)

ggplot(data = data_sp) +
  geom_point(aes(x = fitted, y = residuals, color = region4))

plot(simulateResiduals(zi_model))
