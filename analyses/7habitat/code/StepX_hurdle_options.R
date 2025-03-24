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
library(DHARMa)


rm(list = ls())
gc()

source("analyses/7habitat/code/Step4a_prep_focal_data.R")  # Load the function from the file
source("analyses/7habitat/code/Step4b_build_habitat_models.R")  # Load the function from the file


# Read Data --------------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_kelp_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_2way.Rds"))

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey, lat_dd, sst_annual_obs, sst_monthly_anom,
                species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, 
                all_of(pred_kelp$predictor))  %>% 
  mutate(kg_per_100m2 = kg_per_m2*100) 


# Set Focal Details ------------------------------------------------------------------

focal_group <-"SPUL"
habitat <- "kelp"
re_string <- "msy"
random_effects <- c("region4/affiliated_mpa/site", "year")
regions <- c("North", "Central", "N. Channel Islands", "South")
print(paste0("Starting: ", habitat))
print(paste0("  RE Structure: ", paste(random_effects, collapse = ", ")))
print(paste0("  Focus: ", focal_group))

data_sp <- prep_focal_data(
  type = "species",
  focal_group = focal_group, 
  drop_outliers = "no",
  biomass_variable = "kg_per_100m2",
  data = data_kelp,
  regions = c("North", "Central", "N. Channel Islands", "South")
)

# Prep Data --------------------------------------------------------------------------

# Identify sites where species are infrequently observed
zero_site <- data2 %>%
  group_by(site, site_type, affiliated_mpa, region4) %>% 
  summarize(prop_zero = mean(kg_per_m2 == 0),
            n = n()) %>% 
  filter(prop_zero > 0.3) # drop sites where observed < 10% of years

# Check MPA/Ref balance of remaining sites 
zero_site_balance <- data2 %>% 
  filter(!site %in% zero_site$site) %>% 
  distinct(site, site_type, affiliated_mpa, year) %>% 
  group_by(affiliated_mpa, site_type) %>% 
  summarize(n_site_year = n(), .groups = 'drop') %>% 
  pivot_wider(names_from = site_type, values_from = n_site_year) %>% 
  filter(is.na(MPA) | is.na(Reference))

data3 <- data2 %>% 
  filter(!site %in% zero_site$site) %>% 
  filter(!affiliated_mpa %in% zero_site_balance$affiliated_mpa)

# Scale the static variables at the site-level (e.g. don't weight based on obs. frequency)
site_static <- data3 %>% 
  distinct(site, across(all_of(grep("^hard|soft|depth", names(.), value = TRUE)))) %>%
  mutate_at(vars(grep("^hard|soft|depth", names(.), value = TRUE)), scale)

data4 <- data3 %>%
  mutate(year = as.factor(year),
         bioregion = as.factor(bioregion),
         region4 = as.factor(region4),
         affiliated_mpa = as.factor(affiliated_mpa)) %>% 
  # Drop un-scaled static variables
  dplyr::select(!c(grep("^hard|soft|depth", names(.), value = TRUE))) %>% 
  # Join the scaled static variables
  left_join(site_static, by = "site") %>% 
  # Scale age
  mutate_at(vars(grep("^age|size", names(.), value = TRUE)), scale) %>% 
  # Scale the kelp within each year (so it's relative to the annual average instead of across all years)
  group_by(year) %>%
  mutate_at(vars(grep("^kelp", names(.), value = TRUE)), scale) %>% ungroup() %>% 
  # Drop 2023 because NAs for SST
  filter(year != '2023')

# Save final output for the model
data_sp <- data4
#rm(data2, data3, data4)

# Add a small constant, defined as the minimum value for that species
const <- if_else(min(data_sp$biomass) > 0, 0, min(data_sp$biomass[data_sp$biomass > 0], na.rm = TRUE))
data_sp <- data_sp %>% mutate(log_c_biomass = log(biomass + const)) %>% 
  mutate(sqrt_biomass = sqrt(biomass))

# Create a binary indicator: 1 for zero count, 0 for positive count
data_sp$zero <- ifelse(data_sp$count_per_m2 == 0, 0, 1)

# Subset the data for positive counts only
data_positive <- subset(data_sp, count_per_m2 > 0) %>% 
  mutate(count_per_100m2 = count_per_m2*100,
         log_count_per_100m2 = log(count_per_100m2))# %>% 
 # filter(count < 3000)

# Plots ------------------------------------------------------------------------
data_plot <- data_sp %>% 
  dplyr::select(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, age_at_survey, target_status, biomass, everything()) %>% 
  pivot_longer(cols = depth_cv_100:soft_bottom_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "depth_sd|soft")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+")),
         habitat_type = factor(str_remove(habitat_variable, "_\\d+"))) %>% 
  arrange(desc(habitat_type), scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable)))

plot_kelp <- data_sp %>% 
  dplyr::select(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, age_at_survey, target_status, biomass, everything()) %>% 
  pivot_longer(cols = kelp_annual_100:kelp_annual_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "depth_sd|soft")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+")),
         habitat_type = factor(str_remove(habitat_variable, "_\\d+"))) %>% 
  arrange(desc(habitat_type), scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable)))

# Visualize biomass as a function of each habitat variable 
ggplot(data = data_plot, aes(x = value, y = biomass, color = site_type, fill = site_type)) +
  #geom_point(alpha = 0.2, size = 0.5)+ # shows variation much larger than relationships shown
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "Value of habitat characteristic",
       y = "Biomass (kg per 100m2)",
       color = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable, scales = "free", ncol = 5)

ggplot(data = plot_kelp, aes(x = value, y = biomass, color = site_type, fill = site_type)) +
  geom_point(alpha = 0.2, size = 0.5)+ # shows variation much larger than relationships shown
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "Value of habitat characteristic",
       y = "Biomass (kg per 100m2)",
       color = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable, scales = "free", ncol = 5)


ggplot(data = data_sp, aes(x = lat_dd, y = biomass, color = site_type, fill = site_type)) +
  geom_point(alpha = 0.2, size = 0.5)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "Latitude",
       y = "Biomass (kg per 100m2)",
       color = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = data_sp, aes(x = as.numeric(year), y = sst_annual_obs, color = site_type, fill = site_type)) +
  geom_point(alpha = 0.2, size = 0.5)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(color = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = data_sp, aes(x = sst_annual_obs, y = biomass, color = site_type, fill = site_type)) +
  geom_point(alpha = 0.2, size = 0.5)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(color = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = data_sp, aes(x = sst_monthly_anom, y = biomass, color = site_type, fill = site_type)) +
  geom_point(alpha = 0.2, size = 0.5)+
  geom_smooth(method = "glm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labscolor = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Define Model ----------------------------------------------------------

predictors_zero  <- "hard_bottom_100 * site_type + kelp_annual_100 * site_type + depth_mean_100 * site_type + depth_cv_100 * site_type + site_type * age_at_survey"
predictors_count <- "hard_bottom_100 * site_type + kelp_annual_100 * site_type + depth_mean_100 * site_type + depth_cv_100 * site_type + site_type * age_at_survey"
random_effects <- c("affiliated_mpa/site", "year")

form.lcb   <- as.formula(paste("log_c_biomass", "~", predictors_count, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.sqrt   <- as.formula(paste("sqrt_biomass", "~", predictors_count, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.zero  <- as.formula(paste("zero", "~", predictors_zero, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.count <- as.formula(paste("count_per_100m2" , "~", predictors_count, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.zi    <- as.formula(paste("~", predictors_zero, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
form.log   <- as.formula(paste("log_count_per_100m2", "~", predictors_count, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))

hist(data_sp$sqrt_biomass)
hist(data_sp$biomass)
hist(data_sp$log_c_biomass)
hist(data3$biomass)

test <- data_sp %>% 
  filter(count < 300)

hist(sqrt(test$biomass))

# Fit the standard GLMER
lmer_model <- lmer(sqrt_biomass ~ site_type * age_at_survey + lat_dd + sst_annual_obs + (1|affiliated_mpa/site) + (1|year),
                   data = data_sp, REML = FALSE,
                   control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

summary(lmer_model)
plot(simulateResiduals(lmer_model))

# Fit the tweedie model
tweedie_model <- glmmTMB(biomass ~ site_type * age_at_survey +
                           (1|region4/affiliated_mpa) + (1|year),
                         data = data_sp, 
                         family = glmmTMB::tweedie(link = "log"))

summary(tweedie_model)
sim.res <- simulateResiduals(tweedie_model)
plot(sim.res)
ggpredict(tweedie_model, terms = c("age_at_survey", "site_type")) %>% plot()

testZeroInflation(sim.res, plot = F)
testDispersion(sim.res, plot = F)
testOutliers(tweedie_model)
plotResiduals(sim.res, data_sp$site_type)
plotResiduals(sim.res, data_sp$age_at_survey)
plotResiduals(sim.res, data_sp$lat_dd)
plotResiduals(sim.res, data_sp$sst_annual_obs)
plotResiduals(sim.res, data_sp$hard_bottom_250)
plotResiduals(sim.res, data_sp$kelp_annual_100)
plotResiduals(sim.res, data_sp$depth_mean_100)
plotResiduals(sim.res, data_sp$depth_cv_250)


tweedie_model <- glmmTMB(biomass ~ kelp_annual_25 * site_type + 
                           hard_bottom_25 * site_type +
                           depth_mean_50 * site_type + 
                           depth_cv_250 * site_type  + 
                           site_type * age_at_survey + 
                           (1|region4/affiliated_mpa) + (1|year),
                         data = data_sp, 
                         family = glmmTMB::tweedie(link = "log"))

summary(tweedie_model)
sim.res <- simulateResiduals(tweedie_model)
plot(sim.res)

ggpredict(tweedie_model, terms = c("age_at_survey", "site_type")) %>% plot()
ggpredict(tweedie_model, terms = c("kelp_annual_100", "site_type")) %>% plot()
ggpredict(tweedie_model, terms = c("depth_mean_50", "site_type")) %>% plot()



library(visreg)
visreg(tweedie_model, "age_at_survey", by = "site_type", overlay = T)

# This maybe has potential - let's try it
rms <- glmmTMB(biomass ~ site_type * age_at_survey +
                (1|region4/affiliated_mpa/site) + (1|year),
              data = data_sp, 
              family = glmmTMB::tweedie(link = "log"))

rm <- glmmTMB(biomass ~ site_type * age_at_survey +
                (1|region4/affiliated_mpa) + (1|year),
              data = data_sp, 
              family = glmmTMB::tweedie(link = "log"))


ms <- glmmTMB(biomass ~ site_type * age_at_survey +
               (1|affiliated_mpa/site) + (1|year),
             data = data_sp, 
             family = glmmTMB::tweedie(link = "log"))

s <- glmmTMB(biomass ~ site_type * age_at_survey +
                (1|site) + (1|year),
              data = data_sp, 
              family = glmmTMB::tweedie(link = "log"))


MuMIn::model.sel(rms, rm, ms, s, rank = AICc)

# You started this at 815pm ish on Thursday 3/20.

fit_habitat_models(
  data = data_sp,
  response = "biomass",
  focal_group = focal_group,
  predictors_df = pred_kelp_2way,
  random_effects = c("affiliated_mpa/site", "year"),
  path = "analyses/7habitat/output"
)




library(mgcv)
library(statmod) # for Tweedie support

gam_tweedie <- gam(
  biomass ~ site_type +
    s(age_at_survey, by = site_type, k = 5) +
    s(sst_annual_obs, k = 5) +
    s(lat_dd, k = 5) +
    s(year, bs = "re") +
    s(affiliated_mpa, bs = "re") +
    s(site, bs = "re"),
  data = data3,
  family = tw(link = "log"),  # Tweedie with log link
  method = "REML",
  select = TRUE  # automatic penalization of unneeded smooths
)

summary(gam_tweedie)
plot(gam_tweedie, pages = 1, shade = T, rug = T)
gam.check(gam_tweedie)
sim.res <- simulateResiduals(gam_tweedie)
plot(sim.res)


gam_tweedie <- gam(
  biomass ~ site_type +
    s(hard_bottom_100, by = site_type, k = 10) +
    s(age_at_survey, by = site_type, k = 10) +
    s(sst_annual_obs, k = 5) +
    s(lat_dd, k = 5) +
    s(year, bs = "re") +
    s(affiliated_mpa, bs = "re") +
    s(site, bs = "re"),
  data = data3,
  family = tw(link = "log"),  # Tweedie with log link
  method = "REML",
  select = TRUE  # automatic penalization of unneeded smooths
)


summary(gam_tweedie)
plot(gam_tweedie, pages = 1, shade = T, rug = T)
gam.check(gam_tweedie)
sim.res <- simulateResiduals(gam_tweedie)
plot(sim.res)



# Fit the binary logistic regression (zero vs. positive)
logit_model <- glmer(formula = zero ~ hard_bottom_100 * site_type + 
                       kelp_annual_100 * site_type + 
                       depth_mean_100 * site_type + 
                       depth_cv_100 * site_type + 
                       site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                     data = data_sp,
                     family = binomial, 
                     control = glmerControl(optimizer = "bobyqa"))

summary(logit_model)
plot(logit_model)

# Fit the count model on the positive counts (using negative binomial)
nbinom_model <- glmmTMB(count_per_100m2 ~ site_type * age_at_survey + (1|affiliated_mpa/site) + (1|year),
                       data = data_positive, 
                       family = nbinom2)
summary(nbinom_model)
sim.res <- simulateResiduals(nbinom_model)
plot(sim.res)

# Fit the gamma model on the positive counts
gamma_model <- glmmTMB(count_per_100m2 ~ site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year),
                       data = data_positive, 
                       family = Gamma(link = "log"))
summary(gamma_model)
plot(simulateResiduals(gamma_model))
plot(allEffects(gamma_model), multiline = T, confint = list(style = 'auto'))

# Fit the zero-inflated hurdle model
zi_model <- glmmTMB(form.count, 
                    ziformula = form.zi, 
                    data = data_sp, 
                    family = nbinom2(link = "log"))

summary(logit_model)
summary(count_model)
summary(zi_model)
summary(gamma_model)

check_model(logit_model)
check_model(count_model)
check_model(gamma_model)
check_model(nbinom_model)

check_model(zi_model)
check_model(log_model, residual_type = "simulated", check = c("ncv", "qq", "normality", "vif"))

plot(allEffects(zi_model, partial.residuals = T), confint = T, residuals.pch = 19, residuals.cex = 0.2)
plot(allEffects(count_model, partial.residuals = T),confint = T)
plot(allEffects(logit_model, partial.residuals = T), confint = T)
plot(allEffects(gamma_model, partial.residuals = T), confint = T)

library(DHARMa)
plot(simulateResiduals(gamma_model))
plot(simulateResiduals(zi_model))
plot(simulateResiduals(count_model))
plot(simulateResiduals(nbinom_model))


sim_out <- simulateResiduals(nbinom_model)
plot(sim_out)

par(mfrow = c(2,3))
plotResiduals(sim_out, data_positive$site_type)
plotResiduals(sim_out, data_positive$kelp_annual_250)
plotResiduals(sim_out, data_positive$depth_mean_50)
plotResiduals(sim_out, data_positive$depth_cv_100)
plotResiduals(sim_out, data_positive$age_at_survey)
plotResiduals(sim_out, data_positive$hard_bottom_500)


data_sp$fitted <- fitted(zi_model)
data_sp$residuals <- residuals(zi_model)

ggplot(data = data_sp) +
  geom_point(aes(x = fitted, y = residuals, color = region4))

plot(simulateResiduals(zi_model))
