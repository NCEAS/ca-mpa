# Step5 Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the kelp forest models.

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

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R") 
source("analyses/7habitat/code/Step4b_build_habitat_models.R")  


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined")) %>%  filter(!str_detect(predictor, "aquatic_vegetation|soft_bottom"))
pred_kelp_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_2way.Rds")) %>% filter(!str_detect(predictors, "aquatic_vegetation"))

# Define subset for modeling (reduced number of columns)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey, cluster_area_km2,
                species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, 
                all_of(pred_kelp$predictor))  %>% 
  mutate(kg_per_100m2 = kg_per_m2*100) 
  

# Build Data -------------------------------------------------------------------

# Provide some of the global variables
habitat <- "kelp"
re_string <- "msy"
random_effects <- c("affiliated_mpa/site", "year")
regions <- c("North", "Central", "N. Channel Islands", "South")
print(paste0("Starting: ", habitat))
print(paste0("RE Structure: ", paste(random_effects, collapse = ", ")))
focal_group <- "targeted"

# Prep the data for the given analysis
data_sp <- prep_focal_data(
  type = "target_status",
  focal_group = focal_group, 
  drop_outliers = "no",
  biomass_variable = "kg_per_100m2",
  data = data_kelp,
  regions = c("North", "Central", "N. Channel Islands", "South")
)

# Omit aquatic vegetation for kelp:
pred_kelp <- filter(pred_kelp, !str_detect(predictor, "aquatic_vegetation|soft_bottom"))
pred_kelp_2way <- filter(pred_kelp_2way, !str_detect(predictors, "aquatic_vegetation"))

scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_kelp,
                                 "log_c_biomass", 
                                 random_effects = random_effects)

scale_table <- scale_selection$formatted_table
gtsave(scale_table, paste("tableSX", habitat, re_string, "habitat_scale.png"))

# Only fit models with the top scales
top_scales <- scale_selection$results %>% janitor::clean_names() %>% 
  filter(delta == 0) %>% 
  mutate(scale = str_extract(model, "\\d+"))

pred_kelp_filtered <- pred_kelp_2way %>%
  filter(is.na(hard_scale) | hard_scale == top_scales$scale[top_scales$feature == "hard_bottom"]) %>% 
  filter(is.na(kelp_scale) | kelp_scale == top_scales$scale[top_scales$feature == "kelp_annual"]) %>% 
  filter(is.na(depth_mean_scale) | depth_mean_scale == top_scales$scale[top_scales$feature == "depth_mean"]) %>% 
  filter(is.na(depth_cv_scale) | depth_cv_scale == top_scales$scale[top_scales$feature == "depth_cv"]) %>% 
  dplyr::select(predictors, type, model_id)
  

# Run The Models -------------------------------------------------------------------------------------

#n_workers <- round(parallel::detectCores()/5)
n_workers <- 5
plan(multisession, workers = n_workers)
predictors_df <- pred_kelp_filtered
batch_size <- round(length(predictors_df$model_id)/n_workers)
batches <- split(predictors_df, (seq_len(nrow(predictors_df)) - 1) %/% batch_size)

fit_batch <- function(batch_df, data_sp, response, random_effects) {
  purrr::map_dfr(seq_len(nrow(batch_df)), function(i) {
    predictors <- batch_df$predictors[i]
    model_id   <- batch_df$model_id[i]
    fixed      <- paste(response, "~", predictors)
    random     <- paste0("(1 | ", random_effects, ")", collapse = " + ")
    formula    <- as.formula(paste(fixed, "+", random))
    
    out <- tryCatch({
      model <- suppressMessages(suppressWarnings(lmer(formula, 
                                                      data = data_sp, REML = FALSE, 
                                                      control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))))
      
      singular <- if (isSingular(model)) "Singular fit" else "OK"
      
      tibble(
        model_id = model_id,
        formula = paste(deparse(formula), collapse = ""),
        AICc = AICc(model),
        logLik = as.numeric(logLik(model)),
        n = nobs(model),
        singular_status = singular,
        error = NA_character_
      )
    }, error = function(e) {
      tibble(
        model_id = model_id,
        formula = paste(deparse(formula), collapse = ""),
        AICc = NA_real_,
        logLik = NA_real_,
        n = NA_integer_,
        singular_status = NA_character_,
        error = conditionMessage(e)
      )
    })
    
    out
  })
}

results_list <- future_map(
  batches, ~fit_batch(.x,  data_sp, 
                      response = "log_c_biomass", 
                      random_effects = random_effects),
  .options = furrr_options(seed = TRUE))

models_df <- bind_rows(results_list) %>%
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)



saveRDS(list(models_df = models_df, data_sp = data_sp),
        file.path("analyses/7habitat/output/models",
                  paste(habitat, "filtered", focal_group, re_string, "models.rds", sep = "_")))







# OLD BELOW OOPSIES

# Provide some of the global variables
habitat <- "kelp_subset"
re_string <- "msy"
random_effects <- c("affiliated_mpa/site", "year")
regions <- c("North", "Central", "N. Channel Islands", "South")
print(paste0("Starting: ", habitat))
print(paste0("RE Structure: ", paste(random_effects, collapse = ", ")))
focal_group <- "targeted"

# Info about data prep:
#   type == "species" > focal_group == species_code
#   type == "target_status" > focal_group == "targeted", "nontargeted", "all"
#   type == "targeted_vert" will filter for targeted only > focal_group == "Benthic", "Benthopelagic", "Pelagic"

run_model <- function(focal_group){
  
  data_sp <- prep_focal_data(
    type = "target_status",
    focal_group = focal_group, 
    drop_outliers = "no",
    biomass_variable = "kg_per_100m2",
    data = data_kelp,
    regions = c("North", "Central", "N. Channel Islands", "South")
  )
  
  fit_habitat_models(
    data_sp = data_sp,
    focal_group = focal_group,
    predictors_df = pred_kelp_2way,
    random_effects = random_effects,
    path = "analyses/7habitat/output"
  )
  
}


# Plan for parallel execution
group_list <- c("targeted") # type == "target_status"
# group_list <- unique(data_kelp$vertical_zonation)
num_cores <- min(length(group_list), detectCores()/3)  
plan(multisession, workers = num_cores)


# Run models in parallel
future_walk(group_list, run_model)


# Plots ------------------------------------------------------------------------
data_plot <- data2 %>% 
  dplyr::select(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, age_at_survey, target_status, biomass, everything()) %>% 
  pivot_longer(cols = depth_cv_100:soft_bottom_500, names_to = "habitat_variable", values_to = "value") %>% 
  filter(!str_detect(habitat_variable, "depth_sd|soft")) %>% 
  mutate(scale = as.numeric(str_extract(habitat_variable, "\\d+")),
         habitat_type = factor(str_remove(habitat_variable, "_\\d+"))) %>% 
  arrange(desc(habitat_type), scale) %>% 
  mutate(habitat_variable = factor(habitat_variable, levels = unique(habitat_variable)))

# Visualize biomass as a function of each habitat variable 
ggplot(data = data_plot, aes(x = value, y = biomass, color = site_type, fill = site_type)) +
 # geom_point(alpha = 0.2, size = 0.5)+ # shows variation much larger than relationships shown
  geom_smooth(method = "lm")+
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  labs(x = "Value of habitat characteristic",
       y = "Biomass (kg per 100m2)",
       color = NULL, fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  facet_wrap(~habitat_variable, scales = "free")

# Add the adjusted implementation dates (original historical protection)
mpa_adj_implementation <- data_kelp %>% distinct(affiliated_mpa, implementation_year_adj)

data_adj <- data_kelp %>%
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  group_by(year, site, site_type, bioregion, region4, affiliated_mpa, size_km2, implementation_year, implementation_year_adj, age_at_survey,
           target_status, across(matches("^hard|soft|depth|kelp"))) %>%
  summarize(biomass = sum(kg_per_100m2, na.rm = T), .groups = 'drop') %>%
  filter(target_status == "Targeted") %>% 
  mutate(age_at_survey_adj = year - implementation_year_adj)

# Plot the effect of MPA age on biomass for the MLPA ages
ggplot(data = data2, aes(x = age_at_survey, y = biomass, color = region4, fill = region4)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = "lm") +
  theme_minimal() +
  labs(x = "MPA Age", y = "Biomass Density (kg per 100m2)", color = NULL, fill = NULL) +
  facet_wrap(~site_type)

# Plot the effect of MPA age on biomass based on the historical ages
ggplot(data = data_adj, aes(x = age_at_survey_adj, y = biomass, color = region4, fill = region4)) +
  geom_point(size = 1, alpha = 0.2) +
  geom_smooth(method = "glm") +
  theme_minimal() +
  labs(x = "MPA Age", y = "Biomass Density (kg per 100m2)", color = NULL, fill = NULL) +
  facet_wrap(~site_type)

# Examine the relationships between mean depth, depth cv, and kelp

# a) Rarely any kelp beyond mean depth > 20m at 250m scale
a <- ggplot(data = data2, aes(x = depth_mean_250, y = kelp_annual_50, color = site_type)) + 
  geom_point(alpha = 0.2, size = 1, show.legend = F) +
  theme_minimal() +
  labs(x = "Depth mean 250m", y = "Kelp annual 50m", fill = NULL, color = NULL)

# b) Only a few reference sites with depth cv at 100m > 90 (no MPA sites)
b <- ggplot(data = data2, aes(x = depth_cv_100, y = kelp_annual_50, color = site_type)) + 
  geom_point(alpha = 0.2, size = 1, show.legend = F) +
  theme_minimal() +
  labs(x = "Depth CV 100m", y = "Kelp annual 50m", fill = NULL, color = NULL)

# c) CV generally increases with average depth, same in MPA vs reference, but not too correlated
c <- ggplot(data = data2, aes(x = depth_mean_250, y = depth_cv_100, color = site_type)) + 
  geom_point(alpha = 0.2, size = 1, show.legend = F) +
  geom_smooth(method = 'lm') +
  theme_minimal() +
  labs(x = "Depth mean 250m", y =  "Depth CV 100m", fill = NULL, color = NULL)

# d) No REF sites with standardized average annual kelp > 4
d <- ggplot(data = data_sp, aes(x = kelp_annual_50, y = biomass, color = site_type)) + 
  geom_point(alpha = 0.2, size = 1) +
  theme_minimal() +
  labs(x = "Kelp annual 50m", y =  "Biomass", fill = NULL, color = NULL)

library(patchwork)
(a+b)/(c+d) + plot_annotation(tag_levels = 'A')
# Legacy explorations include:
# mpa-region-year: affiliated mpa + region4 + year 


# Define Kelp Species Lists --------------------------------------------------------------------------
# Bioregion version
# sp_kelp <- data_kelp %>%
#   filter(kg_per_m2 > 0) %>%
#   group_by(species_code, sciname, target_status, bioregion) %>%
#   summarize(total_biomass = sum(weight_kg),
#             total_count = sum(count),
#             n_obs = n(), .groups = 'drop') %>%
#   filter(n_obs > 40) %>%
#   pivot_wider(names_from = bioregion, values_from = c(total_biomass, total_count, n_obs)) %>%
#   mutate(south = if_else(total_count_South < 500 | is.na(n_obs_South), 0, 1),
#          central = if_else(total_count_Central < 500 | is.na(n_obs_Central), 0, 1),
#          north = if_else(total_count_North < 500 | is.na(n_obs_North), 0, 1)) %>%
#   filter(!species_code %in% c("SNEB", "GBY", "KGB"))

# Only model within the region they are prevalent in (defined as > 40 site-year observations)
# kelp_all  <- sp_kelp %>% filter(south == 1 & central == 1 & north == 1) %>% pull(species_code)
# kelp_s    <- sp_kelp %>% filter(south == 1 & central == 0 & north == 0) %>% pull(species_code)
# kelp_c    <- sp_kelp %>% filter(south == 0 & central == 1 & north == 0) %>% pull(species_code)
# kelp_n    <- sp_kelp %>% filter(south == 0 & central == 0 & north == 1) %>% pull(species_code)
# kelp_sc   <- sp_kelp %>% filter(south == 1 & central == 1 & north == 0) %>% pull(species_code)
# kelp_nc   <- sp_kelp %>% filter(south == 0 & central == 1 & north == 1) %>% pull(species_code)

# 4 region version
sp_kelp <- data_kelp_subset %>%
  filter(kg_per_m2 > 0) %>%
  group_by(species_code, sciname, target_status, region4) %>%
  summarize(total_count = sum(count),
            n_obs = n(), .groups = 'drop') %>%
  filter(total_count > 20) %>% 
  pivot_wider(names_from = region4, values_from = c(total_count, n_obs)) %>% 
  mutate(ci = if_else(`n_obs_N. Channel Islands` < 100 | is.na(n_obs_South), 0, 1),
         south = if_else(n_obs_South < 150 | is.na(n_obs_South), 0, 1),
         central = if_else(n_obs_Central < 150 | is.na(n_obs_Central), 0, 1),
         north = if_else(n_obs_North < 40 | is.na(n_obs_North), 0, 1)) 

# Channel Islands Separated Version
kelp_s     <- sp_kelp %>% filter(south == 1 & central == 0 & north == 0 & ci == 0) %>% pull(species_code)
kelp_sc    <- sp_kelp %>% filter(south == 1 & central == 1 & north == 0 & ci == 0) %>% pull(species_code)
kelp_n     <- sp_kelp %>% filter(south == 0 & central == 0 & north == 1 & ci == 0) %>% pull(species_code)

kelp_all   <- sp_kelp %>% filter(south == 1 & central == 1 & north == 1 & ci == 1) %>% pull(species_code)
kelp_ci    <- sp_kelp %>% filter(south == 0 & central == 0 & north == 0 & ci == 1) %>% pull(species_code)
kelp_c     <- sp_kelp %>% filter(south == 0 & central == 1 & north == 0 & ci == 0) %>% pull(species_code)
kelp_nc    <- sp_kelp %>% filter(south == 0 & central == 1 & north == 1 & ci == 0) %>% pull(species_code)
kelp_sci   <- sp_kelp %>% filter(south == 1 & central == 0 & north == 0 & ci == 1) %>% pull(species_code)
kelp_cci   <- sp_kelp %>% filter(south == 0 & central == 1 & north == 0 & ci == 1) %>% pull(species_code)
kelp_scci  <- sp_kelp %>% filter(south == 1 & central == 1 & north == 0 & ci == 1) %>% pull(species_code)
kelp_ncci  <- sp_kelp %>% filter(south == 0 & central == 1 & north == 1 & ci == 1) %>% pull(species_code)



# Fit models for each species --------------------------------------------------------------------


# Combine all species into a single list
#species_list <- c(kelp_all, kelp_s, kelp_c, kelp_n, kelp_sc, kelp_nc) 
#species_list <- c(kelp_all, kelp_s, kelp_c, kelp_n, kelp_sc, kelp_nc, kelp_ci, kelp_sci, kelp_cci, kelp_scci, kelp_ncci) 
species_list <- c("SPUL", "SMIN", "SMYS", "ELAT", "OPIC", "CPRI", "OELO", "SCAR", "SCAU", "EJAC")

# Define a function to process a single species - the 2-way version
run_model <- function(species) {
  # Determine the region
  region <- 
    if (species %in% kelp_all) c("Central", "North", "South", "N. Channel Islands") else
      if (species %in% kelp_s) c("South") else
        if (species %in% kelp_c) c("Central") else
          if (species %in% kelp_n) c("North") else
            if (species %in% kelp_sc) c("South", "Central") else
              if (species %in% kelp_nc) c("North", "Central") else
                if (species %in% kelp_ci) c("N. Channel Islands") else
                  if (species %in% kelp_sci) c("South", "N. Channel Islands") else
                    if (species %in% kelp_cci) c("Central", "N. Channel Islands") else
                      if (species %in% kelp_scci) c("South", "Central", "N. Channel Islands") else
                        if (species %in% kelp_ncci) c("North", "Central", "N. Channel Islands") else
                          stop("Species not found in any region group")
  
  random_effects <- if(length(region) > 1) c("year", "region4", "affiliated_mpa") else c("year", "affiliated_mpa")
  
  # Run the habitat model
  results_df <- refine_habitat(
    species = species,
    response = "log_c_biomass",
    predictors_df = pred_kelp_2way, 
    random_effects = random_effects,
    data = data_kelp_subset,
    regions = region,
    path = "analyses/7habitat/output/2way-4region/kelp-reduced-rescaled"
  )
  
  return(results_df)
}

# Plan for parallel execution
num_cores <- min(length(species_list), detectCores()/3)  
plan(multisession, workers = num_cores)

# Run models in parallel
future_walk(species_list, run_model)


# Options that have been run so far and their locations:
# 2way-: Two way interactions only 
#   4region: run with n. channel islands as separate region
#   with-depth-comb: includes options for depth_mean + depth_cv in same model (otherwise are separate)
#   rmre: removes the random effect for MPA (still keeps region and year) b/c soaks up lots of variability
#   -reduced: drop sites with outliers in static habitat vars and drop sites where species infrequently observed (< 10% of years)
#   -rescaled: adjust scaling so static vars are scaled to site-level means and kelp is scaled within each year



mod <- top_models$`K100+DCV100+AV250*ST+ST*A`
data_sp$residuals <- residuals(mod, type = "response")

ggplot(data_sp, aes(x = aquatic_vegetation_bed_250, y = residuals)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ region4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

ggplot(data_sp, aes(x = aquatic_vegetation_bed_250, y = log_c_biomass, fill = site_type, color = site_type)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ region4) +
  geom_smooth(method = "lm") +
  theme_minimal()


ggplot(data_sp, aes(x = region4, y = aquatic_vegetation_bed_250, fill = site_type)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Region", y = "Aquatic vegetation (250m)")

ggplot(data_sp, aes(x = kelp_annual_100, y = residuals)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ region4) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  theme_minimal()

ggplot(data_sp, aes(x = region4, y = kelp_annual_100, fill = site_type)) +
  geom_boxplot() +
  theme_minimal() 



