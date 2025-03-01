# Refine preferred habitat type for each species
# Cori Lopazanski
# Nov 2024

# This script builds and fits the models across the predictor lists generated in
# the previous step, including interactions with all possible habitat predictors.
# 1. Fit and save all of the candidate models
# 2. Extract the following focal models for each species:
#     -- Top models within 4 AICc of the model with the lowest AICc
#     -- The base model (site type * age at survey only)
#     -- The full models at each scale (all predictors with interactions)

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(tidymodels)
library(lmerTest)

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_kelp_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors_2way.Rds"))

# Prep Data -----

# Load data and relevant variables
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,
                species_code:target_status, assemblage_new, weight_kg:count_per_m2, 
                all_of(pred_kelp$predictor)) %>% 
  filter(!affiliated_mpa == "point dume smca") # remove bc spatial autocorrelation

# Summarize across all targeted fishes
data1 <- data_kelp %>%
  group_by(year, site, site_type, bioregion, region4, affiliated_mpa, age_at_survey,
           target_status, across(matches("^hard|soft|depth|kelp"))) %>%
  summarize(kg_per_100m2 = sum(kg_per_m2, na.rm = T)*100,
            count_per_100m2 = sum(count_per_m2, na.rm = T)*100, .groups = 'drop') %>%
  filter(target_status == "Targeted")

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
# extreme_site <- site_static %>% 
#   pivot_longer(cols = depth_cv_100:hard_bottom_500, names_to = "variable", values_to = "value") %>% 
#   filter(!between(value, -3.5, 3.5)) %>% 
#   pivot_wider(names_from = variable, values_from = value)

# Check balance of remaining sites (ensure still MPA/Ref pairs)
# extreme_site_balance <- data2 %>% 
#   filter(!site %in% extreme_site$site) %>% 
#   distinct(site, site_type, affiliated_mpa, year) %>% 
#   group_by(affiliated_mpa, site_type) %>% 
#   summarize(n_site_year = n(), .groups = 'drop') %>% 
#   pivot_wider(names_from = site_type, values_from = n_site_year) %>% 
#   filter(is.na(MPA) | is.na(Reference))

data3 <- data2 %>%
 # filter(!site %in% extreme_site$site) %>% 
 # filter(!affiliated_mpa %in% extreme_site_balance$affiliated_mpa) %>% 
  # Drop un-scaled static variables
  dplyr::select(!c(grep("^hard|soft|depth", names(.), value = TRUE))) %>% 
  # Join the scaled static variables
  left_join(site_static, by = "site") %>% 
  # Scale age
  mutate_at(vars(grep("^age", names(.), value = TRUE)), scale) %>% 
  # Scale kelp within each year (so relative to annual average instead of across all years)
  group_by(year) %>%
  mutate_at(vars(grep("^kelp", names(.), value = TRUE)), scale) %>% ungroup()

# Save final output for the model
data_sp <- data3 

rm(list = setdiff(ls(), c("data_sp", "pred_kelp_2way")))

refine_habitat <- function(species, response, predictors_df, random_effects, data, regions, path) {
  
  if ("kg_per_100m2" %in% colnames(data_sp)) {
    const <- if_else(min(data_sp$kg_per_100m2) > 0, 0, min(data_sp$kg_per_100m2[data_sp$kg_per_100m2 > 0], na.rm = TRUE))
    data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_100m2 + const)) # kg per 100m2
  } else if ("weight_kg" %in% colnames(data_sp)) {
    const <- min(data_sp$weight_kg[data_sp$weight_kg > 0], na.rm = TRUE)
    data_sp <- data_sp %>% mutate(log_c_biomass = log(weight_kg + const)) # bpue
  } else if ("kg_per_haul" %in% colnames(data_sp)) {
    const <- min(data_sp$kg_per_haul[data_sp$kg_per_haul > 0], na.rm = TRUE)
    data_sp <- data_sp %>% mutate(log_c_biomass = log(kg_per_haul + const)) # bpue
  } else {
    data_sp <- data_sp %>% mutate(log_c_biomass = NA_real_)
  }
  
  models <- list()
  
  models_df <- map_dfr(seq_len(nrow(predictors_df)), function(i) {
    predictors <- predictors_df$predictors[i]
    model_id <- predictors_df$model_id[i]
    
    model_formula <- as.formula(paste(response, "~", predictors, "+", paste0("(1 | ", random_effects, ")", collapse = " + ")))
    warning_message <- NULL
    message_text <- NULL
    error_message <- NULL
    singular_status <- "Unknown"
    
    model <- suppressWarnings(
      tryCatch(
        {
          withCallingHandlers(
            { m <- lmer(model_formula, data = data_sp,
                        control = lmerControl(optCtrl = list(maxfun = 1e7)))
            singular_status <- if (isSingular(m)) "Singular fit" else "OK"
            m
            },
            warning = function(w) {
              warning_message <<- conditionMessage(w) # Capture warning message
              invokeRestart("muffleWarning") # Suppress the warning display
            },
            message = function(msg) {
              message_text <<- conditionMessage(msg) # Capture message
              invokeRestart("muffleMessage") # Suppress message display
            }
          )
        },
        error = function(e) {
          error_message <<- conditionMessage(e) # Capture error message
          return(NULL) # Return NULL for the model on error
        }
      )
    )
    
    models[[model_id]] <<- model
    
    data.frame(model_id = model_id,
               predictors = gsub("\\s*\\+\\s*", " + ", predictors),
               response = response,
               regions = paste(regions, collapse = ", "),
               random_effects = paste(random_effects, collapse = ", "),
               AICc = if (!is.null(model)) AICc(model) else NA,
               logLik = if (!is.null(model)) as.numeric(logLik(model)) else NA,
               n = if (!is.null(model)) nobs(model) else NA,
               n_sites = if (!is.null(model)) n_distinct(data_sp$site) else NA,
               n_mpas = if (!is.null(model)) n_distinct(data_sp$affiliated_mpa) else NA,
               singular_status = singular_status,
               errors = ifelse(is.null(error_message), "OK", error_message),
               warnings = ifelse(is.null(warning_message), "OK", warning_message),
               messages = ifelse(is.null(message_text), "OK", message_text))
  }) %>%
    mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
    arrange(delta_AICc) %>% 
    mutate(across(c(singular_status, errors, warnings, messages), ~ if_else(.x == "OK", NA, .x))) %>% 
    unite("messages", c(singular_status, errors, warnings, messages), sep = ";", remove = T, na.rm = T) %>% 
    mutate(messages = if_else(messages == "", NA, messages))
  
  
  # Extract the base model and full models for each scale
  core_model_names <- predictors_df %>%
    filter(type == "base" | str_starts(type, "core")) %>%
    pull(model_id)
  
  # Extract the top models within deltaAICc of 4
  top_model_names <- models_df %>%
    filter(delta_AICc <= 4) %>%
    pull(model_id)
  
  # Extract the model objects for the core + top models
  models <- models[unique(c(top_model_names, core_model_names))]
  
  # Define the top, core, and full models (but save entire df for debug)
  models_df <- models_df %>%
    mutate(type = case_when(model_id %in% top_model_names ~ "top",
                            predictors == "site_type * age_at_survey" ~ "base",
                            model_id %in% core_model_names ~ "core"))
  
  # Save the subset
  saveRDS(list(models_df = models_df, models = models, data_sp = data_sp),
          file = file.path(path, paste0(species, "_models.rds")))
  
  models_df
}

# Explore random effects structure ----

## First tried the most complex RE structure: region/mpa/site + year
## Region had zero variance, after dropping p > 0.05 so examine: mpa/site + year 

# refine_habitat(
#   species = "targeted",
#   response = "log_c_biomass",
#   predictors_df = pred_kelp_2way, 
#   random_effects = c("affiliated_mpa/site", "year"),
#   data = data_sp,
#   regions = c("North", "Central", "N. Channel Islands", "South"),
#   path = "analyses/7habitat/output/targeted/nested.mpa.site-year" 
# )

# MPA had very low variance (nearly zero or zero for many models) after
# accounting for the RE for site, which makes sense, examine: site + year 
# refine_habitat(
#   species = "targeted",
#   response = "log_c_biomass",
#   predictors_df = pred_kelp_2way, 
#   random_effects = c("site", "year"),
#   data = data_sp,
#   regions = c("North", "Central", "N. Channel Islands", "South"),
#   path = "analyses/7habitat/output/targeted/site-year"
# )

# Some concerns that site is soaking up too much of the habitat variation
# so run one that's just MPA + year to compare the key habitat variables
refine_habitat(
  species = "targeted",
  response = "log_c_biomass",
  predictors_df = pred_kelp_2way,
  random_effects = c("affiliated_mpa", "year"),
  data = data_sp,
  regions = c("North", "Central", "N. Channel Islands", "South"),
  path = "analyses/7habitat/output/targeted/mpa-year"
)


# Legacy explorations include:
# mpa-region-year: affiliated mpa + region4 + year 

data <- readRDS(file.path("analyses/7habitat/output/targeted/nested.mpa.site-year", paste0("targeted", "_models.rds"))) 
models <- data$models

summary(models$`H50*ST+K50+DM500*ST+ST*A`)

data_subset <- results_df %>% 
  filter(!str_detect(model_id, "H50"))

summary(models$`H50*ST+K50+DM100*ST+DCV100*ST+ST*A`)

data_sp <- data$data_sp
plot(allEffects(models$`H50*ST+K50+DM500*ST+ST*A`))
