# Fit the species models
# Cori Lopazanski
# Nov 2024

# This script builds and fits the models across the predictor lists generated in
# the previous step, including interactions with all possible habitat predictors.
# 1. Fit the candidate models
# 2. Extract the AICc and fit information for each
# 3. Save the df used to fit the models

library(tidyverse)
library(glmmTMB)
library(MuMIn)
library(dplyr)
library(tibble)
library(furrr)

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
source("analyses/7habitat/code/Step0_helper_functions.R") 

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
re_string <- "rmsy"
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

rm(list = setdiff(ls(), c("data_sp", "pred_kelp_2way", "focal_group")))

# Run The Models -------------------------------------------------------------------------------------

n_workers <- round(parallel::detectCores()/10)
plan(multisession, workers = n_workers)
predictors_df <- pred_kelp_2way
batch_size <- round(length(predictors_df$model_id)/n_workers)
batches <- split(predictors_df, (seq_len(nrow(predictors_df)) - 1) %/% batch_size)

fit_species_batch <- function(batch_df, data_sp, response, random_effects) {
  purrr::map_dfr(seq_len(nrow(batch_df)), function(i) {
    predictors <- batch_df$predictors[i]
    model_id   <- batch_df$model_id[i]
    fixed      <- paste(response, "~", predictors)
    random     <- paste0("(1 | ", random_effects, ")", collapse = " + ")
    formula    <- as.formula(paste(fixed, "+", random))
    
    out <- tryCatch({
      model <- glmmTMB(formula, data = data_sp,
                       family = tweedie(link = "log"),
                       control = glmmTMBControl(parallel = 1))
      
      vc <- VarCorr(model)
      vc_vals <- unlist(lapply(vc$cond, function(x) attr(x, "stddev")))
      singular <- if (any(vc_vals < 1e-6)) "Singular" else "OK"
      
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

results_list <- future_map(batches,
  ~fit_species_batch(.x, 
                     data_sp, 
                     response = "biomass", 
                     random_effects = c("affiliated_mpa/site", "year")),
  .options = furrr_options(seed = TRUE)
)

models_df <- bind_rows(results_list) %>%
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)

focal_group <-"SPUL"
habitat <- "kelp"
re_string <- "msy"

saveRDS(list(models_df = models_df, data_sp = data_sp), 
        file.path("analyses/7habitat/output/models/species", 
                  paste(habitat, focal_group, re_string, "models.rds", sep = "_")))

# Examine results and fits -------------------------------------------------------------------------------------

delta_threshold <- 2

top_models_df <- models_df %>% 
  filter(delta_AICc <= delta_threshold)

# Start with just refitting the actual top model to see how it looks

focal_model <- top_models_df[1, ]
model_formula <- as.formula(focal_model$formula)

model <- glmmTMB(model_formula, data = data_sp,
                 family = tweedie(link = "log"),
                 control = glmmTMBControl(parallel = 1))

summary(model)
VarCorr(model)
sim_res <- simulateResiduals(model)
plot(sim_res)


# Looks like we should use the nested approach
top_names <- top_models_df$model_id
top_models <- top_models_df %>% 
  mutate(model = map(formula, 
                     ~ glmmTMB(as.formula(.x), 
                               data = data_sp, 
                               family = tweedie(link = "log"), 
                               control = glmmTMBControl(parallel = 1))))

names(top_models$model) <- top_models$model_id

nesting_results <- data.frame(initial = length(top_models$model_id),
                              nesting_rule = NA,
                              LRT = NA,
                              final = NA)

# Check for nested models 
model_set <- model.sel(top_models$model)
nested <- nested(model_set)

top_names <- top_names[nested == FALSE]
top_models <- top_models$model[nested == FALSE]

print(paste("  Nesting rule:", sum(nested)))

nesting_results$nesting_rule <- sum(nested)

# Apply LRT on any remaining nested models
nested <- check_nested_models(top_models) 
top_names <- nested$candidate_list$model
top_models <- top_models[top_names]

nesting_results$LRT <- sum(nested$nested_results$p > 0.05, na.rm = T)
nesting_results$final <- length(top_names)
print(paste("  LRT:", sum(nested$nested_results$p > 0.05, na.rm = T)))
print(paste("    Top models:", length(top_names)))

top_names

summary(top_models$`K250+DM100*ST+DCV100+ST*A`)
sim_res <- simulateResiduals(top_models$`K250+DM100*ST+DCV100+ST*A`)
plot(sim_res)

diagnostics_df <- purrr::map_dfr(names(top_models), function(id) {
  model <- top_models[[id]]
  
  sim <- simulateResiduals(model)
  ks_p    <- testUniformity(sim)$p.value
  disp_p  <- testDispersion(sim)$p.value
  out_p   <- testOutliers(sim)$p.value
  zero_p  <- testZeroInflation(sim)$p.value
  
  broom_out <- broom.mixed::glance(model)
  
  tibble(
    model_id = id,
    ks_p = ks_p,
    dispersion_p = disp_p,
    outlier_p = out_p,
    zero_infl_p = zero_p,
    logLik = broom_out$logLik,
    AICc = broom_out$AICc
  )
})


# Only two pass all tests, #5 and #6
sim_res <- simulateResiduals(top_models$`K500+DM50*ST+DCV100+ST*A`)
plot(sim_res)

sim_res <- simulateResiduals(top_models$`K250*ST+DM25*ST+DCV100+ST*A`)
plot(sim_res)

library(visreg)
visreg(focal_model, "kelp_annual_250", by = "site_type", type = "conditional", overlay = T)
visreg(focal_model, "depth_mean_25", by = "site_type", overlay = T)
visreg(focal_model, "age_at_survey", by = "site_type", overlay = T)

?visreg





