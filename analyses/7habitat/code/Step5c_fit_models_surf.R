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

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R") 
source("analyses/7habitat/code/Step4b_build_habitat_models.R")  

# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
#ltm.dir <- "/Users/lopazanski/Desktop/ltm/update_2024"

pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_surf_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors_2way.Rds"))

# Define subset for modeling (reduced number of columns)
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, cluster_area_km2, age_at_survey,species_code:target_status, 
                assemblage_new, weight_kg, count, kg_per_haul, count_per_haul, 
                all_of(pred_surf$predictor), matches("^aquatic_vegetation_bed"))


# Build Data --------------------------------------------------------------------------
# Provide some of the global variables
habitat <- "surf"
re_string <- "my"
random_effects <- c("affiliated_mpa")
regions <- c("North", "Central", "N. Channel Islands", "South")
print(paste0("Starting: ", habitat))
print(paste0("RE Structure: ", paste(random_effects, collapse = ", ")))
focal_group <- "targeted"

data_sp <- prep_focal_data(
  type = "target_status",
  focal_group = focal_group, 
  drop_outliers = "no",
  biomass_variable = "kg_per_haul",
  data = data_surf,
  regions = c("North", "Central", "N. Channel Islands", "South")
)



# Run The Models -------------------------------------------------------------------------------------

n_workers <- round(parallel::detectCores()/10)
plan(multisession, workers = n_workers)
predictors_df <- pred_surf_2way
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

results_list <- 
  future_map(batches,
             ~fit_batch(.x, 
                        data_sp, 
                        response = "log_c_biomass", 
                        random_effects = c("affiliated_mpa")),
             .options = furrr_options(seed = TRUE)
  )

models_df <- bind_rows(results_list) %>%
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)


re_string <- "m"

saveRDS(list(models_df = models_df, data_sp = data_sp),
        file.path("analyses/7habitat/output/models",
                  paste(habitat, focal_group, re_string, "models.rds", sep = "_")))
 

delta_threshold <- 4

top_models_df <- models_df %>% 
  filter(delta_AICc <= delta_threshold)

# Start with just refitting the actual top model to see how it looks

focal_model <- top_models_df[1, ]
model_formula <- as.formula(focal_model$formula)

model <- lmer(model_formula, data = data_sp, REML = T, 
              control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))

summary(model)
VarCorr(model)
sim_res <- simulateResiduals(model)
plot(sim_res)
effects <- allEffects(model, partial.residuals = T)
plot(effects, multiline = T, confint = list(style = 'auto'))


# Looks like we should use the nested approach
top_names <- top_models_df$model_id
top_models <- top_models_df %>% 
  mutate(model = map(formula, 
                     ~ lmer(as.formula(.x), data = data_sp, REML = FALSE,
                            control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))))

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

summary(top_models[[1]])
sim_res <- simulateResiduals(top_models[[1]])
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



