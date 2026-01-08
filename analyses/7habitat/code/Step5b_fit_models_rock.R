# Step5b Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the rocky reef models.

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(furrr)
library(parallel)
library(glmmTMB)
library(gt)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R")  

# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"
fig.dir <- "analyses/7habitat/figures"

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2,  age_at_survey,
                species_code:target_status, assemblage_new, weight_kg,
                starts_with("hard"), starts_with("kelp"), starts_with("depth"),
                starts_with("tri"), starts_with("slope")) %>% 
  select(-kelp_annual_25)

# Define predictors and scales
pred_rock <- data.frame(predictor = grep("^(hard|kelp|depth|tri|slope)", names(data_rock),  value = TRUE)) %>%  
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
  # Drop TRI because correlated with CV, drop slope SD because not seeming more useful than CV:
  filter(!str_detect(predictor, "tri")) %>% 
  filter(!str_detect(predictor, "slope"))

# Build Data -------------------------------------------------------------------

# Provide some of the global variables
habitat <- "rock"
re_string <- "rmsy"
random_effects <- c("region4/affiliated_mpa/site", "year")
regions <- c("North", "Central", "N. Channel Islands", "South")

data_sp <- prep_focal_data(
  focal_group = "targeted", 
  drop_outliers = "no",
  biomass_variable = "weight_kg",
  data = data_rock,
  regions = c("North", "Central", "N. Channel Islands", "South")
)

scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_rock,
                                 "log_c_biomass", # log_c_biomass for gauss
                                 intx.terms = "* site_type", # for the interaction with the habitat variable
                                 random_effects = random_effects)

scale_table <- scale_selection$formatted_table
scale_table

gtsave(scale_table, file.path(fig.dir, paste("tableSX", habitat, re_string, "habitat_scale.png", sep = "-")))

# Only fit models with the top scales
top_scales <- scale_selection$results %>% janitor::clean_names() %>% 
  filter(delta == 0) %>% 
  pull(model)

predictors_df <- generate_simple_3way(pred_rock %>% filter(predictor %in% top_scales))

# Run The Models -------------------------------------------------------------------------------------

n_workers <- round(parallel::detectCores()/10)
plan(multisession, workers = n_workers)
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
      
      # model <- suppressMessages(suppressWarnings(glmmTMB(as.formula(formula), data = data_sp, 
      #                                                    family = tweedie(link = "log"),
      #                                                    control = glmmTMBControl(optCtrl = list(iter.max = 1e8, eval.max = 1e8)))))
      
      singular <- if (isSingular(model)) "Singular fit" else "OK"
      
      # conv_ok <- if (!is.null(model$sdr)) isTRUE(model$sdr$pdHess) else FALSE
      # singular  <- if (!conv_ok) "Convergence issue" else "OK"
      
      tibble(model_id = model_id,
             formula = paste(deparse(formula), collapse = ""),
             AICc = AICc(model),
             logLik = as.numeric(logLik(model)),
             n = nobs(model),
             singular_status = singular,
             error = NA_character_ )
    }, error = function(e) {
      tibble(model_id = model_id,
             formula = paste(deparse(formula), collapse = ""),
             AICc = NA_real_,
             logLik = NA_real_,
             n = NA_integer_,
             singular_status = NA_character_,
             error = conditionMessage(e))
    })
    
    out
  })
}

results_list <- future_map(batches, ~fit_batch(.x, data_sp, response = "log_c_biomass", 
                                               random_effects = random_effects), .options = furrr_options(seed = TRUE))

models_df <- bind_rows(results_list) %>%
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)

saveRDS(list(models_df = models_df, data_sp = data_sp),
        file.path("analyses/7habitat/output/model-comparison", paste(habitat, re_string, "models.rds", sep = "_")))


m <- glmmTMB(biomass ~ site_type * age_at_survey +     (1 | region4/affiliated_mpa/site) + (1 | year), 
             data = data_sp, family = tweedie("log"))

m <- glmmTMB(biomass ~ site_type * age_at_survey +     (1 | region4/affiliated_mpa/site) + (1 | year), 
             data = data_sp, family = gamma("log"))

m <- lmer(biomass ~ site_type * age_at_survey +  (1 | region4/affiliated_mpa) + (1 | year), 
             data = data_sp)

dh <- simulateResiduals(m)      
plot(dh)                                    # full diagnostic panel
plotResiduals(dh, predict(m, type="response")) # residuals vs fitted
plotResiduals(dh, data_sp$site_type)      # residuals vs site
plotResiduals(dh, data_sp$age_at_survey) # residuals vs covariate

