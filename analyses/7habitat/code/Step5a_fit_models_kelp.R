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
library(gt)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R") 
source("analyses/7habitat/code/Step4b_build_habitat_models.R")  

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
fig.dir <- "analyses/7habitat/figures/3way-figures"

# Read Data --------------------------------------------------------------------
pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined")) %>%  filter(!str_detect(predictor, "aquatic_vegetation|soft_bottom"))

# Define subset for modeling (reduced number of columns)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey, cluster_area_km2,
                species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, kg_per_100m2,
                all_of(pred_kelp$predictor)) 

# Build Data -------------------------------------------------------------------

# Provide some of the global variables
habitat <- "kelp"
re_string <- "my"
random_effects <- c("affiliated_mpa", "year")
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

# Run univariate scale selection
scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_kelp,
                                 "log_c_biomass", 
                                 intx.terms = "",
                                 random_effects = c("affiliated_mpa", "year")) # include site here to account for other habitat differences

#scale_results <- scale_selection$results
scale_table <- scale_selection$formatted_table
scale_table

gtsave(scale_table, file.path(fig.dir, paste("tableSX", habitat, re_string, "habitat_scale.png", sep = "-")))

# Only fit models with the top scales
top_scales <- scale_selection$results %>% janitor::clean_names() %>% 
  filter(delta == 0) %>% 
  pull(model)

pred_kelp_3way <- generate_simple_3way(pred_kelp %>% filter(predictor %in% top_scales))
#pred_kelp_3way <- generate_simple_3way(pred_kelp)
#pred_kelp_filtered <- get_2way_list(pred_kelp %>% filter(predictor %in% top_scales), habitat = "kelp")


# Run The Models -------------------------------------------------------------------------------------

n_workers <- round(parallel::detectCores()/5)
n_workers <- 5
plan(multisession, workers = n_workers)
predictors_df <- pred_kelp_3way #pred_kelp_filtered
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
      
      tibble(model_id = model_id,
             formula = paste(deparse(formula), collapse = ""),
             AICc = AICc(model),
             logLik = as.numeric(logLik(model)),
             n = nobs(model),
             singular_status = singular,
             error = NA_character_)
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

results_list <- future_map(
  batches, ~fit_batch(.x,  data_sp, 
                      response = "log_c_biomass", 
                      random_effects = random_effects),
  .options = furrr_options(seed = TRUE))

models_df <- bind_rows(results_list) %>%
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)


saveRDS(list(models_df = models_df, data_sp = data_sp),
        file.path("analyses/7habitat/output/models", "3way", # remove 3way for 2way results
                  paste(habitat, "filtered", focal_group, re_string, "models.rds", sep = "_")))




