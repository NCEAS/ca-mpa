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
library(future)
library(furrr)
library(parallel)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R") 
source("analyses/7habitat/code/Step4b_build_habitat_models.R")  

# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures/3way-figures"

pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))
pred_surf_2way <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors_2way.Rds")) #%>% filter(!str_detect(predictors, "aquatic_vegetation"))

# Define subset for modeling (reduced number of columns)
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, cluster_area_km2, age_at_survey,species_code:target_status, 
                assemblage_new, weight_kg, count, kg_per_haul, count_per_haul, 
                all_of(pred_surf$predictor), matches("^aquatic_vegetation_bed"))


# Build Data --------------------------------------------------------------------------
# Provide some of the global variables
habitat <- "surf"
re_string <- "m"
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

scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_surf,
                                 "log_c_biomass", 
                                 intx.terms = "* site_type",
                                 random_effects = random_effects)

scale_table <- scale_selection$formatted_table
scale_table
gtsave(scale_table, file.path(fig.dir, paste("tableSX", habitat, re_string, "habitat_scale.png", sep = "-")))

# Only fit models with the top scales
top_scales <- scale_selection$results %>% janitor::clean_names() %>% 
  filter(delta == 0) %>% 
  pull(model)

pred_surf_filtered <- get_2way_list(pred_surf %>% filter(predictor %in% top_scales), habitat = "surf")
# pred_surf_filtered <- pred_surf_filtered %>% filter(!str_detect(predictors, "aquatic_vegetation"))
pred_surf_3way <- generate_surf_3way(pred_surf %>% filter(predictor %in% top_scales))

# Run The Models -------------------------------------------------------------------------------------

n_workers <- round(parallel::detectCores()/10)
plan(multisession, workers = n_workers)
predictors_df <- pred_surf_3way #pred_surf_filtered
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

saveRDS(list(models_df = models_df, data_sp = data_sp),
        file.path("analyses/7habitat/output/models", "3way",
                  paste(habitat, "filtered", focal_group, re_string, "models.rds", sep = "_")))
 

# Now we have:
# models_df = df with fit metrics from all candidate models
# nesting_results = df with the number of models removed via nesting rule + LRTs
# top_models = list containing refit candidate models (REML = F) for model averaging
# data_sp = data used in modeling

# Next steps:
# Do model averaging and rank predictor importance.





