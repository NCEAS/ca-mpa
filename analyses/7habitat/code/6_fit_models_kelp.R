# Step5 Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the kelp forest models.

library(tidyverse)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(lmerTest)
library(furrr)
library(parallel)
library(gt)
library(corrr)
library(glmmTMB)

rm(list = ls())
gc()

source("analyses/7habitat/code/helper_functions.R") 
source("analyses/7habitat/code/3_prep_focal_data.R") 
source("analyses/7habitat/code/5_scale_selection.R") 

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"
fig.dir <- "analyses/7habitat/figures"
habitat <- "kelp"

# Read Data --------------------------------------------------------------------

# Define subset for modeling (reduced number of columns)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey, 
                species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, kg_per_100m2,
                starts_with("hard"), starts_with("kelp"), starts_with("depth"),
                starts_with("tri"), starts_with("slope"), starts_with("relief")) 

# Define predictors and scales
pred_kelp <- data.frame(predictor = grep("^(hard|kelp|depth|tri|slope|relief)", names(data_kelp),  value = TRUE)) %>%  
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
  # Drop TRI because correlated with CV, drop slope SD because not seeming more useful than CV:
  filter(!str_detect(predictor, "tri")) %>% 
  filter(!str_detect(predictor, "slope_mean"))

# Build Data -------------------------------------------------------------------

# Prep the data for the given analysis
data_sp <- prep_focal_data(
  focal_group = "targeted", 
  drop_outliers = "no",
  biomass_variable = "kg_per_100m2",
  data = data_kelp,
  regions = c("North", "Central", "N. Channel Islands", "South")
)

# Provide some of the global variables
random_effects <- c("region4/affiliated_mpa/site", "year")
re_string <- create_re_string(random_effects)

# Run univariate scale selection
scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_kelp,
                                 response = "log_c_biomass", # log c biomass for gauss_log
                                 intx.terms = "",
                                 random_effects = random_effects) 

scale_table <- scale_selection$formatted_table
scale_table

# This yields singular fits for the bathy variables for region - likely
# because REML = F and there is some correlation between depth + region
# (though initial explorations showed likely not problematic enough to change)

# Decide to keep region as random effect because we want to differentiate the habitat
# effects from the region-specific differences.

gtsave(scale_table, file.path(fig.dir, paste("tableSX", habitat, re_string, "habitat_scale.png", sep = "-")))

# Only fit models with the top scales
top_scales <- scale_selection$results %>% janitor::clean_names() %>% 
  filter(delta == 0) %>% 
  pull(model)

# Check correlations
data_corr <- data_sp %>% 
  select(all_of(top_scales)) %>% select(!starts_with("kelp")) %>% distinct() %>% 
  correlate() %>% 
  stretch() %>% 
  filter(!x == y) %>% 
  filter(as.character(x) <= as.character(y))

ggplot(data = data_corr  %>% 
         mutate(across(where(is.character), ~ str_replace_all(.x, "_", " "))), 
       aes(x = x, y = y, fill = r)) +
  geom_tile(color = "white") +
  geom_label(aes(label = sprintf("%.2f", r)), fill = NA, label.size = 0) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 1,   size = 10, hjust = 1)) +
  labs(x = NULL, y = NULL) 

corr_terms <- data_corr %>% 
  filter(!between(r, -0.5, 0.5)) %>% 
  mutate(x = condense_terms(x),
         y = condense_terms(y))

predictors_df <- generate_simple_3way(pred_kelp %>% filter(predictor %in% top_scales))

# Reduce predictor set so that corrleated terms do not appear together
predictors_df <- predictors_df %>% 
  filter(!reduce(pmap(corr_terms, ~ str_detect(model_id, ..1) & str_detect(model_id, ..2)), `|`))


# Run The Models -------------------------------------------------------------------------------------

plan(multisession, workers = max(1, parallel::detectCores() %/% 5))
lmer_ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8))

fit_model <- function(row, data_sp, response, random_effects) {
  predictors <- row$predictors[[1]]
  model_id   <- row$model_id[[1]]
  formula <- reformulate(c(predictors, #paste("region4"), 
                           paste0("(1 | ", random_effects, ")")), response)
  
  model <- suppressMessages(suppressWarnings(
    lmer(formula, data = data_sp, REML = FALSE, control = lmer_ctrl)
  ))
  
  tibble::tibble(
    model_id = model_id,
    formula = paste(deparse(formula), collapse = ""),
    AICc = AICc(model),
    logLik = as.numeric(logLik(model)),
    n = nobs(model),
    singular_status = if (isSingular(model)) "Singular fit" else "OK",
    error = NA_character_
  )
}

results <- future_map_dfr(seq_len(nrow(predictors_df)),
                          ~ fit_model(predictors_df[.x, , drop = FALSE], data_sp, "log_c_biomass", random_effects),
                          .options = furrr::furrr_options(seed = TRUE)) %>% 
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)


saveRDS(list(models_df = results, data_sp = data_sp),
        file.path("analyses/7habitat/output/model-set", 
                  paste(habitat, re_string, "models.rds", sep = "_")))

m <- lmer(log_c_biomass ~ hard_bottom_300 + kelp_annual_100 + depth_mean_500 +     depth_cv_100 * site_type * age_at_survey + site_type * age_at_survey +     (1 | region4/affiliated_mpa/site) + (1 | year),
          data = data_sp, REML = T)

summary(m)
eff <- effects::predictorEffects(m, partial.residuals = T)
eff <- effects::allEffects(m, partial.residuals = T)
plot(eff, multiline = T, confint = list(style = 'auto'))
