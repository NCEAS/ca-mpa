# Step5c Run Habitat Models
# Cori Lopazanski
# Jan 2025

# This script will fit the surf zone models.

library(tidyverse)
library(tidymodels)
library(lme4)
library(MuMIn)
library(dplyr)
library(purrr)
library(lmerTest)
library(future)
library(furrr)
library(parallel)
library(corrr)
library(gt)

rm(list = ls())
gc()

source("analyses/7habitat/code/helper_functions.R") 
source("analyses/7habitat/code/5_scale_selection.R") 
source("analyses/7habitat/code/3_prep_focal_data.R")  


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures"
habitat <- "surf"

# Define subset for modeling (reduced number of columns)
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,species_code:target_status, 
                assemblage_new, weight_kg, count, kg_per_haul, 
                starts_with("hard"), starts_with("soft"), starts_with("kelp"),starts_with("aquatic"), starts_with("depth"),
                starts_with("tri"), starts_with("slope"), starts_with("relief")) %>% 
  filter(!affiliated_mpa == "ten mile smr")

# Define predictors and scales
pred_surf <- data.frame(predictor = grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri|slope|relief)", names(data_surf),  value = TRUE)) %>%  
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
  filter(!predictor %in% c("kelp_annual_25", "kelp_annual_50", "kelp_annual_100", # these have many NAs 
                           "hard_bottom_25", # NAs and zeroes
                           "relief_25", "relief_50")) %>% # not enough variability at these scales
  filter(!str_detect(predictor, "tri|slope_mean")) # not included in latest version

# Build Data --------------------------------------------------------------------------

data_sp <- prep_focal_data(
  focal_group = "targeted", 
  drop_outliers = "no",
  biomass_variable = "kg_per_haul",
  data = data_surf,
  regions = c("North", "Central", "N. Channel Islands", "South")
)

random_effects <- c("affiliated_mpa")
re_string <- create_re_string(random_effects)

scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_surf,
                                 "log_c_biomass", 
                                 intx.terms = " + region4", 
                                 random_effects = random_effects)

# No difference in top scales if add *ST interaction
# Some differences in top scales if add *ST*A
scale_table <- scale_selection$formatted_table
scale_table
gtsave(scale_table, file.path(fig.dir, paste("tableSX", habitat, re_string, "habitat-scale.png", sep = "-")))


# Only fit models with the top scales
top_scales <- scale_selection$results %>% janitor::clean_names() %>% 
  filter(delta == 0) %>% 
  pull(model)

# Check correlations
data_corr <- data_sp %>% 
  dplyr::select(all_of(top_scales)) %>% distinct() %>% 
  rename("max_biotic_extent" = starts_with("aquatic")) %>% 
  correlate() %>% 
  stretch() %>% 
  filter(!x == y) %>% 
  filter(as.character(x) <= as.character(y))

ggplot(data = data_corr %>% 
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

predictors_df <- generate_surf_3way(pred_surf %>% filter(predictor %in% top_scales)) 

predictors_df <- predictors_df %>% 
  # Only select one structural complexity metric because they are highly correlated:
  filter(rowSums(across(c("depc", "deps", "trim", "slsd", "reli"), ~!is.na(.))) <=1)

# Run The Models -------------------------------------------------------------------------------------
plan(multisession, workers = max(1, parallel::detectCores() %/% 5))
lmer_ctrl <- lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8))

fit_model <- function(row, data_sp, response, random_effects) {
  predictors <- row$predictors[[1]]
  model_id   <- row$model_id[[1]]
  formula <- reformulate(c(predictors, paste("region4"), paste0("(1 | ", random_effects, ")")), response)
  
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
 
m <- lmer(log_c_biomass ~ kelp_annual_300 + depth_mean_25 * site_type +     depth_sd_500 + slope_sd_150 * age_at_survey + site_type *     age_at_survey + region4 + (1 | affiliated_mpa),
          data = data_sp, REML = T)
summary(m)
plot(allEffects(m, partial.residuals = T), multiline = T, confint = list(style = 'auto'))
car::vif(m)

performance::check_model(m)

simres <- simulateResiduals(m)
plot(simres)


plotQQunif(simres) # left plot in plot.DHARMa()
plotResiduals(simres) # right plot in plot.DHARMa()
testOutliers(simres) 


outliers <- data_sp[outliers(simres), ]

data_sp$.res <- resid(m, type = "pearson") # , type = "pearson"
data_sp$.fit <- fitted(m)

ggplot(data_sp, aes(.fit, .res, color = region4)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = 2) +
  labs(x = "Fitted", y = "Residuals", color = "MPA") +
  theme_minimal()


ggplot(data = data2) + geom_density(aes(x = aquatic_vegetation_bed_50))
