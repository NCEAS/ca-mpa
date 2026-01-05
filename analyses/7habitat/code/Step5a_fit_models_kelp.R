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
library(corrr)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R") 

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"
fig.dir <- "analyses/7habitat/figures"

# Read Data --------------------------------------------------------------------

# Define subset for modeling (reduced number of columns)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey, 
                species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, kg_per_100m2,
                starts_with("hard"), starts_with("kelp"), starts_with("depth"),
                starts_with("tri"), starts_with("slope")) 

# Define predictors and scales
pred_kelp <- data.frame(predictor = grep("^(hard|kelp|depth|tri|slope)", names(data_kelp),  value = TRUE)) %>%  
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
  # Drop TRI because correlated with CV, drop slope SD because not seeming more useful than CV:
  filter(!str_detect(predictor, "tri")) %>% 
  filter(!str_detect(predictor, "slope"))

# Build Data -------------------------------------------------------------------

# Provide some of the global variables
habitat <- "kelp"
re_string <- "rmsy"
random_effects <- c("region4/affiliated_mpa/site", "year")
regions <- c("North", "Central", "N. Channel Islands", "South")

# Prep the data for the given analysis
data_sp <- prep_focal_data(
  focal_group = "targeted", 
  drop_outliers = "no",
  biomass_variable = "kg_per_100m2",
  data = data_kelp,
  regions = c("North", "Central", "N. Channel Islands", "South")
)

# Run univariate scale selection
scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_kelp,
                                 response = "log_c_biomass", 
                                 intx.terms = "* site_type",
                                 random_effects = random_effects) # include site here to account for other habitat differences

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
  filter(as.character(x) <= as.character(y)) %>% 
  mutate(across(where(is.character), ~ str_replace_all(.x, "_", " "))) 

ggplot(data = data_corr, aes(x = x, y = y, fill = r)) +
  geom_tile(color = "white") +
  geom_label(aes(label = sprintf("%.2f", r)), fill = NA, label.size = 0) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 1,   size = 10, hjust = 1)) +
  labs(x = NULL, y = NULL) 

predictors_df <- generate_simple_3way(pred_kelp %>% filter(predictor %in% top_scales))

# Run The Models -------------------------------------------------------------------------------------

n_workers <- round(parallel::detectCores()/5)
n_workers <- 5
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
     # model <- suppressMessages(suppressWarnings(glmmTMB(as.formula(formula), data = data_sp, family = Gamma(link = "log"))))
      
       singular <- if (isSingular(model)) "Singular fit" else "OK"
      #conv_code <- model$fit$optinfo$conv$conv_code
     # singular  <- if (!is.null(conv_code) && conv_code != 0) "Convergence issue" else "OK"
      
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

results_list <- future_map(batches, 
                           ~fit_batch(.x,  data_sp, 
                                      response = "log_c_biomass", 
                                      random_effects = random_effects),
                           .options = furrr_options(seed = TRUE))

models_df <- bind_rows(results_list) %>%
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)


saveRDS(list(models_df = models_df, data_sp = data_sp),
        file.path("analyses/7habitat/output/model-comparison", 
                  paste(habitat, re_string, "models.rds", sep = "_")))




m <-   lmer(log_c_biomass ~ depth_cv_100 * site_type * age_at_survey +  (1 | region4/affiliated_mpa/site) + (1 |     year),
            data = data2, control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)), REML = T)

summary(m)
plot(allEffects(m), multiline = T)

m <-   glmmTMB(c_biomass ~ hard_bottom_250 + kelp_annual_100 + depth_mean_500 + depth_cv_100 * site_type * age_at_survey +
                 (1 | region4/affiliated_mpa/site) + (1 | year),
            data = data_sp, 
            family = Gamma(link = "log"))

dh <- simulateResiduals(m)
plot(dh)
plot(dh, form = data_sp$depth_cv_100)
plot(dh, form = data_sp$hard_bottom_250)
plot(dh, form = data_sp$site_type)
plot(dh, form = data_sp$age_at_survey)
plot(dh, form = data_sp$kelp_annual_100)
testDispersion(dh)
testQuantiles(dh)
testOutliers(dh)

oneway.test(depth_cv_100 ~ region4, data = data_sp)
kruskal.test(depth_cv_100 ~ region4, data = data_sp)

data2 %>%
  ggplot(aes(x = region4, y = depth_cv_100)) +
  geom_boxplot(outlier.alpha = 0.3) +
  geom_jitter(width = 0.15, alpha = 0.2) +
  labs(x = "Region", y = "Depth CV (100 m)") +
  theme_bw()

m_icc <- lmer(depth_cv_100 ~ 1 + (1 | region4), data = data_sp)
VarCorr(m_icc)

data_sp %>%
  group_by(region4) %>%
  summarise(
    n = n(),
    mean = mean(depth_cv_100, na.rm = TRUE),
    sd = sd(depth_cv_100, na.rm = TRUE),
    min = min(depth_cv_100, na.rm = TRUE),
    max = max(depth_cv_100, na.rm = TRUE),
    range = max - min
  )
