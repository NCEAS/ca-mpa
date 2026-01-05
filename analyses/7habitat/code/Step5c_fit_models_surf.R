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
library(corrr)
library(gt)

rm(list = ls())
gc()

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R") 

# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"
fig.dir <- "~/ca-mpa/analyses/7habitat/figures"

# Define subset for modeling (reduced number of columns)
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,species_code:target_status, 
                assemblage_new, weight_kg, count, kg_per_haul, 
                starts_with("hard"), starts_with("soft"), starts_with("kelp"),starts_with("aquatic"), starts_with("depth"),
                starts_with("tri"), starts_with("slope")) %>% 
  filter(!affiliated_mpa == "ten mile smr")

# Define predictors and scales
pred_surf <- data.frame(predictor = grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri|slope)", names(data_surf),  value = TRUE)) %>%  
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
  filter(!predictor %in% c("kelp_annual_25", "kelp_annual_50", "kelp_annual_100", "kelp_annual_250", 
                           "hard_bottom_25", "hard_bottom_50", "aquatic_vegetation_bed_25")) %>% 
  # Drop TRI because highly correlated with cv:
  filter(!str_detect(predictor, "tri")) %>% 
  filter(!str_detect(predictor, "slope"))

# Build Data --------------------------------------------------------------------------
# Provide some of the global variables
habitat <- "surf"
regions <- c("North", "Central", "N. Channel Islands", "South")
print(paste0("Starting: ", habitat))
focal_group <- "targeted"

data_sp <- prep_focal_data(
  focal_group = focal_group, 
  drop_outliers = "no",
  biomass_variable = "kg_per_haul",
  data = data_surf,
  regions = c("North", "Central", "N. Channel Islands", "South")
)


# Univariate analyses to select scales ----------------------------
re_string <- "rm"
random_effects <- c("region4/affiliated_mpa")
print(paste0("RE Structure: ", paste(random_effects, collapse = ", ")))

scale_selection <- select_scales(data_sp, 
                                 pred_list = pred_surf,
                                 "log_c_biomass", 
                                 intx.terms = "", 
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
  select(all_of(top_scales)) %>% select(!starts_with("kelp")) %>% distinct() %>% 
  correlate() %>% 
  stretch() %>% 
  filter(!x == y) %>% 
  filter(as.character(x) <= as.character(y)) %>% 
  mutate(across(where(is.character), ~ str_replace_all(.x, "_", " "))) %>% 
  mutate(x = str_replace_all(x, "aquatic vegetation bed", "max biotic extent")) %>% 
  mutate(x = factor(x, levels = c("max biotic extent 50", "depth cv 500", "depth mean 25", "hard bottom 250", "slope sd 100")))

ggplot(data = data_corr, aes(x = x, y = y, fill = r)) +
  geom_tile(color = "white") +
  geom_label(aes(label = sprintf("%.2f", r)), fill = NA, label.size = 0) +
  scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                       midpoint = 0, limit = c(-1, 1), space = "Lab",
                       name = "Pearson\nCorrelation") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 65, vjust = 1,   size = 10, hjust = 1)) +
  labs(x = NULL, y = NULL) 

predictors_df <- generate_surf_3way(pred_surf %>% filter(predictor %in% top_scales)) 

# Run The Models -------------------------------------------------------------------------------------
n_workers <- round(parallel::detectCores()/3)
plan(multisession, workers = n_workers)
batch_size <- round(length(predictors_df$model_id)/n_workers)
batches <- split(predictors_df, (seq_len(nrow(predictors_df)) - 1) %/% batch_size)

fit_batch <- function(batch_df, data_sp, response, random_effects) {
  purrr::map_dfr(seq_len(nrow(batch_df)), function(i) {
    predictors <- batch_df$predictors[i]
    model_id   <- batch_df$model_id[i]
    fixed      <- paste(response, "~", predictors)
    random     <- paste0("(1 | ", random_effects, ")", collapse = " + ")
    formula    <- as.formula(paste(fixed, " + ", random))
    
    out <- tryCatch({
      model <- suppressMessages(suppressWarnings(lmer(formula,
                                                      data = data_sp, REML = FALSE)))
                                                      #control = lmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 1e8)))))
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

results_list <- future_map(batches, ~fit_batch(.x, 
                                               data_sp, 
                                               response = "log_c_biomass", 
                                               random_effects = random_effects),
                           .options = furrr_options(seed = TRUE))

models_df <- bind_rows(results_list) %>%
  mutate(delta_AICc = AICc - min(AICc, na.rm = TRUE)) %>%
  arrange(delta_AICc)

saveRDS(list(models_df = models_df, data_sp = data_sp),
        file.path("analyses/7habitat/output/model-comparison",
                  paste(habitat, focal_group, re_string, "models.rds", sep = "_")))
 
m <- lmer(log_c_biomass ~ soft_bottom_100 + kelp_annual_500 + site_type * age_at_survey + region4 + (1 | affiliated_mpa),
          data = data_sp, REML = T)
summary(m)
plot(allEffects(m, partial.residuals = T))
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


