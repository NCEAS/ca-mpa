# Compare and confirm modeling approaches based on the base model for each ecosystem
# Cori Lopazanski
# December 2025


# Setup ------------------------------------------------------------------------
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(purrr)

rm(list = ls()); gc()

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step4a_prep_focal_data.R") 

# Read data --------------------------------------------------------------------

# Surf zone 
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,species_code:target_status, 
                assemblage_new, weight_kg, count, kg_per_haul, 
                starts_with("hard"), starts_with("soft"), starts_with("kelp"),starts_with("aquatic"), starts_with("depth"),
                starts_with("tri"), starts_with("slope")) %>% 
  filter(!affiliated_mpa == "ten mile smr") %>% 
  prep_focal_data(focal_group = "targeted", 
                  drop_outliers = "no",
                  biomass_variable = "kg_per_haul",
                  data = .,
                  regions = c("North", "Central", "N. Channel Islands", "South"))

# Kelp forest
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey, 
                species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, kg_per_100m2,
                starts_with("hard"), starts_with("kelp"), starts_with("depth"),
                starts_with("tri"), starts_with("slope")) %>% 
  prep_focal_data(focal_group = "targeted", 
                  drop_outliers = "no",
                  biomass_variable = "kg_per_100m2",
                  data = .,
                  regions = c("North", "Central", "N. Channel Islands", "South"))

# Shallow reef
data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2,  age_at_survey,
                species_code:target_status, assemblage_new, weight_kg,
                starts_with("hard"), starts_with("kelp"), starts_with("depth"),
                starts_with("tri"), starts_with("slope")) %>% 
  prep_focal_data(focal_group = "targeted", 
                  drop_outliers = "no",
                  biomass_variable = "weight_kg",
                  data = .,
                  regions = c("North", "Central", "N. Channel Islands", "South"))

rm(data2)

# Compare RE structures ---------------------------------------------------------


# Family comparison with per-ecosystem RE
compare_families <- function(dat, re_str) {
  
  # Gamma cannot take zeros → add small offset
  biomass_gamma <- ifelse(dat$biomass == 0, 1e-4, dat$biomass)
  
  form_gauss <- as.formula(
    paste0("log(biomass + 1e-4) ~ site_type * age_at_survey + ", re_str)
  )
  
  form_raw <- as.formula(
    paste0("biomass ~ site_type * age_at_survey + ", re_str)
  )
  
  form_gamma <- as.formula(
    paste0("biomass_gamma ~ site_type * age_at_survey + ", re_str)
  )
  
  fams <- list(
    gaussian_log = list(form = form_gauss,  fam = gaussian(),      dat = dat),
    gamma        = list(form = form_gamma,  fam = Gamma("log"),    dat = dat %>%  mutate(biomass_gamma = biomass_gamma)),
    tweedie      = list(form = form_raw,    fam = tweedie("log"),  dat = dat),
    nb           = list(form = form_raw,    fam = nbinom2(),       dat = dat)
  )
  
  fit_eval <- function(form, fam, df_use) {
    m <- glmmTMB(formula = form, data = df_use, family = fam)
    dh <- simulateResiduals(m, n = 200)
    disp <- testDispersion(dh)$p.value
    zero <- if(sum(df_use$biomass == 0) > 0) testZeroInflation(dh)$p.value else NA_real_
    unif <- testUniformity(dh)$p.value
    
    k <- 5
    folds <- sample(rep(1:k, length.out = nrow(df_use)))
    cv_mse <- mean(map_dbl(1:k, function(i) {
      tr <- df_use[folds != i, ]
      te <- df_use[folds == i, ]
      m_i <- try(glmmTMB(formula = form, data = tr, family = fam), silent = TRUE)
      if(inherits(m_i, "try-error")) return(NA_real_)
      pr <- predict(m_i, newdata = te, type = "response")
      mean((te$biomass - pr)^2, na.rm = TRUE)
    }), na.rm = TRUE)
    
    list(AICc = AICc(m), disp_p = disp, zero_p = zero, unif_p = unif, cv_mse = cv_mse)
  }
  
  out <- imap(fams, ~fit_eval(.x$form, .x$fam, .x$dat))
  
  map_df(names(out), function(nm) {
    x <- out[[nm]]
    data.frame(
      family = nm,
      AICc   = x$AICc,
      disp_p = x$disp_p,
      zero_p = x$zero_p,
      unif_p = x$unif_p,
      cv_mse = x$cv_mse
    )
  })
}

#------------------------------------------------------------
# Run for each ecosystem
#------------------------------------------------------------
datasets <- list(kelp = data_kelp, rock = data_rock, surf = data_surf)

results <- imap(datasets, function(dat, nm) {
  chosen_re <- compare_re(dat)$best
  tab <- compare_families(dat, chosen_re)
  list(chosen_re = chosen_re, diagnostics = tab)
})

results$kelp
results$rock
results$surf

# Inspect

m <- glmmTMB(formula = as.formula(paste0("c_biomass ~ hard_bottom_250 + site_type * age_at_survey + ", kelp_re$best)), 
             data = data_kelp, family = Gamma(link = "log"))
dh <- simulateResiduals(m, n = 200)
disp <- testDispersion(dh)$p.value
unif <- testUniformity(dh)$p.value
