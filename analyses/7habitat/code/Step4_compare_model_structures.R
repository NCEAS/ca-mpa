# Compare and confirm modeling approaches based on the base model for each ecosystem
# Cori Lopazanski
# December 2025
# Updated model comparison with RE-sensitivity and grouped CV
# Cori Lopazanski — adjusted Jan 2026

library(tidyverse)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(DHARMa)
library(MuMIn)
library(purrr)
library(patchwork)

rm(list = ls()); gc()

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"

source("analyses/7habitat/code/Step0_helper_functions.R") 
source("analyses/7habitat/code/Step3_prep_focal_data.R") 

# Read data --------------------------------------------------------------------
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey,species_code:target_status, assemblage_new, weight_kg, count, kg_per_haul, starts_with("hard"), starts_with("soft"), starts_with("kelp"),starts_with("aquatic"), starts_with("depth"), starts_with("tri"), starts_with("slope")) %>% 
  filter(!affiliated_mpa == "ten mile smr") %>% 
  prep_focal_data(focal_group = "targeted", drop_outliers = "no", biomass_variable = "kg_per_haul", data = ., regions = c("North", "Central", "N. Channel Islands", "South"))

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2, age_at_survey, species_code:target_status, assemblage_new, vertical_zonation, name, common_name, weight_kg:count_per_m2, kg_per_100m2, starts_with("hard"), starts_with("kelp"), starts_with("depth"), starts_with("tri"), starts_with("slope")) %>% 
  prep_focal_data(focal_group = "targeted", drop_outliers = "no", biomass_variable = "kg_per_100m2", data = ., regions = c("North", "Central", "N. Channel Islands", "South"))

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  dplyr::select(year:affiliated_mpa, size_km2,  age_at_survey, species_code:target_status, assemblage_new, weight_kg, starts_with("hard"), starts_with("kelp"), starts_with("depth"), starts_with("tri"), starts_with("slope")) %>% 
  prep_focal_data(focal_group = "targeted", drop_outliers = "no", biomass_variable = "weight_kg", data = ., regions = c("North", "Central", "N. Channel Islands", "South"))

rm(data2)

# Ensure common helper columns for sensitivity
prepare_for_comparison <- function(dat) {
  const <- if_else(min(dat$biomass) > 0, 0, min(dat$biomass[dat$biomass > 0]/2, na.rm = TRUE))
  dat <- dat %>% mutate(
    biomass_gamma = ifelse(biomass == 0, const, biomass),
    biomass_log = log(biomass + 1e-4))
  dat
}

data_kelp <- prepare_for_comparison(data_kelp)
data_rock <- prepare_for_comparison(data_rock)
data_surf <- prepare_for_comparison(data_surf)


# Plot the distribution of biomass (response var) and with log-transformation
k1 <- ggplot(data_kelp, aes(x = biomass)) + geom_histogram() + theme_minimal() + labs(x = "Biomass (kg per m2)", y = "Count (# of obs.)")
k2 <- ggplot(data_kelp, aes(x = log_c_biomass))+ geom_histogram() + theme_minimal() + labs(x = "Log-transformed biomass (kg per m2)", y = "Count (# of obs.)")
k <- k1 + k2 + plot_annotation("Kelp forest")

r1 <- ggplot(data_rock, aes(x = biomass)) + geom_histogram() + theme_minimal() + labs(x = "Biomass (kg per angler hour)", y = "Count (# of obs.)")
r2 <- ggplot(data_rock, aes(x = log_c_biomass))+ geom_histogram() + theme_minimal() + labs(x = "Log-transformed biomass (kg per angler hour)", y = "Count (# of obs.)")
r <- r1 + r2 + plot_annotation("Shallow reef")

s1 <- ggplot(data_surf, aes(x = biomass)) + geom_histogram() + theme_minimal() + labs(x = "Biomass (kg per haul)", y = "Count (# of obs.)")
s2 <- ggplot(data_surf, aes(x = log_c_biomass))+ geom_histogram() + theme_minimal() + labs(x = "Log-transformed biomass (kg per haul)", y = "Count (# of obs.)")
s <- s1 + s2 + plot_annotation("Surf zone")

wrap_plots(wrap_elements(full = r),
           wrap_elements(full = k),
           wrap_elements(full = s),
           nrow = 3)

check_distribution(data_rock$biomass)
check_distribution(data_rock$log_c_biomass)
check_distribution(data_rock$biomass_gamma)

check_distribution(data_kelp$biomass)
check_distribution(data_kelp$log_c_biomass)
check_distribution(data_kelp$biomass_gamma)

check_distribution(data_surf$biomass)
check_distribution(data_surf$log_c_biomass)
check_distribution(data_surf$biomass_gamma)

# Evaluate supported random effect structure ------------------------------------
re_cands <- list(
  rmsy = "(1 | region4/affiliated_mpa/site) + (1 | year)",
  rms  = "(1 | region4/affiliated_mpa/site)",
  rmy  = "(1 | region4/affiliated_mpa) + (1 | year)",
  rm   = "(1 | region4/affiliated_mpa)",
  ry   = "(1 | region4) + (1 | year)",
  msy  = "(1 | affiliated_mpa/site) + (1 | year)",
  ms   = "(1 | affiliated_mpa/site)",
  my   = "(1 | affiliated_mpa) + (1 | year)",
  sy   = "(1 | site) + (1 | year)",
  s    = "(1 | site)",
  m    = "(1 | affiliated_mpa)",
  r    = "(1 | region4)",
  y    = "(1 | year)"
)

# Compare RE structures for specified response variable (RE comparison done with REML)
compare_re <- function(dat, response_var) {
  re_names <- c("site:(affiliated_mpa:region4)", "affiliated_mpa:region4", "region4", "affiliated_mpa", "site:affiliated_mpa", "site", "year")
  
  results <- purrr::map_dfr(names(re_cands), function(nm) {
    re_str <- re_cands[[nm]]
    frm_fixed <- reformulate("hard_bottom_100 * site_type * age_at_survey+ kelp_annual_100 * site_type * age_at_survey + depth_mean_100 * site_type * age_at_survey + depth_cv_100 * site_type * age_at_survey + site_type * age_at_survey", response = response_var)
    frm_full  <- as.formula(paste0(deparse(frm_fixed), " + ", re_str))
    fit <- lme4::lmer(frm_full, data = dat, REML = TRUE)
    
    re_values <- setNames(rep(NA_real_, length(re_names)), re_names)     # default NA values for the RE columns
    
    tib <- tibble(name = nm,
                  re = re_str,
                  AICc = NA_real_,
                  singular = NA,
                  any_near_zero = NA)
    
    vc_df <- as.data.frame(lme4::VarCorr(fit))
    
    # fill re_values for groups that match our expected RE names
    if(nrow(vc_df) > 0) {
      matches <- vc_df$grp %in% re_names
      if(any(matches)) {
        for(i in which(matches)) {
          grp <- vc_df$grp[i]
          re_values[[grp]] <- vc_df$vcov[i]
        }
      }
      vc <- vc_df$vcov
    } else {
      vc <- numeric(0)
    }
    
    near_zero <- if(length(vc) == 0) FALSE else (vc < 1e-6 | (vc / sum(vc)) < 1e-3)
    
    tib <- tibble(name = nm,
                  re = re_str,
                  AICc = MuMIn::AICc(fit),
                  singular = isSingular(fit, tol = 1e-4),
                  any_near_zero = any(near_zero))
    tib <- bind_cols(tib, as_tibble(as.list(re_values))) %>% 
      mutate(site = coalesce(`site:(affiliated_mpa:region4)`, `site:affiliated_mpa`, site),
             affiliated_mpa = coalesce(`affiliated_mpa:region4`, affiliated_mpa)) %>% 
      select(!c(`site:(affiliated_mpa:region4)`, `site:affiliated_mpa`, `affiliated_mpa:region4`))
    tib
    })
  
  # pick best non-singular non-near-zero if possible
  best_name <- results %>% 
    filter(!isTRUE(singular)) %>% 
    filter(!isTRUE(any_near_zero)) %>% 
    arrange(AICc) %>% 
    slice(1) %>% 
    pull(name)
  
  if(length(best_name) == 0) best_name <- results %>% arrange(AICc) %>% slice(1) %>% pull(name)
  best_re <- re_cands[[best_name]]
  list(table = results, best = best_re)
}


# Compare RE and get best supported structure using gaussian LMM (with LMER):
re_kelp <- compare_re(data_kelp, "log_c_biomass")
re_rock <- compare_re(data_rock, "log_c_biomass")
re_surf <- compare_re(data_surf, "log_c_biomass")

re_kelp$table # somewhat minimal support for region, likely especially when combined with habitat variables
re_rock$table # all supported
re_surf$table # no support for year; all others ok

# Grouped CV helper (folds by site to avoid leakage)
make_grouped_folds <- function(df, group_var = "site", k = 5, seed = 42) {
  set.seed(seed)
  groups <- unique(df[[group_var]])
  folds_g <- sample(rep(1:k, length.out = length(groups)))
  group_to_fold <- tibble(group = groups, fold = folds_g)
  df %>% left_join(group_to_fold, by = setNames("group", group_var)) %>% pull(fold)
}

# Plot the residual vs. fitted for each of those models
plot_resid_fitted <- function(model, dat, response = "biomass") {
  
  dat$.fitted_resp <- predict(model, type = "response")
  dat$.pearson     <- residuals(model, type = "pearson")

  p1 <- ggplot(dat, aes(x = .fitted_resp, y = .pearson)) +
    geom_point(alpha = 0.4) +
    geom_smooth(method = "loess", se = FALSE) +
    labs(x = "Fitted values (response scale)",
         y = "Pearson residuals") +
    theme_bw()
  
  list(pearson_response = p1)
}


# Family comparison with grouped CV (use chosen RE string; CV grouped by site)
compare_families <- function(dat, re_str, group_var = "site",
                             k = 5, seed = 42,
                             make_plots = TRUE) {
  
  form_gauss <- as.formula(paste0("log_c_biomass ~ site_type * age_at_survey + ", re_str))
  form_raw   <- as.formula(paste0("biomass ~ site_type * age_at_survey + ", re_str))
  form_gamma <- as.formula(paste0("biomass_gamma ~ site_type * age_at_survey + ", re_str))
  
  fams <- list(
    gaussian_log = list(form = form_gauss, fam = gaussian()),
    gamma        = list(form = form_gamma, fam = Gamma("log")),
    tweedie      = list(form = form_raw,   fam = tweedie("log"))
  )
  
  folds <- make_grouped_folds(dat, group_var = group_var, k = k, seed = seed)
  
  fit_eval <- function(form, fam) {
    
    m <- try(glmmTMB(formula = form, data = dat, family = fam), silent = TRUE)
    if(inherits(m, "try-error")) {
      return(list(
        summary = data.frame(AICc = NA, disp_p = NA, zero_p = NA,
                             unif_p = NA, cv_mse = NA, converged = FALSE),
        model = NULL,
        plots = NULL
      ))
    }
    
    dh <- simulateResiduals(m, n = 200)
    
    disp  <- testDispersion(dh)$p.value
    zero  <- testZeroInflation(dh)$p.value
    unif  <- testUniformity(dh)$p.value
    
    dat$.fold <- folds
    cv_mse <- mean(map_dbl(1:k, function(i) {
      tr <- dat %>% filter(.fold != i) %>% select(-.fold)
      te <- dat %>% filter(.fold == i) %>% select(-.fold)
      m_i <- glmmTMB(formula = form, data = tr, family = fam)
      pr  <- predict(m_i, newdata = te, type = "response", re.form = NA)
      mean((te$biomass - pr)^2, na.rm = TRUE)
    }), na.rm = TRUE)
    
    plots <- if (make_plots) plot_resid_fitted(m, dat) else NULL
    
    list(
      summary = data.frame(
        AICc = AICc(m),
        disp_p = disp,
        zero_p = zero,
        unif_p = unif,
        cv_mse = cv_mse,
        converged = TRUE
      ),
      model = m,
      plots = plots
    )
  }
  
  out <- imap(fams, ~fit_eval(.x$form, .x$fam))
  
  list(
    table = map_df(names(out), ~cbind(family = .x, out[[.x]]$summary)),
    models = map(out, "model"),
    plots  = map(out, "plots")
  )
}




# Run for each ecosystem 
kelp_results <- compare_families(data_kelp, re_kelp$best, group_var = "site", k = 5, seed = 42)
rock_results <- compare_families(data_rock, re_rock$best, group_var = "site", k = 5, seed = 42)
surf_results <- compare_families(data_surf, re_surf$best, group_var = "site", k = 5, seed = 42)


# Inspect
kelp_results$table
rock_results$table
surf_results$table

kelp_results$plots$gaussian_log$pearson_response + kelp_results$plots$tweedie$pearson_response +  kelp_results$plots$gamma$pearson_response

rock_results$plots$gaussian_log$pearson_response + rock_results$plots$tweedie$pearson_response +  rock_results$plots$gamma$pearson_response

surf_results$plots$gaussian_log$pearson_response + surf_results$plots$tweedie$pearson_response +  surf_results$plots$gamma$pearson_response

