# Step 4. Run comparisons
# Cori Lopazanski
# Nov 2024

# Estimate the effect of protection with and without controlling for habitat
# This step runs the following models:
# 1. Biomass ~ Site Type * Age 
# 2. Biomass ~ Site Type * Age + Preferred Habitat
# 3. Biomass ~ Site Type * Age + Site Type * Habitat
# 4. Biomass ~ Site Type * Age + [All Habitat at Scale of Pref Habitat]

# Then, the goal is to:
# - Compare the models to see which is "best" (using AIC)
# - Look at the effects and statistical significance across the models
# - Generate outputs and diagnostics for the different models

library(tidyverse)
library(lmerTest)
library(performance)
library(see)
library(gt)


# Read Data --------------------------------------------------------------------
#rm(list = setdiff(ls(), "data_subset"))
#gc()

# Examine habitat preference and scores ----------------------------------------
examine_habitat <- consolidated_results %>% 
  filter(importance_score >= 0.5) %>% 
  filter(sign == 1)


# Build models -----------------------------------------------------------------

run_comparison <- function(species){
  print(paste("Species: ", species))
  
  preferred_habitat <- consolidated_results %>% 
    filter(species_code == species) %>% 
    filter(importance_score >= 0.5) %>% 
    filter(sign == 1) %>% 
    pull(predictor)
  print(paste("Pref Habitat: ", preferred_habitat))
  
  preferred_scale <- str_sub(preferred_habitat, -3, -1) 
  all_habitat <- names(data_subset)[str_ends(names(data_subset), preferred_scale[1])]
  print(paste("Pref Scale: ", preferred_scale))
  
  data_sp <- data_subset %>% 
    filter(species_code == species) %>% 
    mutate(pref_habitat = rowSums(across(all_of(preferred_habitat)), na.rm = TRUE)) 
  
  f1 <- as.formula(paste(response, " ~ site_type * age_at_survey + size_km2 + (1 | year)"))
  f2 <- as.formula(paste(response, " ~ site_type * age_at_survey + pref_habitat + size_km2 +(1 | year)"))
  f3 <- as.formula(paste(response, " ~ site_type * age_at_survey + site_type * pref_habitat + size_km2 +(1 | year)"))
  f4 <- as.formula(paste(response, " ~ site_type * age_at_survey + size_km2 +", paste(all_habitat, collapse = " + "), "+ (1 | year)"))
  
  m1 <- lmer(f1, data = data_sp, REML = FALSE)
  m2 <- lmer(f2, data = data_sp, REML = FALSE)
  m3 <- lmer(f3, data = data_sp, REML = FALSE)
  m4 <- lmer(f4, data = data_sp, REML = FALSE)
  
  model_comparison <- compare_performance(m1, m2, m3, m4)
  
  saveRDS(
    list(
      species = species,
      preferred_habitat = preferred_habitat,
      preferred_scale = preferred_scale,
      models = list(m1 = m1, m2 = m2, m3 = m3, m4 = m4),
      model_comparison = model_comparison
    ),
    file = file.path(save_path, paste0(species, "_model_comparison.rds"))
  )
  
  print(model_comparison)

}

# Run and save the comparisons
response = "log_kg_per_m2"
save_path = "analyses/7habitat/output/refine_pref_habitat"
walk(unique(examine_habitat$species_code), run_comparison)


# Table and diagnostics for each species ----------------------------------------

model_diagnostics <- function(species){
  data <- readRDS(file.path(save_path, paste0(species, "_model_comparison.rds")))
  
}


species <- "SPUL"
data <- readRDS(file.path(save_path, paste0(species, "_model_comparison.rds")))

mod_comp <- as.data.frame(data$model_comparison) %>% 
  mutate(delta_AICc = AICc - min(AICc)) %>% 
  dplyr::select(Model = Name, AICc, delta_AICc, AIC_wt, R2_conditional, R2_marginal, RMSE, Sigma) %>% 
  mutate(Model = case_when(Model == "m4" ~ "All Habitat Variables",
                           Model == "m3" ~ "Habitat Interaction",
                           Model == "m2" ~ "Habitat Control",
                           Model == "m1" ~ "Protection Only")) %>% 
  dplyr::arrange(-AIC_wt, AICc) %>% 
  gt() %>% 
  tab_header(title = "Model Comparison",
             subtitle = "All models: Log-Transformed Biomass ~ Site Type * MPA Age + MPA Size + ...") %>% 
  fmt_number(columns = c(AICc, AIC_wt, delta_AICc), decimals = 1) %>% 
  fmt_number(columns = c(R2_conditional, R2_marginal, RMSE, Sigma), decimals = 4) %>% 
  tab_style(style = cell_text(weight = "bold"), locations = cells_column_labels(everything())) %>% 
  cols_align(align = "right",
             columns = where(is.numeric)) %>%
  opt_table_lines(extent = "none")
mod_comp


summary(data$models$m1)

check_model(model_comparison$models$m3, residual_type = "simulated", check = c("ncv", "qq", "vif", "normality"))
check_model(model_comparison$models$m3, check = c("vif"))
check_outliers(model_comparison$models$m3)
check_residuals(model_comparison$models$m3)

result <- check_heteroskedasticity(model_comparison$models$m3)
plot(result)

library(performance)
library(see)

comparison <- compare_performance(model_comparison$models$m1, model_comparison$models$m2,model_comparison$models$m3, model_comparison$models$m4)
comparison
test_performance(model_comparison$models$m3, model_comparison$models$m1, model_comparison$models$m2, model_comparison$models$m4)


# Compare models usm3# Compare models using AICc
summary(m1) 
summary(m2) 
summary(m3)
summary(m4)

# Get confidence intervals for interaction term from each model
conf_m1 <- confint(m1, parm = "site_typeReference:age_at_survey", level = 0.95)
conf_m2 <- confint(m2, parm = "site_typeReference:age_at_survey", level = 0.95)
conf_m3 <- confint(m3, parm = "site_typeReference:age_at_survey", level = 0.95)
conf_m4 <- confint(m4, parm = "site_typeReference:age_at_survey", level = 0.95)

# Combine the coefficients and confidence intervals into a single data frame
effects_df <- rbind(
  data.frame(model = "M1", estimate = fixef(m1)["site_typeReference:age_at_survey"], 
             conf.low = conf_m1[1], conf.high = conf_m1[2]),
  data.frame(model = "M2", estimate = fixef(m2)["site_typeReference:age_at_survey"], 
             conf.low = conf_m2[1], conf.high = conf_m2[2]),
  data.frame(model = "M3", estimate = fixef(m3)["site_typeReference:age_at_survey"], 
             conf.low = conf_m2[1], conf.high = conf_m2[2]),
  data.frame(model = "M4", estimate = fixef(m4)["site_typeReference:age_at_survey"], 
             conf.low = conf_m2[1], conf.high = conf_m2[2])
)


ggplot(effects_df, aes(x = model, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Effect Sizes for Protection (MPA vs. Reference)",
       x = "Model",
       y = "Effect Size (Estimate ± 95% CI)") +
  theme_minimal()

