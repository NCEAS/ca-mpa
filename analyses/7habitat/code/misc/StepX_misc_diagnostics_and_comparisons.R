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
library(kableExtra)
library(gt)


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) 
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds"))
data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds"))

# Examine habitat preference and scores ----------------------------------------
sp_kelp <- data_kelp %>%
  distinct(species_code, sciname, target_status, bioregion, assemblage_new)

sp_surf <- data_surf %>%
  distinct(species_code, sciname, target_status, bioregion, assemblage_new)

sp_rock <- data_rock %>% 
  distinct(species_code, sciname, target_status, bioregion, assemblage_new) %>% 
  mutate(sciname = case_when(species_code == "OYT"~ "Sebastes serranoides and S. flavidus",
                             T~sciname)) %>% 
  filter(bioregion == "South") %>% 
  mutate(assemblage_new = case_when(species_code == "OYT" ~ "Hard Bottom Biotic", T~assemblage_new)) %>% 
  distinct() 


  
# 
# surf_info <- data_surf %>% 
#   distinct(species_code, sciname, target_status, bioregion, assemblage_new)
# #consolidated_results <- readRDS("analyses/7habitat/output/refine_pref_habitat/consolidated_results.Rds")
# 
examine_habitat <- consolidated_results %>%
 # filter(importance_score >= 0.5) %>%
 # filter(sign == 1) %>%
  left_join(sp_info %>% filter(bioregion == "South")) %>%
  dplyr::select(species_code, sciname:assemblage_new, predictor, importance_score, sign, num_models)

examine_habitat %>%
  mutate(assemblage_new = case_when(species_code == "OYT" ~ "Hard Bottom Biotic",
                                    species_code == "ATHE" ~ "Generalist (?)", TRUE ~ assemblage_new)) %>%
  dplyr::select(`Code` = species_code, `Scientific Name` = sciname, `Target Status` = target_status,
                `Assemblage` = assemblage_new, `Predictor` = predictor, `Importance Score` = importance_score, `Sign` = sign, `# models` = num_models) %>%
  # group_by(`Code`, `Scientific Name`, `Target Status`, `Assemblage`) %>%
  # mutate(`Code` = if_else(row_number() == 1, `Code`, ""),
  #        `Scientific Name` = if_else(row_number() == 1, `Scientific Name`, ""),
  #        `Target Status` = if_else(row_number() == 1, `Target Status`, ""),
  #        `Assemblage` = if_else(row_number() == 1, `Assemblage`, "")) %>%  # Blank out repeated entries
  mutate(`Importance Score` = round(`Importance Score`, 2)) %>% # Format numbers
  kable(format = "html", title = "Step 1. Refine Preferred Habitat - Species Results") %>%
  kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
  collapse_rows(columns = c(1:4), target = 1, valign = "top", row_group_label_position = "identity") %>%
  row_spec(0, bold = TRUE)

4# examine_habitat %>% 
#   mutate(assemblage_new = case_when(species_code == "OYT" ~ "Hard Bottom Biotic", TRUE ~ assemblage_new)) %>% 
#   dplyr::select(`Code` = species_code, `Scientific Name` = sciname, `Target Status` = target_status,
#                 `Assemblage` = assemblage_new, `Predictor` = predictor, `Importance Score` = importance_score, `Models` = num_models) %>% 
#   mutate(`Importance Score` = round(`Importance Score`, 2)) %>% # Format numbers
#   kable(format = "html", title = "Step 1. Refine Preferred Habitat - Species Results") %>%
#   kable_styling(full_width = FALSE, bootstrap_options = c("striped", "hover")) %>%
#   collapse_rows(columns = c(1:4), target = 1, valign = "top", row_group_label_position = "identity") %>%
#   row_spec(0, bold = TRUE)
           

# Build models -----------------------------------------------------------------

run_comparison <- function(species, data_subset, regions, response, random_effects, path){
  print(paste("Species: ", species))
  
  consolidated_results <- readRDS(file.path(path, "consolidated_results.Rds"))
  
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
    filter(bioregion %in% regions) %>% 
    mutate(pref_habitat = rowSums(across(all_of(preferred_habitat)), na.rm = TRUE))# %>% 
  #  mutate(across(where(is.numeric), scale))
  
  f1 <- as.formula(paste(response, " ~ site_type * age_at_survey  + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
  f2 <- as.formula(paste(response, " ~ site_type * age_at_survey + pref_habitat + ", paste0("(1 | ", random_effects, ")", collapse = " + ")))  
  f3 <- as.formula(paste(response, " ~ site_type * age_at_survey + site_type * pref_habitat + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
  f4 <- as.formula(paste(response, " ~ site_type * age_at_survey +", paste(all_habitat, collapse = " + "), " + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
  
  m1 <- lmer(f1, data = data_sp)
  print("Model 1 Complete")
  m2 <- lmer(f2, data = data_sp, REML = FALSE)
  print("Model 2 Complete")
  m3 <- lmer(f3, data = data_sp, REML = FALSE)
  print("Model 3 Complete")
  m4 <- lmer(f4, data = data_sp, REML = FALSE)
  print("Model 4 Complete")
  model_comparison <- compare_performance(m1, m2, m3, m4)
  
  saveRDS(
    list(
      data_sp = data_sp,
      species = species,
      preferred_habitat = preferred_habitat,
      preferred_scale = preferred_scale,
      models = list(m1 = m1, m2 = m2, m3 = m3, m4 = m4),
      model_comparison = model_comparison
    ),
    file = file.path(path, paste0(species, "_model_comparison.rds"))
  )
  
  print(model_comparison)

}

# Test with the interactions to revise
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction"
species <- "SMIN"
response <- "log_kg_per_m2"
random_effects = c("bioregion", "affiliated_mpa")
data_subset <- data_kelp_subset
regions <- c("North", "Central", "South")
consolidated_results <- readRDS(file.path(path, "consolidated_results.Rds"))

preferred_habitat <- consolidated_results %>%
  rename(species_code = species) %>% 
  filter(species_code == species) %>% 
  filter(importance >= 0.5) %>%
  filter(str_detect(predictor, "hard|soft|kelp")) %>% 
  mutate(type = if_else(str_detect(predictor, ":"), "interaction", "normal")) %>% 
  mutate(predictor = gsub(":site_type|site_type:", "", predictor)) %>% 
  group_by(predictor) %>%
  mutate(has_interaction = any(type == "interaction")) %>%
  filter(!(has_interaction & type == "normal")) %>%
  dplyr::select(-has_interaction) %>%
  ungroup() %>% 
  pull(predictor, type)

print(paste("Pref Habitat: ", preferred_habitat))

preferred_scale <- str_sub(preferred_habitat, -3, -1) 
all_habitat <- names(data_subset)[str_ends(names(data_subset), preferred_scale[1])]
print(paste("Pref Scale: ", preferred_scale))

data_sp <- data_subset %>% 
  filter(species_code == species) %>% 
  filter(bioregion %in% regions) %>% 
  mutate(pref_habitat = rowSums(across(all_of(preferred_habitat)), na.rm = TRUE)) %>% 
  mutate(across(where(is.numeric), scale))

f1 <- as.formula(paste(response, " ~ site_type * age_at_survey  + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
f2 <- as.formula(paste(response, " ~ site_type * age_at_survey + pref_habitat + ", paste0("(1 | ", random_effects, ")", collapse = " + ")))  
f3 <- as.formula(paste(response, " ~ site_type * age_at_survey + site_type * pref_habitat + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 
f4 <- as.formula(paste(response, " ~ site_type * age_at_survey +", paste(all_habitat, collapse = " + "), " + ", paste0("(1 | ", random_effects, ")", collapse = " + "))) 

m1 <- lmer(f1, data = data_sp, REML = FALSE) 
print("Model 1 Complete")
m2 <- lmer(f2, data = data_sp, REML = FALSE)
print("Model 2 Complete")
m3 <- lmer(f3, data = data_sp, REML = FALSE)
print("Model 3 Complete")
m4 <- lmer(f4, data = data_sp, REML = FALSE)
print("Model 4 Complete")
model_comparison <- compare_performance(m1, m2, m3, m4)

saveRDS(
  list(
    data_sp = data_sp,
    species = species,
    preferred_habitat = preferred_habitat,
    preferred_scale = preferred_scale,
    models = list(m1 = m1, m2 = m2, m3 = m3, m4 = m4),
    model_comparison = model_comparison
  ),
  file = file.path(path, paste0(species, "_model_comparison.rds"))
)

print(model_comparison)




# Run and save the comparisons
kelp_codes <- c("ELAT", "OELO", "OPIC", "OYT",  "SCAR", "SMEL", "SMIN", "SMYS")
walk(kelp_codes, ~ run_comparison(.x, 
                                  data_subset = data_kelp_subset,
                                  response = "log_kg_per_m2",
                                  random_effects = c("year", "bioregion"),
                                  path = "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions"))

surf_codes <- c("AARG", "AAFF")
walk(surf_codes, ~ run_comparison(.x, 
                                  data_subset = data_surf_subset,
                                  response = "log_kg_per_haul",
                                  random_effects = c("year", "bioregion"),
                                  path = "analyses/7habitat/output/refine_pref_habitat/surf/all_regions"))

rock_codes <- c("BLU", "BWN", "CPR", "GPR", "LCD", "OYT", "RSY", "VER")
walk(rock_codes, ~ run_comparison(.x, 
                                  data_subset = data_rock_subset,
                                  response = "log_bpue_kg",
                                  random_effects = c("year", "bioregion"),
                                  path = "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"))

# Test for south only
walk(kelp_codes, ~ run_comparison(.x, 
                                  data_subset = data_kelp_subset,
                                  regions = c("South"),
                                  response = "log_kg_per_m2",
                                  random_effects = c("year"),
                                  path = "analyses/7habitat/output/refine_pref_habitat/kelp/south"))

# Table and diagnostics for each species ----------------------------------------

species <- "AARG"
path <- "analyses/7habitat/output/refine_pref_habitat/surf/all_regions"
data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

species <- "ELAT"
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions"
data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

species <- "BLU"
path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"
data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

species <- "OYT"
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/south"
data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

species <- "SMIN"
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction"
data <- readRDS(file.path(path, paste0(species, "_models.rds")))
regions <- c("North", "Central", "South")


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
summary(data$models$m2) 
summary(data$models$m3) 
summary(data$models$m4) 

broom.mixed::tidy(data$models$m1, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")

broom.mixed::tidy(data$models$m2, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")

broom.mixed::tidy(data$models$m3, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")

broom.mixed::tidy(data$models$m4, effects = "fixed") %>% 
  dplyr::select(!effect) %>% 
  mutate(p.value = case_when(p.value < 0.001 ~ "< 0.001", T~as.character(round(p.value, 3)))) %>% 
  gt() %>% 
  fmt_number(columns = c(statistic, df), decimals = 1) %>% 
  fmt_scientific(columns = c(estimate, std.error), decimals = 2, exp_style = "e")

# View the table
print(summary_table)

check_model(data$models$m3, residual_type = "simulated", check = c("ncv", "qq", "vif", "normality"))
check_model(model_comparison$models$m3, check = c("vif"))
check_outliers(data$models$m3)
outlier <- data$data_sp[21, ] # View the flagged case
check_residuals(data$models$m2)

result <- check_heteroskedasticity(data$models$m2)
plot(result)

library(performance)
library(see)
library(lme4)
library(effects)





# Compute effects
effects_list <- allEffects(data$models$m3)

# Plot effects
plot(effects_list)

# Compute effects
effects_list <- allEffects(data$models$m3)

# Extract the effect for a single term
partial_age <- as.data.frame(effects_list[[1]]) %>% 
  mutate()
partial_habitat <- as.data.frame(effects_list[[2]]) 

age <- ggplot(partial_age, aes(x = age_at_survey)) +
  geom_line(aes(y = fit, color = site_type), size = 1, show.legend = F) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type), alpha = 0.2, show.legend = F) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  scale_y_continuous(limits = c(-1, 2.5), expand = c(0,0))+
  labs(x = "Age at survey (scaled)", 
       y = "Biomass (scaled)",
       title = "Site Type * MPA Age",
       color = NULL, fill = NULL) +
  theme_minimal()
  
habitat <- ggplot(partial_habitat, aes(x = pref_habitat)) +
  geom_line(aes(y = fit, color = site_type), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type), alpha = 0.2) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  scale_y_continuous(limits = c(-1, 2.5), expand = c(0,0))+
  labs(x = "Amount of preferred habitat (scaled)", 
       y = NULL,
       title = "Site Type * Pref Habitat",
       color = NULL, fill = NULL) +
  theme_minimal()

age + habitat

sm3 <- data$models$m3
data_sp$partial_pref_habitat <- resid(m3) +
  model.matrix(m3)[, "pref_habitat"] * fixef(m3)["pref_habitat"] +
  model.matrix(m3)[, "site_typeMPA:pref_habitat"] * fixef(m3)["site_typeMPA:pref_habitat"]

# Plot partial residuals
ggplot(data_sp, aes(x = pref_habitat, y = partial_pref_habitat, color = site_type)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(
    x = "Preferred Habitat",
    y = "Partial Residuals",
    color = NULL
  ) + theme_minimal()


m1 <- data$models$m1
m2 <- data$models$m2
m3 <- data$models$m3
data_sp <- data$data_sp
library(ggplot2)
library(dplyr)

# Residuals for M1 plotted against pref_habitat (no partial residuals)
data_sp$residuals_m1 <- resid(m1)

# Partial residuals for M2
data_sp$partial_pref_habitat_m2 <- resid(m2) + model.matrix(m2)[, "pref_habitat"] * fixef(m2)["pref_habitat"]

# Partial residuals for M3 (with interaction term)
data_sp$partial_pref_habitat_m3 <- resid(m3) +
  model.matrix(m3)[, "pref_habitat"] * fixef(m3)["pref_habitat"] +
  model.matrix(m3)[, "site_typeMPA:pref_habitat"] * fixef(m3)["site_typeMPA:pref_habitat"]

# Combine data for plotting
plot_data <- data_sp %>%
  dplyr::select(pref_habitat, site_type, residuals_m1, partial_pref_habitat_m2, partial_pref_habitat_m3) %>%
  pivot_longer(
    cols = c(residuals_m1, partial_pref_habitat_m2, partial_pref_habitat_m3),
    names_to = "model",
    values_to = "residuals"
  ) %>%
  mutate(model = recode(model, 
                        "residuals_m1" = "M1 (raw residuals)",
                        "partial_pref_habitat_m2" = "M2 (partial residuals)",
                        "partial_pref_habitat_m3" = "M3 (partial residuals)"))

# Plot residuals for all models
ggplot(plot_data, aes(x = pref_habitat, y = residuals, color = site_type)) +
  geom_point(alpha = 0.4) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  facet_wrap(~model) +
  labs(
    x = "Preferred Habitat",
    y = "Residuals / Partial Residuals",
    color = NULL
  ) + theme_minimal()




# Compare models usm3# Compare models using AICc
summary(m1) 
summary(m2) 
summary(m3)
summary(m4)

# Get confidence intervals for interaction term from each model
conf_m1 <- confint(data$models$m1, parm = "site_typeReference:age_at_survey", level = 0.95)
conf_m2 <- confint(data$models$m2, parm = "site_typeReference:age_at_survey", level = 0.95)
conf_m3 <- confint(data$models$m3, parm = "site_typeReference:age_at_survey", level = 0.95)
conf_m4 <- confint(data$models$m4, parm = "site_typeReference:age_at_survey", level = 0.95)

# Combine the coefficients and confidence intervals into a single data frame
effects_df <- rbind(
  data.frame(model = "M1", estimate = fixef(data$models$m1)["site_typeReference:age_at_survey"], 
             conf.low = conf_m1[1], conf.high = conf_m1[2]),
  data.frame(model = "M2", estimate = fixef(data$models$m2)["site_typeReference:age_at_survey"], 
             conf.low = conf_m2[1], conf.high = conf_m2[2]),
  data.frame(model = "M3", estimate = fixef(data$models$m3)["site_typeReference:age_at_survey"], 
             conf.low = conf_m3[1], conf.high = conf_m3[2])#,
  #data.frame(model = "M4", estimate = fixef(data$models$m4)["site_typeReference:age_at_survey"], 
   #          conf.low = conf_m2[1], conf.high = conf_m2[2])
)


ggplot(effects_df, aes(x = model, y = estimate)) +
  geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2) +
  labs(title = "Effect Sizes for Protection (MPA vs. Reference)",
       x = "Model",
       y = "Effect Size (Estimate ± 95% CI)") +
  theme_minimal()



# Loop through species
library(dplyr)

# Initialize an empty data frame to store results
effect_size_comparison <- data.frame()

sp <- rock_codes
path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"
# Loop through species
for (species in sp) {
  data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))
  
  # Extract coefficients and confidence intervals for the interaction term
  species_results <- bind_rows(
    data.frame(
      species = species,
      model = "M1",
      estimate = fixef(data$models$m1)["site_typeReference:age_at_survey"], 
      conf.low = confint(data$models$m1, parm = "site_typeReference:age_at_survey", level = 0.95)[1], 
      conf.high = confint(data$models$m1, parm = "site_typeReference:age_at_survey", level = 0.95)[2]
    ),
    data.frame(
      species = species,
      model = "M2",
      estimate = fixef(data$models$m2)["site_typeReference:age_at_survey"], 
      conf.low = confint(data$models$m2, parm = "site_typeReference:age_at_survey", level = 0.95)[1], 
      conf.high = confint(data$models$m2, parm = "site_typeReference:age_at_survey", level = 0.95)[2]
    ),
    data.frame(
      species = species,
      model = "M3",
      estimate = fixef(data$models$m3)["site_typeReference:age_at_survey"], 
      conf.low = confint(data$models$m3, parm = "site_typeReference:age_at_survey", level = 0.95)[1], 
      conf.high = confint(data$models$m3, parm = "site_typeReference:age_at_survey", level = 0.95)[2]
    ),
    data.frame(
      species = species,
      model = "M4",
      estimate = fixef(data$models$m4)["site_typeReference:age_at_survey"], 
      conf.low = confint(data$models$m4, parm = "site_typeReference:age_at_survey", level = 0.95)[1], 
      conf.high = confint(data$models$m4, parm = "site_typeReference:age_at_survey", level = 0.95)[2]
    )
  )
  
  # Add to effect_size_comparison
  effect_size_comparison <- bind_rows(effect_size_comparison, species_results)
}

# View effect_size_comparisonhttps://aurora.nceas.ucsb.edu/rstudio/graphics/plot_zoom_png?width=906&height=841
#print(effect_size_comparison)
effect_size_comparison <- effect_size_comparison %>%
  mutate(
    direction = ifelse(estimate > 0, "Positive", "Negative"),
    significant = ifelse(conf.low > 0 | conf.high < 0, "Significant", "Not Significant"),
    group = paste(direction, significant, sep = " - ")  # Combine categories
  )

species_transitions <- effect_size_comparison %>%
  group_by(species) %>%
  arrange(model) %>%
  mutate(model_order = as.numeric(factor(model)))  # Numeric order for models

library(ggrepel)

library(ggrepel)

ggplot(species_transitions %>% 
         filter(!model == "M4"), aes(x = model_order, y = estimate, color = species, group = species)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black") +
  geom_line(alpha = 0.7, show.legend = FALSE, linewidth = 0.6) +  # Remove color legend for lines
  geom_point(aes(shape = significant), size = 2) +  # Keep shape legend for significance
  scale_x_continuous(breaks = 1:4, labels = c("M1", "M2", "M3", "M4")) +
  geom_text_repel(
    data = species_transitions %>% 
      filter(!model == "M4") %>% 
      group_by(species) %>% 
      filter(model_order == max(model_order)), 
    aes(label = species), 
    nudge_x = 0.1,  # Nudges labels slightly to the right
    show.legend = FALSE,
    max.overlaps = Inf,  # Allow all labels to repel without being dropped
    segment.color = NA  # Removes connecting lines
  ) +
  labs(
    x = "Model",
    y = "Estimate",
    shape = NULL
  ) +
  guides(color = "none") +  # Remove the species color legend
  theme_minimal()+
  theme(legend.position = "top")


# Try the two degree plot:

# Initialize an empty data frame to store results
effect_size_comparison <- data.frame()

sp <- rock_codes
path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"
interaction_terms <- c("site_typeReference:age_at_survey", "site_typeReference:pref_habitat")

# Loop through species
for (species in sp) {
  data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))
  
  # Loop through models and interaction terms
  for (model_name in c("m1", "m2", "m3", "m4")) {
    for (term in interaction_terms) {
      # Check if the term exists in the fixed effects to avoid errors
      if (term %in% names(fixef(data$models[[model_name]]))) {
        result <- data.frame(
          species = species,
          model = toupper(model_name), # Convert model name to "M1", "M2", etc.
          interaction_term = term,
          estimate = fixef(data$models[[model_name]])[term],
          conf.low = confint(data$models[[model_name]], parm = term, level = 0.95)[1],
          conf.high = confint(data$models[[model_name]], parm = term, level = 0.95)[2]
        )
        
        # Add to effect_size_comparison
        effect_size_comparison <- bind_rows(effect_size_comparison, result)
      }
    }
  }
}

effect_df <- effect_size_comparison %>% 
  mutate(
    direction = ifelse(estimate > 0, "Positive", "Negative"),
    significant = ifelse(conf.low > 0 | conf.high < 0, "Significant", "Not Significant"),
    group = paste(direction, significant, sep = " - ")  # Combine categories
  ) %>% 
  filter(model == "M3")

est_df <- effect_df %>% 
  pivot_wider(id_cols = species, names_from = "interaction_term", values_from = "estimate")


ggplot() +
  geom_hline(yintercept = 0) +
  geom_vline(xintercept = 0)+
  geom_point(data = est_df,
             aes(x = `site_typeReference:age_at_survey`, y = `site_typeReference:pref_habitat`, color = species))


# Misc Plots for Meeting with Darcy on 12-13 -----

# New stuff --------------------------------------------------------------------
library(effects)
library(performance)

species <- "ELAT"
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction"

#Read data containing all the focal models 
data <- readRDS(file.path(path, paste0(species, "_subset.rds"))) 
models_df <- data$models_df
data_sp <- data$data_sp


full_model <-  data$models$`H30m250*ST+H100m250*ST+K250*ST+S30m250*ST+S100m250*ST+ST*A`
top_model <- data$models$`K250*ST+S30m250*ST+ST*A`

check_model(full_model, residual_type = "simulated", check = c("ncv", "qq", "normality"))
check_model(top_model, residual_type = "simulated", check = c("ncv", "qq", "normality"))

check_collinearity(full_model) %>% 
  as.data.frame() %>% 
  mutate(Term = fct_reorder(factor(Term), VIF)) %>% 
  ggplot() +
  geom_bar(aes(x = VIF, y = Term), stat = "identity") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
  labs(
    title = "Variance Inflation Factor (VIF)",
    x = "VIF"
  ) +
  theme_minimal() 

vif_results <- check_collinearity(top_model) %>% 
  as.data.frame() %>% 
  mutate(Term = fct_reorder(factor(Term), VIF)) %>% 
  ggplot() +
  geom_bar(aes(x = VIF, y = Term), stat = "identity") +
  geom_vline(xintercept = 5, linetype = "dashed", color = "red") +
  labs(
    title = "Variance Inflation Factor (VIF)",
    x = "VIF"
  ) +
  theme_minimal() 


# Compute effects for the full model
effects_list <- allEffects(full_model)
plot(effects_list)

# Compute effects for the top model
effects_list <- allEffects(top_model)
plot(effects_list)



# Extract the effect for a single term
partial_age <- as.data.frame(effects_list[[1]]) %>% 
  mutate()
partial_habitat <- as.data.frame(effects_list[[2]]) 

age <- ggplot(partial_age, aes(x = age_at_survey)) +
  geom_line(aes(y = fit, color = site_type), size = 1, show.legend = F) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type), alpha = 0.2, show.legend = F) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  scale_y_continuous(limits = c(-1, 2.5), expand = c(0,0))+
  labs(x = "Age at survey (scaled)", 
       y = "Biomass (scaled)",
       title = "Site Type * MPA Age",
       color = NULL, fill = NULL) +
  theme_minimal()

habitat <- ggplot(partial_habitat, aes(x = pref_habitat)) +
  geom_line(aes(y = fit, color = site_type), size = 1) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type), alpha = 0.2) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  scale_y_continuous(limits = c(-1, 2.5), expand = c(0,0))+
  labs(x = "Amount of preferred habitat (scaled)", 
       y = NULL,
       title = "Site Type * Pref Habitat",
       color = NULL, fill = NULL) +
  theme_minimal()

age + habitat


# Test out plotting the predictions

species <- "SMIN"
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions/interaction"

#Read data containing all the focal models 
data <- readRDS(file.path(path, paste0(species, "_subset.rds"))) 
models_df <- data$models_df
data_sp <- data$data_sp

pred1 <- predict(data$models$`ST*A`, data$data_sp, re.form = NA)
pred2 <- predict(data$models$`H30m500*ST+ST*A`, data$data_sp, re.form = NA)

# Extract scaling attributes for each variable
log_kg_center <- attr(data$data_sp$log_kg_per_m2, "scaled:center")
log_kg_scale <- attr(data$data_sp$log_kg_per_m2, "scaled:scale")

age_center <- attr(data$data_sp$age_at_survey, "scaled:center")
age_scale <- attr(data$data_sp$age_at_survey, "scaled:scale")

H30m500_center <- attr(data$data_sp$hard_bottom_0_30m_500, "scaled:center")
H30m500_scale <- attr(data$data_sp$hard_bottom_0_30m_500, "scaled:scale")

# Unscale the predictions and any predictors if necessary
new_data <- data$data_sp %>%
  mutate(
    # Reverse scaling for predictions
    pred1_unscaled = exp(pred1 * log_kg_scale + log_kg_center) - 1,
    pred2_unscaled = exp(pred2 * log_kg_scale + log_kg_center) -1,
    
    # Optionally unscale predictors for clarity in plotting
    age_at_survey_unscaled = age_at_survey * age_scale + age_center,
    H30m500_unscaled = hard_bottom_0_30m_500 * H30m500_scale + H30m500_center
  ) %>%
  pivot_longer(
    cols = starts_with("pred"), 
    names_to = "model", 
    values_to = "value"
  ) %>%
  mutate(
    model_name = factor(case_when(
      model == "pred1_unscaled" ~ "Protection Only",
      model == "pred2_unscaled" ~ "Best Model"
    ), levels = c("Protection Only", "Best Model"))
  )


## Plots 

plot <- ggplot(new_data) +
  #  geom_point(data = data$data_sp,
  #            aes(x = age_at_survey, y = log_kg_per_m2, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = H30m500_unscaled, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#7e67f8","#e5188b")) +
  labs(title = paste0("Species: ", species),
       x = "Area of hard bottom 0-30m at 500m buffer",
       y = "Predicted Biomass (kg per m2)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
plot

surf_lines <- ggplot(surf_new_data) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  scale_y_continuous(limits = c(0, 0.5), expand = c(0,0)) +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name)
surf_lines

surf_age <- ggplot(surf_new_data) +
  geom_point(data = surf_data$data_sp,
             aes(x = age_at_survey, y = kg_per_haul, color = site_type), alpha = 0.2, show.legend = F) +
  geom_smooth(aes(x = age_at_survey, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name)
surf_age

surf_age_lines <- ggplot(surf_new_data) +
  geom_smooth(aes(x = age_at_survey, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(x = NULL,
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name)
surf_age_lines



