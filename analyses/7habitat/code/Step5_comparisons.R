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

}

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



length(unique(data$data_sp$affiliated_mpa))
length(unique(data$data_sp$site))
unique(data$data_sp$year)
length(unique(data$data_sp$year))
mean(data$data_sp$age_at_survey)
unique(data$data_sp$age_at_survey)


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

comparison <- compare_performance(model_comparison$models$m1, model_comparison$models$m2,model_comparison$models$m3, model_comparison$models$m4)
comparison
test_performance(model_comparison$models$m3, model_comparison$models$m1, model_comparison$models$m2, model_comparison$models$m4)


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

