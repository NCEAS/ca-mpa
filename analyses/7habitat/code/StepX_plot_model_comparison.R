# Step X Plot predictions
# Cori Lopazanski
# Nov 2024

# This script takes the various final models and plots the predictions

# Setup --------------------------------------------------------------------
library(patchwork)
library(merTools)

rm(list = ls())

gc()


# out.dir <- "analyses/7habitat/output/refine_pref_habitat/all_regions"
# 
# pdf("analyses/7habitat/output/refine_pref_habitat/all_regions/species-habitat-plots.pdf", width = 7, height = 4) 
# 
# for (species in sp_list){
#   sp_models <- readRDS(file.path(out.dir, paste0(species, "_model_comparison.rds")))
#   
#   pred1 <- predict(sp_models$models$m1, sp_models$data_sp, re.form = NA)
#   pred2 <- predict(sp_models$models$m2, sp_models$data_sp, re.form = NA)
#   pred3 <- predict(sp_models$models$m3, sp_models$data_sp, re.form = NA)
#   
#   new_data <- sp_models$data_sp %>% 
#     mutate(pred1 = exp(pred1 - 1),
#            pred2 = exp(pred2 - 1),
#            pred3 = exp(pred3 - 1),
#     ) %>% 
#     pivot_longer(cols = starts_with("pred"), 
#                  names_to = "model", values_to = "value") %>% 
#     mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
#                                          model == "pred2" ~ "Protection + Pref. Habitat",
#                                          model == "pred3" ~ "Protection * Pref. Habitat"), 
#                                levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))
#   
#   p <- ggplot(new_data) +
#     geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
#     scale_color_manual(values = c("#e5188b", "#7e67f8")) +
#     labs(title = paste0("Species: ", species, 
#                         "\nTarget Status: ", unique(new_data$target_status), 
#                         "\nRegions: ", paste(unique(new_data$bioregion), collapse = ", "),
#                         "\nPref. Habitat: ", paste(sp_models$preferred_habitat, collapse = ", ")),
#          x = "Amount of preferred habitat (m2)",
#          y = "Predicted Biomass (kg per m2)",
#          color = "Site Type",
#          fill = "Site Type") +
#     theme_minimal() +
#     facet_wrap(~model_name)
#   
#   print(p)
# 
# }
# 
# dev.off()
# 



## Run the predictions individually 
species <- "AARG"
path <- "analyses/7habitat/output/refine_pref_habitat/surf/all_regions"
surf_data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

surf_pred1 <- predict(surf_data$models$m1, surf_data$data_sp, re.form = NA)
surf_pred2 <- predict(surf_data$models$m2, surf_data$data_sp, re.form = NA)
surf_pred3 <- predict(surf_data$models$m3, surf_data$data_sp, re.form = NA)

surf_new_data <- surf_data$data_sp %>% 
  mutate(pred1 = exp(surf_pred1) - 1 ,
         pred2 = exp(surf_pred2) - 1,
         pred3 = exp(surf_pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))

species <- "BLU"
path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"
rock_data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

rock_pred1 <- predict(rock_data$models$m1, rock_data$data_sp, re.form = NA)
rock_pred2 <- predict(rock_data$models$m2, rock_data$data_sp, re.form = NA)
rock_pred3 <- predict(rock_data$models$m3, rock_data$data_sp, re.form = NA)

rock_new_data <- rock_data$data_sp %>% 
  mutate(pred1 = exp(rock_pred1) - 1,
         pred2 = exp(rock_pred2) - 1,
         pred3 = exp(rock_pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))

species <- "OYT"
path <- "analyses/7habitat/output/refine_pref_habitat/kelp/all_regions"
kelp_data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

kelp_pred1 <- predict(kelp_data$models$m1, kelp_data$data_sp, re.form = NA)
kelp_pred2 <- predict(kelp_data$models$m2, kelp_data$data_sp, re.form = NA)
kelp_pred3 <- predict(kelp_data$models$m3, kelp_data$data_sp, re.form = NA)

kelp_new_data <- kelp_data$data_sp %>% 
  mutate(pred1 = exp(kelp_pred1) - 1,
         pred2 = exp(kelp_pred2) - 1,
         pred3 = exp(kelp_pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))



## Plots 

surf <- ggplot(surf_new_data) +
  geom_point(data = surf_data$data_sp,
             aes(x = pref_habitat, y = kg_per_haul, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = paste0("Species: ", species, 
                      "\nPref. Habitat: ", paste(surf_data$preferred_habitat, collapse = ", ")),
       x = "Amount of preferred habitat (m2)",
       y = "Predicted Biomass (kg per haul)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
surf

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


rock <- ggplot(rock_new_data) +
  geom_point(data = rock_data$data_sp,
             aes(x = pref_habitat, y = weight_kg, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = paste0("Species: ", species, 
                      "\nPref. Habitat: ", paste(rock_data$preferred_habitat, collapse = ", ")),
       x = "Amount of preferred habitat (m2)",
       y = "Predicted Biomass (kg per trip-cell)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
rock

rock_lines <- ggplot(rock_new_data) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = "Amount of preferred habitat (m2)",
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name) 

rock_lines

rock_age <- ggplot(rock_new_data) +
   geom_point(data = rock_data$data_sp,
              aes(x = age_at_survey, y = weight_kg, color = site_type), alpha = 0.2, show.legend = F) +
  geom_smooth(aes(x = age_at_survey, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = "Years since implementation",
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name) 

rock_age

rock_age_lines <- ggplot(rock_new_data) +
  geom_smooth(aes(x = age_at_survey, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = "Years since implementation",
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name) 

rock_age_lines

rock_yr_lines <- ggplot(rock_new_data) +
  geom_smooth(aes(x = year, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = "Years since implementation",
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name) 

rock_yr_lines

kelp <- ggplot(kelp_new_data) +
  geom_point(data = kelp_data$data_sp,
             aes(x = pref_habitat, y = kg_per_m2, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = paste0("Species: ", species, 
                      "\nPref. Habitat: ", paste(kelp_data$preferred_habitat, collapse = ", ")),
       x = "Amount of preferred habitat (m2)",
       y = "Predicted Biomass (kg per m2)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
kelp

kelp_lines <- ggplot(kelp_new_data) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = NULL,
       y = "Predicted Biomass",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name) 

kelp_lines

kelp_age <- ggplot(kelp_new_data) +
  geom_point(data = kelp_data$data_sp,
             aes(x = age_at_survey, y = kg_per_m2, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = age_at_survey, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(x = NULL,
       y = "Predicted Biomass (kg per m2)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
kelp_age

kelp_age_lines <- ggplot(kelp_new_data) +
  geom_smooth(aes(x = age_at_survey, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(x = NULL,
       y = "Predicted Biomass (kg per m2)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
kelp_age_lines

surf_lines / kelp_lines / rock_lines
surf / kelp / rock
surf_age / kelp_age / rock_age
surf_age_lines / kelp_age_lines / rock_age_lines

# Compare multiple species with different relationships

species <- "BLU"
path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"
rock_data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

rock_pred1 <- predict(rock_data$models$m1, rock_data$data_sp, re.form = NA)
rock_pred2 <- predict(rock_data$models$m2, rock_data$data_sp, re.form = NA)
rock_pred3 <- predict(rock_data$models$m3, rock_data$data_sp, re.form = NA)

rock_new_data <- rock_data$data_sp %>% 
  mutate(pred1 = exp(rock_pred1) - 1,
         pred2 = exp(rock_pred2) - 1,
         pred3 = exp(rock_pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))

blu_lines <- ggplot(rock_new_data) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = "Amount of preferred habitat (m2)",
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name) 

blu_lines

species <- "CPR"
path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"
rock_data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

rock_pred1 <- predict(rock_data$models$m1, rock_data$data_sp, re.form = NA)
rock_pred2 <- predict(rock_data$models$m2, rock_data$data_sp, re.form = NA)
rock_pred3 <- predict(rock_data$models$m3, rock_data$data_sp, re.form = NA)

rock_new_data <- rock_data$data_sp %>% 
  mutate(pred1 = exp(rock_pred1) - 1,
         pred2 = exp(rock_pred2) - 1,
         pred3 = exp(rock_pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))

cpr_lines <- ggplot(rock_new_data) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = "Amount of preferred habitat (m2)",
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name) 

cpr_lines


species <- "OYT"
path <- "analyses/7habitat/output/refine_pref_habitat/rock/all_regions"
rock_data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

rock_pred1 <- predict(rock_data$models$m1, rock_data$data_sp, re.form = NA)
rock_pred2 <- predict(rock_data$models$m2, rock_data$data_sp, re.form = NA)
rock_pred3 <- predict(rock_data$models$m3, rock_data$data_sp, re.form = NA)

rock_new_data <- rock_data$data_sp %>% 
  mutate(pred1 = exp(rock_pred1) - 1,
         pred2 = exp(rock_pred2) - 1,
         pred3 = exp(rock_pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))

oyt_lines <- ggplot(rock_new_data) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm", show.legend = F) +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = NULL,
       x = "Amount of preferred habitat (m2)",
       y = NULL,
       color = NULL,
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~model_name) 

oyt_lines

blu_lines / cpr_lines / oyt_lines

