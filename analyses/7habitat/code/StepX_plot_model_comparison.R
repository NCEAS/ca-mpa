# Step X Plot predictions
# Cori Lopazanski
# Nov 2024

# This script takes the various final models and plots the predictions

# Setup --------------------------------------------------------------------
library(patchwork)
library(merTools)

rm(list = ls())

out.dir <- "analyses/7habitat/output/refine_pref_habitat/all_regions"
sp_list <- unique(examine_habitat$species_code)


pdf("analyses/7habitat/output/refine_pref_habitat/all_regions/species-habitat-plots.pdf", width = 7, height = 4) 

for (species in sp_list){
  sp_models <- readRDS(file.path(out.dir, paste0(species, "_model_comparison.rds")))
  
  pred1 <- predict(sp_models$models$m1, sp_models$data_sp, re.form = NA)
  pred2 <- predict(sp_models$models$m2, sp_models$data_sp, re.form = NA)
  pred3 <- predict(sp_models$models$m3, sp_models$data_sp, re.form = NA)
  
  new_data <- sp_models$data_sp %>% 
    mutate(pred1 = exp(pred1 - 1),
           pred2 = exp(pred2 - 1),
           pred3 = exp(pred3 - 1),
    ) %>% 
    pivot_longer(cols = starts_with("pred"), 
                 names_to = "model", values_to = "value") %>% 
    mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                         model == "pred2" ~ "Protection + Pref. Habitat",
                                         model == "pred3" ~ "Protection * Pref. Habitat"), 
                               levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))
  
  p <- ggplot(new_data) +
    geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
    scale_color_manual(values = c("#e5188b", "#7e67f8")) +
    labs(title = paste0("Species: ", species, 
                        "\nTarget Status: ", unique(new_data$target_status), 
                        "\nRegions: ", paste(unique(new_data$bioregion), collapse = ", "),
                        "\nPref. Habitat: ", paste(sp_models$preferred_habitat, collapse = ", ")),
         x = "Amount of preferred habitat (m2)",
         y = "Predicted Biomass (kg per m2)",
         color = "Site Type",
         fill = "Site Type") +
    theme_minimal() +
    facet_wrap(~model_name)
  
  print(p)

}

dev.off()

##  Individually 
species <- "AARG"
path <- "analyses/7habitat/output/refine_pref_habitat/surf/all_regions"
data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

pred1 <- predict(data$models$m1, data$data_sp, re.form = NA)
pred2 <- predict(data$models$m2, data$data_sp, re.form = NA)
pred3 <- predict(data$models$m3, data$data_sp, re.form = NA)

new_data <- data$data_sp %>% 
  mutate(pred1 = exp(pred1) - 1 ,
         pred2 = exp(pred2) - 1,
         pred3 = exp(pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))

p <- ggplot(new_data) +
  geom_point(data = data$data_sp,
             aes(x = pref_habitat, y = kg_per_haul, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = paste0("Species: ", species, 
                      "\nTarget Status: ", unique(new_data$target_status), 
                      "\nRegions: ", paste(unique(new_data$bioregion), collapse = ", "),
                      "\nPref. Habitat: ", paste(data$preferred_habitat, collapse = ", ")),
       x = "Amount of preferred habitat (m2)",
       y = "Predicted Biomass (kg per haul)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
p


##  Individually 
species <- "OYT"
path <- "analyses/7habitat/output/refine_pref_habitat/all_regions"
data <- readRDS(file.path(path, paste0(species, "_model_comparison.rds")))

pred1 <- predict(data$models$m1, data$data_sp, re.form = NA)
pred2 <- predict(data$models$m2, data$data_sp, re.form = NA)
pred3 <- predict(data$models$m3, data$data_sp, re.form = NA)

new_data <- data$data_sp %>% 
  mutate(pred1 = exp(pred1) - 1,
         pred2 = exp(pred2) - 1,
         pred3 = exp(pred3) - 1,
  ) %>% 
  pivot_longer(cols = starts_with("pred"), 
               names_to = "model", values_to = "value") %>% 
  mutate(model_name = factor(case_when(model == "pred1" ~ "Protection",
                                       model == "pred2" ~ "Protection + Pref. Habitat",
                                       model == "pred3" ~ "Protection * Pref. Habitat"), 
                             levels = c("Protection",  "Protection + Pref. Habitat", "Protection * Pref. Habitat")))

p <- ggplot(new_data) +
  geom_point(data = data$data_sp,
             aes(x = pref_habitat, y = kg_per_m2, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = paste0("Species: ", species, 
                      "\nTarget Status: ", unique(new_data$target_status), 
                      "\nRegions: ", paste(unique(new_data$bioregion), collapse = ", "),
                      "\nPref. Habitat: ", paste(data$preferred_habitat, collapse = ", ")),
       x = "Amount of preferred habitat (m2)",
       y = "Predicted Biomass (kg per m2)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
p

p <- ggplot(new_data) +
 # geom_point(data = data$data_sp,
 #            aes(x = pref_habitat, y = kg_per_m2, color = site_type), alpha = 0.2) +
  geom_smooth(aes(x = pref_habitat, y = value, color = site_type), method = "glm") +
  scale_color_manual(values = c("#e5188b", "#7e67f8")) +
  labs(title = paste0("Species: ", species, 
                      "\nTarget Status: ", unique(new_data$target_status), 
                      "\nRegions: ", paste(unique(new_data$bioregion), collapse = ", "),
                      "\nPref. Habitat: ", paste(data$preferred_habitat, collapse = ", ")),
       x = "Amount of preferred habitat (m2)",
       y = "Predicted Biomass (kg per m2)",
       color = "Site Type",
       fill = "Site Type") +
  theme_minimal() +
  facet_wrap(~model_name)
p
