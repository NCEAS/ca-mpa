# Plot some of the habitat data
# Cori Lopazanski
# Nov 2024

# Setup --------------------------------------------------------------------------------

rm(list = ls())
gc()

ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

# Read data -----------------------------------------------------------------------------

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) #%>% map(~ .x[.x != "site_type * age_at_survey"])
pred_surf <- readRDS(file.path("analyses/7habitat/intermediate_data/surf_predictors.Rds")) 
pred_rock <- readRDS(file.path("analyses/7habitat/intermediate_data/rock_predictors.Rds"))

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) %>% 
  dplyr::select(site, site_type, bioregion, affiliated_mpa, size_km2, year,
                all_of(pred_kelp$predictor)) %>% distinct()

data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% 
  dplyr::select(site, site_type, bioregion, affiliated_mpa, size_km2, year,
                all_of(pred_surf$predictor)) %>% distinct()

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% 
  dplyr::select(site, site_type, bioregion, affiliated_mpa, size_km2, 
                all_of(pred_rock$predictor)) %>% distinct() 
#habitat_predictors <- grep("^(hard|soft)", names(data_kelp), value = TRUE)

# Make plots -----------------------------------------------------------------------------

surf <- data_surf %>% 
  pivot_longer(cols = grep("^(hard|soft|kelp)", names(data_surf), value = TRUE),
               names_to = "habitat",
               values_to = "area") %>% 
  mutate(scale = sub("_", "", str_sub(habitat, -3, -1)),
         bioregion = factor(bioregion, levels = c("North", "Central", "South")),
         habitat2 = habitat %>%
           sub("_[^_]*$", "", .) %>%
           str_replace_all("_", " ") %>%
           str_to_title() %>%
           factor(levels = c("Soft Bottom 0 30m", "Hard Bottom 0 30m", "Kelp Annual"))) %>% 
  group_by(site, site_type, bioregion, affiliated_mpa, size_km2, habitat, scale, habitat2) %>% 
  summarize(area = mean(area, na.rm = T))


ggplot(data = surf %>% filter(scale == "500") %>% 
         mutate(site = str_remove(site, "Surf zone-")) %>% 
         arrange(affiliated_mpa)) +
  geom_col(aes(x = site, y = area, fill = habitat2), position = "stack") +
  facet_wrap(~site_type+bioregion, scales = "free") +
  labs(x = "Site",
       y = "Area",
       fill = NULL) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
        legend.position = "top") +
  scale_fill_manual(values = c("Hard Bottom 0 30m" = "#8B4513",    
                               "Kelp Annual" = "#006400", 
                               "Soft Bottom 0 30m" = "#D2B48C"))


rock <- data_rock %>% 
  pivot_longer(cols = grep("^(hard|soft|kelp)", names(data_rock), value = TRUE),
               names_to = "habitat",
               values_to = "area") %>% 
  mutate(scale = sub("_", "", str_sub(habitat, -3, -1)),
         bioregion = factor(bioregion, levels = c("North", "Central", "South")),
         habitat2 = habitat %>%
           sub("_[^_]*$", "", .) %>%
           str_replace_all("_", " ") %>%
           str_to_title() %>% 
           factor(levels = c("Kelp Annual", "Soft Bottom 0 30m", "Soft Bottom 30 100m",
                             "Hard Bottom 0 30m", "Hard Bottom 30 100m"))) %>% 
  group_by(site, site_type, bioregion, affiliated_mpa, size_km2, habitat, scale, habitat2) %>% 
  summarize(area = mean(area, na.rm = T))

ggplot(data = rock %>% 
         filter(scale == "500") %>% arrange(affiliated_mpa)) +
  geom_col(aes(x = site, y = area, fill = habitat2), position = "stack") +
  geom_hline(yintercept = 785398, linetype = "dashed", color = "red") +
  labs(x = "Site",
       y = "Area",
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~site_type+bioregion, scales = "free") +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
        axis.text.x = element_blank(),
        legend.position = "top") +
  scale_fill_manual(
    values = c(
      "Hard Bottom 0 30m" = "#8B4513",         
      "Kelp Annual" = "#006400",
      "Soft Bottom 0 30m" = "#D2B48C",     
      "Hard Bottom 30 100m" = "#5C3317",  
      "Soft Bottom 30 100m" = "#A6896B"      
    )
  )


variability <- kelp %>% 
  group_by(bioregion, habitat) %>% 
  summarize(mean = mean(area, na.rm = T),
            sd = sd(area, na.rm = T))


# Kelp
kelp <- data_kelp %>% 
  pivot_longer(cols = grep("^(hard|soft|kelp)", names(data_kelp), value = TRUE),
               names_to = "habitat",
               values_to = "area") %>% 
  mutate(scale = sub("_", "", str_sub(habitat, -3, -1)),
         bioregion = factor(bioregion, levels = c("North", "Central", "South")),
         habitat2 = habitat %>%
           sub("_[^_]*$", "", .) %>%
           str_replace_all("_", " ") %>%
           str_to_title() %>% 
           factor(levels = c("Kelp Annual", "Soft Bottom 0 30m", "Soft Bottom 30 100m",
                             "Hard Bottom 0 30m", "Hard Bottom 30 100m"))) %>% 
  group_by(site, site_type, bioregion, affiliated_mpa, size_km2, habitat, scale, habitat2) %>% 
  summarize(area = mean(area, na.rm = T))


ggplot(data = kelp %>% 
         filter(scale == "100") %>% arrange(affiliated_mpa)) +
  geom_col(aes(x = site, y = area, fill = habitat2), position = "stack") +
  geom_hline(yintercept = 31416, linetype = "dashed", color = "red") +
  labs(x = "Site",
       y = "Area",
       fill = NULL) +
  theme_minimal() +
  facet_wrap(~site_type+bioregion, scales = "free") +
  theme(#axis.text.x = element_text(angle = 45, hjust = 1), # Rotate x-axis labels for better readability
    axis.text.x = element_blank(),
    legend.position = "top") +
  scale_fill_manual(
    values = c(
      "Hard Bottom 0 30m" = "#8B4513",        
      "Kelp Annual" = "#006400", 
      "Soft Bottom 0 30m" = "#D2B48C",        
      "Hard Bottom 30 100m" = "#5C3317",      
      "Soft Bottom 30 100m" = "#A6896B"        
    )
  )



