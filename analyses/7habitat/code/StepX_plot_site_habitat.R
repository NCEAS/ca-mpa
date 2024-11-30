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
  dplyr::select(site, site_type, bioregion, affiliated_mpa, size_km2, 
                all_of(setdiff(unique(unlist(pred_kelp)), c("site_type", "age_at_survey")))) %>% distinct()

data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% 
  dplyr::select(site, site_type, bioregion, affiliated_mpa, size_km2, 
                all_of(setdiff(unique(unlist(pred_surf)), c("site_type", "age_at_survey")))) %>% distinct()

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% 
  dplyr::select(site, site_type, bioregion, affiliated_mpa, size_km2, 
                all_of(setdiff(unique(unlist(pred_rock)), c("site_type", "age_at_survey")))) %>% distinct()

#habitat_predictors <- grep("^(hard|soft)", names(data_kelp), value = TRUE)

# Make plots -----------------------------------------------------------------------------

surf <- data_surf %>% 
  pivot_longer(cols = grep("^(hard|soft)", names(data_surf), value = TRUE),
               names_to = "habitat",
               values_to = "area") %>% 
  mutate(scale = sub("_", "", str_sub(habitat, -3, -1)),
         bioregion = factor(bioregion, levels = c("North", "Central", "South")),
         habitat2 = habitat %>%
           sub("_[^_]*$", "", .) %>%
           str_replace_all("_", " ") %>%
           str_to_title() %>%
           factor(levels = c("Soft Bottom 0 30m", "Soft Bottom Biotic 0 30m", 
                             "Hard Bottom Biotic 0 30m", "Hard Bottom 0 30m"))) 


ggplot(data = surf %>% filter(scale == "100") %>% 
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
                               "Hard Bottom Biotic 0 30m" = "#006400", 
                               "Soft Bottom Biotic 0 30m" = "#32CD32",
                               "Soft Bottom 0 30m" = "#D2B48C"))


rock <- data_rock %>% 
  pivot_longer(cols = grep("^(hard|soft)", names(data_kelp), value = TRUE),
               names_to = "habitat",
               values_to = "area") %>% 
  mutate(scale = sub("_", "", str_sub(habitat, -3, -1)),
         bioregion = factor(bioregion, levels = c("North", "Central", "South")),
         habitat2 = habitat %>%
           sub("_[^_]*$", "", .) %>%
           str_replace_all("_", " ") %>%
           str_to_title() %>% 
           factor(levels = c("Soft Bottom 0 30m", "Soft Bottom 30 100m",
                             "Soft Bottom Biotic 0 30m", "Hard Bottom Biotic 0 30m",
                             "Hard Bottom 0 30m", "Hard Bottom 30 100m"))) %>% 
  group_by(scale, site) %>%
  mutate(total_area = sum(area, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    site_scale = paste(scale, site, sep = "_"), # Create a unique identifier for site within each scale
    site_scale = fct_reorder(site_scale, -total_area)
  )

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
      "Hard Bottom 0 30m" = "#8B4513",         # Dark brown
      "Hard Bottom Biotic 0 30m" = "#006400", # Dark green
      "Soft Bottom Biotic 0 30m" = "#32CD32", # Medium green
      "Soft Bottom 0 30m" = "#D2B48C",        # Light brown
      "Hard Bottom 30 100m" = "#5C3317",       # Darker brown
      "Soft Bottom 30 100m" = "#A6896B"        # Darker light brown
    )
  )


variability <- kelp %>% 
  group_by(bioregion, habitat) %>% 
  summarize(mean = mean(area, na.rm = T),
            sd = sd(area, na.rm = T))


# Rock
kelp <- data_kelp %>% 
  pivot_longer(cols = grep("^(hard|soft)", names(data_kelp), value = TRUE),
               names_to = "habitat",
               values_to = "area") %>% 
  mutate(scale = sub("_", "", str_sub(habitat, -3, -1)),
         bioregion = factor(bioregion, levels = c("North", "Central", "South")),
         habitat2 = habitat %>%
           sub("_[^_]*$", "", .) %>%
           str_replace_all("_", " ") %>%
           str_to_title() %>% 
           factor(levels = c("Soft Bottom 0 30m", "Soft Bottom 30 100m",
                             "Soft Bottom Biotic 0 30m", "Hard Bottom Biotic 0 30m",
                             "Hard Bottom 0 30m", "Hard Bottom 30 100m"))) %>% 
  group_by(scale, site) %>%
  mutate(total_area = sum(area, na.rm = TRUE)) %>%
  ungroup() %>%
  mutate(
    site_scale = paste(scale, site, sep = "_"), # Create a unique identifier for site within each scale
    site_scale = fct_reorder(site_scale, -total_area)
  )

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
      "Hard Bottom 0 30m" = "#8B4513",         # Dark brown
      "Hard Bottom Biotic 0 30m" = "#006400", # Dark green
      "Soft Bottom Biotic 0 30m" = "#32CD32", # Medium green
      "Soft Bottom 0 30m" = "#D2B48C",        # Light brown
      "Hard Bottom 30 100m" = "#5C3317",       # Darker brown
      "Soft Bottom 30 100m" = "#A6896B"        # Darker light brown
    )
  )



