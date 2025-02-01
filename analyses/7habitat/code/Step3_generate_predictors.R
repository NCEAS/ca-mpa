# Generate habitat predictors
# Nov 2024
# Cori Lopazanski


# This script generates the list of predictors for each ecosystem. 
# 1. For each buffer radii, identify habitat types that are included in at least one size (not all = 0)
# 2. Select base predictors: site*age, size
# 3. Create list of all combinations of habitat types for each buffer radii + base predictors
# 4. Filter to just the unique combinations
# 5. Export the habitat predictors list for each ecosystem

# Setup ----
library(tidyverse)
library(corrr)
library(patchwork)

# Read data -----
rm(list = ls())
gc()

# Load the data to examine the habitat characteristics across sites for each ecosystem
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"
ltm.dir <- "/Users/lopazanski/Desktop/ltm/update_2024"

# This selecting excludes the biotic habitats from PMEP (kelp only from kelpwatch)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% # 148 sites
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth)", names(.), value = TRUE))) %>% 
  distinct() 
  
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% # 26 sites
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth)", names(.), value = TRUE))) %>% 
  distinct()

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% # 335 sites (grid cells)
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth)", names(.), value = TRUE))) %>% 
  distinct() 

data_deep <- readRDS(file.path(ltm.dir, "combine_tables/deep_full.Rds")) %>% 
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth)", names(.), value = TRUE))) %>% 
  distinct()

# Build initial predictor lists ----
# Identify the predictors to retain for each ecosystem 
# -- Remove predictors that are zero across all of the sites 
# -- Explore those that are limited in variability within a region (could skew results)

kelp_predictors <- data_kelp %>%
  dplyr::select(year, site, site_type, bioregion, where(~ max(.) > 0)) %>% 
  pivot_longer(cols = -c(site, site_type, bioregion, year), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>%
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "landward")) %>% 
  filter(!str_detect(predictor, "100_200m")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)),
         pred_group = case_when(str_detect(predictor, "0_30m|30_100m|100_200m|200m") ~ "depth",
                                str_detect(predictor, "kelp|depth") ~ "all",
                                T ~ "combined")) 

rock_predictors <- data_rock %>%
  dplyr::select(year, site, site_type, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(year, site, site_type, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>% 
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "landward")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)),
         pred_group = case_when(str_detect(predictor, "0_30m|30_100m|100_200m|200m") ~ "depth",
                                str_detect(predictor, "kelp|depth") ~ "all",
                                T ~ "combined")) 

surf_predictors <- data_surf %>%
  dplyr::select(year, site, site_type, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(year, site, site_type, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value, na.rm = T), .groups = "drop") %>%
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "landward")) %>% 
  filter(!str_detect(predictor, "kelp_annual_50")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)),
         pred_group = case_when(str_detect(predictor, "0_30m|30_100m|100_200m|200m") ~ "depth",
                                str_detect(predictor, "kelp|depth") ~ "all",
                                T ~ "combined")) 


# surf_na <- data_surf  %>% 
#   filter(if_any(everything(), is.na)) %>% # Keep rows with any NA
#   dplyr::select(site, where(~ any(is.na(.))))          # Keep columns with any

deep_predictors <- data_deep %>% 
  dplyr::select(year, site, site_type, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(year, site, site_type, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>% 
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "landward")) %>% 
  filter(!str_detect(predictor, "kelp_annual_100")) %>% 
  filter(!str_detect(predictor, "kelp_annual_250")) %>% 
  filter(!str_detect(predictor, "bottom_200m")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)),
         pred_group = case_when(str_detect(predictor, "0_30m|30_100m|100_200m|200m") ~ "depth",
                                str_detect(predictor, "kelp|depth") ~ "all",
                                T ~ "combined")) 


# Save the predictor list for subsetting the dataset to only the variables of interest
saveRDS(kelp_predictors, file.path("analyses/7habitat/intermediate_data", "kelp_predictors.Rds")) 
saveRDS(rock_predictors, file.path("analyses/7habitat/intermediate_data", "rock_predictors.Rds")) 
saveRDS(surf_predictors, file.path("analyses/7habitat/intermediate_data", "surf_predictors.Rds")) 
saveRDS(deep_predictors, file.path("analyses/7habitat/intermediate_data", "deep_predictors.Rds"))


# Examine correlation -----

# Create site tables
kelp <- data_kelp %>% dplyr::select(site, site_type, bioregion, all_of(kelp_predictors$predictor))
rock <- data_rock %>% dplyr::select(site, site_type, bioregion, all_of(rock_predictors$predictor))
surf <- data_surf %>% dplyr::select(site, site_type, bioregion, all_of(surf_predictors$predictor))
deep <- data_deep %>% dplyr::select(site, site_type, bioregion, all_of(deep_predictors$predictor))

# Plot correlations

plot_site_corr <- function(site_table, predictor_list){
  sites <- site_table %>%
    # Get means across all years for kelp canopy cover
    group_by(site, site_type, bioregion) %>%
    summarize(across(where(is.numeric), mean, na.rm = T), .groups = 'drop') %>%
    dplyr::select(!c(site, site_type, bioregion))

  matrix_depth <- sites %>%
    dplyr::select(any_of(predictor_list %>%
                    filter(pred_group %in% c("all", "depth")) %>%
                    pull(predictor))) %>%
    correlate() %>% rearrange()

  p1 <- rplot(matrix_depth %>%  shave()) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

  matrix_combined <- sites %>%
    dplyr::select(any_of(predictor_list %>%
                    filter(pred_group %in% c("all", "combined")) %>%
                    pull(predictor))) %>%
    correlate() %>%   rearrange()

  p2 <- rplot(matrix_combined %>%  shave()) +
    theme(axis.text.x = element_text(angle = 60, hjust = 1))

  matrix_depth_long <- matrix_depth %>%
    stretch() %>%
    rename(Var1 = x, Var2 = y, value = r) %>%
    mutate(buffer1 = sub(".*_", "", Var1),
           buffer2 = sub(".*_", "", Var2),
           habitat1 = sub("_[^_]*$", "", Var1),
           habitat2 = sub("_[^_]*$", "", Var2)) %>%
    filter(buffer1 == buffer2) %>%
    mutate(buffer = as.numeric(buffer1)) %>%
    filter(as.character(Var1) <= as.character(Var2)) %>%
    filter(!(Var1 == Var2)) %>%
    dplyr::select(-buffer1, -buffer2)

  p3 <- ggplot(data = matrix_depth_long, aes(x = habitat1, y = habitat2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 65, vjust = 1,   size = 10, hjust = 1)) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~ buffer, scales = "free")

  matrix_combined_long <- matrix_combined %>%
    stretch() %>%
    rename(Var1 = x, Var2 = y, value = r) %>%
    mutate(buffer1 = sub(".*_", "", Var1),
           buffer2 = sub(".*_", "", Var2),
           habitat1 = sub("_[^_]*$", "", Var1),
           habitat2 = sub("_[^_]*$", "", Var2)) %>%
    filter(buffer1 == buffer2) %>%
    mutate(buffer = as.numeric(buffer1)) %>%
    filter(as.character(Var1) <= as.character(Var2)) %>%
    filter(!(Var1 == Var2)) %>%
    dplyr::select(-buffer1, -buffer2)

  p4 <- ggplot(data = matrix_combined_long, aes(x = habitat1, y = habitat2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1, 1), space = "Lab",
                         name = "Pearson\nCorrelation") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 65, vjust = 1,   size = 10, hjust = 1)) +
    labs(x = NULL, y = NULL) +
    facet_wrap(~ buffer, scales = "free")

  return( (p1 + p2) / (p3 + p4))

}

#plot_site_corr(site_table = kelp, predictor_list = kelp_predictors)
#plot_site_corr(site_table = deep, predictor_list = deep_predictors)
#plot_site_corr(site_table = surf, predictor_list = surf_predictors)
#plot_site_corr(site_table = rock, predictor_list = rock_predictors)

# Take Home:
# - Each metric is correlated with itself across various scales = use one scale at a time for each metric
# - Depth metrics (sd and mean) are correlated with each other = use one metric at a time for depth
# - Hard/soft bottom correlated with each other (strongest within each scale/depth)
#       Use consolidated version since depth is included?
#       Pick one or calculate proportion/ratio?
# - Probably still okay to cross scales ACROSS habitat metrics (e.g. hard 250 + kelp 100)
  
# Generate combinations independent of scale for the depth and kelp, using hard only
get_list <- function(predictors_df){
  # Create depth combinations
  depth_combos <- predictors_df %>%
    filter(pred_group %in% c("all", "combined")) %>% 
    filter(str_detect(predictor, "depth")) %>%
    mutate(intx = paste0(predictor, " * site_type"),
           intx2 = paste0(predictor, "* site_type * age_at_survey")) %>%
    pivot_longer(cols = c(predictor, intx, intx2), names_to = NULL, values_to = "depth_predictor") %>%
    bind_rows(tibble(scale = NA, depth_predictor = NA)) %>% 
    summarize(depth_predictor = list(c(depth_predictor)), .groups = 'drop')
  
  # Create kelp combinations
  kelp_combos <- predictors_df %>%
    filter(str_detect(predictor, "kelp")) %>%
    mutate(intx = paste0(predictor, " * site_type"),
           intx2 = paste0(predictor, "* site_type * age_at_survey")) %>%
    pivot_longer(cols = c(predictor, intx, intx2), names_to = NULL, values_to = "kelp_predictor") %>%
    bind_rows(tibble(scale = NA, kelp_predictor = NA)) %>% 
    summarize(kelp_predictor = list(c(kelp_predictor)), .groups = 'drop') 
  
  # Create hard combinations
  hard_combos <- predictors_df %>% 
    filter(pred_group %in% c("all", "combined")) %>% 
    filter(str_detect(predictor, "hard")) %>% 
    mutate(intx = paste0(predictor, " * site_type"),
           intx2 = paste0(predictor, "* site_type * age_at_survey")) %>%
    pivot_longer(cols = c(predictor, intx, intx2), names_to = NULL, values_to = "hard_predictor") %>%
    bind_rows(tibble(scale = NA, hard_predictor = NA)) 

  list_intx <- hard_combos %>% 
    dplyr::select(hard_predictor, hard_scale = scale) %>% 
    bind_cols(kelp_combos) %>% 
    unnest(kelp_predictor) %>% 
    bind_cols(depth_combos) %>% 
    unnest(depth_predictor) %>% 
    mutate(kelp_scale = str_extract(kelp_predictor, "\\d+"),
           depth_scale = str_extract(depth_predictor, "\\d+")) %>% 
    mutate(base_predictor = "site_type * age_at_survey") %>% 
    unite("predictors", c(hard_predictor, kelp_predictor, depth_predictor, base_predictor),
          sep = " + ",  na.rm = TRUE, remove = FALSE) %>% 
    mutate(type = case_when(str_detect(hard_predictor, "age_at_survey") & 
                               str_detect(kelp_predictor, "age_at_survey") &
                               str_detect(depth_predictor, "age_at_survey") &
                               (hard_scale == kelp_scale) &
                               (kelp_scale == depth_scale) ~ "core_3way",
                            str_detect(hard_predictor, "age_at_survey") & 
                              str_detect(kelp_predictor, "age_at_survey") &
                              is.na(depth_predictor) &
                              (hard_scale == kelp_scale) ~ "no_depth_3way",
                            str_detect(hard_predictor, "site") & 
                               str_detect(kelp_predictor, "site") &
                               str_detect(depth_predictor, "site") &
                               !str_detect(hard_predictor, "age_at_survey") & 
                               !str_detect(kelp_predictor, "age_at_survey") &
                               !str_detect(depth_predictor, "age_at_survey") &
                               (hard_scale == kelp_scale) &
                               (kelp_scale == depth_scale) ~ "core_2way",
                            str_detect(hard_predictor, "site") & 
                              str_detect(kelp_predictor, "site") &
                              is.na(depth_predictor) &
                              !str_detect(hard_predictor, "age_at_survey") & 
                              !str_detect(kelp_predictor, "age_at_survey") &
                              (hard_scale == kelp_scale) ~ "no_depth_2way",
                             predictors == "site_type * age_at_survey" ~ "base",
                             T~NA)) %>% 
    mutate(model_id = 
             str_replace_all(predictors, "hard_bottom_(\\d+)", "H\\1") %>% 
             str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
             str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
             str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
             str_replace_all("depth_cv_(\\d+)", "DCV\\1") %>% 
             str_replace_all("site_type", "ST") %>%
             str_replace_all("age_at_survey", "A") %>% 
             str_replace_all("\\s+", "")) %>% 
    dplyr::select(predictors, type, model_id, hard_predictor, kelp_predictor, depth_predictor, hard_scale, kelp_scale, depth_scale)
  
  return(list_intx)
}


kelp_list <- get_list(kelp_predictors) # 2541 now 7936 now 11776
rock_list <- get_list(rock_predictors) # 2541 now 7936 now 11776
surf_list <- get_list(surf_predictors) # 1155 now 3472 now 5152
deep_list <- get_list(deep_predictors) # 693 now 1984 now 2944

saveRDS(kelp_list, file.path("analyses/7habitat/intermediate_data", "kelp_predictors_interactions.Rds")) 
saveRDS(rock_list, file.path("analyses/7habitat/intermediate_data", "rock_predictors_interactions.Rds")) 
saveRDS(surf_list, file.path("analyses/7habitat/intermediate_data", "surf_predictors_interactions.Rds")) 
saveRDS(deep_list, file.path("analyses/7habitat/intermediate_data", "deep_predictors_interactions.Rds")) 


