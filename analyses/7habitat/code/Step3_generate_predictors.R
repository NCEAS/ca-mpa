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
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024/2025"

# This selecting excludes the biotic habitats from PMEP (kelp only from kelpwatch)
data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_full.Rds")) %>% # 148 sites
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri)", names(.), value = TRUE))) %>% 
  distinct() 
  
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% # 26 sites
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri)", names(.), value = TRUE))) %>% 
  distinct()

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% # 335 sites (grid cells)
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri)", names(.), value = TRUE))) %>% 
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
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)),
         pred_group = case_when(str_detect(predictor, "0_30m|30_100m|100_200m|200m") ~ "depth",
                                str_detect(predictor, "kelp|depth|aquatic") ~ "all",
                                T ~ "combined")) %>% 
  filter(!predictor == "depth_cv_25")

rock_predictors <- data_rock %>%
  dplyr::select(year, site, site_type, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(year, site, site_type, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>% 
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)),
         pred_group = case_when(str_detect(predictor, "0_30m|30_100m|100_200m|200m") ~ "depth",
                                str_detect(predictor, "kelp|depth") ~ "all",
                                T ~ "combined")) %>% 
  filter(!predictor == "kelp_annual_25") %>% 
  filter(!predictor == "depth_cv_25")

surf_predictors <- data_surf %>%
  dplyr::select(year, site, site_type, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(year, site, site_type, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value, na.rm = T), .groups = "drop") %>%
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)),
         pred_group = case_when(str_detect(predictor, "0_30m|30_100m|100_200m|200m") ~ "depth",
                                str_detect(predictor, "kelp|depth|aquatic") ~ "all",
                                T ~ "combined")) %>% 
  filter(!predictor == "depth_cv_25") %>% 
  filter(!predictor %in% c("kelp_annual_25", "kelp_annual_50", "kelp_annual_100"))


# Save the predictor list for subsetting the dataset to only the variables of interest
saveRDS(kelp_predictors, file.path("analyses/7habitat/intermediate_data", "kelp_predictors.Rds")) 
saveRDS(rock_predictors, file.path("analyses/7habitat/intermediate_data", "rock_predictors.Rds")) 
saveRDS(surf_predictors, file.path("analyses/7habitat/intermediate_data", "surf_predictors.Rds")) 


# Examine correlation -----

# Create site tables
kelp <- data_kelp %>% dplyr::select(site, site_type, bioregion, all_of(kelp_predictors$predictor))
rock <- data_rock %>% dplyr::select(site, site_type, bioregion, all_of(rock_predictors$predictor))
surf <- data_surf %>% dplyr::select(site, site_type, bioregion, all_of(surf_predictors$predictor))

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

  plot <- (p1 + p2) / (p3 + p4)
  
  hard_soft_values <- matrix_combined %>% 
    filter(str_detect(term, "hard")) %>% 
    dplyr::select(term1 = term, contains("soft")) %>% 
    pivot_longer(cols = contains('soft'), names_to = "term2", values_to = 'corr') %>% 
    mutate(term1_scale = str_extract(term1, "\\d+"),
           term2_scale = str_extract(term2, "\\d+")) %>% 
    filter(term1_scale == term2_scale)
  
  return(list(plot = plot, hard_soft_values = hard_soft_values))

}

kelp_site_corr <- plot_site_corr(site_table = kelp, predictor_list = kelp_predictors)
#plot_site_corr(site_table = deep, predictor_list = deep_predictors)
plot_site_corr(site_table = surf, predictor_list = surf_predictors)
rock_site_corr <- plot_site_corr(site_table = rock, predictor_list = rock_predictors)

# Take Home:
# - Each metric is correlated with itself across various scales = use one scale at a time for each metric
# - Depth metrics (sd and mean) are correlated with each other = use one metric at a time for depth
# - Hard/soft bottom correlated with each other (strongest within each scale/depth)
#       Use consolidated version since depth is included?
#       Pick one or calculate proportion/ratio?
# - Probably still okay to cross scales ACROSS habitat metrics (e.g. hard 250 + kelp 100)

# Generate combinations independent of scale for the depth and kelp, using hard only


# Add soft and aquatic vegetation bed



# Test adding "|soft" (Feb 19) to see if made more sense to have either hard or soft bottom as 
# the substrate predictor, but made more confusing when interpreting output because some still
# don't map as expected or have multiple in the top model because they are so correlated. So 
# will keep just hard and use the direction to interpret the preference. Maybe rename to "substrate"?

# Now that CV is a depth option, remove SD. Reasoning: SD tends to have more outliers relative to 
# the other depth characteristics, and CV is more likely to be showing the actual structural var.
# for the site (how variable is it relative to its average). There is also correlation between
# depth_mean and depth_sd, and depth_sd and depth_cv. So eliminating SD likely makes sense?

# Test adding both depths (mean and cv, either at matching scale or across scales) as potential 
# predictors, since they are not highly correlated. But - outputs from models makes it harder to 
# interpret depth independently when both are in the model, and makes depth factors more dominant 
# in the models themselves. Opt out for now?
# Challenge remains that there are more candidate models for different depths, so in the model averaging
# this can get confusing about which remain the most important if both are not options. 

# Dropped the 25m scale for kelp forest predictors on Feb 26 after looking more closely at the
# distribution of the variable - was almost binary and causing fit challenges.


# Include 3-way interactions ----

get_3way_list <- function(predictors_df){
  predictors_df <- predictors
  predictors_df <- predictors_df %>% 
    filter(pred_group %in% c("all", "combined")) %>% 
    filter(!str_detect(predictor, "depth_sd")) %>% 
    mutate(st = paste0(predictor, " * site_type"),
           sta = paste0(predictor, " * site_type * age_at_survey"),
           st_a = paste0(predictor, " * site_type + ", predictor, " * age_at_survey"),
           a = paste0(predictor, " * age_at_survey"))
  
  hard_vars <- predictors_df %>% filter(str_detect(predictor, "hard"))
  kelp_vars <- predictors_df %>% filter(str_detect(predictor, "kelp")) 
  depth_vars <- predictors_df %>% filter(str_detect(predictor, "depth")) 
  
  # Version with both depths all scales
  depth_comb <- expand.grid(depth_mean = c(depth_vars$predictor[str_detect(depth_vars$predictor, "depth_mean")],
                                           depth_vars$st[str_detect(depth_vars$st, "depth_mean")], 
                                           depth_vars$sta[str_detect(depth_vars$sta, "depth_mean")],
                                           depth_vars$st_a[str_detect(depth_vars$st_a, "depth_mean")],
                                           depth_vars$a[str_detect(depth_vars$a, "depth_mean")]),
                            depth_cv = c(depth_vars$predictor[str_detect(depth_vars$predictor, "depth_cv")],
                                         depth_vars$st[str_detect(depth_vars$st, "depth_cv")], 
                                         depth_vars$sta[str_detect(depth_vars$sta, "depth_cv")],
                                         depth_vars$st_a[str_detect(depth_vars$st_a, "depth_cv")],
                                         depth_vars$a[str_detect(depth_vars$a, "depth_cv")])) %>%
    unite("predictors", c(depth_mean, depth_cv), sep = " + ") %>% pull(predictors)
  
  # Extract interaction terms
  hard_st       <- hard_vars %>% pull(st)
  kelp_st       <- kelp_vars %>% pull(st)
  depth_st      <- depth_vars %>% pull(st)
  
  hard_sta  <- hard_vars %>% pull(sta)
  kelp_sta  <- kelp_vars %>% pull(sta)
  depth_sta <- depth_vars %>% pull(sta)
  
  hard_st_a  <- hard_vars %>% pull(st_a)
  kelp_st_a  <- kelp_vars %>% pull(st_a)
  depth_st_a <- depth_vars %>% pull(st_a)
  
  hard_a <- hard_vars %>% pull(a)
  kelp_a <- kelp_vars %>% pull(a)
  depth_a <- depth_vars %>% pull(a)
  
  # Generate all models (all combinations of H, K, and D at any scale)
  pred_list <- 
    expand_grid(hard  = c(NA, hard_vars %>% pull(predictor), hard_st, hard_a, hard_st_a, hard_sta),
                kelp  = c(NA, kelp_vars %>% pull(predictor), kelp_st, kelp_a, kelp_st_a, kelp_sta),
                depth = c(NA, depth_vars %>% pull(predictor), depth_st, depth_a, depth_st_a, depth_sta, depth_comb)) %>% 
    mutate(hard_scale  = str_extract(hard, "\\d+"),
           kelp_scale  = str_extract(kelp, "\\d+"),
           depth_scale = str_extract(depth, "\\d+"),
           base_terms = "site_type * age_at_survey") %>% 
    unite("predictors", c(hard, kelp, depth, base_terms), sep = " + ", na.rm = TRUE, remove = FALSE) %>% 
    mutate(model_id = 
             str_replace_all(predictors, "hard_bottom_(\\d+)", "H\\1") %>% 
             str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
             str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
             str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
             str_replace_all("depth_cv_(\\d+)", "DCV\\1") %>% 
             str_replace_all("site_type", "ST") %>%
             str_replace_all("age_at_survey", "A") %>% 
             str_replace_all("\\s+", "")) %>% 
    mutate(type = case_when(hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*ST\\*A\\+K\\d+\\*ST\\*A\\+DCV\\d+\\*ST\\*A\\+ST\\*A$") ~ "core_sta",
                            hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*ST\\*A\\+K\\d+\\*ST\\*A\\+DM\\d+\\*ST\\*A\\+ST\\*A$") ~ "core_sta",
                            hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*ST\\+H\\d+\\*A\\+K\\d+\\*ST\\+K\\d+\\*A\\+DCV\\d+\\*ST\\+DCV\\d+\\*A\\+ST\\*A$") ~ "core_st_a",
                            hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*ST\\+H\\d+\\*A\\+K\\d+\\*ST\\+K\\d+\\*A\\+DM\\d+\\*ST\\+DM\\d+\\*A\\+ST\\*A$") ~ "core_st_a",
                            hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*ST\\+K\\d+\\*ST\\+DCV\\d+\\*ST\\+ST\\*A$") ~ "core_st",
                            hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*ST\\+K\\d+\\*ST\\+DM\\d+\\*ST\\+ST\\*A$") ~ "core_st",
                            hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*A\\+K\\d+\\*A\\+DCV\\d+\\*A\\+ST\\*A$") ~ "core_a",
                            hard_scale == kelp_scale & kelp_scale == depth_scale &
                              str_detect(model_id, "^H\\d+\\*A\\+K\\d+\\*A\\+DM\\d+\\*A\\+ST\\*A$") ~ "core_a",
                            predictors == "site_type * age_at_survey" ~ "base",
                            T~NA)) %>% 
    dplyr::select(predictors, type, model_id)
  
  return(pred_list)
}

kelp_list <- get_3way_list(kelp_predictors) # 
rock_list <- get_3way_list(rock_predictors) # 
surf_list <- get_3way_list(surf_predictors) # 
deep_list <- get_3way_list(deep_predictors) #

saveRDS(kelp_list, file.path("analyses/7habitat/intermediate_data", "kelp_predictors_interactions.Rds")) 
saveRDS(rock_list, file.path("analyses/7habitat/intermediate_data", "rock_predictors_interactions.Rds")) 
saveRDS(surf_list, file.path("analyses/7habitat/intermediate_data", "surf_predictors_interactions.Rds")) 
saveRDS(deep_list, file.path("analyses/7habitat/intermediate_data", "deep_predictors_interactions.Rds")) 
