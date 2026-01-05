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
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri|slope)", names(.), value = TRUE))) %>% 
  distinct() 
  
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_full.Rds")) %>% # 26 sites
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri|slope)", names(.), value = TRUE))) %>% 
  distinct()

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_full.Rds")) %>% # 335 sites (grid cells)
  dplyr::select(year, site, site_type, bioregion, all_of(grep("^(hard|soft|kelp|depth|aquatic_vegetation|tri|slope)", names(.), value = TRUE))) %>% 
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
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) 

rock_predictors <- data_rock %>%
  dplyr::select(year, site, site_type, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(year, site, site_type, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>% 
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
  filter(!predictor == "kelp_annual_25") 

surf_predictors <- data_surf %>%
  dplyr::select(year, site, site_type, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(year, site, site_type, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value, na.rm = T), .groups = "drop") %>%
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
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

