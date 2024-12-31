# Generate habitat predictors
# Nov 2024
# Cori Lopazanski


# This script generates the list of predictors for each ecosystem. 
# 1. For each buffer radii, identify habitat types that are included in at least one size (not all = 0)
# 2. Select base predictors: site*age, size
# 3. Create list of all combinations of habitat types for each buffer radii + base predictors
# 4. Filter to just the unique combinations
# 5. Export the habitat predictors list for each ecosystem

# Read data -----
rm(list = ls())
gc()

# Load the data to examine the habitat characteristics across sites for each ecosystem
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

data_kelp <- readRDS(file.path(ltm.dir, "combine_tables/kelp_combine_table.Rds")) %>% # 148 sites
  dplyr::select(site, bioregion, all_of(grep("^(hard|soft|kelp|depth)", names(.), value = TRUE))) %>% 
  distinct()
  
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% # 26 sites
  dplyr::select(site, bioregion, all_of(grep("^(hard|soft|kelp|depth)", names(.), value = TRUE))) %>% 
  distinct()

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% # 335 sites
  dplyr::select(site, bioregion, all_of(grep("^(hard|soft|kelp|depth)", names(.), value = TRUE))) %>% 
  distinct()


# Build ----
# Identify the predictors to retain for each ecosystem 
# -- Remove predictors that are zero across all of the sites 
# -- Explore those that are limited in variability within a region (could skew results)

kelp_predictors <- data_kelp %>%
  mutate(across(starts_with("depth_mean"), ~ .x * -1)) %>% 
  dplyr::select(site, bioregion, where(~ max(.) > 0)) %>% 
  pivot_longer(cols = -c(site, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>%
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "landward")) %>% 
  filter(!str_detect(predictor, "30_100m_25$")) %>% 
  filter(!str_detect(predictor, "30_100m_50$")) %>% 
  filter(!str_detect(predictor, "30_100m_100$")) %>% 
  filter(!str_detect(predictor, "100_200m_500")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) # 29 predictors for kelp

rock_predictors <- data_rock %>%
  mutate(across(starts_with("depth_mean"), ~ .x * -1)) %>% 
  dplyr::select(site, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(site, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>% 
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "landward")) %>% 
  filter(!str_detect(predictor, "100_200m")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) # 35 predictors for rock

surf_predictors <- data_surf %>%
  mutate(across(starts_with("depth_mean"), ~ .x * -1)) %>% 
  dplyr::select(site, bioregion, where(~ max(., na.rm = T) > 0)) %>%
  pivot_longer(cols = -c(site, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value, na.rm = T), .groups = "drop") %>%
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "landward")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) # 24 predictors for surf

# surf_na <- data_surf  %>% 
#   filter(if_any(everything(), is.na)) %>% # Keep rows with any NA
#   dplyr::select(site, where(~ any(is.na(.))))          # Keep columns with any

# Save the predictor list for subsetting the dataset to only the variables of interest
saveRDS(kelp_predictors, file.path("analyses/7habitat/intermediate_data", "kelp_predictors.Rds")) # last write 30 Dec 2024
saveRDS(rock_predictors, file.path("analyses/7habitat/intermediate_data", "rock_predictors.Rds")) # last write 30 Dec 2024
saveRDS(surf_predictors, file.path("analyses/7habitat/intermediate_data", "surf_predictors.Rds")) # last write 30 Dec 2024


## Version 1. Generate Predictor Lists within Scale ----

# Function to generate combinations including interactions for each scale
generate_combinations <- function(predictors, interaction = "site_type") {
  all_combinations <- 1:length(predictors) %>%
    map(~ combn(predictors, ., simplify = FALSE)) %>%
    unlist(recursive = FALSE)
  
  expanded_combinations <- unlist(
    lapply(all_combinations, function(combo) {
      interaction_matrix <- expand.grid(rep(list(c(0, 1)), length(combo)))
      apply(interaction_matrix, 1, function(row) {
        paste(paste(ifelse(row == 1, paste0(combo, " * ", interaction), combo), collapse = " + "),
              "+ site_type * age_at_survey")
      })}))
  
  results <- tibble(predictors = c("site_type * age_at_survey", expanded_combinations)) %>% 
    mutate(type = case_when(predictors == "site_type * age_at_survey" ~ "base",
                            nchar(predictors) == max(nchar(predictors)) ~ "full",
                            TRUE ~ NA))
}

# Generate combinations by scale
run_combinations <- function(predictor_list){
  list_intx <- predictor_list %>% 
    group_by(scale) %>%
    summarise(predictors = list(generate_combinations(predictor))) %>%
    unnest(predictors) %>% 
    distinct(predictors, .keep_all = T) %>% 
    mutate(scale = case_when(type == "base" ~ NA, T~scale)) %>% 
    mutate(model_id = # create shorthand
             str_replace_all(predictors, "hard_bottom_\\d+_(\\d+m?)_(\\d+)", "H\\1\\2") %>% 
             str_replace_all("soft_bottom_\\d+_(\\d+m?)_(\\d+)", "S\\1\\2") %>% 
             str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
             str_replace_all("site_type", "ST") %>%
             str_replace_all("age_at_survey", "A") %>% 
             str_replace_all("\\s+", ""))
  return(list_intx)
}

kelp_list_intx <- run_combinations(kelp_predictors %>% filter(!str_detect(predictor, "depth")))  # 563
rock_list_intx <- run_combinations(rock_predictors %>% filter(!str_detect(predictor, "depth"))) # 1211
surf_list_intx <- run_combinations(surf_predictors %>% filter(!str_detect(predictor, "depth"))) # 113

saveRDS(kelp_list_intx, file.path("analyses/7habitat/intermediate_data", "kelp_predictors_interactions.Rds")) # no size last write 30 Dec 2024
saveRDS(rock_list_intx, file.path("analyses/7habitat/intermediate_data", "rock_predictors_interactions.Rds")) # no size last write 30 Dec 2024
saveRDS(surf_list_intx, file.path("analyses/7habitat/intermediate_data", "surf_predictors_interactions.Rds")) # no size last write 30 Dec 2024

# Add depth combinations for each predictor combination above
# 1. ... no depth at all
# 2. ... + depth_mean
# 3. ... + depth_sd
# 4. ... + depth_mean * site_type
# 5. ... + depth_sd * site_type

predictors_df <- kelp_predictors
list_intx <- kelp_list_intx

add_depth <- function(predictors_df, list_intx){
  depth_combos <- predictors_df %>%
    filter(str_detect(predictor, "depth")) %>%
    bind_rows(tibble(predictor = predictors_df$predictor[str_detect(predictors_df$predictor, "depth")],scale = NA)) %>% 
    mutate(depth_intx = paste0(predictor, " * site_type")) %>%
    pivot_longer(cols = c(predictor, depth_intx),names_to = NULL, values_to = "depth_predictor") %>%
    bind_rows(tibble(scale = unique(predictors_df$scale), depth_predictor = NA)) %>% 
    bind_rows(tibble(scale = NA, depth_predictor = NA)) %>% 
    group_by(scale) %>% 
    summarize(depth_predictors = list(c(depth_predictor)), .groups = 'drop')
  
  list_intx_depth <- list_intx %>%
    left_join(depth_combos, by = "scale") %>% # Join by scale
    unnest(depth_predictors) %>% # Expand rows for each depth predictor
    mutate(predictors = if_else(is.na(depth_predictors),
                                predictors, 
                                paste(predictors, depth_predictors, sep = " + "))) %>% 
    mutate(depth_id = 
             str_replace_all(depth_predictors, "site_type", "ST") %>% 
             str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
             str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
             str_replace_all("\\s+", "")) %>% 
    mutate(model_id = if_else(is.na(depth_predictors),
                              model_id,
                              paste(model_id, depth_id, sep = "+"))) %>% 
    mutate(type = case_when(type == "base" & !is.na(depth_predictors) ~ "base + depth",
                            type == "full" & !is.na(depth_predictors) ~ "full + depth", T~type)) %>% 
    select(scale, predictors, type, model_id)
  
  
  return(list_intx_depth)
}

kelp_intx_depth <- add_depth(kelp_predictors, kelp_list_intx)
rock_intx_depth <- add_depth(rock_predictors, rock_list_intx)
surf_intx_depth <- add_depth(surf_predictors, surf_list_intx)

saveRDS(kelp_intx_depth, file.path("analyses/7habitat/intermediate_data", "kelp_predictors_interactions_depth.Rds")) # no size last write 30 Dec 2024
saveRDS(rock_intx_depth, file.path("analyses/7habitat/intermediate_data", "rock_predictors_interactions_depth.Rds")) # no size last write 30 Dec 2024
saveRDS(surf_intx_depth, file.path("analyses/7habitat/intermediate_data", "surf_predictors_interactions_depth.Rds")) # no size last write 30 Dec 2024

# Version 2. Allow Kelp/Depth Independent of Scale ----

# Generate combinations independent of scale for the depth and kelp
run_independent_combos <- function(predictors_df){
  # Create depth combinations
  depth_combos <- predictors_df %>%
    filter(str_detect(predictor, "depth")) %>%
    mutate(depth_intx = paste0(predictor, " * site_type")) %>%
    pivot_longer(cols = c(predictor, depth_intx), names_to = NULL, values_to = "depth_predictor") %>%
    bind_rows(tibble(scale = NA, depth_predictor = NA)) %>% 
    summarize(depth_predictors = list(c(depth_predictor)), .groups = 'drop')
  
  # Create kelp combinations
  kelp_combos <- predictors_df %>%
    filter(str_detect(predictor, "kelp")) %>%
    mutate(kelp_intx = paste0(predictor, " * site_type")) %>%
    pivot_longer(cols = c(predictor, kelp_intx), names_to = NULL, values_to = "kelp_predictor") %>%
    bind_rows(tibble(scale = NA, kelp_predictor = NA)) %>% 
    summarize(kelp_predictors = list(c(kelp_predictor)), .groups = 'drop') 
  
  # Create hard/soft combinations
  list_intx <- predictors_df %>% 
    mutate(predictor = 
             str_replace_all(predictor, "0_30m_", "") %>% 
             str_replace_all("30_100m_", "")) %>% 
    distinct() %>% 
    filter(str_detect(predictor, "hard|soft")) %>% 
    group_by(scale) %>%
    summarise(predictors = list(generate_combinations(predictor))) %>%
    unnest(predictors) %>% 
    distinct(predictors, .keep_all = T) %>% 
    mutate(scale = case_when(type == "base" ~ NA, T~scale))
    
  # Add the kelp and the depth to the hard/soft combos
  list_intx_add <- list_intx %>%
    bind_cols(kelp_combos) %>% 
    unnest(kelp_predictors) %>% 
    mutate(predictors = if_else(is.na(kelp_predictors),
                                predictors, 
                                paste(predictors, kelp_predictors, sep = " + "))) %>% 
    bind_cols(depth_combos) %>% 
    unnest(depth_predictors) %>% 
    mutate(predictors = if_else(is.na(depth_predictors),
                                predictors, 
                                paste(predictors, depth_predictors, sep = " + "))) %>% 
    mutate(model_id = # create shorthand
             str_replace_all(predictors, "hard_bottom_(\\d+)", "H\\1") %>% 
             str_replace_all("soft_bottom_(\\d+)", "S\\1") %>% 
             str_replace_all("kelp_annual_(\\d+)", "K\\1") %>% 
             str_replace_all("depth_mean_(\\d+)", "DM\\1") %>% 
             str_replace_all("depth_sd_(\\d+)", "DSD\\1") %>% 
             str_replace_all("site_type", "ST") %>%
             str_replace_all("age_at_survey", "A") %>% 
             str_replace_all("\\s+", "")) %>% 
    mutate(type = case_when(type == "base" & !is.na(depth_predictors) ~ "base + depth",
                            type == "full" & !is.na(depth_predictors) ~ "full + depth", T~type)) %>% 
    select(scale, predictors, type, model_id)
  
  
  return(list_intx_add)
}


kelp_list_add <- run_independent_combos(kelp_predictors)
rock_list_add <- run_independent_combos(rock_predictors)
surf_list_add <- run_independent_combos(surf_predictors)

saveRDS(kelp_list_add, file.path("analyses/7habitat/intermediate_data", "kelp_predictors_interactions_add.Rds")) # no size last write 30 Dec 2024
saveRDS(rock_list_add, file.path("analyses/7habitat/intermediate_data", "rock_predictors_interactions_add.Rds")) # no size last write 30 Dec 2024
saveRDS(surf_list_add, file.path("analyses/7habitat/intermediate_data", "surf_predictors_interactions_add.Rds")) # no size last write 30 Dec 2024

