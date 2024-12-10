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
  dplyr::select(site, bioregion, all_of(grep("^(hard|soft|kelp)", names(.), value = TRUE))) %>% 
  distinct()
  
data_surf <- readRDS(file.path(ltm.dir, "combine_tables/surf_combine_table.Rds")) %>% # 26 sites
  dplyr::select(site, bioregion, all_of(grep("^(hard|soft|kelp)", names(.), value = TRUE))) %>% 
  distinct()

data_rock <- readRDS(file.path(ltm.dir, "combine_tables/ccfrp_combine_table.Rds")) %>% # 335 sites
  dplyr::select(site, bioregion, all_of(grep("^(hard|soft|kelp)", names(.), value = TRUE))) %>% 
  distinct()


# Identify the predictors that are not all zero across all sites
kelp_predictors <- data_kelp %>%
  dplyr::select(site, bioregion, where(~ max(.) > 0)) %>% 
  pivot_longer(cols = -c(site, bioregion), names_to = "predictor", values_to = "value") %>%
  group_by(bioregion, predictor) %>%
  summarize(sd = sd(value), .groups = "drop") %>%
  pivot_wider(names_from = "bioregion", values_from = "sd") %>% 
  filter(!str_detect(predictor, "30_100m_25$")) %>% 
  filter(!str_detect(predictor, "30_100m_50$")) %>% 
  filter(!str_detect(predictor, "30_100m_100$")) %>% 
  filter(!str_detect(predictor, "100_200m_500")) %>% 
  dplyr::select(predictor) %>% 
  mutate(scale = sub("_", "", str_sub(predictor, -3, -1)))# %>% 
 # group_by(scale) %>%
 # summarise(predictors = list(predictor), .groups = "drop") %>% 
 # deframe()

# Need to revisit these now that kelp was added - the above had to be corrected
# rock_predictors <- data_rock %>% 
#   dplyr::select(site, bioregion, where(~ max(., na.rm = T) > 0)) %>% 
#   pivot_longer(cols = -c(site, bioregion), names_to = "predictor", values_to = "value") %>%
#   group_by(bioregion, predictor) %>%
#   summarize(sd = sd(value)) %>% ungroup() %>% 
#   filter(!str_detect(predictor, "landward")) %>% 
#   filter(!str_detect(predictor, "100_200m")) %>% 
#   #filter(sd > 0) %>% # if you use this drops kelp below 250 buffer b/c not in north?
#   filter(!str_detect(predictor, "biotic")) %>% 
#   pivot_wider(names_from = predictor, values_from = sd) %>% 
#   dplyr::select(where(~ all(!is.na(.))), -bioregion) %>% 
#   names() %>% tibble(predictor = .) %>% 
#   mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
#   group_by(scale) %>%
#   summarise(predictors = list(predictor), .groups = "drop") %>% 
#   deframe()
# 
# surf_predictors <- data_surf %>% 
#   dplyr::select(where(~ max(., na.rm = T) > 0), -c(site, bioregion)) %>% 
#   names() %>% tibble(predictor = .) %>% 
#   mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
#   filter(!str_detect(predictor, "landward")) %>% 
#   filter(!str_detect(predictor, "biotic")) %>% 
#   filter(!scale %in% c("25", "50")) %>%  # drop for now because NAs
#   group_by(scale) %>%
#   summarise(predictors = list(predictor), .groups = "drop") %>% 
#   deframe()

# surf_na <- data_surf  %>% 
#   filter(if_any(everything(), is.na)) %>% # Keep rows with any NA
#   dplyr::select(site, where(~ any(is.na(.))))          # Keep columns with any


# Save the predictor list for subsetting the dataset to only the variables of interest
saveRDS(kelp_predictors, file.path("analyses/7habitat/intermediate_data", "kelp_predictors.Rds")) # last write 10 Dec 2024


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
kelp_list_intx <- kelp_predictors %>%
  group_by(scale) %>%
  summarise(predictors = list(generate_combinations(predictor))) %>%
  unnest(predictors) %>% 
  distinct(predictors, .keep_all = T) %>% 
  mutate(scale = case_when(type == "base" ~ NA, T~scale))


saveRDS(kelp_list_intx, file.path("analyses/7habitat/intermediate_data", "kelp_predictors_interactions.Rds")) # no size last write 10 Dec 2024
#saveRDS(rock_list_intx, file.path("analyses/7habitat/intermediate_data", "rock_predictors_interactions.Rds")) # no size last write 3 Dec 2024
#saveRDS(surf_list_intx, file.path("analyses/7habitat/intermediate_data", "surf_predictors_interactions.Rds")) # no size last write 3 Dec 2024







# Generate predictors manually ? -----------------------------------------------------------
# Old format before dropping the areas where variability was zero in a region:
# rock_predictors <- data_rock %>% 
#   dplyr::select(where(~ max(.) > 0), -site) %>%
#   names() %>% tibble(predictor = .) %>%
#   mutate(scale = sub("_", "", str_sub(predictor, -3, -1))) %>% 
#   filter(!str_detect(predictor, "landward")) %>% 
#   group_by(scale) %>%
#   summarise(predictors = list(predictor), .groups = "drop") %>% 
#   deframe()
# 
# 
# habitat_25 <- c("hard_bottom_biotic_0_30m_25", 
#                 "soft_bottom_biotic_0_30m_25",  
#                 "hard_bottom_0_30m_25",         
#                 "soft_bottom_0_30m_25") 
# 
# habitat_50  <- c("hard_bottom_biotic_0_30m_50",  # problem with 3 NAs for surf
#                  "soft_bottom_biotic_0_30m_50",  # problem with 3 NAs for surf
#                  "hard_bottom_0_30m_50",         # problem with 3 NAs for surf
#                  "soft_bottom_0_30m_50")         # problem with 3 NAs for surf
# 
# habitat_100 <- c("hard_bottom_biotic_0_30m_100", 
#                  "soft_bottom_biotic_0_30m_100", 
#                  "hard_bottom_0_30m_100",   
#                  "soft_bottom_0_30m_100")
# 
# habitat_250 <- c("hard_bottom_biotic_0_30m_250", 
#                  "soft_bottom_biotic_0_30m_250",
#                  "hard_bottom_0_30m_250",    
#                  "soft_bottom_0_30m_250",
#                  "hard_bottom_30_100m_250",    # rm for surf zone
#                  "soft_bottom_30_100m_250"     # rm for surf zone
# )   
# habitat_500 <- c("hard_bottom_biotic_0_30m_500",
#                  "soft_bottom_biotic_0_30m_500",
#                  "hard_bottom_0_30m_500", 
#                  "soft_bottom_0_30m_500",      
#                  "hard_bottom_30_100m_500",    # rm for surf zone
#                  "soft_bottom_30_100m_500"     # rm for surf zone
# )
# 
# predictors_list <- NULL
# get_predictors <- function(habitat_buffer_list) {
#   predictors <- list()
#   
#   for (r in 1:length(habitat_buffer_list)) {
#     habitat_combinations <- combn(habitat_buffer_list, r, simplify = FALSE)
#     
#     for (combo in habitat_combinations) {
#       predictors <- append(predictors, list(c(combo, base_predictors)))}}
#   
#   return(predictors)
# }
# 
# predictors_list <- c(
#   get_predictors(habitat_25),
#   get_predictors(habitat_50),
#   get_predictors(habitat_100),
#   get_predictors(habitat_250),
#   get_predictors(habitat_500)
# )
# 
# predictors_list <- unique(predictors_list)


