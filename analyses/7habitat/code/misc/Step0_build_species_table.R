# Combine PMEP and MLPA Species Tables

# About --------------------------------------------------------------------------------

# Combine the MLPA authoritative table (from LTM groups) with the species information
# from the PMEP Habitat Report


# Setup --------------------------------------------------------------------------------
library(tidyverse)
library(janitor)

sync.dir <- "/home/shares/ca-mpa/data/sync-data/"
out.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

# Read the final authoritative species table
mlpa_sp <- read_csv(file.path(sync.dir, "species_traits/processed/species_key_2025.csv")) %>% 
  clean_names() %>% 
  filter(kingdom == "Animalia" & phylum == "Chordata") %>% 
  select(family, genus, sciname, level, target_status = target_status_standardized) %>% 
  distinct() %>% 
 # filter(!is.na(target_status)) %>% 
  filter(!is.na(sciname))

# Read the length-weight table (contains the lw parameters where they exist)
mlpa_lw <- read_csv(file.path(sync.dir, "species_traits/processed/lw_parameters_fish_2025.csv")) %>% 
  mutate(level = case_when(is.na(level) & is.na(genus) ~ "family",
                           is.na(level) & str_detect(sciname, "spp") ~ "genus",
                           is.na(level) ~ "species",
                           TRUE ~ level))

# Read the pmep cleaned table
pmep_sp <- readRDS(file.path(sync.dir, "habitat_pmep/processed/pmep_species_processed.Rds")) 


# Build --------------------------------------------------------------------------------

# 12 species from the MLPA species list are missing from the PMEP list
missing_pmep <- mlpa_sp %>%
  filter(!sciname %in% pmep_sp$species) %>%
  rename(species = sciname) %>%
  filter(level == "species")

# Create full list 
sp <- mlpa_sp %>% 
  full_join(mlpa_lw) %>% 
  rename(species = sciname) %>% 
  left_join(pmep_sp, by = c("genus", "species")) %>% 
  mutate(assemblage_source = if_else(!is.na(assemblage), "pmep", NA)) %>% 
  rename(family_ltm = family.x,
         family_pmep = family.y) %>% 
  select(family_ltm, family_pmep, genus, species, common_name, everything()) %>% 
  mutate(across(c(min_max_m, common_m), ~ str_replace_all(., "[-–]", "_"))) %>% 
  mutate(depth_min_m = str_split_i(min_max_m, "_", 1),
         depth_max_m = str_split_i(min_max_m, "_", 2),
         depth_common_min_m = case_when(str_detect(common_m, "<") ~ "0",
                                        str_detect(common_m, ">") ~ str_split_i(common_m, "> ", 2),
                                        str_detect(common_m, "_") ~ str_split_i(common_m, "_", 1)),
         depth_common_max_m = case_when(str_detect(common_m, "<") ~ str_split_i(common_m, "<", 2),
                                        str_detect(common_m, ">") ~ NA,
                                        str_detect(common_m, "_") ~ str_split_i(common_m, "_", 2)))
           
# To do: change Kelp Bass to Hard Bottom Biotic re chat with JC (no info in table)

saveRDS(sp, file.path(out.dir, "species_lw_habitat2.Rds"))

mismatch <- anti_join(mlpa_sp, pmep_sp, by = c("genus", "sciname" = "species"))
