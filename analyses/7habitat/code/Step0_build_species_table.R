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
mlpa_sp <- read_csv(file.path(sync.dir, "species_traits/processed/species_key.csv")) %>% 
  clean_names() %>% 
  filter(kingdom == "Animalia" & phylum == "Chordata") %>% 
  select(family, genus, sciname, level, target_status = target_status_standardized) %>% 
  distinct() %>% 
  filter(!is.na(target_status)) %>% 
  filter(!is.na(sciname))

# Read the length-weight table (contains the lw parameters where they exist)
mlpa_lw <- read_csv(file.path(sync.dir, "species_traits/processed/lw_parameters_fish.csv")) %>% 
  mutate(level = case_when(is.na(level) & is.na(genus) ~ "family",
                           is.na(level) & str_detect(sciname, "spp") ~ "genus",
                           is.na(level) ~ "species",
                           TRUE ~ level))

# Read the pmep cleaned table
pmep_sp <- readRDS(file.path(sync.dir, "habitat_pmep/processed/pmep_species_processed.Rds")) %>% 
  mutate(across(mud:sfmi, as.numeric)) %>% 
  mutate(intertidal = as.numeric(if_else(is.na(intertidal), NA, 1)),
         subtidal = as.numeric(if_else(is.na(subtidal), NA, 1))) %>% 
  select(-order)


# Build --------------------------------------------------------------------------------

# Only 6 species from the MLPA species list are missing from the PMEP list
# missing_pmep <- mlpa_sp %>% 
#   filter(!sciname %in% pmep_sp$species) %>% 
#   rename(species = sciname) %>% 
#   filter(level == "species") 

# Create full list 
sp <- mlpa_sp %>% 
  full_join(mlpa_lw) %>% 
  rename(species = sciname) %>% 
  left_join(pmep_sp, by = c("genus", "species")) %>% 
  mutate(assemblage_source = if_else(!is.na(assemblage), "species", NA)) %>% 
  rename(family_ltm = family.x,
         family_pmep = family.y) %>% 
  select(family_ltm, family_pmep, genus, species, common_name, everything()) 


saveRDS(sp, file.path(out.dir, "species_lw_habitat.Rds"))

mismatch <- anti_join(mlpa_sp, pmep_sp, by = c("genus", "sciname" = "species"))
