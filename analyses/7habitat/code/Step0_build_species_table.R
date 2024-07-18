# Combine PMEP and MLPA Species Tables

# About --------------------------------------------------------------------------------

# Combine the MLPA authoritative table (from LTM groups) with the species information
# from the PMEP Habitat Report


# Setup --------------------------------------------------------------------------------
library(tidyverse)
sync.dir <- "/home/shares/ca-mpa/data/sync-data/"


# Read the final authoritative species table
mlpa_sp <- read_csv(file.path(sync.dir, "species_traits/processed/species_key.csv")) %>% 
  clean_names() %>% 
  filter(kingdom == "Animalia") %>% 
  select(family, genus, sciname, level, target_status = target_status_standardized) %>% 
  distinct() %>% 
  filter(!is.na(target_status)) %>% 
  filter(!is.na(sciname))

mlpa_lw <- read_csv(file.path(sync.dir, "species_traits/processed/lw_parameters_fish.csv"))

# Read the pmep cleaned table
pmep_sp <- readRDS(file.path(sync.dir, "habitat_pmep/processed/pmep_species_processed.Rds"))


# Build --------------------------------------------------------------------------------
