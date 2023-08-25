# Process Surf Zone Data
# Cori Lopazanski
# August 18, 2023

# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Summary of key changes from original code:
# - Added column for whether reference site is actually inside SMCA

# Note potential concerns for next steps:
# - Some of the lengths seem to be already converted to centimeters


# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(readxl)

# Set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read surf monitoring data
surf_zone_raw <- read.csv(file.path(datadir, "monitoring_sandy-beach/surf_zone_fish_seine_data.csv")) %>%
  clean_names()

# Read taxonomy lookup table
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  filter(habitat == "Surf Zone")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(name, bioregion, region4 = four_region_north_ci) %>%
  mutate(name = tolower(name))

# Read de-facto SMRs
defacto_smr_surf <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>%
  filter(group == "surf") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

# Read length-weight parameters
params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv") %>%
  mutate(ScientificName_accepted = recode(ScientificName_accepted, "Sebastes spp." = "Sebastes spp")) %>%
  filter(!is.na(ScientificName_accepted))

# Process Data ------------------------------------------------------------------------

# Identify paired mpa-reference sites
# Surf zone used an alphabetic naming convention ('site_pair') to identify matched pairs (inside vs. out)

pairs <- surf_zone_raw %>% 
  dplyr::select(site_code, site_type, mpa_name_short, affiliated_mpa, site_pair, mpa_status, mpa_type) %>% 
  distinct() %>%
  # Surf zone called any SMR or SMCA a 'MPA', so use the affiliated_mpa name to identify the state class 
  mutate(mpa_state_class = word(affiliated_mpa, -1),
         mpa_state_designation = ifelse(mpa_status == "Reference", "ref", tolower(mpa_state_class))) %>% 
  # Creat column that notes that the reference is an SMCA - 6 in surf zone
  mutate(ref_is_mpa = if_else(mpa_status == "Reference" & !(mpa_name_short == ""), "yes", "no"))


data <- surf_zone_raw %>% 
  # Add paired mpa-reference sites
  left_join(pairs) %>% 
  rename(weight_g = fish_weight_individual,
         total_weight_g = fish_weight) %>%
  mutate(total_weight_kg = total_weight_g / 1000,
         fish_length = fish_length / 10, # NOTE CONCERN: SOME ALREADY CONVERTED?
         affiliated_mpa = tolower(affiliated_mpa)) %>% 
  # Add de-facto smr designations
  left_join(defacto_smr_surf) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr")) %>% 
  # Add regions
  left_join(regions, by = c("affiliated_mpa" = "name")) %>% 
  mutate(mpa_defacto_designation = ifelse(mpa_state_designation == "ref", "ref", tolower(mpa_defacto_class))) %>% 
  dplyr::select(year, month, day, bioregion, region4, affiliated_mpa, 
                mpa_state_class, mpa_state_designation, mpa_defacto_class, 
                mpa_defacto_designation, ref_is_mpa, haul_number, species_code, class, order, 
                family, genus, species, target_status = targeted, fish_length, weight_g, total_weight_g, 
                count, total_weight_kg) %>%
  mutate(total_weight_g = ifelse(species_code == "NOSP", 0, total_weight_g),
         total_weight_kg = ifelse(species_code == "NOSP", 0, total_weight_kg))


# Test for matching taxa 
taxa_match <- data %>% 
  select(species_code:target_status) %>% unique() %>% 
  left_join(taxon_tab, by = c("species_code" = "habitat_specific_code")) %>% 
  filter(is.na(sciname))

  # Add taxa 
  left_join(taxon_tab, by = c("species_code" = "habitat_specific_code"))

# Write data ------------------------------------------------------------------------
#write.csv(data, row.names = FALSE, file.path(outdir, "biomass_processed", "surf_zone_fish_biomass.csv"))


