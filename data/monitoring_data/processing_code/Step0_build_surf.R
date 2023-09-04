# Process Surf Zone Data
# Cori Lopazanski
# August 18, 2023

# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Summary of key changes from original code:
# - Added column for whether reference site is actually inside SMCA
# - Added taxa information from species_key 


# Note potential concerns for next steps:
# - Some of the lengths seem to be already converted to centimeters -8/31/23 used tl_mm instead (not fish_length) -JGS
# - There are some taxa that don't match fully (mostly higher groupings) - 8/31/23 added these to taxon table -JGS


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
         tl_cm = tl_mm / 10, # changed to calculate from tl_mm instead of fish_length -JGS
         sl_cm = sl_mm / 10,
         affiliated_mpa = tolower(affiliated_mpa)) %>% 
  # Add de-facto smr designations
  left_join(defacto_smr_surf) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr")) %>% 
  # Add regions
  left_join(regions, by = c("affiliated_mpa" = "name")) %>% 
  mutate(mpa_defacto_designation = ifelse(mpa_state_designation == "ref", "ref", tolower(mpa_defacto_class))) %>% 
  dplyr::select(year, month, day, bioregion, region4, affiliated_mpa, 
                mpa_state_class, mpa_state_designation, mpa_defacto_class, 
                mpa_defacto_designation, ref_is_mpa, haul_number, 
                species_code, # Intentionally drop other taxa info - default to the surf taxon table
                tl_cm,sl_cm, weight_g, total_weight_g, 
                count, total_weight_kg) %>%
  # Change "NOSP" to "NO_ORG" to match other habitats
  mutate(species_code = recode(species_code, "NOSP" = "NO_ORG")) %>% 
  mutate(total_weight_g = ifelse(species_code == "NO_ORG", 0, total_weight_g),
         total_weight_kg = ifelse(species_code == "NO_ORG", 0, total_weight_kg)) %>% 
  # Add taxa info from surf taxon table
  left_join(taxon_tab, by = c("species_code" = "habitat_specific_code"))

# Check taxa NAs
taxa_na <- data %>% 
  filter(is.na(sciname) & !(species_code == "NO_ORG"))

# Test for matching taxa
taxa_match <- data %>% 
  distinct(species_code) %>% 
  filter(!is.na(species_code)) %>% 
  filter(!(species_code == "NO_ORG")) %>% 
  filter(!(species_code %in% taxon_tab$habitat_specific_code))

## Note: There are still 4 with no taxonomic info that need to be updated in 
## the main species_key if we want to include beyond tracking effort (e.g. 
## manually fill in appropriate taxa information across columns when processing
## surf zone taxon table)

#added above to surf zone taxon table on 8/31/23 -JGS
# Unspecified, HALI, RFYOY, FFUN


# Write data ------------------------------------------------------------------------
write.csv(data, row.names = FALSE, file.path(outdir, "surf_zone_fish_processed.csv"))


