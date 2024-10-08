# Processing monitoring data for habitat analyses
# Cori Lopazanski; lopazanski@bren.ucsb.edu
# July 2024

# About --------------------------------------------------------------------------------
# This script uses the cleaned calculated biomass (output from ca-mpa/data/
# monitoring_data/processing_code/Step1_process_biomass.R). The biomass was 
# calculated from the cleaned monitoring data (outputs from Step0 in monitoring) 
# using the length/weight parameters (from species_traits/Step4). 

# - Notes about key variables:
#     * mpa_class = whether the MPA is a SMR or SMCA (or something else)
#     * mpa_designation = whether the SITE was inside or outside
#     * habitat_specific_spp_name = used to join species_key to raw monitoring data (in Step0)
#     * sciname = corrected name for joining length-weight parameters

# - Size units in raw data should all be cm. Conversion parameters are all in cm and g.


# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(stringr)

# Directories
ltm.dir <-"/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
spe.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

surf_orig <- read_csv(file.path(ltm.dir,"biomass_processed/surf_zone_fish_biomass_updated.csv")) %>% 
  filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths (WARNING: currently drops one full haul!)
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))  # this drops: RFYOY, FFUN, HALI, Zoarcidae spp (after previous step to avoid dropping NO_ORG)

kelp_orig <- read_csv(file.path(ltm.dir,"biomass_processed/kelpforest_fish_biomass_updated.6.csv")) %>%  # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

rock_orig <- read_csv(file.path(ltm.dir,"biomass_processed/ccfrp_fish_biomass_updated.csv")) %>% # WARNING: NA REMOVALS DROPS 2 CELL TRIPS
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status)) # drop for now - spp without target status identified (see notes for details)

deep_orig <- read_csv(file.path(ltm.dir,"biomass_processed/deep_reef_fish_biomass_updated.csv")) %>% #WARNING: NA REMOVALS DROPS 12 TRANSECTS
  filter(!is.na(weight_kg)) %>% # drop fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG" & is.na(target_status), "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))


# Build species dataframes -----------------------------------------------------

## Kelp ----

kelp_effort <- kelp_orig %>% 
  # Identify distinct transects - 28300 (after the NA dropped above)
  distinct(year, month, day, site, zone, transect, 
           bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  # Calculate effort as total n transects per site, per year
  group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(n_rep = n()) # 3101 site-year combos

# Find true zeroes (no fish across an entire site in an entire year)
kelp_zeroes <- kelp_orig %>% 
  group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(total_biomass_kg = sum(weight_kg)) %>% 
  filter(total_biomass_kg == 0) # one zero site, only two transects; ok just dropping this info
  
kelp <- kelp_orig %>% 
  group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           sciname, family, genus, species, target_status, level) %>%
  # Total biomass of each species per site per year
  summarise(total_biomass_kg = sum(weight_kg),
            total_count = sum(count)) %>%
  full_join(kelp_effort) %>% 
  filter(!target_status == "NO_ORG") # drop these bc aren't true zeroes


## Surf ----

surf_effort <- surf_orig %>% 
  # Identify distinct hauls - 857 (after NA droped above)
  distinct(year, month, day, 
           affiliated_mpa,  mpa_defacto_class, mpa_defacto_designation, haul_number) %>% 
  # Caclulate effort as total n hauls per site (mpa/ref) per year
  group_by(year, affiliated_mpa,  mpa_defacto_class, 
           mpa_defacto_designation) %>% 
  summarize(n_rep = n())

surf <- surf_orig %>%
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           sciname, family, genus, species, target_status, level) %>%
  dplyr::summarize(total_biomass_kg = sum(weight_kg)) %>% 
  full_join(surf_effort)


## Rock (CCFRP) ----

rock_effort <- rock_orig %>% 
  # Identify distinct cell-trips (2 dropped above bc missing data) - 2411
  distinct(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id,
           id_cell_per_trip) %>% 
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id) %>% 
  # Caculate number of cell-trips per site per year
  summarize(n_rep = n())

# Calculated biomass per unit effort (trip-cell)
rock <- rock_orig %>% 
  select(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id,
         id_cell_per_trip, species_code, sciname, family, genus, species, target_status,
         total_angler_hrs_cell, weight_kg) %>% 
  # Calculate biomass per unit effort (trip-cell) for each fish
  mutate(bpue_kg = weight_kg/total_angler_hrs_cell) %>% 
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id,
           species_code, sciname, family, genus, species, target_status) %>% 
  summarize(total_bpue_kg = sum(bpue_kg)) %>% 
  full_join(rock_effort) %>% 
  filter(!(affiliated_mpa == "trinidad NA")) # drop trinidad
  

## Deep ----
deep_effort <- deep_orig %>% 
  # Identify distinct transects (1524)
  distinct(year, affiliated_mpa, 
           mpa_defacto_class, mpa_defacto_designation, line_id) %>% 
  # Calculate effort as number of transects per site per year
  group_by(year, affiliated_mpa, 
           mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(n_rep = n()) # 133 site-year combos

# calculate total biom for each rep unit (transect - aka line_id)
deep <- deep_orig %>%
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           line_id, family, genus, sciname, target_status, level) %>%
  dplyr::summarize(total_biomass_kg = sum(weight_kg)) %>% 
  full_join(deep_effort)


# Export -------------------------------------------------------------------------------------
saveRDS(kelp, file.path(ltm.dir, "biomass_site_year/kelp_biomass_site_year.Rds"))
saveRDS(surf, file.path(ltm.dir, "biomass_site_year/surf_biomass_site_year.Rds"))
saveRDS(rock, file.path(ltm.dir, "biomass_site_year/ccfrp_biomass_site_year.Rds"))
saveRDS(deep, file.path(ltm.dir, "biomass_site_year/deep_biomass_site_year.Rds"))


saveRDS(kelp_effort, file.path(ltm.dir, "biomass_site_year/kelp_site_year_effort.Rds"))


