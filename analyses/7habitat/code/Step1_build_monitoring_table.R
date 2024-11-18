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
gc()

# Load required packages
library(tidyverse)
library(janitor)
library(stringr)

# Directories
ltm.dir <-"/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
sp.dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"


# surf_orig <- read_csv(file.path(ltm.dir,"biomass_processed/surf_zone_fish_biomass_updated.csv")) %>% 
#   filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths (WARNING: currently drops one full haul!)
#   mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
#   filter(!is.na(target_status))  # this drops: RFYOY, FFUN, HALI, Zoarcidae spp (after previous step to avoid dropping NO_ORG)

kelp_orig <- read_csv(file.path(ltm.dir,"update_2024/kelpforest_fish_biomass_updated.6.csv")) %>%  # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

rock_orig <- read_csv(file.path(ltm.dir,"update_2024/ccfrp_fish_biomass_updated.2024.csv")) %>% # WARNING: NA REMOVALS DROPS 2 CELL TRIPS
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting
  filter(!is.na(target_status)) %>% # drop for now - spp without target status identified (see notes for details)
  filter(!affiliated_mpa == "trinidad NA")

# deep_orig <- read_csv(file.path(ltm.dir,"biomass_processed/deep_reef_fish_biomass_updated.csv")) %>% #WARNING: NA REMOVALS DROPS 12 TRANSECTS
#   filter(!is.na(weight_kg)) %>% # drop fishes unknown or without lengths/conversion params
#   mutate(target_status = if_else(species_code == "NO_ORG" & is.na(target_status), "NO_ORG", target_status)) %>%  # helpful for inspecting 
#   filter(!is.na(target_status))

# Read the species table
sp <- readRDS(file.path(sp.dir, "species_lw_habitat.Rds")) %>% 
  dplyr::select(genus, sciname = species, common_name, level, target_status, vertical_zonation, 
         region, assemblage, assemblage_new, depth_min_m:depth_common_max_m)

# Read the MPA table
mpas_orig <- readRDS(file.path("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed", "CA_mpa_metadata.Rds")) %>% 
  dplyr::select(name = mpa, region) %>% 
  mutate(name = str_replace(name, " \\s*\\([^\\)]+\\)", ""))  # fix name to match join

mpas <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  mutate(implementation_year = as.numeric(format(implementation_date, '%Y'))) %>% 
  left_join(mpas_orig) 

# Build species dataframes -----------------------------------------------------

## Kelp ----

kelp_effort <- kelp_orig %>% 
  # Identify distinct transects - 35251 (after the NA dropped above)
  distinct(year, month, day, site, zone, transect, 
           bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  # Calculate effort as total n transects per site, per year
  group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(n_rep = n()) %>%  # 3131 site-year combos
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year)) 

# Find true zeroes (no fish across an entire site in an entire year)
# kelp_zeroes <- kelp_orig %>% 
#   group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
#   summarize(total_biomass_kg = sum(weight_kg)) %>% 
#   filter(total_biomass_kg == 0) # one zero site, only two transects; ok just dropping this info

kelp <- kelp_orig %>% 
  group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           species_code, sciname, genus, target_status) %>% # , sciname, family, genus, species, target_status, level
  # Total biomass of each species per site per year
  summarise(weight_kg = sum(weight_kg),
            count = sum(count), .groups = 'drop') %>%
  filter(!target_status == "NO_ORG")    # drop these bc aren't true zeroes

# kelp_missing <- kelp %>%  # 7 sites with no info about inside/outside
#   filter(is.na(site_type)) %>% 
#   group_by(site, affiliated_mpa, mpa_defacto_designation, bioregion, site_type) %>% 
#   summarize(n_years = length(unique(year)),
#             n_fish = sum(total_count))

kelp_complete <- kelp_effort %>% 
  # Create complete grid of all species at all sites and years
  expand_grid(species_code = unique(kelp$species_code)) %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(kelp %>% dplyr::select(!c(sciname, genus, target_status))) %>% 
  # Add the scinames to match with species dataframe later
  left_join(kelp %>% distinct(species_code, sciname, genus, target_status)) %>% 
  # Change NAs to zeroes (those species were not observed in that site-year)
  mutate_at(vars(weight_kg), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count), ~ replace(., is.na(.), 0)) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  mutate(kg_per_m2 = weight_kg/(n_rep*60), # 30x2x2m but JC says typical density is per m2 (60)
         count_per_m2 = count/(n_rep*60),
         age_at_survey = year - implementation_year) %>% 
  mutate(region = case_when(bioregion == "South" ~ "scb",
                            bioregion == "Central" ~ "cce",
                            bioregion == "North" ~ "pnw")) %>% 
  left_join(sp %>% dplyr::select(sciname, region, assemblage, assemblage_new)) %>% 
  mutate(assemblage_new = case_when(sciname == "Paralabrax clathratus" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes rastrelliger" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes miniatus" ~ "Hard Bottom", 
                                    T~assemblage_new)) %>% 
  dplyr::select(year, site, site_type, 
                bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, 
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                weight_kg, count, kg_per_m2, count_per_m2)

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
  # Identify distinct cell-trips (2 dropped above bc missing data; trinidad dropped)
  distinct(year, grid_cell_id, id_cell_per_trip, 
           bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  group_by(year, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id) %>% 
  # Caculate number of cell-trips per site per year
  summarize(n_rep = n()) %>% ungroup() %>% 
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year))

# Calculated biomass per unit effort (trip-cell)
rock <- rock_orig %>% 
  dplyr::select(year, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id,
         id_cell_per_trip, species_code, sciname, family, genus, species, target_status,
         total_angler_hrs_cell, weight_kg, count) %>% 
  # Calculate biomass per unit effort (trip-cell) for each fish
  mutate(bpue_kg = weight_kg/total_angler_hrs_cell) %>% 
  # Total BPUE for each species in each site (grid cell) and year
  group_by(year, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id,
           species_code, sciname, genus, target_status) %>% 
  summarize(bpue_kg = sum(bpue_kg),
            count = sum(count)) %>% ungroup() %>% 
  # Drop NO_ORG because already captured effort above
  filter(!species_code == "NO_ORG")

rock_complete <- rock_effort %>% 
  # Create complete grid of all species at all sites and years
  expand_grid(species_code = unique(rock$species_code)) %>% 
  # Add counts and bpue (those that were not seen in a site-year will be NA)
  left_join(rock %>% dplyr::select(!c(sciname, genus, target_status))) %>% 
  # Add sciname to match with species dataframe later
  left_join(rock %>% distinct(species_code, sciname, genus, target_status)) %>% 
  # Change NAs to zeroes (those species were not observed in that site-year)
  mutate_at(vars(bpue_kg), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count), ~ replace(., is.na(.), 0)) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  mutate(age_at_survey = year - implementation_year) %>% 
  mutate(region = case_when(bioregion == "South" ~ "scb",
                            bioregion == "Central" ~ "cce",
                            bioregion == "North" ~ "pnw")) %>% 
  left_join(sp %>% dplyr::select(sciname, region, assemblage, assemblage_new)) %>% 
  mutate(assemblage_new = case_when(sciname == "Paralabrax clathratus" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes rastrelliger" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes miniatus" ~ "Hard Bottom", 
                                    T~assemblage_new)) %>% 
  dplyr::select(year, site = grid_cell_id, site_type, 
                bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, 
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                weight_kg = bpue_kg, count)


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
saveRDS(kelp, file.path(ltm.dir, "update_2024/kelp_biomass_site_year.Rds")) # last write Nov 14 2024
saveRDS(surf, file.path(ltm.dir, "update_2024/biomass/biomass_site_year/surf_biomass_site_year.Rds"))
saveRDS(rock, file.path(ltm.dir, "update_2024/ccfrp_biomass_site_year.Rds")) # last write Nov 14 2024
saveRDS(deep, file.path(ltm.dir, "update_2024/biomass/biomass_site_year/deep_biomass_site_year.Rds"))


saveRDS(kelp_effort, file.path(ltm.dir, "update_2024/kelp_site_year_effort.Rds"))
saveRDS(rock_effort, file.path(ltm.dir, "update_2024/ccfrp_site_year_effort.Rds"))

saveRDS(kelp_complete, file.path(ltm.dir, "update_2024/kelp_biomass_complete.Rds")) # last write Nov 15 2024
saveRDS(rock_complete, file.path(ltm.dir, "update_2024/rock_biomass_complete.Rds")) # last write Nov 18 2024



