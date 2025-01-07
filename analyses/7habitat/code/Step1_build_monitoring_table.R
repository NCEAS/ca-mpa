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

surf_orig <- read_csv(file.path(ltm.dir,"update_2024/surf_zone_fish_biomass_updated.csv")) %>%
  filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths (WARNING: currently drops one full haul!)
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting
  filter(!is.na(target_status))  # this drops: RFYOY, FFUN, HALI, Zoarcidae spp (after previous step to avoid dropping NO_ORG)

kelp_orig <- read_csv(file.path(ltm.dir,"update_2024/kelpforest_fish_biomass_updated.6.csv")) %>%  # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

rock_orig <- read_csv(file.path(ltm.dir,"update_2024/ccfrp_fish_biomass_updated.2024.csv")) %>% # WARNING: NA REMOVALS DROPS 2 CELL TRIPS
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting
  filter(!is.na(target_status)) %>% # drop for now - spp without target status identified (see notes for details)
  filter(!affiliated_mpa == "trinidad NA")

deep_orig <- read_csv(file.path(ltm.dir,"update_2024/deep_reef_fish_biomass_updated.csv")) %>% #WARNING: NA REMOVALS DROPS 12 TRANSECTS
  filter(!is.na(weight_kg)) %>% # drop fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG" & is.na(target_status), "NO_ORG", target_status)) %>%  # helpful for inspecting
  filter(!is.na(target_status))

# Read the original species table
# mlpa_sp <- read_csv(file.path(sync.dir, "species_traits/processed/species_key.csv")) %>% 
#   clean_names() %>% 
#   filter(kingdom == "Animalia" & phylum == "Chordata") 

# Read the species + habitat table
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

rm(mpas_orig)

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
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2)) 

# Find true zeroes (no fish across an entire site in an entire year)
# kelp_zeroes <- kelp_orig %>% 
#   group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
#   summarize(total_biomass_kg = sum(weight_kg)) %>% 
#   filter(total_biomass_kg == 0) # one zero site, only two transects; ok just dropping this info

kelp <- kelp_orig %>% 
  group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           species_code, sciname, genus, target_status, name) %>% # , sciname, family, genus, species, target_status, level
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
  left_join(kelp %>% dplyr::select(!c(sciname, genus, target_status, name))) %>% 
  # Add the scinames to match with species dataframe later
  left_join(kelp %>% distinct(species_code, sciname, genus, target_status, name)) %>% 
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
                bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2,
                age_at_survey, n_rep, species_code, sciname, name, genus, target_status, assemblage, assemblage_new,
                weight_kg, count, kg_per_m2, count_per_m2)

## Surf ----
# Read the site names for matching with the habitat site names (boo Chris bad processing making extra work!)
surf_sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/site_tables/processed/surf_site_names.Rds") 

surf_effort <- surf_orig %>% 
  # Identify distinct hauls - 857 (after NA droped above)
  distinct(year, month, day, bioregion, affiliated_mpa,  mpa_defacto_class, mpa_defacto_designation, ref_is_mpa, site_name, haul_number) %>% 
  # Caclulate effort as total n hauls per site (mpa/ref) per year
  group_by(year, bioregion, site_name, affiliated_mpa,  mpa_defacto_class,  mpa_defacto_designation, ref_is_mpa) %>% 
  summarize(n_rep = n()) %>% 
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2)) %>% 
  left_join(surf_sites) 

surf <- surf_orig %>%
  group_by(year, site_name, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           species_code, sciname, genus, target_status) %>%
  summarize(weight_kg = sum(weight_kg),
            count = sum(count), .groups = 'drop') %>% 
  filter(!species_code == "NO_ORG") # drop b/c not true zeroes (will capture in complete expansion)

surf_complete <- surf_effort %>% 
  # Create complete grid of all species at all sites and years
  expand_grid(species_code = unique(surf$species_code)) %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(surf %>% dplyr::select(!c(sciname, genus, target_status))) %>% 
  # Add the scinames to match with species dataframe later
  left_join(surf %>% distinct(species_code, sciname, genus, target_status)) %>% 
  # Change NAs to zeroes (those species were not observed in that site-year)
  mutate_at(vars(weight_kg), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count), ~ replace(., is.na(.), 0)) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  mutate(kg_per_haul = weight_kg/(n_rep), # 30x2x2m but JC says typical density is per m2 (60)
         count_per_haul = count/(n_rep),
         age_at_survey = year - implementation_year) %>% 
  mutate(region = case_when(bioregion == "South" ~ "scb",
                            bioregion == "Central" ~ "cce",
                            bioregion == "North" ~ "pnw")) %>% 
  left_join(sp %>% dplyr::select(sciname, region, assemblage, assemblage_new)) %>% 
  mutate(assemblage_new = case_when(sciname == "Paralabrax clathratus" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes rastrelliger" ~ "Hard Bottom Biotic", 
                                    sciname == "Sebastes miniatus" ~ "Hard Bottom", 
                                    T~assemblage_new)) %>% 
  dplyr::select(year, site, site_name, site_type, 
                bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2,
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                weight_kg, count, kg_per_haul, count_per_haul)

## Rock (CCFRP) ----
rock_effort <- rock_orig %>% 
  # Identify distinct cell-trips (2 dropped above bc missing data; trinidad dropped)
  distinct(year, grid_cell_id, id_cell_per_trip, 
           bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  group_by(year, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id) %>% 
  # Caculate number of cell-trips per site per year
  summarize(n_rep = n()) %>% ungroup() %>% 
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2))

# Calculated biomass per unit effort (trip-cell)
rock <- rock_orig %>% 
  dplyr::select(year, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id,
         id_cell_per_trip, species_code, sciname, family, genus, species, target_status,
         total_angler_hrs_cell, weight_kg, count) %>% 
  # Calculate biomass per unit effort (trip-cell) for each fish
  mutate(bpue_kg = weight_kg/total_angler_hrs_cell) %>% 
  # Update OYT to include both OLV and YTL
  # mutate(species_code = ifelse(species_code %in% c("OLV", "YTL"), "OYT", species_code)) %>%  
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
                bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2,
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                weight_kg = bpue_kg, count)


## Deep ----
# Read the deep reef transect information for the cleaned transect length (!!!)
deep_transects <- readRDS(file.path(ltm.dir, "update_2024/deep_reef_transect_metadata.Rds"))

deep_effort <- deep_orig %>%
  # Identify distinct transects (1583 but not all match with the lat/long information)
  distinct(year, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, line_id, transect_id_desig) %>%
  # Add the metadata for each transect (location, distance/area)
  left_join(deep_transects) %>% 
  # Drop transects without location data (will be removed from data below)
  filter(!is.na(avg_lat)) %>% # 1504
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2)) 

deep <- deep_orig %>%
  group_by(year, transect_id_desig, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, sciname, genus, target_status) %>%
  # Biomass and count for each species on each transect
  dplyr::summarize(biomass_kg = sum(weight_kg),
                   count = sum(count), .groups = 'drop') %>% # 10935
  # Drop NO_ORG (not true zeroes)
  filter(!target_status == "NO_ORG") %>%  # 10876
  # Join the area information
  left_join(deep_transects %>% dplyr::select(year, transect_id_desig, area_m2)) %>% 
  # Drop transects without location/distance information
  filter(!is.na(area_m2)) %>%  # 9282
  # Calculate BPUE for each transect
  mutate(kg_per_m2 = biomass_kg/area_m2,
         count_per_m2 = count/area_m2)

deep_complete <- deep_effort %>% 
  # Create complete grid of all species at all sites and years
  expand_grid(sciname = unique(deep$sciname)) %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(deep %>% dplyr::select(!c(genus, target_status))) %>% 
  # Add the scinames to match with species dataframe later
  left_join(deep %>% distinct(sciname, genus, target_status)) %>% 
  # Change NAs to zeroes (those species were not observed in that site-year)
  mutate_at(vars(biomass_kg, count, kg_per_m2, count_per_m2), ~ replace(., is.na(.), 0)) %>% 
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
  mutate(species_code = toupper(paste0(substr(sciname, 1, 1),
                                       substr(gsub("^\\S+\\s*(\\S+).*", "\\1", sciname), 1, 3)))) %>% 
  dplyr::select(year, site = transect_id_desig, site_type, dive,
                bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2,
                age_at_survey, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                biomass_kg, count, kg_per_m2, count_per_m2)


# Export -------------------------------------------------------------------------------------
#saveRDS(kelp, file.path(ltm.dir, "update_2024/kelp_biomass_site_year.Rds")) # last write Nov 14 2024
#saveRDS(surf, file.path(ltm.dir, "update_2024/biomass/biomass_site_year/surf_biomass_site_year.Rds"))
#saveRDS(rock, file.path(ltm.dir, "update_2024/ccfrp_biomass_site_year.Rds")) # last write Nov 14 2024
#saveRDS(deep, file.path(ltm.dir, "update_2024/biomass/biomass_site_year/deep_biomass_site_year.Rds"))


saveRDS(kelp_effort, file.path(ltm.dir, "update_2024/kelp_site_year_effort.Rds"))
saveRDS(rock_effort, file.path(ltm.dir, "update_2024/ccfrp_site_year_effort.Rds"))
saveRDS(deep_effort, file.path(ltm.dir, "update_2024/deep_site_year_effort.Rds"))

saveRDS(kelp_complete, file.path(ltm.dir, "update_2024/kelp_biomass_complete.Rds")) # last write Dec 5 2024;
saveRDS(rock_complete, file.path(ltm.dir, "update_2024/rock_biomass_complete.Rds")) # last write Dec 5 2024; w/out update to OYT but updated incorrect cell coding for BH07
saveRDS(surf_complete, file.path(ltm.dir, "update_2024/surf_biomass_complete.Rds")) # last write Dec 5 2024;
saveRDS(deep_complete, file.path(ltm.dir, "update_2024/deep_biomass_complete.Rds")) # last write Jan 6 2025



