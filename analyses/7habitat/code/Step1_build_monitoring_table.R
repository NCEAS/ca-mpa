# Processing monitoring data for habitat analyses
# Cori Lopazanski; lopazanski@bren.ucsb.edu
# July 2024

# About --------------------------------------------------------------------------------
# This script uses the cleaned calculated biomass. The biomass was 
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
  filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths 
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting
  filter(!is.na(target_status))  # this drops: RFYOY, FFUN, HALI, Zoarcidae spp (after previous step to avoid dropping NO_ORG)

kelp_orig <- read_csv(file.path(ltm.dir,"update_2024/kelpforest_fish_biomass_updated.6.csv")) %>%  # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks sites)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

rock_orig <- read_csv(file.path(ltm.dir,"update_2024/ccfrp_fish_biomass_updated.2024.csv")) %>% # WARNING: NA REMOVALS DROPS 2 CELL TRIPS
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting
  filter(!is.na(target_status)) %>% # drop for now - spp without target status identified (see notes for details)
  filter(!affiliated_mpa == "trinidad NA")

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
           bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  # Calculate effort as total n transects per site, per year
  group_by(year, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(n_rep = n()) %>%  # 3131 site-year combos
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2)) %>% 
  # Adjust details
  # Big Creek was protected in 1994, expanded to deep water in 2007. Given that kelp monitoring
  # occurred in the original MPA, will adjust the implementation date to reflect that
  #  https://docs.google.com/spreadsheets/d/1Q9raxVu0lqlHVEIKyZ8XKmg0Rd6egLWt/edit?gid=325537399#gid=325537399
  mutate(implementation_year_adj =
           case_when(affiliated_mpa == "big creek smr" ~ 1994, # kf only
                     affiliated_mpa == "lovers point - julia platt smr" ~ 1984,# kf and ri 
                     affiliated_mpa == "point lobos smr" ~ 1973, # all but deep
                     affiliated_mpa == "anacapa island smr" ~ 1978, # kf only
                     affiliated_mpa == "blue cavern onshore smca" ~ 1988, # kf only, maybe
                     T~implementation_year))
           

# Find true zeroes (no fish across an entire site in an entire year)
# kelp_zeroes <- kelp_orig %>%
#   group_by(year, site, bioregion, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>%
#   summarize(total_biomass_kg = sum(weight_kg)) %>%
#   filter(total_biomass_kg == 0) # one zero site, only two transects; ok just dropping this info

kelp <- kelp_orig %>% 
  group_by(year, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, species_code, target_status) %>% 
  # Total biomass of each species per site per year
  summarise(weight_kg = sum(weight_kg),
            count = sum(count), .groups = 'drop') %>%
  filter(!target_status == "NO_ORG") %>%    # drop these bc aren't true zeroes
  select(-target_status)

# kelp_missing <- kelp %>%  # 7 sites with no info about inside/outside
#   filter(is.na(site_type)) %>% 
#   group_by(site, affiliated_mpa, mpa_defacto_designation, bioregion, site_type) %>% 
#   summarize(n_years = length(unique(year)),
#             n_fish = sum(total_count))

kelp_complete <- kelp_effort %>% 
  # Create complete grid of all species at all sites and years
  expand_grid(species_code = unique(kelp$species_code)) %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(kelp) %>% 
  # Add the scinames to match with species dataframe later
  left_join(kelp_orig %>% distinct(species_code, sciname, genus, target_status, name)) %>% 
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
  # Add vertical zonation and common name (region doesn't matter)
  left_join(sp %>% distinct(sciname, common_name, vertical_zonation)) %>% 
  mutate(vertical_zonation = case_when(species_code %in% c("EMBI", "OYB", "OYT", "SYNG", "SACA") ~ "Benthic, WC",
                                       species_code %in% c("BATH", "BOTH", "CITH", "GBY", "KGB", "HEXA", "PHOL", "PLEU", "RALL", "SCARSCAU", "STICH") ~ "Benthic",
                                       species_code %in% c("ATHE") ~ "Pelagic",
                                       T~vertical_zonation)) %>% 
  filter(!species_code %in% c("RFYOY", "SEBSPP")) %>% 
  dplyr::select(year, site, site_type, 
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, implementation_year_adj, size_km2,
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new, vertical_zonation, name, common_name,
                weight_kg, count, kg_per_m2, count_per_m2)


## Surf ----
# Read the site names for matching with the habitat site names (boo Chris bad processing making extra work!)
surf_sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/site_tables/processed/surf_site_names.Rds") 

surf_effort <- surf_orig %>% 
  # Identify distinct hauls
  distinct(year, month, day, bioregion, region4, affiliated_mpa,  mpa_defacto_class, mpa_defacto_designation, ref_is_mpa, site_name, haul_number) %>% 
  # Caclulate effort as total n hauls per site (mpa/ref) per year
  group_by(year, bioregion, region4, site_name, affiliated_mpa,  mpa_defacto_class,  mpa_defacto_designation, ref_is_mpa) %>% 
  summarize(n_rep = n()) %>%
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2)) %>% 
  left_join(surf_sites) 

surf <- surf_orig %>%
  group_by(year, site_name, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
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
  mutate(kg_per_haul = weight_kg/(n_rep), # 
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
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2, 
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                weight_kg, count, kg_per_haul)

## Rock (CCFRP) ----
rock_effort <- rock_orig %>% 
  # Identify distinct cell-trips (2 dropped above bc missing data; trinidad dropped)
  distinct(year, grid_cell_id, id_cell_per_trip, total_angler_hrs_cell,
           bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  group_by(year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, grid_cell_id) %>% 
  # Caculate number of cell-trips per site per year
  summarize(n_rep = n(),
            n_angler_hrs_cell = sum(total_angler_hrs_cell)) %>% ungroup() %>% 
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2))

# Calculated biomass per unit effort (trip-cell)
rock <- rock_orig %>% 
  # Calculate biomass per unit effort (trip-cell) for each individual fish
  mutate(bpue_kg = weight_kg/total_angler_hrs_cell) %>% 
  # Total BPUE for each species for each trip-cell
  group_by(year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, 
           grid_cell_id, id_cell_per_trip,
           species_code, sciname, genus, target_status) %>% 
  summarize(bpue_kg = sum(bpue_kg),
            count = sum(count), .groups = 'drop') %>% 
  # Drop NO_ORG because already captured effort above
  filter(!species_code == "NO_ORG")

# Get the biomass per unit effort for each site and year (relative to # trips to that site)
rock <- rock %>% 
  group_by(year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, 
           grid_cell_id, species_code, sciname, genus, target_status) %>% 
  summarize(bpue_kg = sum(bpue_kg)/n(),
            count = sum(count)/n(), .groups = 'drop')


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
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2, 
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                weight_kg = bpue_kg, count)


# Export Complete --------------------------------------------------------------------

saveRDS(kelp_effort, file.path(ltm.dir, "update_2024/2025/kelp_site_year_effort.Rds"))
saveRDS(rock_effort, file.path(ltm.dir, "update_2024/2025/ccfrp_site_year_effort.Rds"))
saveRDS(surf_effort, file.path(ltm.dir, "update_2024/2025/surf_site_year_effort.Rds"))


saveRDS(kelp_complete, file.path(ltm.dir, "update_2024/2025/kelp_biomass_complete.Rds")) # last write Feb 21 2025
saveRDS(rock_complete, file.path(ltm.dir, "update_2024/2025/rock_biomass_complete.Rds")) # last write Feb 21 2025
saveRDS(surf_complete, file.path(ltm.dir, "update_2024/2025/surf_biomass_complete.Rds")) # last write Feb 21 2025

# Clean Subsets -----------------------------------------------------------------------------

kelp_sites <- kelp_complete %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, implementation_year_adj, size_km2) %>%
  mutate(before = if_else(year < implementation_year, 1, 0),
         after = if_else(year >= implementation_year, 1, 0)) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, region4, affiliated_mpa,  mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = n_before + n_after, 
            years_after_2012 = paste(unique(year[year >= 2012]), collapse = ", "), .groups = 'drop') %>% 
  # Focus on data after MPA establishment
  filter(n_after > 0) %>% 
  # Focus on data 
  # Drop sites with no inside/outside information
  filter(!is.na(site_type)) %>%   # 331 sites with location info & attributed to specific MPA/Reference area; 54 MPAs
  # Drop sites that haven't been visited at least 5 times
  filter(n_after >= 5) # 211 sites; 41 MPAs

kelp_mpas <- kelp_sites %>%
  group_by(bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, site_type) %>%
  summarize(n_total = sum(n_total), .groups = 'drop') %>%
  pivot_wider(names_from = site_type, values_from = n_total) %>% 
  filter(!is.na(Reference)) %>% 
  filter(!is.na(MPA)) %>% # 33 MPAs with both inside/outside data
  filter(mpa_defacto_class == "smr") # 26 with defacto SMR status

kelp_subset <- kelp_complete %>% 
  # Drop observations for dropped sites 
  filter(site %in% kelp_sites$site) %>% 
  # Drop observations for dropped MPAs
  filter(affiliated_mpa %in% kelp_mpas$affiliated_mpa) %>% 
  # Drop before data
  filter(age_at_survey >= 0) %>% 
  # Join site visitation information
  left_join(kelp_sites)  %>% 
  mutate(kg_per_100m2 = kg_per_m2*100)


## Rock (CCFRP) ----

rock_sites <- rock_complete %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year) %>% 
  mutate(before = if_else(year < implementation_year, 1, 0),
         after = if_else(year >= implementation_year, 1, 0)) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after), # max visited is 16; most visited <5 times
            n_total = n_before + n_after, 
            years = paste(unique(year), collapse = ", "), .groups = 'drop') 

rock_mpas <- rock_sites %>%
  group_by(bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, site_type) %>%
  summarize(n_total = sum(n_total), .groups = 'drop') %>%
  pivot_wider(names_from = site_type, values_from = n_total) %>% 
  filter(MPA > 15 & Reference > 15) # visited at least 3x (these two only one year per above)

rock_subset <- rock_complete %>% 
  # Drop observations for dropped MPAs
  filter(affiliated_mpa %in% rock_mpas$affiliated_mpa) %>% 
  # Drop before data
  filter(age_at_survey >= 0) %>% 
  # Join site visitation information
  left_join(rock_sites) 

## Surf ----

surf_sites <- surf_complete %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_name, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, site_name, bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, site_type) %>% 
  summarize(n_total = n(), .groups = 'drop') %>%  # Started in 2019 so no need to do BA breakdown
  mutate(site = paste(site_name, site_type)) %>% 
  # ten mile reference only 3x, and low # hauls per effort table above - remove it
  filter(!affiliated_mpa  == "ten mile smr")


surf_subset <- surf_complete %>% 
  mutate(site = paste(site_name, site_type)) %>% 
  filter(affiliated_mpa %in% surf_sites$affiliated_mpa) %>% 
  # Join habitat and site visitation information
  left_join(surf_sites) 

# Test whether surf has any empty targeted hausl
test <- surf_complete %>% 
  group_by(year, site, site_type, region4, affiliated_mpa, target_status) %>% 
  summarize(kg_per_haul = sum(kg_per_haul, na.rm = F)) %>% 
  filter(target_status == "Targeted") # just one at ten mile, which we drop anyway
  

surf_effort <- surf_orig %>% 
  # Identify distinct hauls
  distinct(year, month, day, bioregion, region4, affiliated_mpa,  mpa_defacto_class, mpa_defacto_designation, ref_is_mpa, site_name, haul_number) %>% 
  # Caclulate effort as total n hauls per site (mpa/ref) per year
  group_by(year, bioregion, region4, site_name, affiliated_mpa,  mpa_defacto_class,  mpa_defacto_designation, ref_is_mpa) %>% 
  summarize(n_rep = n()) %>%
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2)) %>% 
  left_join(surf_sites) 



saveRDS(kelp_subset, file.path(ltm.dir, "update_2024/2025/kelp_biomass_subset.Rds"))
saveRDS(rock_subset, file.path(ltm.dir, "update_2024/2025/rock_biomass_subset.Rds"))
saveRDS(surf_subset, file.path(ltm.dir, "update_2024/2025/surf_biomass_subset.Rds"))

