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

# deep_orig <- read_csv(file.path(ltm.dir,"update_2024/deep_reef_fish_biomass_updated.csv")) %>% #WARNING: NA REMOVALS DROPS 12 TRANSECTS
#   filter(!is.na(weight_kg)) %>% # drop fishes unknown or without lengths/conversion params
#   mutate(target_status = if_else(species_code == "NO_ORG" & is.na(target_status), "NO_ORG", target_status)) %>%  # helpful for inspecting
#   filter(!is.na(target_status))

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

# Calculate the total size for clusters
cluster_area <- mpas %>% 
  filter(!cluster == "no") %>% 
  group_by(cluster) %>% 
  summarize(cluster_area_km2 = sum(size_km2), .groups = 'drop')

mpas <- mpas %>% 
  left_join(cluster_area) %>% 
  mutate(cluster_area_km2 = if_else(is.na(cluster_area_km2), size_km2, cluster_area_km2))


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
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2, cluster_area_km2)) %>% 
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
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, implementation_year_adj, size_km2, cluster_area_km2,
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new, vertical_zonation, name, common_name,
                weight_kg, count, kg_per_m2, count_per_m2)

# Code for vertical zonation after quick review:
# mutate(vertical_zonation = case_when(
#   sciname %in% c("Lethops connectens", "Scorpaena guttata", "Anarrhichthys ocellatus", "Porichthys notatus",
#                  "Squatina californica", "Tetronarce californica") ~ "Benthic", 
#   sciname %in% c("Embiotoca jacksoni", "Semicossyphus pulcher", "Embiotoca lateralis", 
#                  "Rhacochilus toxotes", "Sebastes hopkinsi", "Sebastes mystinus", "Hypsypops rubicundus", 
#                  "Girella nigricans", "Cymatogaster aggregata", "Balistes polylepis", "Phanerodon atripes",
#                  "Micrometrus minimus", "Kyphosus azureus", "Amphistichus argenteus") ~ "Benthic", # would be BP
#   sciname %in% c("Oxyjulis californica", "Hyperprosopon argenteum", "Hyperprosopon anale", "Hyperprosopon ellipticum",
#                  "Aulorhynchus flavidus", "Atractoscion nobilis") ~ "Pelagic",
#   vertical_zonation == "Benthic" ~ "Benthic",
#   vertical_zonation == "WC" ~ "Benthic", # would be BP
#   vertical_zonation == "Benthic, WC" ~ "Benthic", # would be BP
#   vertical_zonation == "Pelagic" ~ "Pelagic",
#   T~NA))

## Surf ----
# Read the site names for matching with the habitat site names (boo Chris bad processing making extra work!)
surf_sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/site_tables/processed/surf_site_names.Rds") 

surf_effort <- surf_orig %>% 
  # Identify distinct hauls - 2607 (after NA droped above)
  distinct(year, month, day, bioregion, region4, affiliated_mpa,  mpa_defacto_class, mpa_defacto_designation, ref_is_mpa, site_name, haul_number) %>% 
  # Caclulate effort as total n hauls per site (mpa/ref) per year
  group_by(year, bioregion, region4, site_name, affiliated_mpa,  mpa_defacto_class,  mpa_defacto_designation, ref_is_mpa) %>% 
  summarize(n_rep = n()) %>% 
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2, cluster_area_km2)) %>% 
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
       #  count_per_haul = count/(n_rep), # this probably no longer accurate after expanding in processing
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
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2, cluster_area_km2,
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
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2, cluster_area_km2))

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
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2, cluster_area_km2,
                age_at_survey, n_rep, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                weight_kg = bpue_kg, count)


## Deep ----
deep_effort <- deep_orig %>%
  # Identify distinct transects that have location information - 1532
  distinct(year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, dive, transect_id, transect_id_desig, area_m2) %>% 
  # Join MPA metadata (region, implementation year)
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_year, size_km2, cluster_area_km2)) 

deep <- deep_orig %>%
  group_by(year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,dive, transect_id, transect_id_desig, area_m2, species_code) %>%
  # Biomass and count for each species on each transect
  dplyr::summarize(biomass_kg = sum(weight_kg),
                   count = sum(count), .groups = 'drop') %>% # 9961
  # Drop NO_ORG (not true zeroes)
  filter(!species_code == "NO_ORG") %>%  # 9903
  # Calculate BPUE for each transect
  mutate(kg_per_m2 = biomass_kg/area_m2,
         count_per_m2 = count/area_m2)

deep_complete <- deep_effort %>% 
  # Create complete grid of all species at all sites and years
  expand_grid(species_code = unique(deep$species_code)) %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(deep) %>% 
  # Add the scinames to match with species dataframe later
  left_join(deep_orig %>% distinct(species_code, sciname, genus, target_status)) %>% 
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
  dplyr::select(year, site = transect_id_desig, site_type, dive,
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2, cluster_area_km2,
                age_at_survey, species_code, sciname, genus, target_status, assemblage, assemblage_new,
                biomass_kg, count, kg_per_m2, count_per_m2)

# Export Complete --------------------------------------------------------------------
#saveRDS(kelp, file.path(ltm.dir, "update_2024/kelp_biomass_site_year.Rds")) # last write Nov 14 2024
#saveRDS(surf, file.path(ltm.dir, "update_2024/biomass/biomass_site_year/surf_biomass_site_year.Rds"))
#saveRDS(rock, file.path(ltm.dir, "update_2024/ccfrp_biomass_site_year.Rds")) # last write Nov 14 2024
#saveRDS(deep, file.path(ltm.dir, "update_2024/biomass/biomass_site_year/deep_biomass_site_year.Rds"))


# saveRDS(kelp_effort, file.path(ltm.dir, "update_2024/kelp_site_year_effort.Rds"))
# saveRDS(rock_effort, file.path(ltm.dir, "update_2024/ccfrp_site_year_effort.Rds"))
# saveRDS(deep_effort, file.path(ltm.dir, "update_2024/deep_site_year_effort.Rds"))
# 
# saveRDS(kelp_complete, file.path(ltm.dir, "update_2024/kelp_biomass_complete.Rds")) # last write Feb 21 2025
# saveRDS(rock_complete, file.path(ltm.dir, "update_2024/rock_biomass_complete.Rds")) # last write Feb 21 2025
# saveRDS(surf_complete, file.path(ltm.dir, "update_2024/surf_biomass_complete.Rds")) # last write Feb 21 2025
# saveRDS(deep_complete, file.path(ltm.dir, "update_2024/deep_biomass_complete.Rds")) # last write 3 Mar 2025

saveRDS(kelp_effort, file.path(ltm.dir, "update_2024/2025/kelp_site_year_effort.Rds"))
saveRDS(rock_effort, file.path(ltm.dir, "update_2024/2025/ccfrp_site_year_effort.Rds"))
saveRDS(deep_effort, file.path(ltm.dir, "update_2024/2025/deep_site_year_effort.Rds"))

saveRDS(kelp_complete, file.path(ltm.dir, "update_2024/2025/kelp_biomass_complete.Rds")) # last write Feb 21 2025
saveRDS(rock_complete, file.path(ltm.dir, "update_2024/2025/rock_biomass_complete.Rds")) # last write Feb 21 2025
saveRDS(surf_complete, file.path(ltm.dir, "update_2024/2025/surf_biomass_complete.Rds")) # last write Feb 21 2025
saveRDS(deep_complete, file.path(ltm.dir, "update_2024/2025/deep_biomass_complete.Rds")) # last write 3 Mar 2025

# Clean Subsets -----------------------------------------------------------------------------

kelp_sites <- kelp_complete %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, cluster_area_km2) %>%
  mutate(before = if_else(year < implementation_year, 1, 0),
         after = if_else(year >= implementation_year, 1, 0)) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, region4, affiliated_mpa,  mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, cluster_area_km2, site_type) %>% 
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
  group_by(bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, cluster_area_km2, site_type) %>%
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
  summarize(n_total = n(), .groups = 'drop') %>%  # Started in 2019 so no need to do breakdown
  mutate(site = paste(site_name, site_type))

# Even sampling across years and MPAs, no neeed to filter
surf_subset <- surf_complete %>% 
  mutate(site = paste(site_name, site_type)) %>% 
  # Join habitat and site visitation information
  left_join(surf_sites) 

## Deep ----
deep_sites <- deep_complete %>%
  # Identify distinct site-year combinations (site is a transect)
  distinct(year, site, site_type, dive, bioregion, affiliated_mpa, mpa_defacto_class, implementation_year, size_km2, cluster_area_km2) %>%
  mutate(before = if_else(year < implementation_year, 1, 0), # only point lobos visited once before
         after = if_else(year >= implementation_year, 1, 0),
         total = before + after) 

deep_mpas <- deep_sites %>%
  group_by(bioregion, affiliated_mpa, mpa_defacto_class, site_type) %>%
  # Total transects per MPA/REF across all years
  summarize(n_total = sum(total)) %>%
  pivot_wider(names_from = site_type, values_from = n_total) %>%
  filter(!is.na(Reference)) %>%
  filter(!is.na(MPA)) %>%
  filter(MPA > 4 & Reference > 4) %>% # Changed from 5 to 4 so...
  filter(mpa_defacto_class == "smr") # 16 MPAs

deep_subset <- deep_complete %>% 
  # Drop observations for dropped MPAs
  filter(affiliated_mpa %in% deep_mpas$affiliated_mpa) %>% 
  # Join visitation information
  left_join(deep_sites) 

# Consolidate so each site is a dive associated with a particular year and MPA/REF status
deep_subset2 <- deep_subset %>% 
  mutate(year_dive_type = paste(year, dive, site_type, sep = "-")) %>% 
  group_by(year, year_dive_type, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class,
           mpa_defacto_designation, implementation_year, size_km2, cluster_area_km2, age_at_survey, species_code, sciname, genus,
           target_status, assemblage, assemblage_new) %>% 
  summarize(biomass_kg = sum(biomass_kg),
            count = sum(count),
            kg_per_m2 = sum(kg_per_m2)/n(),
            count_per_m2 = sum(count_per_m2)/n(), .groups = 'drop')

deep_subset3 <- deep_subset2 %>% 
  rename(site = year_dive_type)



# saveRDS(kelp_subset, file.path(ltm.dir, "update_2024/kelp_biomass_subset.Rds")) 
# saveRDS(rock_subset, file.path(ltm.dir, "update_2024/rock_biomass_subset.Rds")) 
# saveRDS(surf_subset, file.path(ltm.dir, "update_2024/surf_biomass_subset.Rds"))
# saveRDS(deep_subset3, file.path(ltm.dir, "update_2024/deep_biomass_subset.Rds")) 

saveRDS(kelp_subset, file.path(ltm.dir, "update_2024/2025/kelp_biomass_subset.Rds"))
saveRDS(rock_subset, file.path(ltm.dir, "update_2024/2025/rock_biomass_subset.Rds"))
saveRDS(surf_subset, file.path(ltm.dir, "update_2024/2025/surf_biomass_subset.Rds"))
#saveRDS(deep_subset3, file.path(ltm.dir, "update_2024/2025/deep_biomass_subset.Rds"))




# library(vegan)
# kelp_test <- readRDS(file.path(ltm.dir, "update_2024/kelp_biomass_subset.Rds"))
# 
# 
# kelp_diversity <- kelp_test %>% 
#   dplyr::select(year:n_rep, species_code, count) %>% 
#   pivot_wider(names_from = species_code, values_from = count)
# 
# kelp_diversity[is.na(kelp_diversity)] <- 0
# 
# species_mat <- kelp_diversity %>% 
#   dplyr::select(-(year:n_rep)) %>% 
#   as.matrix()
# 
# kelp_diversity$richness <- specnumber(species_mat)
# kelp_diversity$shannon  <- diversity(species_mat, index = "shannon")
# 
# kelp_diversity <- kelp_diversity %>% 
#   dplyr::select(year:n_rep, richness, shannon) %>% 
#   mutate(shannon_weighted = shannon/n_rep,
#          richness_weighted = richness/n_rep)
# 
# ggplot(kelp_diversity) +
#   geom_density(aes(x = shannon_weighted, fill = site_type, color = site_type), alpha = 0.5) +
#   facet_wrap(~region4, scales = 'free')
# 
# 
# kelp_diversity_targeted <- kelp_test %>% 
#   filter(target_status == "Targeted") %>% 
#   dplyr::select(year:n_rep, species_code, count) %>% 
#   pivot_wider(names_from = species_code, values_from = count)
#   
# kelp_diversity_targeted[is.na(kelp_diversity_targeted)] <- 0
# 
# species_mat <- kelp_diversity_targeted %>% 
#   dplyr::select(-(year:n_rep)) %>% 
#   as.matrix()
# 
# kelp_diversity_targeted$richness <- specnumber(species_mat)
# kelp_diversity_targeted$shannon  <- diversity(species_mat, index = "shannon")
# 
# kelp_diversity_targeted <- kelp_diversity_targeted %>% 
#   dplyr::select(year:n_rep, richness, shannon) %>% 
#   mutate(shannon_weighted = shannon/n_rep,
#          richness_weighted = richness/n_rep)
# 
# ggplot(kelp_diversity_targeted) +
#   geom_density(aes(x = shannon_weighted, fill = site_type, color = site_type), alpha = 0.5)
# 
# saveRDS(kelp_diversity, "kelp_div.Rds")
# saveRDS(kelp_diversity_targeted, "kelp_div_targ.Rds")
# 
