#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 22, 2023
#Cori Lopazanski;lopazanski@bren.ucsb.edu

# About ----------------------------------------------------------------------------
# This script reads the clean biomass data created in Step 1 for each habitat. We are not
# using weight data, but want to leverage the same dataframe structure to ultimately combine
# with the biomass data. 
# We use the same processing steps as in Step 2 (repeated; note to CL to update for efficiency if/when 
# time allows). For each habitat group, for each MPA (inside or reference), for each year: We calculate
# the total effort, the total count for each species (or cpue, for CCFRP), and then use the vegan 
# package to calculate unweighted shannon diversity and richness. Note that this is taxonomic diversity
# and taxonomic richness, because we take the lowest classification used by the habitat group
# when identifying species. We use the effort calculations to calculate the weighted shannon diversity
# and richness, and then calculate the log response ratio for MPA/REF pairs for each year. 

# OUTPUT: richness_diversity_MPA_means.csv
# LOCATION: /home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"

# Processing notes
#1. use NO_ORG to keep track of effort when no species were observed. 

#2. we can only use species-level data for diversity, so drop higher taxa. All habitats recorded 
#   fish to the spp level unless they were unsure of an ID. 

#3. Diversity and richness are calculated at the MPA, NOT the transect level, since
#   some groups used depth-stratified sampling, and we ultimately care about H' and richness
#   at the MPA level. However we still need to account for effort

#4. Since effort could vary by MPA (e.g., ome sites have more transects than others),
#   we will weight each diversity and richness estimate by the number of replicates 
#   at that location. 

# Setup ----------------------------------------------------------------------------

rm(list=ls())

librarian::shelf(tidyverse, here, janitor, stringr, vegan)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"

surf_biomass <- read_csv(file.path(datadir, "surf_zone_fish_biomass_updated.csv"))
kelp_biomass <- read_csv(file.path(datadir, "kelpforest_fish_biomass_updated.csv"))
deep_biomass <- read_csv(file.path(datadir, "deep_reef_fish_biomass_updated.csv"))
ccfrp_biomass <- read_csv(file.path(datadir, "ccfrp_fish_biomass_updated.csv"))


# Process Biomass DFs --------------------------------------------------------------
# This step mirrors the processing steps taken in Step2 for calculating mpa-level means

surf <- surf_biomass %>% 
  filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths (WARNING: currently drops one full haul!)
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))  # this drops: RFYOY, FFUN, HALI, Zoarcidae spp (after previous step to avoid dropping NO_ORG)

kelp <- kelp_biomass %>% # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

ccfrp <- ccfrp_biomass %>% # WARNING: NA REMOVALS DROPS 2 CELL TRIPS
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status)) # drop for now - spp without target status identified (see notes for details)

deep <-  deep_biomass %>% # WARNING: NA REMOVALS DROPS 12 TRANSECTS
  filter(!is.na(weight_kg)) %>% # drop fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG" & is.na(target_status), "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))


# Calculate Diversity --------------------------------------------------------------------

## Surf zone ----------------------------------------------------------------------------

# Calculate effort
surf_effort <- surf %>% 
  # Individual hauls
  distinct(year, month, day, affiliated_mpa, mpa_defacto_designation, haul_number) %>% 
  # Calculate number of hauls per mpa, per year
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>% 
  summarize(n_rep = n()) 

# Find total counts of each species per MPA, per year
surf_diversity_build1 <- surf %>%
  # Drop anything not ID to species level
  filter(!is.na(sciname)) %>% # updated to sciname here CL Oct 11 2023 (CONFIRM REASONING WITH JS)
  group_by(year, affiliated_mpa, mpa_defacto_designation,
           species_code, class, order, family, genus, species, sciname) %>%
  # Summarize counts of individuals for each species in a given mpa/ref site per year
  summarize(n_fish = sum(count)) %>% 
  # Calculate MPA-level diversity
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>%
  summarize(shannon_unweighted = diversity(n_fish, index = "shannon"), #n MPAs should match with surf_effort
            richness_unweighted = length(unique(sciname))) %>%
  # Join effort
  full_join(., surf_effort) %>% # note that ten mile smr ref had all empty transects
  replace_na(list(shannon_unweighted = 0, richness_unweighted = 0)) %>% # replace those NAs with true zeroes
  # Calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_rep,
         richness_weighted = richness_unweighted / n_rep) %>% ungroup()

#Reshape
surf_diversity <- surf_diversity_build1 %>% 
  # Drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_designation == "smca")) %>%
  group_by(year, affiliated_mpa) %>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_rep" ,"shannon_weighted", "richness_weighted")) %>% 
  mutate(habitat = "Surf zone") %>% ungroup() %>% 
  # Some years have missing pairs (e.g. didn't sample both inside and outside), so these get dropped
  filter(!(is.na(n_rep_ref) | is.na(n_rep_smr)))


## Kelp forest -----------------------------------------------------------------------

# Calculate effort
kelp_effort <- kelp %>% 
  distinct(year, month, day, site, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           zone, transect) %>% 
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>% 
  summarize(n_rep = n())

# Find total counts of each species per MPA, per year
kelp_diversity_build <- kelp %>%
  filter(!is.na(sciname)) %>% # this will get rid of NO_ORG and any other species not identified to lowest level
  group_by(year, affiliated_mpa, mpa_defacto_designation,
           species_code, family, genus, species, sciname) %>%
  dplyr::summarize(n_fish = sum(count)) %>% 
  # Calculate MPA-level diversity
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>%
  dplyr::summarize(shannon_unweighted = diversity(n_fish, index = "shannon"), 
                   richness_unweighted = length(unique(sciname))) %>% # using sciname means "Sebastes spp" only counted as one species
  #join effort
  full_join(., kelp_effort) %>%
  #calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_rep,
         richness_weighted = richness_unweighted / n_rep)
#Reshape
kelp_diversity <- kelp_diversity_build %>% 
  #drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_designation == "smca")) %>%
  group_by(year, affiliated_mpa) %>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_rep" ,"shannon_weighted", "richness_weighted")) %>% 
  mutate(habitat = "Kelp forest") %>% ungroup() %>% 
  # Some years have missing pairs (e.g. didn't sample both inside and outside), so these get dropped
  filter(!(is.na(n_rep_ref) | is.na(n_rep_smr)))


## Shallow reef (CCFRP) ------------------------------------------------------------
ccfrp_effort <- ccfrp %>%
  # Distinct cell-trips after NA drop (2411)
  distinct(year, affiliated_mpa, mpa_defacto_designation, id_cell_per_trip) %>% 
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>% 
  # Sum the total number of cell-trips for each MPA/REF site, per year
  summarize(n_rep = n()) %>% ungroup() 
  
ccfrp_diversity_build1 <- ccfrp %>%
  # Drop anything not ID to lowest taxonomic level
  filter(!is.na(sciname)) %>%
  # Calculate CPUE (count per total angler hours on that particular cell-trip)
  mutate(cpue = count/total_angler_hrs_cell) %>% 
  group_by(year, affiliated_mpa, mpa_defacto_designation,
           species_code, family, genus, species, sciname) %>%
  # Sum total CPUE of each species per MPA, per year
  dplyr::summarize(n_fish = sum(cpue)) %>% 
  #calculate MPA-level diversity
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>%
  dplyr::summarize(shannon_unweighted = diversity(n_fish, index = "shannon"), 
                   richness_unweighted = length(unique(sciname))) %>%
  #join effort
  full_join(., ccfrp_effort) %>%
  #calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_rep,
         richness_weighted = richness_unweighted / n_rep) %>% ungroup()


# Reshape
ccfrp_diversity <- ccfrp_diversity_build1 %>% 
  #drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_designation == "smca")) %>%
  group_by(year, affiliated_mpa)%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_rep" ,"shannon_weighted", "richness_weighted")) %>% 
  mutate(habitat = "Shallow reef") %>% ungroup() %>% 
  filter(!(is.na(n_rep_smr) | is.na(n_rep_ref))) # this drops the 2 years trinidad was sampled (ref only)



## Deep reef ------------------------------------------------------------------------------
# Calculate effort for each MPA
deep_effort <- deep %>% 
  distinct(year, affiliated_mpa, mpa_defacto_designation, line_id) %>% 
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>% 
  summarize(n_rep = n()) %>% ungroup()

# Find total counts of each species per MPA
deep_diversity_build1 <- deep %>%
  filter(!is.na(sciname)) %>%
  group_by(year, affiliated_mpa, mpa_defacto_designation,
           family, genus, species, sciname) %>%
  # Summarize counts of individuals for each species 
  dplyr::summarize(n_fish = sum(count)) %>% 
  # Calculate MPA-level diversity
  group_by(year, affiliated_mpa, mpa_defacto_designation) %>%
  dplyr::summarize(shannon_unweighted = diversity(n_fish, index = "shannon"), 
                   richness_unweighted = length(unique(sciname))) %>%
  # Join effort
  full_join(., deep_effort) %>%
  #calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_rep,
         richness_weighted = richness_unweighted / n_rep)

# # Check if there were any MPAs where no species were observed
# deep_diversity_MPAs <- deep_diversity_build1 %>% 
#   group_by(year, bioregion, region4, affiliated_mpa,
#           # mpa_state_class, mpa_state_designation,
#            mpa_defacto_class, mpa_defacto_designation) %>% distinct()
# 
# nrow(deep_effort) - nrow(deep_diversity_MPAs) #one MPA has no fish
# 
# no_fish_MPA <- anti_join(deep_effort, deep_diversity_MPAs)

deep_diversity <- deep_diversity_build1 %>% 
  #drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_designation == "smca")) %>%
  group_by(year, affiliated_mpa) %>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_rep" ,"shannon_weighted", "richness_weighted")) %>% 
  filter(!(is.na(n_rep_ref) | is.na(n_rep_smr))) %>% 
  mutate(habitat = "Deep reef") %>%  ungroup()

# Join data ----------------------------------------------------------------------------
diversity_full <- bind_rows(surf_diversity, kelp_diversity, ccfrp_diversity, deep_diversity)


# Calculate response ratio ---------------------------------------------------------------

# Step 1: Pivot longer
diversity_long <- diversity_full %>%
  pivot_longer(
    cols = c("shannon_unweighted_ref", "shannon_unweighted_smr", 
             "richness_unweighted_ref", "richness_unweighted_smr", 
             "shannon_weighted_ref", "shannon_weighted_smr", 
             "richness_weighted_ref", "richness_weighted_smr"),
    names_to = "factor",
    values_to = "value"
  )

# Step 2: Calculate 10% of the mean and create 'scalar', add to each factor level
diversity_long_scaled <- diversity_long %>%
  group_by(year, habitat, factor) %>%
  mutate(value = value + 0.1 * mean(value, na.rm = TRUE)) %>% 
  arrange(year, habitat, factor)

# Step 3: Pivot wider back to the original format
diversity_scaled <- diversity_long_scaled %>%
  pivot_wider(
    names_from = factor,
    values_from = value
  )

original_order <- c("habitat", "year", "bioregion", "region4", "affiliated_mpa")
pivot_wider_result <- pivot_wider_result %>%
  dplyr::select(original_order, everything())

# Step 5: calculate response ratio
richness_diversity_RR <- diversity_scaled %>%
  mutate(shannon_weighted_logRR    = log(shannon_weighted_smr/shannon_unweighted_ref),
         shannon_unweighted_logRR  = log(shannon_unweighted_smr/shannon_unweighted_ref),
         richness_weighted_logRR   = log(richness_weighted_smr/richness_weighted_ref),
         richness_unweighted_logRR = log(richness_unweighted_smr/richness_unweighted_ref))
  

#write.csv(row.names = FALSE, richness_diversity_RR, file.path(datadir, "richness_diversity_MPA_means.csv"))
# last write Oct 26 2023








