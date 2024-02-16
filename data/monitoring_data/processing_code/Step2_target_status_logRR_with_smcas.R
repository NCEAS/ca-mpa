#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 22, 2023
#Updated by Cori Lopazanski; Sept 2023

#processing steps

#1. calculate total biomass for each target_status level at the replicate unit
#2. keep track of effort
#3. determine the average biomass across replicates for a given MPA. Error is calculated using the effort
#4. make wide format (every row is a single replicate within a MPA with a column for inside (ref) and outside (smr)) and filter only defacto SMRs
#5. following typical naming
#6. join everything into a single dataset

# Setup --------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)

# Set directories 
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"

# Load data
surf_biomass <- read.csv(file.path(datadir, "surf_zone_fish_biomass_updated.csv"))
kelp_biomass <- read.csv(file.path(datadir, "kelpforest_fish_biomass_updated.csv")) 
ccfrp_biomass <- read.csv(file.path(datadir, "ccfrp_fish_biomass_updated.csv"))
deep_biomass <- read.csv(file.path(datadir, "deep_reef_fish_biomass_updated.csv"))

# Process Biomass DFs -----
surf_zone_raw <- surf_biomass %>% 
  filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths (WARNING: currently drops one full haul!)
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))  # this drops: RFYOY, FFUN, HALI, Zoarcidae spp (after previous step to avoid dropping NO_ORG)

kelp_raw <- kelp_biomass %>% # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~871)
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

rocky_reef_raw <- ccfrp_biomass %>% # WARNING: NA REMOVALS DROPS 2 CELL TRIPS
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status)) # drop for now - spp without target status identified (see notes for details)

deep_reef_raw <-  deep_biomass %>% # WARNING: NA REMOVALS DROPS 12 TRANSECTS
  filter(!is.na(weight_kg)) %>% # drop fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG" & is.na(target_status), "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status))

# Calculate lnRRs for targeted vs nontargeted ------------------------------------------------------------------------------------------------------

## Surf Zone  -----------------------------------------------------------------------------------------------------

# Calculate effort as n hauls 
surf_effort <- surf_biomass %>% 
  distinct(year, month, day, affiliated_mpa, mpa_defacto_designation, haul_number) %>% 
  arrange(year, month, day, affiliated_mpa, mpa_defacto_designation, haul_number)

# Compare effort to see how many hauls are dropped when NAs excluded above
# one full haul is dropped (918 to 917)
surf_effort_drop <- surf_zone_raw %>% 
  distinct(year, month, day, affiliated_mpa, mpa_defacto_designation, haul_number)

# Calculate total biom for each rep unit (haul)
surf_build1 <- surf_zone_raw %>%
  group_by(year, month, day, # updated by CL: need month and day to get individual hauls
           # bioregion, region4, 
           affiliated_mpa, 
           # mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           haul_number, target_status) %>%
  dplyr::summarize(total_biomass = sum(weight_kg)) %>%
  #make wider to fill NAs with true zeros 
  pivot_wider(names_from = target_status, values_from = total_biomass) %>%
  #drop NO_ORG column
  select(-NO_ORG) %>% # updated by CL for better QC; there should be no NA column 
  #replace NAs with 0s, since these are true zeros
  replace_na(list(Targeted = 0, Nontargeted = 0)) %>%
  #make longer
  pivot_longer(cols = c(Targeted, Nontargeted), names_to = "target_status", values_to = "total_biomass_kg") # colname updated by CL to _kg


# Calculate MPA average and error
surf_build2 <- surf_build1 %>%
  #leave out haul number to take the average across hauls for a given MPA
  group_by(year, affiliated_mpa, mpa_defacto_class,
           mpa_defacto_designation, target_status) %>%
  summarize(
    biomass = mean(total_biomass_kg),
    n_rep = n(),
    sd = sd(total_biomass_kg),
    se = sd / sqrt(n_rep))%>%
  mutate(site_type = ifelse(mpa_defacto_designation == "ref","ref","mpa"))

# Reshape -- updated by CL to complete in one step
surf_build3 <- surf_build2 %>% 
  ungroup()%>%
  select(!(mpa_defacto_designation))%>%
  # drop smcas. We only want to include no-take SMRs in our analysis
  #filter(!(mpa_defacto_class == "smca")) %>% 
  pivot_wider(names_from = "site_type", 
              values_from = c(biomass, n_rep, sd, se)) %>% 
  mutate(habitat = "Surf zone") %>% ungroup() %>% 
  left_join(surf_biomass %>% distinct(affiliated_mpa, bioregion, region4)) %>% 
  select(habitat, year, bioregion, region4, affiliated_mpa, mpa_defacto_class, target_status,
         biomass_ref, biomass_mpa, n_rep_ref, n_rep_mpa,
         sd_ref, sd_mpa,se_ref, se_mpa)

## Kelp Forest --------------------------------------------------------------------------------------------

# Identify distinct transects in full dataset (before NAs dropped)
# 29173
kelp_effort_transect <- kelp_biomass %>% 
  distinct(year, month, day, site, # need this to get actual individual transects - updated by CL
           affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           zone, transect) 

# Identify distinct transects after NAs dropped (for subsequent analyses)
# 28300
kelp_effort_transect_drop <- kelp_raw %>% 
  distinct(year, month, day, site, # need this to get actual individual transects - updated by CL
           affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           zone, transect) %>% 
  arrange(year, month, day, site, affiliated_mpa, mpa_defacto_designation, zone, transect) # Could then group to calculate transects; not needed yet

# calculate effort as n transects per MPA year (separately for smr and ref)
kelp_effort_mpa <- kelp_effort_transect_drop %>% 
  group_by(year, affiliated_mpa, mpa_defacto_class,  mpa_defacto_designation) %>% 
  summarize(n_rep = n()) # for comparison later

# calculate total biom for each rep unit (transect)
kelp_build1 <- kelp_raw %>% # updated by CL - filter separately
  group_by(year, 
           month, day, site, # updated by CL
           affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, 
           zone, transect, target_status) %>%
  summarise(total_biomass = sum(weight_kg)) %>%
  # Widen to inspect and correct true zeroes
  pivot_wider(names_from = target_status, values_from = total_biomass) %>%
  # There are 6 transects where there are both species listed and an entry for "NO_ORG" 
  # Can check by stopping here and running: 
  #filter(!is.na(NO_ORG) & (!is.na(Targeted) | !is.na(Nontargeted)))
  # Assume that the NO_ORG was entered incorrectly and fully remove these cases
  filter(is.na(NO_ORG) | # keep transects where there's no NO_ORG entry
           (!is.na(NO_ORG) & is.na(Targeted) & is.na(Nontargeted))) %>%  # OR where NO_ORG = 0 and there are no other species listed
  # Drop NO_ORG column
  select(-NO_ORG) %>% 
  #replace NAs with 0s, since these are true zeros
  replace_na(list(Targeted = 0, Nontargeted = 0)) %>%
  #make longer
  pivot_longer(cols = c(Targeted, Nontargeted), names_to = "target_status", values_to = "total_biomass_kg")


#calculate MPA average and error
kelp_build2 <- kelp_build1 %>%
  group_by(year, affiliated_mpa, 
           mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  summarize(biomass = mean(total_biomass_kg, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass_kg, na.rm=TRUE),
            se = sd/sqrt(n_rep))  %>% 
#check if any sites do not match (this would mean no fish for an entire MPA or REF site!)
#anti_join(kelp_effort_mpa, by = c("year", "affiliated_mpa","mpa_defacto_class", "mpa_defacto_designation"))
  mutate(site_type = ifelse(mpa_defacto_designation == "ref","ref","mpa"))
  
#Reshape
kelp_build3 <- kelp_build2 %>% 
  ungroup()%>%
  select(!(mpa_defacto_designation))%>%
  # drop smcas. We only want to include no-take SMRs in our analysis
  #filter(!(mpa_defacto_class == "smca")) %>% 
  pivot_wider(names_from = "site_type", 
              values_from = c(biomass, n_rep, sd, se)) %>% 
  #drop sites that do not have pairs. We've already accounted for true zeros, so any missing data means there is not a pair
  filter(!(is.na(n_rep_mpa)|is.na(n_rep_ref))) %>% 
  mutate(habitat = "Kelp forest") %>% ungroup() %>% 
  left_join(kelp_biomass %>% distinct(affiliated_mpa, bioregion, region4)) %>% 
  select(habitat, year, bioregion, region4, affiliated_mpa, mpa_defacto_class, target_status,
         biomass_mpa, biomass_ref, n_rep_mpa, n_rep_ref,
         sd_mpa, sd_ref,se_mpa, se_ref)


# Note: These MPAs have missing reference sites for ALL years, indicating there is no
# reference measurement (ever). These five MPAs are dropped from the analyses in previous
# step - but demonstrating here for clarity.
affiliated_mpa_with_all_na <- kelp_build2 %>%
  pivot_wider(names_from = "mpa_defacto_designation", 
              values_from = c(biomass, n_rep, sd, se)) %>% 
  group_by(affiliated_mpa) %>%
  summarise(all_na = all(is.na(biomass_ref))) %>%
  filter(all_na) %>%
  dplyr::select(affiliated_mpa)


## CCFRP ---------------------------------------------------------------------------------------
### Treats cell-trip as the unit of replication
### Approached updated by CL September 2023

# Calculate number of cell-trips in full data - 2413
ccfrp_effort <- ccfrp_biomass %>% 
  distinct(id_cell_per_trip)

# Calculate number of cell-trips after NA drop - 2411
# 2 full cell-trips are dropped due to missing data
ccfrp_effort_drop <- rocky_reef_raw %>% 
  distinct(id_cell_per_trip)

# Calculated biomass per unit effort (trip-cell)
ccfrp_build1 <- rocky_reef_raw %>% 
  select(year, affiliated_mpa, 
         mpa_defacto_class, mpa_defacto_designation,
         id_cell_per_trip, species_code, sciname, target_status,
         total_angler_hrs_cell, weight_kg) %>% 
  # Calculate biomass per unit effort (trip-cell) for each fish
  mutate(bpue_kg = weight_kg/total_angler_hrs_cell) %>% 
  # Calculate the total targeted/non-targeted BPUE for each trip-cell
  # (This sums across all drifts within a single cell on a single trip)
  group_by(year, affiliated_mpa,  mpa_defacto_class, mpa_defacto_designation, 
           id_cell_per_trip, target_status) %>% 
  summarize(total_bpue_kg = sum(bpue_kg, na.rm = T)) %>% 
  # Make wider 
  pivot_wider(names_from = target_status, values_from = total_bpue_kg) %>% 
  # NEEDS CONFIRMATION: Effort associated with NO_ORG drifts already captured in 
  # BPUE calculations in data processing (effort calculated as total hours at cell-trip level)
  # therefore can ignore the NO_ORG entries EXCEPT for cases where there were no fishes
  # caught in an entire cell-trip. There are 7 of these (after NA drop above).
  # Confirm by filtering for this: 
  #filter(is.na(Targeted) & is.na(Nontargeted) & NO_ORG == 0)
  # Note: NO_ORG is entered at the drift level, therefore it is OK if there is NO_ORG = 0
  # and values for targeted/nontargeted (unlike other habitats where this would be a flag)
  
  # Replace NAs with 0s, since these are true zeros
  replace_na(list(Targeted = 0, Nontargeted = 0)) %>% 
  # Drop NO_ORG column
  select(-NO_ORG) %>% 
  pivot_longer(cols = c(Targeted, Nontargeted), 
               names_to = "target_status", 
               values_to = "total_biomass_kg") 

ccfrp_build2 <- ccfrp_build1 %>%    
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, target_status) %>% 
  summarize(biomass = mean(total_biomass_kg),
            n_rep = n(),
            sd = sd(total_biomass_kg, na.rm=TRUE),
            se = sd/sqrt(n_rep)) %>%
  mutate(site_type = ifelse(mpa_defacto_designation == "ref","ref","mpa"))

ccfrp_build3 <- ccfrp_build2 %>% 
  ungroup()%>%
  select(!(mpa_defacto_designation))%>%
 # filter(!(mpa_defacto_class == "smca")) %>%  # drop smcas
  filter(!(target_status == "Nontargeted")) %>% # drop nontargeted (for now); many many zeroes
  filter(!(affiliated_mpa == "trinidad NA")) %>% # drop trinidad
  pivot_wider(names_from = "site_type",
              values_from = c(biomass, n_rep, sd, se)) %>% 
  mutate(habitat = "Shallow reef") %>% ungroup() %>% 
  left_join(ccfrp_biomass %>% distinct(affiliated_mpa, bioregion, region4)) %>% 
  select(habitat, year, bioregion, region4, affiliated_mpa, mpa_defacto_class, target_status,
         biomass_mpa, biomass_ref, n_rep_mpa, n_rep_ref,
         sd_mpa, sd_ref,se_mpa, se_ref)


## Deep Reef --------------------------------------------------------------------


# Calculate number of transects in full dataset (before drop NAs) 
# 1647
deep_effort <- deep_biomass %>% 
  distinct(year, affiliated_mpa, line_id)

# Compare to number of transects after dropping NAs
# 1635
deep_effort_drop <- deep_reef_raw %>% 
  distinct(year, affiliated_mpa, line_id)

# calculate total biom for each rep unit (transect - aka line_id)
deep_build1 <- deep_reef_raw %>%
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           line_id, target_status) %>%
  dplyr::summarize(total_biomass = sum(weight_kg)) %>%
  #make wider to fill NAs with true zeros 
  pivot_wider(names_from = target_status, values_from = total_biomass) %>%
  #drop NO_ORG column
  select(!NO_ORG) %>% 
  #replace NAs with 0s, since these are true zeros
  replace_na(list(Targeted = 0, Nontargeted = 0)) %>%
  #make longer
  pivot_longer(cols = c(Targeted, Nontargeted), names_to = "target_status", values_to = "total_biomass")


# calculate MPA average and error
deep_build2 <- deep_build1 %>%
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  summarize(biomass = mean(total_biomass, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass, na.rm=TRUE),
            se = sd/sqrt(n_rep)) %>%
  mutate(site_type = ifelse(mpa_defacto_designation == "ref","ref","mpa"))


# Reshape
deep_build3 <- deep_build2 %>% 
 # filter(!(mpa_defacto_class == "smca"))%>%
  ungroup()%>%
  select(!(mpa_defacto_designation))%>%
  pivot_wider(names_from = "site_type", 
              values_from = c(biomass, n_rep, sd, se)) %>% 
  filter(!(is.na(n_rep_ref)|is.na(n_rep_mpa))) %>% 
  mutate(habitat = "Deep reef") %>% ungroup() %>% 
  left_join(deep_biomass %>% distinct(affiliated_mpa, bioregion, region4)) %>% 
  select(habitat, year, bioregion, region4, mpa_defacto_class, affiliated_mpa, target_status,
         biomass_mpa, biomass_ref, n_rep_mpa, n_rep_ref,
         sd_mpa, sd_ref,se_mpa, se_ref)



# Join data ----------------------------------------------------------------------------
identical(colnames(surf_build3), colnames(kelp_build3))
identical(colnames(kelp_build3), colnames(ccfrp_build3))
identical(colnames(ccfrp_build3), colnames(deep_build3))

target_full <- rbind(surf_build3, kelp_build3, ccfrp_build3, deep_build3) %>%
                #add state class
                mutate(mpa_state_class = word(affiliated_mpa, -1)) %>%
                select(habitat:affiliated_mpa, mpa_state_class, everything())

# Calculate response ratio ------------------------------------------------------------------------

target_status_RR <- target_full %>%
  #deal with the 0s by adding 10% of the mean for each group. 
  group_by(year, habitat, mpa_defacto_class)%>% #Adding year really changes the shape of the histogram, so need to check on this. 
  mutate(scalar_mpa = mean(biomass_mpa)*.10, #determine 10% of the mean biomass for each habitat inside MPAs
         scalar_ref = mean(biomass_ref)*.10) %>% #determine 10% of the mean biomass for each habitat outside MPAs
  ungroup() %>%
  mutate(logRR = log((biomass_mpa+scalar_mpa) / (biomass_ref + scalar_ref))) #add appropriate scalar back to bioass estimate and calculate RR

hist(target_status_RR$logRR)

# New Version Sept 2023 (after CL processing)
#write.csv(target_status_RR, row.names = F, "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed/target_status_biomass_MPA_means.csv") #old


write.csv(target_status_RR, row.names = F, "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed/target_status_biomass_MPA_means_updated.csv")
# Last write 16 Feb 2024

# Old Version:                    
#write.csv(row.names = FALSE, target_status_RR, file.path(datadir, "targeted_nontargeted_biomass_MPA_means.csv"))


