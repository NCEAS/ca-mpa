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
# surf_zone_raw <- read.csv(file.path(datadir, "surf_zone_fish_biomass.csv"))
# kelp_raw <- read.csv(file.path(datadir, "kelpforest_fish_biomass.csv")) 
# rocky_reef_raw <- read.csv(file.path(datadir, "ccfrp_fish_biomass.csv"))
# deep_reef_raw <- read.csv(file.path(datadir, "deep_reef_fish_biomass.csv"))

# Currently avoiding overwriting any files - these four dataframes will read directly from
# step one (e.g. run step one through the biomass conversion, and then the following lines
# will set each dataset up appropriately to run the analysis). Once processing is confirmed 
# I will write the biomass dataframes to csv and read directly as in above. For now can compare
# results by toggling whether you use the above "load data" dataframes (with original calcs before
# CL code review) or the ones below.

# START HERE IF PROCEEDING DIRECTLY AFTER STEP 1 WITHOUT CLEARING WS:

surf_zone_raw <- surf_biomass %>% 
  mutate(weight_kg = weight_g/1000) %>% 
  filter(!is.na(weight_g)) %>%  # drop for now - these are all fishes that are unknown or species with no lengths (WARNING: currently drops one full haul!)
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status)) %>% # this drops: RFYOY, FFUN, HALI, Zoarcidae spp
  arrange(year, month, day, affiliated_mpa, mpa_defacto_designation, haul_number, target_status, sciname)

kelp_raw <- kelp_biomass %>% # WARNING: THE NA REMOVALS HERE DROPS LOTS OF TRANSECTS (~3000)
  mutate(weight_kg = weight_g/1000) %>% 
  filter(!is.na(affiliated_mpa)) %>% # drops sites with no mpa (yellowbanks, trinidad, etc - see kf processing for details)
  filter(!is.na(weight_kg)) %>%  # drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status))

  
rocky_reef_raw <- ccfrp_biomass %>% 
  mutate(weight_kg = weight_g/1000) %>% 
  filter(!is.na(weight_kg)) %>%   #  drops fishes unknown or without lengths/conversion params
  mutate(target_status = if_else(species_code == "NO_ORG", "NO_ORG", target_status)) %>%  # helpful for inspecting 
  filter(!is.na(target_status)) # drop for now - spp without target status identified (see notes for details)

deep_reef_raw <-  deep_biomass

# Calculate lnRRs for targeted vs nontargeted ------------------------------------------------------------------------------------------------------

# SURF ZONE  -----------------------------------------------------------------------------------------------------

# Calculate effort as n hauls
surf_effort <- surf_zone_raw %>% 
  distinct(year, month, day, affiliated_mpa, mpa_defacto_designation, haul_number) %>% 
  arrange(year, month, day, affiliated_mpa, mpa_defacto_designation, haul_number)

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
  group_by(year,
           #bioregion, region4,
           affiliated_mpa,
           mpa_defacto_class,
           mpa_defacto_designation,
           target_status) %>%
  summarize(
    biomass = mean(total_biomass_kg),
    n_rep = n(),
    sd = sd(total_biomass_kg),
    se = sd / sqrt(n_rep))

# Reshape -- updated by CL to complete in one step
surf_build3 <- surf_build2 %>% 
  # drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_class == "SMCA")) %>% 
  pivot_wider(names_from = "mpa_defacto_designation", 
              values_from = c(biomass, n_rep, sd, se)) %>% 
  mutate(habitat = "Surf zone")

# Kelp Forest --------------------------------------------------------------------------------------------

# identify distinct individual transects 
kelp_effort_transect <- kelp_raw %>% 
  distinct(year, month, day, site, # need this to get actual individual transects - updated by CL
           affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           zone, level, transect) %>% 
  arrange(year, month, day, site, affiliated_mpa, mpa_defacto_designation, zone, level, transect) # Could then group to calculate transects; not needed yet

# calculate effort as n transects per MPA year (individually for smr and ref)
kelp_effort_mpa <- kelp_effort_transect %>% 
  group_by(year, affiliated_mpa, mpa_defacto_class,  mpa_defacto_designation) %>% 
  summarize(n_rep = n()) # for comparison later
                      
# calculate total biom for each rep unit (transect)
kelp_build1 <- kelp_raw %>% # updated by CL - filter separately
  group_by(year, 
           month, day, site, # updated by CL
           affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, 
           zone, level, transect, target_status) %>%
  summarise(total_biomass = sum(weight_kg)) %>%
  # Widen to inspect and correct true zeroes
  pivot_wider(names_from = target_status, values_from = total_biomass) %>%
  # There are 127 transects where there are both species listed and an entry for "NO_ORG" (see google doc for notes/data)
  # Assume that the NO_ORG was entered incorrectly (for now) and remove these cases
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
            se = sd/sqrt(n_rep)) # %>% 
  #check if any sites do not match (this would mean no fish for an entire MPA or REF site!)
  #anti_join(kelp_effort_mpa, by = c("year", "affiliated_mpa","mpa_defacto_class", "mpa_defacto_designation"))

#Reshape
kelp_build3 <- kelp_build2 %>% 
  # drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_class == "smca")) %>% 
  pivot_wider(names_from = "mpa_defacto_designation", 
              values_from = c(biomass, n_rep, sd, se)) %>% 
  #drop sites that do not have pairs. We've already accounted for true zeros, so any missing data means there is not a pair
 # filter(!(is.na(n_rep_ref)|is.na(n_rep_smr))) %>% 
  mutate(habitat = "Kelp forest")


####NOTE
#THESE Sites have missing reference sites for ALL years, indicating there might
#be an issue with the pair

### I THINK I FIXED THIS 08/09.2023 -JS
## ARE YOU SURE? Sept 12 2023 - CL
## THESE FIVE WILL BE DROPPED FROM ANALYSES AS OF NOW
affiliated_mpa_with_all_na <- kelp_build2 %>%
  pivot_wider(names_from = "mpa_defacto_designation", 
              values_from = c(biomass, n_rep, sd, se)) %>% 
  group_by(affiliated_mpa) %>%
  summarise(all_na = all(is.na(biomass_ref))) %>%
  filter(all_na) %>%
  dplyr::select(affiliated_mpa)


# CCFRP ---------------------------------------------------------------------------------------
#calculate total biom for each rep unit
ccfrp_build1 <- rocky_reef_raw %>%
  #drop species without biomass estimate
  filter(!(is.na(total_biomass)))%>%
  #some cells were sampled more than once in a year. 
  #calculate mean between these
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           grid_cell_id, species_code)%>%
  dplyr::summarize(cell_bpue = mean(bpue)) %>%
  #since all CCFRP species are targeted, we can now calculate total cell biomass
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           grid_cell_id)%>%
  dplyr::summarize(total_biomass = sum(cell_bpue)) 


#REshape
ccfrp_targeted_RR <- ccfrp_build2 %>% 
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  mutate(habitat = "Shallow reef")


# NOTE: CL ALTERNATIVE BELOW - Treats cell-trip as the unit of replication

# Calculated biomass per unit effort (trip-cell)
ccfrp_build1b <- rocky_reef_raw %>% 
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

ccfrp_build2b <- ccfrp_build1b %>%    
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, target_status) %>% 
  summarize(biomass = mean(total_biomass_kg),
            n_rep = n(),
            sd = sd(total_biomass_kg, na.rm=TRUE),
            se = sd/sqrt(n_rep)) 

ccfrp_build3b <- ccfrp_build2b %>% 
  filter(!(mpa_defacto_class == "smca")) %>%  # drop smcas
  filter(!(target_status == "Nontargeted")) %>% # drop nontargeted (for now); many many zeroes
  filter(!(affiliated_mpa == "trinidad NA")) %>% # drop trinidad
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>% 
  mutate(habitat = "Shallow reef")



#------------------------DEEP REEF---------------------------------------------#

#calculate total biom for each rep unit
deep_reef_build1 <- deep_reef_raw %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           line_id, target_status)%>%
  dplyr::summarize(total_biomass = sum(total_biom_kg)) %>%
  #make wider to fill NAs with true zeros 
  pivot_wider(names_from = target_status, values_from = total_biomass)%>%
  #drop NA column
  dplyr::select(!("NA"))%>%
  #replace NAs with 0s, since these are true zeros
  replace_na(list(Targeted = 0, Nontargeted = 0)) %>%
  #make longer
  pivot_longer(cols = c(Targeted, Nontargeted), names_to = "target_status", values_to = "total_biomass")


#calculate MPA average and error
deep_reef_build2 <- deep_reef_build1 %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  summarize(biomass = mean(total_biomass, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass, na.rm=TRUE),
            se = sd/sqrt(n_rep))


#Reshape

deep_reef_targeted_RR <- deep_reef_build2 %>% filter(target_status == "Targeted") %>%
  mutate(mpa_defacto_designation = tolower(mpa_defacto_designation),
         mpa_defacto_class = tolower(mpa_defacto_class))%>%
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se))%>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))

deep_reef_nontargeted_RR <- deep_reef_build2 %>% filter(target_status == "Nontargeted") %>%
  mutate(mpa_defacto_designation = tolower(mpa_defacto_designation),
         mpa_defacto_class = tolower(mpa_defacto_class))%>%
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))

deep_reef_target_RR <- rbind(deep_reef_targeted_RR, deep_reef_nontargeted_RR) %>%
                        mutate(habitat ="Deep reef")

################################################################################
# join data

target_RR_full <- rbind(surf_target_RR, kelp_target_RR, ccfrp_targeted_RR, deep_reef_target_RR) %>%
                    dplyr::select(habitat, everything())


################################################################################
# calculate response ratio

target_status_RR <- target_RR_full %>%
                    #deal with the 0s by adding 10% of the mean for each group. 
                    group_by(year, habitat)%>% #Adding year really changes the shape of the histogram, so need to check on this. 
                    mutate(scalar_smr = mean(biomass_smr)*.10, #determine 10% of the mean biomass for each habitat inside MPAs
                           scalar_ref = mean(biomass_ref)*.10)%>% #determine 10% of the mean biomass for each habitat outside MPAs
                    ungroup()%>%
                    mutate(logRR = log((biomass_smr+scalar_smr) / (biomass_ref + scalar_ref))) #add appropriate scalar back to bioass estimate and calculate RR

hist(target_status_RR$logRR)
                    
#write.csv(row.names = FALSE, target_status_RR, file.path(datadir, "targeted_nontargeted_biomass_MPA_means.csv"))


