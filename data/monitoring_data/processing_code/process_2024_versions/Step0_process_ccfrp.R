# Process CCFRP Data
# Cori Lopazanski
# October 2024

# About --------------------------------------------------------------------------------
# Goal: Separate processing code from biomass conversion code, and review all steps
# for errors and potential concerns that could propagate.

# Summary of key changes from original code:
# - Corrected NA values upon reading data (several just empty)
# - Keeping total_fishes_caught longer in the processing highlights that there
#   are a few values where there is length data but no species code listed - 
#   these were dropped or coded as NO_ORG drifts but there are actually fish
#   counted at higher levels

# Note potential concerns for next steps:
# - See above
# - The "total_caught_fishes" doesn't always add up (see end of script)
# - There is one Trip ID (SPR08291814 in 2018) that lists Grid Cell SP14 as 
#   both MPA and REF. The rest of the years, SP14 is only REF. Currrent approach 
#   corrects SP14 to REF. 

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)

# Directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_ccfrp/update_2024/MLPA_ccfrp_10.31.2024"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

# Read CCFRP monitoring and effort data ---------------------------------------------
# All caught fishes and associated drift_id
ccfrp_caught_fishes <- read_csv(file.path(datadir, "4-Caught Fishes.csv"), na = c("")) %>% 
  clean_names() %>% 
  dplyr::select(drift_id, species_code, length_cm)

# Site and effort for each drift (effort = angler hours)
ccfrp_drift <- read_csv(file.path(datadir, "3-Drift Information.csv")) %>% 
  clean_names() %>% 
  dplyr::select(drift_id, trip_id, id_cell_per_trip, grid_cell_id, site_mpa_ref,
         total_angler_hrs, total_fishes_caught, excluded_drift_comment, drift_time_hrs) %>% 
  # Update single entry where SP14 is listed as MPA (all other times listed as REF) and in 2020 BH07 was coded as a reference
  mutate(site_mpa_ref = if_else(grid_cell_id == "SP14", "REF", site_mpa_ref)) %>% 
  mutate(site_mpa_ref = if_else(grid_cell_id == "BH07", "REF", site_mpa_ref))
  
# Location and date of each trip
ccfrp_trip_info <- read_csv(file.path(datadir, "1-Trip Information.csv")) %>% 
  clean_names() %>% 
  dplyr::select(trip_id, area, year = year_automatic, month, day) 

# Full names and associated MPA for each trip
ccfrp_areas <- read_csv(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_ccfrp/CCFRP_database/CCFRP_database_2007-2020_csv/Monitoring_Areas.csv"), 
                        na = c("N/A")) %>% 
  clean_names() %>% 
  dplyr::select(area = area_code, name, mpa_designation) %>%
  mutate(mpa_designation = gsub("SMR/SMCA", "SMR", mpa_designation)) %>% 
  mutate(affiliated_mpa = paste(tolower(name), tolower(mpa_designation), sep = " "), 
         affiliated_mpa = recode(affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr"))

# This derived effort table will be used at the very end to compare the calculated
# effort values to those calculated by the team in the technical report (note: 
# they should be very close but may not be exact)
ccfrp_effort <- read.csv(file.path(datadir, "2007_2023_CCFRP_derived_effort_table.csv")) %>%
   clean_names()

# Read additional data ----------------------------------------------------------------

# Read taxonomy table 
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>% 
  #reassign target_status_standardized for downstream code
  dplyr::select(-target_status)%>%
  rename(target_status = target_status_standardized)%>%
  filter(habitat == "Rocky reef")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci,
                mpa_state_class = mpa_class) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))%>%
  mutate(mpa_state_class = ifelse(affiliated_mpa == "año nuevo smr","smr",mpa_state_class))

# Read de-facto SMRs
defacto_smr_ccfrp <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>% 
  filter(group == "ccfrp") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

# Read length-weight parameters
# params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv") %>%
#   mutate(ScientificName_accepted = recode(ScientificName_accepted, "Sebastes spp." = "Sebastes spp")) %>%
#   filter(!is.na(ScientificName_accepted))

# Process Data --------------------------------------------------------------------------------------------
# Note: the unit of replication for CCFRP is grid cell on a given trip
data <- ccfrp_caught_fishes %>% 
  # Combine catch information with drift information (full join to retain drifts w/ zero catch)
  full_join(ccfrp_drift, by = "drift_id") %>% 
  left_join(ccfrp_trip_info) %>% # Add trip information 
  left_join(ccfrp_areas) %>% # Add area information
  left_join(taxon_tab, by = c("species_code" = "habitat_specific_code")) %>% # Add taxa
  # Update species code for drifts with no catches
  mutate(species_code = ifelse(is.na(species_code) & total_fishes_caught == 0, "NO_ORG", species_code)) %>% 
  # Add defacto (all MPAs are defacto SMR for CCFRP)
  mutate(mpa_defacto_class = "smr",
         mpa_defacto_designation = case_when(site_mpa_ref == "MPA" ~ "smr",
                                             site_mpa_ref == "REF" ~ "ref")) %>% 
  # Correct swamis spelling
  mutate(affiliated_mpa = recode(affiliated_mpa, "swamis smca" = "swami's smca")) %>% 
  left_join(regions) %>% # Add regions
  dplyr::select(year, month, day, # temporal
         bioregion, region4, affiliated_mpa, mpa_state_class, mpa_defacto_class, mpa_defacto_designation, #  spatial
         drift_id, id_cell_per_trip, grid_cell_id, # sample
         total_angler_hrs, species_code, sciname, 
         class, order, family, 
         genus, species, tl_cm = length_cm, # data
         target_status, level, excluded_drift_comment, drift_time_hrs, total_fishes_caught) # extra


# Per instructions on DataONE, exclude certain drifts and cells
# See pages 2 and 3 for more info: 
# https://opc.dataone.org/metacat/d1/mn/v2/object/urn:uuid:8fdbb007-c386-4371-bbe9-ba328c0f0477 

# List of cells to exclude from DataOne instructions
excluded_cells = c("TDRR", "CMMM", "CMRR", "TMMM", "TMRR",
                   "FNMM","FNRR","SPMM", "SPRR", "BHMM", "BHRR",
                   "ANMM", "ANRR","PLMM","PLMN", "PLMO","PLRR",
                   "BLMM", "BLRR","PBMM", "PBRR",
                   "PCMM", "PCRR", "CPMM", "CPRR", "AIMM", "AIRR",
                   "LBMM", "LBRR", "SWMM", "SMRR", "LJMM", "LJRR")

data2 <- data %>% 
  filter(is.na(excluded_drift_comment)) %>% 
  dplyr::select(-excluded_drift_comment) %>% 
  filter(!(grid_cell_id %in% excluded_cells)) %>% #%>% 
  filter(drift_time_hrs > (2/60)) %>%  # Drop drifts less than 2 min
  # Fix drift where there are 2 fishes recorded but total_fishes_caught is zero?
  mutate(total_fishes_caught = if_else(drift_id %in% c("PCM1008181201"), 2, total_fishes_caught)) %>% 
  # Update unidentified to match other habitats
  mutate(species_code = case_when(species_code == "UNK" ~ "UNKNOWN", # change code to match
                                  is.na(species_code) & !is.na(tl_cm) ~ "UNKNOWN", # length data but no species
                                  drift_id == "BLR0802194001" ~ "UNKNOWN", # one entry with 1 fish but no species info
                                  TRUE ~ species_code))

# Calculate adjusted effort, following CCFRP effort calculation instructions on 
# DataOne. Effort is total angler hours at the unit of replication, which is an individual 
# grid cell on an individual trip (sampling day). Effort = total angler hours per grid cell 
# per trip, where a trip is a single sampling day. (CONFIRM THIS LANGUAGE WITH CCFRP). 
# Note: only keep trip-cells where the total angler hours is > 2.

# Calculate the total angler hours per cell per day
effort <- data2 %>% 
  dplyr::select(year, drift_id, id_cell_per_trip, grid_cell_id, total_angler_hrs) %>% distinct() %>% 
  group_by(id_cell_per_trip, grid_cell_id) %>% 
  summarize(total_angler_hrs_cell = sum(total_angler_hrs)) %>% ungroup() 
 
data3 <- data2 %>% 
  # Add the total angler hours per cell per day ('total_angler_hours_cell)
  left_join(effort, by = c("id_cell_per_trip", "grid_cell_id")) %>% 
  filter(total_angler_hrs_cell > 2) %>% # Retain trip-cell where total angler hours > 2 hours 
  # Add count variable to match other habitats
  mutate(count = if_else(species_code == "NO_ORG", 0, 1)) %>%  # works b/c there are no NAs in species_code 
  mutate(sl_cm = NA) # add to match other habitats


# Test to confirm all taxa in the data are already in the CCFRP taxon table
# - Only 2 in this dataframe should be UNKNOWN and NO_ORG
taxa_match <- data2 %>% 
  dplyr::select(species_code) %>% 
  distinct() %>% 
  filter(!(species_code %in% taxon_tab$habitat_specific_code)) 

desig_match <- data3 %>% # this should be zero after correcting SP14 and BH07 earlier on
  distinct(year, grid_cell_id, mpa_defacto_designation) %>% 
  group_by(grid_cell_id, mpa_defacto_designation) %>% 
  summarize(years = list(year)) %>% 
  pivot_wider(names_from = mpa_defacto_designation, values_from = years)%>%
  filter(map_lgl(smr, ~ !is.null(.)) & map_lgl(ref, ~ !is.null(.)))


# Write to csv ---------------------------------------------------------------------------------------
write.csv(data3, file.path(outdir, "ccfrp_processed.2024.csv"), row.names = F)
# last write 5 Dec 2024


# Explore potential remaining concerns ---------------------------------------------------------------
# Note: there are other drifts where the total_fishes_caught does not match
# the number of fishes recorded for the drift? 
total_fish_mismatch <- data2 %>% 
  filter(!(species_code == "NO_ORG")) %>% 
  group_by(drift_id) %>% 
  summarize(test_total = n(),
            total_fishes_caught = mean(total_fishes_caught)) %>% 
  ungroup() %>% 
  mutate(match = if_else(test_total == total_fishes_caught, "Yes", "No"),
         diff = abs(total_fishes_caught - test_total)) %>% 
  filter(match == "No")


# PI Recommend drop Trinidad (but likely better to do this later as there 
# may be other sites with no affiliated MPA; esp in other habitats) 

# CCFRP did their own length weight conversion which is included in ccfrp_effort
# that is already loaded. We are going to process their data and apply our own biomass
# conversion params to make sure the same params are applied for species shared 
# between habitats (kelp forest and deep reef). We can use the ccfrp_effort table
# to QAQC our processed data. cpue and no. caught fishes should match exactly. 
# bpue might be slightly different since we are using our own conversion params. 
# it would be worthwhile at some point to compare how well our estimates align with theirs. 
# -- We checked our effort calculations versus the effort table = those match (CL on 8 Sept 2023)
# -- Will perhaps still want to check the CPUE; see above comment about caught fishes
