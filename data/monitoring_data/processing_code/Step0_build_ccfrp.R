# Process CCFRP Data
# Cori Lopazanski
# August 18, 2023

# About --------------------------------------------------------------------------------
# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Goal: Separate processing code from biomass conversion code, and review all steps
# for errors and potential concerns that could propagate.

# Summary of key changes from original code:
# - 

# Note potential concerns for next steps:
# - 

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(readxl)

# Directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_ccfrp/"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read CCFRP monitoring and effort data ---------------------------------------------
# All caught fishes and associated drift_id
ccfrp_caught_fishes <- read_csv(file.path(datadir, "CCFRP_database/CCFRP_database_2007-2020_csv/4-Caught_Fishes.csv"), na = c("")) %>% 
  clean_names() %>% 
  select(drift_id, species_code, length_cm)

# Site and effort for each drift (effort = angler hours)
ccfrp_drift <- read_csv(file.path(datadir, "CCFRP_database/CCFRP_database_2007-2020_csv/3-Drift_Information.csv")) %>% 
  clean_names() %>% 
  select(drift_id, trip_id, id_cell_per_trip, grid_cell_id, site_mpa_ref,
         total_angler_hrs, total_fishes_caught ,excluded_drift_comment, drift_time_hrs)
  
# Location and date of each trip
ccfrp_trip_info <- read_csv(file.path(datadir, "CCFRP_database/CCFRP_database_2007-2020_csv/1-Trip_Information.csv")) %>% 
  clean_names() %>% 
  select(trip_id, area, year = year_automatic, month, day) 

# Full names and associated MPA for each trip
ccfrp_areas <- read_csv(file.path(datadir, "CCFRP_database/CCFRP_database_2007-2020_csv/Monitoring_Areas.csv"), 
                        na = c("N/A")) %>% 
  clean_names() %>% 
  select(area = area_code, name, mpa_designation) %>%
  mutate(mpa_designation = gsub("SMR/SMCA", "SMR", mpa_designation)) %>% 
  mutate(affiliated_mpa = paste(tolower(name), tolower(mpa_designation), sep = " "), 
         affiliated_mpa = recode(affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr"))

# THIS DERIVED EFFORT TABLE IS LOADED BUT NOT USED
# ccfrp_effort <- read.csv(file.path(datadir, "CCFRP_derived_data_tables_DataONE/CCFRP_derived_effort_table.csv")) %>%
#   clean_names()

# Read additional data ----------------------------------------------------------------

# Read taxonomy table 
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>% 
  filter(habitat == "Rocky reef")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))

# Read de-facto SMRs
defacto_smr_ccfrp <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>% 
  filter(group == "ccfrp") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

# Read length-weight parameters
params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv") %>%
  mutate(ScientificName_accepted = recode(ScientificName_accepted, "Sebastes spp." = "Sebastes spp")) %>%
  filter(!is.na(ScientificName_accepted))

# Process Data ------------------------------------------------------------------------
# Note: the unit of replication for CCFRP is grid cell
data <- ccfrp_caught_fishes %>% 
  # Combine catch information with drift information (full join to retain drifts w/ zero catch)
  full_join(ccfrp_drift, by = "drift_id") %>% 
  # Add trip information 
  left_join(ccfrp_trip_info) %>% 
  # Add area information
  left_join(ccfrp_areas) %>% 
  # Add taxa
  left_join(taxon_tab, by = c("species_code" = "habitat_specific_code")) %>% 
  # Update species code for drifts with no catches
  mutate(species_code = ifelse(is.na(species_code) & total_fishes_caught == 0, "NO_ORG", species_code)) %>% 
  # Add defacto (all MPAs are defacto SMR for CCFRP)
  mutate(mpa_defacto_class = "smr",
         mpa_defacto_designation = case_when(site_mpa_ref == "MPA" ~ "smr",
                                             site_mpa_ref == "REF" ~ "ref")) %>% 
  # Add regions
  left_join(regions) %>% 
  select(year, month, day, # temporal
         bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, #  spatial
         drift_id, id_cell_per_trip, grid_cell_id, # sample
         total_angler_hrs, species_code, sciname, TL_cm = length_cm, # data
         target_status, excluded_drift_comment, drift_time_hrs, total_fishes_caught) # extra


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
  filter(is.na(excluded_drift_comment)) %>% select(-excluded_drift_comment) %>% 
  filter(!(grid_cell_id %in% excluded_cells)) %>% #%>% 
  filter(drift_time_hrs > (2/60)) # Drop drifts less than 2 min
  
# PI Recommend drop Trinidad (but likely better to do this later as there are other sites
# with no affiliated MPA)

# Calculate the total angler hours per cell per day
effort <- data2 %>% 
  select(id_cell_per_trip, grid_cell_id, total_angler_hrs) %>% distinct() %>% 
  group_by(id_cell_per_trip, grid_cell_id) %>% 
  summarize(cell_hours = sum(total_angler_hrs)) %>% ungroup() %>%
  arrange(id_cell_per_trip, grid_cell_id)

test <- ccfrp_drift %>% 
  filter(drift_id %in% data2$drift_id) %>% 
  select(id_cell_per_trip, grid_cell_id, total_angler_hrs) %>% distinct() %>% 
  group_by(id_cell_per_trip, grid_cell_id) %>% 
  summarize(cell_hours = sum(total_angler_hrs)) %>% ungroup() %>%
  arrange(id_cell_per_trip, grid_cell_id)

################################################################################

# CCFRP did their own length weight conversion which is included in ccfrp_effort
# that is already loaded. We are going to process their data and apply our own biomass
# conversion params to make sure the same params are applied for species shared 
# between habitats (kelp forest and deep reef). We can use the ccfrp_effort table
# to QAQC our processed data. cpue and no. caught fishes should match exactly. 
# bpue might be slightly different since we are using our own conversion params. 
# it would be worthwhile at some point to compare how well our estimates align with theirs. 



