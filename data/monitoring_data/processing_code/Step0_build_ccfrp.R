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
# TEST DRIFT PROCESSING
test_drift <- ccfrp_drift %>% 
  filter(is.na(excluded_drift_comment)) %>% select(-excluded_drift_comment) %>% 
  filter(!(grid_cell_id %in% excluded_cells)) %>% #%>% 
  filter(drift_time_hrs > (2/60))


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
         id_cell_per_trip, grid_cell_id, # sample
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
  
# PI Recommend drop Trinidad (but likely will happen with other sites later)
# Calculate the total angler hours per cell per day
effort2 <- data2 %>% 
  select(id_cell_per_trip, grid_cell_id, total_angler_hrs) %>% distinct()
  group_by(id_cell_per_trip, grid_cell_id) %>% 
  summarize(cell_hours = sum(total_angler_hrs)) %>% ungroup() %>%
  arrange(id_cell_per_trip, grid_cell_id)

test <- effort %>% ungroup() %>% 
  select(id_cell_per_trip, grid_cell_id, cell_hours) %>% 
  arrange(id_cell_per_trip, grid_cell_id)

  #calculate effort as the total angler hours per cell day
  effort <- ccfrp_build3 %>%
  dplyr::select(year, month, day, trip_id, drift_id, id_cell_per_trip, grid_cell_id, total_angler_hrs) %>% distinct() %>% #USE ID CELL PER TRIP
  mutate(year = as.factor(year),
         month = as.factor(month),
         day = as.factor(day))%>%
  #some cells were sampled more than once in a given year, so take the total amount of effort in a year for that cell
  group_by(year, month, day, id_cell_per_trip, grid_cell_id) %>%
  summarize(cell_hours = sum(total_angler_hrs)) 

test <- left_join(data2, effort) %>% 
  select(cell_hours, total_angler_hrs)
  
################################################################################
#Note: the unit of replication for CCFRP is cell
# CCFRP did their own length weight conversion which is included in ccfrp_effort
# that is already loaded. We are going to process their data and apply our own biomass
# conversion params to make sure the same params are applied for species shared 
# between habitats (kelp forest and deep reef). We can use the ccfrp_effort table
# to QAQC our processed data. cpue and no. caught fishes should match exactly. 
# bpue might be slightly different since we are using our own conversion params. 
# it would be worthwhile at some point to compare how well our estimates align with theirs. 

#step 1 -- select variables of interest

#this is what was caught
ccfrp_caught_fishes1 <- ccfrp_caught_fishes %>% 
  #drift_id is the common join field
  dplyr::select(drift_id, species_code, length_cm)

#this is where they were at and effort (angler hrs)
ccfrp_drift1 <- ccfrp_drift %>% 
  dplyr::select(drift_id, trip_id, id_cell_per_trip, grid_cell_id, site_mpa_ref,
                total_angler_hrs, total_fishes_caught ,excluded_drift_comment, drift_time_hrs)

#more location info
ccfrp_trip_info1 <- ccfrp_trip_info %>%
  dplyr::select(trip_id, area, year = year_automatic, month, day)

#more location info
ccfrp_areas1 <- ccfrp_areas %>% dplyr::select(area_code, name, mpa_designation)

#join everything
ccfrp_build0 <- merge(ccfrp_caught_fishes, ccfrp_drift, by="drift_id", all=TRUE)


## INSPECT: 
build0_test <- anti_join(ccfrp_build0, ccfrp_caught_fishes)

ccfrp_build1 <- merge(ccfrp_build0, ccfrp_trip_info, by="trip_id", all=TRUE) # cori trying build1 to build0
ccfrp_build2 <- left_join(ccfrp_build1, ccfrp_areas) %>% # cori trying build2 to build1
  dplyr::select(year, month, day, trip_id, drift_id, id_cell_per_trip, grid_cell_id, name, mpa_designation, site_mpa_ref, 
                total_angler_hrs, species_code,
                length_cm, excluded_drift_comment, drift_time_hrs) 

# INSPECT
build2_test <- ccfrp_build2 %>% 
  arrange(year, month, day, # temporal
          trip_id, drift_id, id_cell_per_trip, grid_cell_id, # sample info
          name, mpa_designation, site_mpa_ref, # mpa location
          total_angler_hrs, species_code, length_cm, # data
          excluded_drift_comment, drift_time_hrs) # extra

#step 2 -- select variables of interest, drop reps, and calculate effort

#per the instructions on DataONE, exclude these drifts below. 
#see pgs 2 and 3 for more info: https://opc.dataone.org/metacat/d1/mn/v2/object/urn:uuid:8fdbb007-c386-4371-bbe9-ba328c0f0477 
ccfrp_build3 <- ccfrp_build2 %>%
  #drop excluded drifts
  filter(excluded_drift_comment == "") %>% dplyr::select(!c(excluded_drift_comment))%>% 
  #drop excluded cells
  filter(!(grid_cell_id == "TDRR" |
             grid_cell_id == "CMMM" |
             grid_cell_id == "CMRR" |
             grid_cell_id == "TMMM" |
             grid_cell_id == "TMRR" |
             grid_cell_id =="FNMM" |
             grid_cell_id == "FNRR" |
             grid_cell_id == "SPMM" |
             grid_cell_id == "SPRR" |
             grid_cell_id == "BHMM" |
             grid_cell_id == "BHRR" |
             grid_cell_id == "ANMM" |
             grid_cell_id == "ANRR" |
             grid_cell_id == "PLMM" |
             grid_cell_id == "PLMN" |
             grid_cell_id == "PLMO" |
             grid_cell_id == "PLRR" |
             grid_cell_id == "BLMM" |
             grid_cell_id == "BLRR" | 
             grid_cell_id == "PBMM" |
             grid_cell_id == "PBRR" |
             grid_cell_id == "PCMM" |
             grid_cell_id == "PCRR" |
             grid_cell_id == "CPMM" |
             grid_cell_id == "CPRR" |
             grid_cell_id == "AIMM" |
             grid_cell_id =="AIRR" |
             grid_cell_id == "LBMM" |
             grid_cell_id == "LBRR" |
             grid_cell_id == "SWMM" |
             grid_cell_id == "SMRR" |
             grid_cell_id == "LJMM" |
             grid_cell_id == "LJRR")) %>%
  #drop drifts less than 2 min
  filter(drift_time_hrs > (2/60))

#calculate effort as the total angler hours per cell day
effort <- ccfrp_build3 %>%
  dplyr::select(year, month, day, trip_id, drift_id, id_cell_per_trip, grid_cell_id, total_angler_hrs) %>% distinct() %>% #USE ID CELL PER TRIP
  mutate(year = as.factor(year),
         month = as.factor(month),
         day = as.factor(day))%>%
  #some cells were sampled more than once in a given year, so take the total amount of effort in a year for that cell
  group_by(year, month, day, id_cell_per_trip, grid_cell_id) %>%
  summarize(cell_hours = sum(total_angler_hrs)) 

#filter taxon tab
ccfrp_taxa <- taxon_tab %>% filter(habitat =="Rocky reef")

#Join species ID
ccfrp_build4 <- left_join(ccfrp_build3, ccfrp_taxa, by=c("species_code" = "habitat_specific_code"),
                          na_matches="never") %>%
  #follow naming
  rename(fish_tl = length_cm) %>%
  #keep track of zeros for effort, replace with "NO_ORG"
  mutate(species_code = ifelse(is.na(species_code),"NO_ORG",species_code))


#step 3 -- calculate biomass at cell level for a given year
ccfrp_build5 <- convert_dat(params_tab, ccfrp_build4)
ccfrp_build6 <- bio_fun(ccfrp_build5) %>%
  dplyr::select(year, month, day, name, mpa_designation, site_mpa_ref, id_cell_per_trip, grid_cell_id,
                species_code, TL_cm, sciname,
                weight_g, target_status) 

#step 4 -- add regions

ccfrp_build7 <- ccfrp_build6 %>%
  mutate(mpa_defacto_class = "smr", #all MPAs are defacto SMR for CCFRP
         mpa_defacto_designation = tolower(site_mpa_ref),
         mpa_defacto_designation = recode(mpa_defacto_designation, "mpa" = "smr"),
         affiliated_mpa = paste(tolower(name),mpa_defacto_class),
         #follow naming
         affiliated_mpa = recode(affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr",
                                 "SE farallon islands smr" = "southeast farallon island smr",
                                 "swamis smr" = "swami's smca"
         )
  )%>%
  #drop per PI recommendation
  filter(!(name == "Trinidad"))

ccfrp_build8 <- left_join(ccfrp_build7, regions, by=c("affiliated_mpa"="name")) %>%
  dplyr::select(year, month, day, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
                id_cell_per_trip, grid_cell_id, species_code, sciname,
                TL_cm, weight_g, target_status) 


#step 5 -- calculate total species biomass per cell day
ccfrp_build9 <- ccfrp_build8 %>%
  mutate(weight_kg = weight_g/1000,
         year = as.character(year),
         month = as.character(month),
         day = as.character(day),
         grid_cell_id = trimws(as.character(grid_cell_id)),
         mpa_defacto_designation = as.character(mpa_defacto_designation)
  ) %>%
  group_by(year, month, day, bioregion, region4, affiliated_mpa, mpa_defacto_class,
           mpa_defacto_designation, id_cell_per_trip, grid_cell_id, species_code, sciname, target_status) %>%
  dplyr::summarize(total_biomass = sum(weight_kg))

#step 6 -- add effort
ccfrp_build10 <- left_join(ccfrp_build9, effort, by=c("year","month","day","id_cell_per_trip","grid_cell_id"), multiple = "all") %>%
  #drop these cells per PI ... apparently there was an issue. 
  filter(!(is.na(cell_hours)))%>%
  mutate(bpue = total_biomass / cell_hours)


#step 8 -- calculate cpue and then join with biomass at the cell level
cpue <- ccfrp_build8 %>% group_by(year, month, day, bioregion, region4, 
                                  affiliated_mpa, mpa_defacto_class,id_cell_per_trip, grid_cell_id,
                                  species_code)%>%
  dplyr::summarize(n_caught = n()) %>%
  #join with effort
  mutate(year = as.character(year),
         month = as.character(month),
         day = as.character(day))%>%
  left_join(effort, by=c("year","month","day","id_cell_per_trip","grid_cell_id"), multiple = "all") %>%
  mutate(cpue = n_caught / cell_hours) %>%
  ungroup()%>%
  dplyr::select(year, month, day, id_cell_per_trip, grid_cell_id, species_code, cpue, n_caught)


#step 9 -- join cpue with cpue
ccfrp_build11 <- ccfrp_build10 %>% left_join(cpue, by=c("year","month","day","id_cell_per_trip","grid_cell_id","species_code")) 

#check to see if any cells have no fish at all
id_trip_per_cell_with_only_NO_ORG <- ccfrp_build11 %>%
  group_by(id_cell_per_trip) %>%
  summarize(has_only_NO_ORG = all(species_code == "NO_ORG")) %>%
  filter(has_only_NO_ORG) %>%
  dplyr::select(id_cell_per_trip)


ccfrp_build12 <- ccfrp_build11 %>% filter(!(species_code == "NO_ORG"))



#write.csv(ccfrp_build12, row.names = F, file.path(outdir,"/biomass_processed/ccfrp_fish_biomass.csv"))         


