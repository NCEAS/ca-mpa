# Process Deep Reef Data
# Cori Lopazanski
# August 21, 2023

# About --------------------------------------------------------------------------------
# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Goal: Separate processing code from biomass conversion code, and review all steps
# for errors and potential concerns that could propagate.

# Summary of key changes from original code:
# - Corrected treatment of NA values 
# - Several species still weren't matching with taxonomy table - fixed
# - Removed step that added duplicates for multiple affiliated MPAs (until can confirm - see below)
# - Removed conversion of Point Buchon SMR to Point Buchon SMCA (until can confirm w/ JS)
# - BIG NOTE: Removed changes to target status for now (until confirm best place to make those adjustments)

# Note potential concerns for next steps:
# - There are several combinations of group-site-designation that aren't in the main
#   deep reef site table
# - How to treat "Farallon Island" site (for which MPA?)
# - Site-related concerns: mpa_name sometimes has multiple sites listed; this is almost
#   always when a reference site is affiliated with MPAs, but there are two combinations where
#   MPA sites are associated with multiple references. There also does not seem to be a consistent
#   order of how these multiple names are listed (e.g. South Point and Gull Island) which makes
#   it difficult to determine which could be the "primary" vs "secondary" affiliated MPA
# - There is a combination where type == Reference and designation == Reference. For now we
#   treat these as reference sites.
# - Want to make sure we are consistent to how we assign the affiliated MPA when the transect
#   is near a cluster ... e.g. Asilomar and Pacific Grove are listed together, but why not also
#   Lovers Point and Julia Platt? Another concern: how would sites near San Miguel be appropriate
#   references for Santa Cruz Island?
# 

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)

# Directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/ROV_Dataset"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read deep reef monitoring data ---------------------------------------------
deep_reef_raw <- read.csv(file.path(datadir, "/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), na = c("N/A", "", " ")) %>% 
  clean_names() 

# This is their site table from DataOne but it is incomplete given sites in the data
#deep_reef_sites <- readxl::read_excel(file.path(datadir, "/MidDepth_ROV_Site_Table.xlsx")) %>% clean_names()

# Read additional data ----------------------------------------------------------------
# Read taxonomy table 
taxon_deep <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>% 
  filter(habitat == "Deep reef")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))

# Read de-facto SMRs
defacto_smr_deep_reef <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>% 
  filter(group=="deep_reef") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr")) 

# Build Data ------------------------------------------------------------------------
# Secondary/Tertiary Naming Correction:
# Names are not always consistently listed in order -- create primary, secondary, tertiary as follows:
# - When there is only one mpa_name given, it always matches the mpa_group + type listed 
# - If no mpa_name is given, the affiliated_mpa is the mpa_group + type listed with no secondary
# - When multiple mpa_names are given,
#      * If the first mpa_name listed matches the mpa_group + type listed, the affiliated_mpa
#         remains the mpa_group + type listed, and the second mpa_name becomes the secondary_mpa 
#      * If the first mpa_name listed does not match the mpa_group + type listed, 
#         the affiliated_mpa remains the mpa_group + type listed, and the first mpa_name 
#         becomes the secondary_mpa 

data <- deep_reef_raw %>% 
  # Trim whitespace across all columns
  mutate(across(everything(), str_trim)) %>% 
  # Per PI instructions: Only keep transects affiliated with MPAs and that do not cross boundaries
  filter(type %in% c("SMR", "SMCA", "Reference")) %>% 
  # Drop the "_REF" from the names
  mutate(mpa_name = str_remove_all(mpa_name, "_REF"),
         mpa_name = str_replace_all(mpa_name, "_", " ")) %>% 
  # Correct naming across all columns
  mutate(across(location:designation, str_replace, 'Ano Nuevo','Año Nuevo')) %>%
  mutate(across(location:designation, str_replace, 'Islands','Island')) %>%
  mutate(across(location:designation, str_replace, 'SE ','Southeast ')) %>%
  mutate(across(location:designation, str_replace, 'Bodega Bay','Bodega Head')) %>%
  mutate(across(location:designation, str_replace, 'Point St. George','Point St. George Reef Offshore')) %>%
  # Correct Ano Nuevo to SMR (incorrectly listed as SMCA) 
  mutate(type = if_else(mpa_group == 'Año Nuevo', "SMR", type)) %>% 
  # Create affiliated_mpa variable 
  mutate(affiliated_mpa = 
           case_when(type %in% c("SMCA", "SMR") ~ paste(mpa_group, type, sep = " "),
                     # Provide the full affiliated MPA name for sites called "Reference"
                     type == "Reference" &
                       mpa_group %in% c("Campus Point", "Farallon Islands", "Pillar Point", 
                                        "Point St. George Reef Offshore", "Portuguese Ledge") ~ paste(mpa_group, "SMCA", sep = " "),
                     type == "Reference" & 
                       mpa_group %in% c("Año Nuevo", "Carrington Point", "Gull Island", "Harris Point", 
                                        "Point Buchon", "Point Conception", "Point Lobos", 
                                        "Sea Lion Gulch", "South Point", "Ten Mile") ~ paste(mpa_group, "SMR", sep = " "),
                     # These MPAs have both SMR and SMCA - will select SMR as the primary affiliated MPA and later list SMCA as secondary
                     type == "Reference" & 
                       mpa_group %in% c("Big Creek", "Bodega Head", "Point Sur") ~  paste(mpa_group, "SMR", sep = " "))) %>% 
  separate(mpa_name, into=c("primary_mpa", "secondary_mpa","tertiary_mpa"),sep = ", ", convert = TRUE) %>% 
  # Secondary/tertiary naming correction (described above)
  mutate(secondary_mpa = if_else(!(affiliated_mpa == primary_mpa), primary_mpa, secondary_mpa)) %>% 
  # These MPAs have both SMR and SMCA - select SMCA as secondary affiliated MPA
  mutate(secondary_mpa = if_else(type == "Reference" & mpa_group %in% c("Big Creek", "Bodega Bay", "Point Sur"),
                                 paste(mpa_group, "SMCA", sep = " "), secondary_mpa)) %>% 
  # Create date columns
  # Note: day not used b/c recorded in GMT and therefore sometimes crosses over to next day
  mutate(survey_date = mdy(case_when(dive == 320 ~ "8/19/2006", # Use date instead of survey_year b/c sometimes survey_year is NA
                                     dive == 332 ~ "9/1/2006",
                                     dive == 341 ~ "9/26/2006",
                                     TRUE~survey_date)),
         year = year(survey_date),
         month = month(survey_date)) %>%
  # Convert affiliated mpa columns to lowercase 
  mutate((across(c(affiliated_mpa, secondary_mpa, tertiary_mpa), str_to_lower))) %>% 
  # Add defacto SMRs based on affiliated_mpa
  left_join(defacto_smr_deep_reef, by = "affiliated_mpa") %>% 
  mutate(mpa_defacto_class = if_else(is.na(mpa_defacto_class) & type == "SMR", "SMR", mpa_defacto_class),
         mpa_defacto_designation = if_else(type == "Reference" | designation == "Reference", "REF", mpa_defacto_class)) %>% 
  # Add official mpa_state_class and mpa_state_designation to match other datasets 
  mutate(mpa_state_class = if_else(type == "Reference", str_to_upper(word(affiliated_mpa, -1)), type),
         mpa_state_designation = if_else(designation == "Reference", "REF", mpa_state_class)) %>% 
  # Fix
  mutate(mpa_defacto_designation = if_else(is.na(mpa_defacto_designation) & mpa_state_class == "SMR", "SMR", mpa_defacto_designation)) %>% 
  # Add regions
  left_join(regions, by = "affiliated_mpa") %>% 
  # Add taxa
  left_join(taxon_deep, by = c("scientific_name" = "habitat_specific_spp_name")) %>% 
  # Assign NO_ORG for empty transects
  mutate(species_code = case_when(is.na(scientific_name) & is.na(estimated_length_cm) ~ "NO_ORG", 
                                  !is.na(scientific_name) & is.na(sciname) ~ "UNKNOWN", 
                                  is.na(scientific_name) & is.na(sciname) & !is.na(estimated_length_cm) ~ "UNKNOWN", # length recorded but not identified
                                  T~NA)) %>% 
  mutate(sl_cm = NA) %>% # create to match other habitats
  select(year, month,
         mpa_group, type, designation, # keep these for now until confirm site treatment
         bioregion, region4, affiliated_mpa, secondary_mpa, tertiary_mpa,
         mpa_state_class, mpa_state_designation, mpa_defacto_class, mpa_defacto_designation,
         line_id, dive, line, scientific_name, count, tl_cm = estimated_length_cm, sl_cm,
         class, order, family, genus, species, 
         sciname, species_code, target_status, level)
# Note: Warning "Expected 3 Pieces" is OK (not every entry has secondary & tertiary mpa)

# Check for entries with missing sciname (6 ok)
taxa_na <- data %>% 
  filter(is.na(sciname)) %>% 
  distinct(scientific_name)

# Write to CSV
#write.csv(data, row.names = F, file.path(outdir,"/deep_reef_processed.csv"))  
# last write 7 Sept 2023

### EXPLORING ISSUES BELOW

# Test fixing dates
fix_dates <- data %>% 
  distinct(survey_year, survey_date, year, month, day, line_id, dive) %>% 
  group_by(dive) %>% 
  summarize(n_dates = length(unique(survey_date)),
            n_years = length(unique(year))) %>% 
  filter(n_dates > 1 | n_years > 1)

dives <- data %>% 
  distinct(year, month, dive, line_id)

test4 <- data %>% 
  mutate(transect_id = paste(year, line_id, sep = "_")) %>% 
  distinct(survey_year, year, month, line_id, dive, transect_id, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(multiple = if_else(transect_id %in% transect_id[duplicated(transect_id)], T, F)) %>% 
  filter(multiple)

