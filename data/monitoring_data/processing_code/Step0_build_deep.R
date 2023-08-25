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
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() 

taxon_deep <- taxon_tab %>% 
  filter(habitat == "Deep reef") %>% 
  # Update deep reef entry in taxon table to simplify matching later
  mutate(habitat_specific_spp_name = recode(habitat_specific_spp_name, "Sebastes" = "Sebastes spp"))

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))

# Read de-facto SMRs
defacto_smr_deep_reef <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>% 
  filter(group=="deep_reef") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr")) 

# Read length-weight parameters - currently not used in this script
# params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv") %>%
#   mutate(ScientificName_accepted = recode(ScientificName_accepted, "Sebastes spp." = "Sebastes spp")) %>%
#   filter(!is.na(ScientificName_accepted))


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
  mutate((across(location:designation, str_replace, 'Ano Nuevo','Año Nuevo'))) %>%
  mutate((across(location:designation, str_replace, 'Islands','Island'))) %>%
  mutate((across(location:designation, str_replace, 'SE ','Southeast '))) %>%
  mutate((across(location:designation, str_replace, 'Bodega Bay','Bodega Head'))) %>%
  mutate((across(location:designation, str_replace, 'Point St. George','Point St. George Reef Offshore'))) %>%
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
  select(year = survey_year, mpa_group:line, scientific_name, common_name, 
         count, fish_tl = estimated_length_cm, 
         affiliated_mpa, secondary_mpa, tertiary_mpa) %>% 
  # Convert affilaited mpa columns to lowercase 
  mutate((across(affiliated_mpa:tertiary_mpa, str_to_lower))) %>% 
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
  # Remove special characters from scientific names & trim whitespace
  mutate(scientific_name = str_trim(str_replace_all(scientific_name, "[^[:alnum:]]", " "))) %>% 
  # Manually correct misspelled scientific names
  mutate(scientific_name = recode(scientific_name,
                                  "Beringraja binoculata" = "Beringraja spp",                   
                                  "Beringraja rhina" = "Beringraja spp",                     
                                  "Chromis Punctipinnis" = "Chromis punctipinnis",                 
                                  "Hexagrammus decagrammus" =  "Hexagrammos decagrammus",            
                                  "Hydrolagus collei" = "Hydrolagus colliei",
                                  "Oxylebius rictus" = "Oxylebius pictus",
                                  "Sebastes aurora or diploproa" = "Sebastes spp",
                                  "Sebases serranoides" = "Sebastes serranoides",                     
                                  "Sebaste miniatus"  = "Sebastes miniatus",                     
                                  "Sebastes Caurinus" = "Sebastes caurinus",   
                                  "Sebastes dalii" = "Sebastes dallii",
                                  "Sebastes hopskini" = "Sebastes hopkinsi",                    
                                  "Sebastes letiginosus" = "Sebastes lentiginosus", 
                                  "Sebastes minatus"  = "Sebastes miniatus",                        
                                  "Sebastes paucipinis" = "Sebastes paucispinis",                   
                                  "Sebastes pauscipinis" = "Sebastes paucispinis",
                                  "Sebastes services" =  "Sebastes serriceps", 
                                  "Starry Rockfish" =  "Sebastes constellatus", 
                                  "Unidentified Sebastes sp" = "Sebastes spp",
                                  "Zaniolepis Spp" = "Zaniolepis spp",  
                                  "Racochilus vacca" = "Rhacochilus vacca",
                                  "rhacochilus vacca" = "Rhacochilus vacca",                 
                                  "sebastes maliger" = "Sebastes maliger",                      
                                  "sebastes miniatus" = "Sebastes miniatus",
                                  "Torpedo california" = "Torpedo californica",
                                  "Young of year   10 cm Sebastes sp" = "Sebastes spp"))
  
# Some species are in the deep reef dataset but not in the deep reef 
# taxonomy table. Will add their taxonomy information from the tables
# from other habitats or complete from fishbase (cannot only complete from
# fishbase because the summary "Genus spp" will not complete via fishbase)
taxa_match <- data %>% 
  # Add taxa information to see where it will be incomplete
  left_join(taxon_deep, by=c("scientific_name"="habitat_specific_spp_name")) %>% 
  select(scientific_name, habitat:level) %>% distinct() %>% 
  filter(!is.na(scientific_name))

taxa_add <- taxa_match %>% 
  filter(is.na(sciname)) %>% 
  select(scientific_name) %>%
  # Add taxon table from other habitats 
  left_join(taxon_tab, multiple = "any", by = c("scientific_name" = "sciname")) %>% 
  select(scientific_name, kingdom:target_status) 

taxa_complete_from_other_habitats <- taxa_add %>% 
  filter(!is.na(kingdom)) %>% 
  mutate(sciname = scientific_name)

# Complete any remaining entries from fishbase
taxa_complete_from_fishbase <- freeR::taxa(species=taxa_add$scientific_name[is.na(taxa_add$kingdom)]) %>% 
  mutate(scientific_name = sciname,
         kingdom = "Animalia",
         phylum = "Chordata") %>% 
  select(-type)

# Join into one main taxon table
taxa_complete <- taxa_match %>% 
  left_join(taxa_complete_from_other_habitats, na_matches = "never") %>% 
  left_join(taxa_complete_from_fishbase,  na_matches = "never")

# Add the completed taxon table to the main data
data2 <- data %>%
  # Add taxa information to data
  left_join(taxa_complete) %>% 
  select(year, mpa_group, type, designation, # keep these for now until confirm site treatment
         bioregion, region4, affiliated_mpa, secondary_mpa, tertiary_mpa,
         mpa_state_class, mpa_state_designation, mpa_defacto_class, mpa_defacto_designation,
         line_id, dive, line, scientific_name, count, fish_tl,
         class, order, family, sciname, target_status)

write.csv(data2, row.names = F, file.path(outdir,"/deep_reef_processed.csv"))  

