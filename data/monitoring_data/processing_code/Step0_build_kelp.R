# Process Kelp Forest Data
# Cori Lopazanski
# August 18, 2023

# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Goal: Separate kelp forest monitoring data processing code from the biomass 
# conversion, and review code for potential errors.

# Summary of key changes from original code:
# - Corrected NA values upon reading data
# - Added smca as the defacto designation for arrow point to lion head point as the
#   default option (no info provided for that mpa)
# - Removed CANOPY transects (per PI recommendation) - Central Coast stopped measuring
#   canopy ~ early 2010s therefore want to be consistent across programs
# - Combine MID and BOT for each transect (per PI recommendation following conversation
#   with APF at UCSB) - the bottom and midwater transects are conducted simultaneously
#   by stacked divers and therefore the KFM groups typically combine them to get 
#   density measures.

# Note potential concerns for next steps:
# - Some sites have no affiliated MPA (Yellowbanks, Valley, Trinidad) - these will be 
#   ultimately dropped 
# - No sciname for "UNID" and "BAITBALL" data (baitball are perciformes; 109 observations)
#   these will eventually be dropped from analyses
# - Counts with no length data (113 observations) will eventually be dropped from analyses
# - Matching taxa: how do we want to treat Clupeiformes spp? Right now this entry is
#   the only one that is identified to the Order level, so may make most sense to 
#   group it with other unspecified categories? (awaiting to confirm with JS)

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Packages
library(tidyverse)
library(janitor)

# Directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read kelp forest monitoring data (fishes)
kelp_forest_raw <- read.csv(file.path(datadir, "monitoring_kelp/MLPA_kelpforest_fish.4.csv"))

# Read kelp forest site table
kelp_sites_raw <- read.csv(file.path(datadir, "monitoring_kelp/MLPA_kelpforest_site_table.4.csv"), 
                           na.strings = c("N/A")) %>% clean_names()
  
# Read taxonomy lookup table & filter for kelp forest only
kelp_code <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  filter(habitat == "Kelp forest") %>% 
  rename(taxon_group = level) %>% clean_names()

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))

# Read de-facto SMRs
defacto_smr_kelp <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>%
  filter(group=="kelp") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class) %>% 
  mutate(mpa_defacto_class = tolower(mpa_defacto_class)) %>% 
  add_row(affiliated_mpa = "arrow point to lion head point smca", # NEW CL 
          mpa_defacto_class = "smca")

# Build ----
# Note: the unit of replication for kelp forest is transect, which now corresponds to 
# removing the fishes associated with canopy counts (level == CAN) and combines the 
# bottom (BOT) and midwater (MID) into a single transect.

# Process kelp forest sites 
kelp_sites <- kelp_sites_raw %>%
  distinct(site, ca_mpa_name_short, site_designation, site_status) %>% 
  mutate(
    affiliated_mpa = tolower(if_else(ca_mpa_name_short == "Swamis SMCA", "swami's smca", ca_mpa_name_short)),
    mpa_state_class = tolower(site_designation),
    mpa_state_designation = if_else(site_status == "reference", "ref", mpa_state_class)) %>%
  left_join(regions) %>% # Add regions
  left_join(defacto_smr_kelp) %>% # Add defacto designation
  mutate(mpa_defacto_designation = if_else(mpa_state_designation == "ref", "ref", mpa_defacto_class))


# Process monitoring data 
data <- kelp_forest_raw %>% 
  mutate(site = ifelse(site == "Swami's","SWAMIS",site)) %>% # Fix site name for join
  # Join sites 
  left_join(kelp_sites, by="site") %>% 
  # Join taxonomy 
  left_join(kelp_code, by = c("classcode"="habitat_specific_code")) %>% 
  # Modify columns to match other habitats
  mutate(sl_cm = NA,
         species_code = if_else(classcode == "UNID", "UNKNOWN", classcode)) %>% 
  select(year, month, day, # temporal
         bioregion, region4, affiliated_mpa, # spatial
         mpa_state_class, mpa_state_designation,
         mpa_defacto_class, mpa_defacto_designation, 
         site, zone, level, transect, # sample
         tl_cm = fish_tl, sl_cm, count, min_tl, max_tl,
         species_code,  sciname, 
         kingdom, phylum, class, order, family, 
         genus, species, target_status, taxon_group) %>% 
  # PI (Jenn Caselle) recommends drop 1999, "year of figuring things out" and 
  # there are some concerning entries where there are both NO_ORG and species recorded
  filter(!(year == 1999)) %>% 
  # PI recommends drop CANOPY counts because not consistently measured across sites/years
  filter(!(level == "CAN")) %>% 
  # PI recommends combine BOT and MID; group by everything except LEVEL: 
  group_by(year, month, day, # temporal
           bioregion, region4, affiliated_mpa, # spatial
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation, 
           site, zone, transect, # not grouping by LEVEL here
           tl_cm, sl_cm, count, min_tl, max_tl,
           species_code,  sciname, 
           kingdom, phylum, class, order, family, 
           genus, species, target_status, taxon_group) %>% 
  summarize(count = sum(count)) %>% ungroup() %>% 
  rename(level = taxon_group) 

# Test taxa match -- four are OK for now (NO ORG, UNID, BAITBALL, CLUP)
taxa_match <- data %>% 
  select(species_code, sciname:target_status) %>% distinct() %>% 
  filter(is.na(sciname)) 

# Write processed data
write.csv(data, file.path(outdir, "kelp_processed.csv"), row.names = F)
# Last write 16 October 2023

  