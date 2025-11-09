# Summarize length weight parameters
# CL 29 Aug 2023

# This script reads the species key and length weight parameter data created in
# Step 1 and Step 2. The species key contains an identifier to match with every
# observation in the raw data (habitat_specific_spp_name) and the correct taxonomic
# details verified through fishbase. The length weight parameters are pulled directly 
# from Fishbase, filtered to include any Class, Genus, or Species in the species_key.

# We build a dataframe which contains the length-weight parameters from Fishbase 
# for each species in the species_key. We include the lw parameters at the species,
# genus, and class. When multiple lw parameters are provided, we take the average.
# We take the lowest classificaiton possible -- if there are no lw params for the species,
# we use the average lw parameters for the genus, and so forth.

# OUTPUT: fishbase_lw_parameters.csv
# LOCATION: "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

# The next script will select the appropriate parameter from either this list
# or the literature review. 

# Setup ------------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
basedir <- "/home/shares/ca-mpa/data/sync-data/" #Josh
datadir <- file.path(basedir, "species_traits/processed")

# Read length weight parameter data
lw_orig <- read.csv(file.path(datadir, "species_lw_parameters_from_fishbase_full_new.csv"), as.is=T) %>% 
  clean_names() %>% 
  rename(sciname = species)

# Read species key (cleaned taxonomy data for all habitats)
spp_orig <- read.csv(file.path(datadir, "species_key_2025.csv"), as.is=T) %>% 
  clean_names()

# Get taxa from fishbase and sealife base ------------------------------------------
sp_fb <- rfishbase::load_taxa("fishbase") %>% 
  mutate(database = "fishbase") %>% 
  select(database, SpecCode, sciname = Species, Genus, Family, Order, Class) 

sp_slb <- rfishbase::load_taxa("sealifebase") %>% 
  mutate(database = "sealifebase") %>% 
  select(database, SpecCode, sciname = Species, Genus, Family, Order, Class)

# Combine into one taxa list and remove previous
taxa <- full_join(sp_fb, sp_slb) %>% 
  setNames(tolower(colnames(.)))
  
rm(sp_fb, sp_slb)

# Get length-length data for conversion when needed --------------------------------
# NOTE: Fishbase's formula for calculating between weights is as follows:
#  Length 2 = a + b * Length 1
#  Because this is opposite of convention, we will rename explicitly as 
#  slope and intercept such that
#  Output Length = Intercept + Slope * Input Length

ll_fb <- rfishbase::length_length(server = "fishbase") %>% 
  mutate(database = "fishbase") %>% select(database, everything()) 

ll_slb <- rfishbase::length_length(server = "sealifebase") %>% 
  mutate(database = "sealifebase") %>% select(database, everything()) 

ll <- plyr::rbind.fill(ll_fb, ll_slb) %>% 
  select(database, speccode = SpecCode, input_length = Length1, output_length = Length2, intercept_ll = a, slope_ll = b) %>% 
  left_join(taxa, by = c("database", "speccode")) %>% 
  arrange(sciname)

rm(ll_fb, ll_slb)

ll_tl <- ll %>% 
  filter(input_length == "TL") %>% # only keep conversions from TL
  select(database, sciname, input_length, output_length, intercept_ll, slope_ll) %>% 
  group_by(database, sciname, input_length, output_length) %>% 
  # There are a few cases where there are multiple conversions for a single species and length-length type
  summarize(intercept_ll = median(intercept_ll, na.rm = T),
            slope_ll = median(slope_ll, na.rm = T)) %>% ungroup()

# Build species list -----------------------------------------------------------------
# Filter out fish identified at least to family level or lower
# (Remove unidentified groups, algae, inverts)
spp_fish <- spp_orig %>% 
  filter(!is.na(sciname)) %>% # sciname includes groupings up to family level; all else treated as unknown
  filter(phylum == "Chordata" | is.na(phylum)) # keep the is.na because phylum not entered for all species

# Build LW params ---------------------------------------------------------------

# Species summary
lw_spp <- lw_orig %>% 
  filter(sciname %in% spp_fish$sciname & !is.na(type)) %>% 
  group_by(sciname, type) %>% 
  summarize(a_spp=median(a, na.rm=T),
            b_spp=median(b, na.rm=T)) %>% 
  ungroup() %>% 
  group_by(sciname) %>% 
  mutate(has_tl = any(type == "TL")) %>% # Create identifier for whether species has TL parameters
  ungroup() %>% 
  filter(!has_tl | (has_tl & type == "TL")) %>% # Remove other types when species has TL
  select(-has_tl) %>% # Remove identifier
  left_join(ll_tl, by = c("sciname", "type" = "output_length")) %>% # Join LL conversions for non-TL parameters
  filter(type %in% c("TL", "WD", "FL") |!is.na(intercept_ll)) %>% 
  select(-database, -input_length) %>% rename(type_spp = type)

# Genus summary
ll_gen <- ll_tl %>% 
  left_join(taxa %>% select(sciname, genus)) %>% 
  filter(genus %in% spp_fish$genus) %>% 
  group_by(genus, output_length) %>% 
  summarize(intercept_ll_gen = median(intercept_ll, na.rm=T),
            slope_ll_gen = median(slope_ll, na.rm = T)) %>% ungroup()

lw_gen <- lw_orig %>%
  filter(genus %in% spp_fish$genus & !is.na(type)) %>% 
  group_by(genus, type) %>% 
  summarize(a_gen = median(a, na.rm = T),
            b_gen = median(b, na.rm = T)) %>% ungroup() %>% 
  group_by(genus) %>% 
  mutate(has_tl = any(type == "TL")) %>% ungroup() %>% 
  filter(!has_tl | (has_tl & type == "TL")) %>% 
  select(-has_tl) %>% 
  left_join(ll_gen, by = c("genus", "type" = "output_length")) %>% # Join LL conversions for non-TL parameters
  filter(type %in% c("TL", "WD", "FL") | !is.na(intercept_ll_gen)) %>% 
  rename(type_gen = type)
  
#write.csv(lw_gen, file=file.path(datadir, "fishbase_lw_parameters_by_genus.csv"), row.names = F)

# Family summary
lw_fam <- lw_orig %>% 
  filter(family %in% spp_fish$family & !is.na(type)) %>% 
  group_by(family, type) %>% 
  summarize(a_fam = median(a, na.rm = T),
            b_fam = median(b, na.rm = T)) %>% 
  ungroup() %>% 
  filter(type == "TL") %>%  # Note: all families have TL 
  rename(type_fam = type)

#write.csv(lw_fam, file=file.path(datadir, "fishbase_lw_parameters_by_family.csv"), row.names = F)

# Create final data
data <- spp_fish %>% 
  distinct(family, genus, sciname) %>% 
  left_join(lw_spp, by=c("sciname")) %>% # Add species lw
  left_join(lw_gen, by=c("genus")) %>% # Add genus lw
  left_join(lw_fam, by=c("family")) %>%  # Add family lw
  # Select lowest classification possible
  mutate(lw_source = case_when(!is.na(a_spp) ~ "species",
                               !is.na(a_gen) & is.na(a_spp) ~ "genus",
                               !is.na(a_fam) & is.na(a_spp) & is.na(a_gen) ~ "family",
                               T ~ "Unknown")) %>% 
  mutate(a = case_when(lw_source=="species" ~ a_spp,
                       lw_source=="genus" ~ a_gen,
                       lw_source=="family" ~ a_fam,
                       T ~ NA),
         b = case_when(lw_source == "species" ~ b_spp,
                       lw_source == "genus" ~ b_gen,
                       lw_source == "family" ~ b_fam,
                       T ~ NA),
         type = case_when(lw_source == "species" ~ type_spp,
                          lw_source == "genus" ~ type_gen,
                          lw_source == "family" ~ type_fam,
                          T ~ NA)) %>% 
  mutate(intercept_ll = case_when(lw_source == "species" & !(type == "TL") ~ intercept_ll,
                                  lw_source == "genus" & !(type == "TL") ~ intercept_ll_gen,
                                  T ~ NA),
         slope_ll = case_when(lw_source == "species" & !(type == "TL") ~ slope_ll,
                              lw_source == "genus" & !(type == "TL") ~ slope_ll_gen,
                              T ~ NA)) %>% 
  select(family, genus, sciname, lw_source, a, b, lw_type = type, slope_ll, intercept_ll)

# Export data
#write.csv(data, file=file.path(datadir, "fishbase_lw_parameters.csv"), row.names = F)
# last write Oct 19 2023

#write.csv(data, file=file.path(datadir, "fishbase_lw_parameters_2025.csv"), row.names = F)
# last write Nov 9 2025



