
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "species_traits/processed")

# Read data
data_orig <- read.csv(file.path(datadir, "thermal_affinities_target_status (2).csv")) %>% 
freeR::complete(data_orig)

# Format data
################################################################################

# Species table
spp_df <- data_orig %>% 
  # Rename
  janitor::clean_names("snake") %>% 
  rename(pisco_code=pisco_classcode) %>% 
  # Simplify
  select(pisco_code, genus, species) %>% 
  # Unique
  unique() %>% 
  # Remove ones without genus
  filter(genus!="") %>% 
  # Sci name
  mutate(sci_name_orig=paste(genus, species)) %>% 
  select(pisco_code, genus, sci_name_orig) %>% 
  mutate(sci_name_orig=recode(sci_name_orig,
                              "Clupeiformes "="Clupeiformes spp",
                              "Mytilus "="Mytilus spp")) %>% 
  # Mark species or group
  mutate(level=ifelse(grepl("spp|/|,", sci_name_orig), "group", "species")) %>% 
  # Correct species names
  mutate(sci_name=recode(sci_name_orig,
                         # "Cribrinopsis albopunctata"=, 
                         # "Diaperoforma californica"=,   
                         # "Evasterias troschelii"=,        
                         # "Neoagarum fimbriatum"=,        
                         # "Neobernaya spadicea"=,          
                         # "Pseudobatos productus"=,        
                         # "Pugettia foliata"=,        
                         # "Stephanocystis osmundacea"=,    
                         # "Tetronarce californica"=
                         "Sarda chiliensis chiliensis"="Sarda chiliensis"))

# Inspect
freeR::which_duplicated(spp_df$pisco_code)

# Check names
names2check <- spp_df$sci_name[spp_df$level=="species"]
freeR::check_names(names2check)

# Retrieve length weight







