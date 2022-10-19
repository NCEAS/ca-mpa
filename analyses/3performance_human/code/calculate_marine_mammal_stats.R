


# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(countrycode)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "inaturalist/processed")

# Read data
data_orig <- readRDS(file.path(datadir, "2000_2021_inaturalist_data_inside_mpas_100m_buffer.Rds"))


# Build data
################################################################################

# Mammal key
mammal_key <- data_orig %>% 
  # Reduce to mammals
  filter(taxa_catg=="Mammalia") %>% 
  select(comm_name, sci_name) %>% 
  unique() %>% 
  # Mark marine mammals
  mutate(marine_mammal_yn=grepl("dolphin|otter|porpoise|seal|whale|sea lion|orca|pinniped|cetaceans", tolower(comm_name)))

# Stats
stats <- data_orig %>% 
  # Mark marine mammals
  left_join(mammal_key) %>% 
  group_by(mpa) %>% 
  summarise(n=n(),
            n_mammals=sum(marine_mammal_yn==T & captive_cultivated=="false", na.rm=T),
            p_mammals=n_mammals/n) %>% 
  ungroup()









