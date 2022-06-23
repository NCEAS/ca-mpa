

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- "analyses/3performance_human/figures"

# Read MPA attributes
mpas_orig <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Read score card ingredients
pop_orig <- readRDS(file.path(basedir, "census_data/processed", "MPA_population_within_50km.Rds"))
watch_orig <- readRDS(file.path(basedir, "mpa_watch/processed", "MPA_Watch_2011_2022_surveys_ca_programs_wide.Rds"))
inaturalist_orig <- readRDS(file.path(basedir, "inaturalist/processed", "2000_2020_inaturalist_data_inside_mpas_100m_buffer.Rds"))
reef_orig <- readRDS(file.path(basedir, "reef/processed", "REEF_1994_2022_survey_metadata.Rds"))
permits_orig <- readRDS(file.path(basedir, "scientific_permits/processed", "CA_2012_2021_mpa_scientific_permits.Rds")) 

# Format data
################################################################################

# Population data
pop <- pop_orig %>% 
  select(name, npeople_50km) %>% 
  rename(mpa=name)

# iNaturalist data
inaturalist <- inaturalist_orig %>% 
  # 2018
  filter(year_obs==2018 & !is.na(taxa_catg)) %>% 
  # Summarize
  group_by(mpa) %>% 
  summarize(inat_observers_2018=n_distinct(user_id),
            inat_observations_2018=n()) %>% 
  ungroup()

# REEF data
reef <- reef_orig %>% 
  # Reduce to MPA sites
  filter(!is.na(mpa)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Total by MPA-year
  group_by(mpa, year) %>% 
  summarize(n=n()) %>% 
  ungroup() %>% 
  # Fill in missing MPA-years
  complete(fill=list(n=0)) %>%
  # Average annual
  group_by(mpa) %>% 
  summarize(reef_surveys_yr=mean(n)) %>% 
  ungroup()

# Permit data
# Total # of permits, 2012-2021
permits <- permits_orig %>% 
  group_by(mpa) %>% 
  summarize(npermits_tot=sum(npermits)) %>% 
  ungroup()

# Merge data
################################################################################

# Build data
data <- mpas_orig %>% 
  # Simplify
  select(mpa, mpa_short, authority, type, region, area_sqkm, long_dd, lat_dd) %>% 
  # Add population
  left_join(pop, by="mpa") %>% 
  # Add MPA watch
  # Add iNaturalist
  left_join(inaturalist, by="mpa") %>% 
  # Add REEF survey
  left_join(reef, by="mpa") %>% 
  # Add permits
  left_join(permits, by="mpa")

# Inspect
freeR::complete(data)

plot(nobservers ~ npeople_50km, data)  
  
  
  
  
  
