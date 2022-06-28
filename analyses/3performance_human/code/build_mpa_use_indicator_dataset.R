

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
plotdir <- "analyses/3performance_human/figures"
outputdir <- "analyses/3performance_human/output"

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
# Total # of observers/observations, 2000-2018
range(inaturalist_orig$date_obs)
inaturalist <- inaturalist_orig %>% 
  filter(year_obs <= 2018) %>% 
  # Summarize
  group_by(mpa) %>% 
  summarize(inat_observers_tot=n_distinct(user_id),
            inat_observations_tot=n()) %>% 
  ungroup()

# REEF data
# Total number of surveys, 1993-2022
range(reef_orig$date)
reef <- reef_orig %>% 
  # Reduce to MPA sites
  filter(!is.na(mpa)) %>% 
  # Add year
  mutate(year=lubridate::year(date)) %>% 
  # Total by MPA
  group_by(mpa) %>% 
  summarise(reef_surveys_tot=n()) %>% 
  ungroup()
  # # Total by MPA-year
  # group_by(mpa) %>% 
  # summarize(n=n()) %>% 
  # ungroup() %>% 
  # # Fill in missing MPA-years
  # complete(fill=list(n=0)) %>%
  # # Average annual
  # group_by(mpa) %>% 
  # summarize(reef_surveys_yr=mean(n)) %>% 
  # ungroup()

# Permit data
# Total # of permits, 2012-2021
range(permits_orig$year)
permits <- permits_orig %>% 
  group_by(mpa) %>% 
  summarize(npermits_tot=sum(npermits)) %>% 
  ungroup()

# MPA Watch data
# Average annual activities/hour
watch <- watch_orig %>% 
  # Simplify
  select(survey_id, mpa, mpa_id, survey_type, date, time_start1, time_end1, duration_hr, 
         total_nonconsumptive_activities, total_consumptive_activities) %>% 
  # Reduce to valid surveys inside MPAs
  filter(duration_hr > 0 & survey_type=="MPA") %>% 
  # Reduce to surveys during years w/ similar coverage
  filter(date>="2015-01-01" & date<="2021-12-31") %>% 
  # Reduce to surveys occurring during the day
  filter(lubridate::hour(time_start1) >= 7 & lubridate::hour(time_start1) <= 19) %>% 
  # Calculate activities/hour
  mutate(consump_hr=total_consumptive_activities/duration_hr,
         nonconsump_hr=total_nonconsumptive_activities/duration_hr) %>% 
  # Summarize by MPA
  group_by(mpa) %>% 
  summarize(consump_hr=median(consump_hr),
            nonconsump_hr=median(nonconsump_hr)) %>% 
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
  left_join(watch, by="mpa") %>% 
  # Add iNaturalist
  left_join(inaturalist, by="mpa") %>% 
  # Add REEF survey
  left_join(reef, by="mpa") %>% 
  # Add permits
  left_join(permits, by="mpa") 

# Inspect
freeR::complete(data)

# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "CA_MPA_human_use_indicators.Rds"))

  
  
  
  
  
