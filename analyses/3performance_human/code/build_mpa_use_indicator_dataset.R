

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

# Read indicators
pop_orig <- readRDS(file.path(basedir, "census_data/processed", "MPA_population_within_50km.Rds"))
watch_consump_orig <- readRDS(file.path(outputdir,  "mpa_watch_consumptive_indicators.Rds"))
watch_nonconsump_orig <- readRDS(file.path(outputdir,  "mpa_watch_nonconsumptive_indicators.Rds"))
inaturalist_orig <- readRDS(file.path(outputdir,  "inaturalist_indicators.Rds"))
reef_orig <- readRDS(file.path(outputdir, "reef_indicators.Rds"))
ebird_orig <- readRDS(file.path(outputdir, "ebird_indicators.Rds"))
permits_orig <- readRDS(file.path(outputdir, "scientific_permits_indicators.Rds")) 
citations_orig <- readRDS(file.path(outputdir, "citations_indicators.Rds"))


# Format indicators data
################################################################################

# Population data
pop <- pop_orig %>% 
  select(name, npeople_50km) %>% 
  rename(mpa=name)

# MPA Watch non-consumptive
head(watch_nonconsump_orig)
watch_nonconsump <- watch_nonconsump_orig %>% 
  select(mpa, psurveys, activity_hr) %>% 
  rename(nonconsump_psurveys=psurveys, 
         nonconsump_hr=activity_hr)

# MPA Watch consumptive
head(watch_consump_orig)
watch_consump <- watch_consump_orig %>% 
  select(mpa, psurveys, activity_hr) %>% 
  rename(consump_psurveys=psurveys, 
         consump_hr=activity_hr)

# iNaturalist data
head(inaturalist_orig)
inat <- inaturalist_orig %>% 
  select(-nspecies) %>% 
  rename(inat_observers_n=nobservers,
         inat_observations_n=nobservations)

# eBird data
head(ebird_orig)
ebird <- ebird_orig %>%
  select(mpa, n_observers,  n_observations, n_surveys) %>% # n_species
  rename(ebird_observers_n=n_observers, 
         ebird_observations_n=n_observations,
         ebird_surveys_n=n_surveys)
  
# REEF data
head(reef_orig)
reef <- reef_orig %>% 
  select(mpa, nsurveys, nyrs) %>% 
  rename(reef_n=nsurveys, 
         reef_nyr=nyrs)

# Permit data
head(permits_orig)
permits <- permits_orig %>% 
  select(mpa, npermits, nyears) %>% 
  rename(permits_n=npermits, 
         permits_nyr=nyears)

# Citation data
head(citations_orig)
citations <- citations_orig %>% 
  select(mpa, ncitations, nyears) %>% 
  rename(citations_n=ncitations, 
         citations_nyr=nyears)



# Merge indicators
################################################################################

# Build data
data <- mpas_orig %>% 
  # Simplify
  select(mpa, mpa_short, authority, type, mlpa, region, area_sqkm, long_dd, lat_dd) %>% 
  # Add population
  left_join(pop, by="mpa") %>%
  # Add MPA watch non-consumptive
  left_join(watch_nonconsump, by="mpa") %>% 
  # Add MPA watch consumptive
  left_join(watch_consump, by="mpa") %>% 
  # Add iNaturalist
  left_join(inat, by="mpa") %>%
  # Add eBird
  left_join(ebird, by="mpa") %>% 
  # Add REEF
  left_join(reef, by="mpa") %>% 
  # Add permits
  left_join(permits, by="mpa") %>% 
  # Add citations
  left_join(citations, by="mpa")

# Inspect
freeR::complete(data)


# Export data
################################################################################

# Export data
saveRDS(data, file=file.path(outputdir, "CA_MPA_human_use_indicators.Rds"))

  
  
  
  
  
