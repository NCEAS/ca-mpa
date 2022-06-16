

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
# (only iNaturalist is ready now)
inaturalist_orig <- readRDS(file.path(basedir, "inaturalist/processed", "2000_2020_inaturalist_data_inside_mpas_100m_buffer.Rds"))
permits_orig <- readRDS(file.path(basedir, "scientific_permits/processed", "CA_2012_2021_mpa_scientific_permits.Rds")) 
pop_orig <- readRDS(file.path(basedir, "census_data/processed", "MPA_population_within_50km.Rds")) 
  

# Build data
################################################################################


# Summarize iNaturalist performance
inaturalist <- inaturalist_orig %>% 
  # 2018
  filter(year_obs==2018 & !is.na(taxa_catg)) %>% 
  # Summarize
  group_by(mpa) %>% 
  summarize(nobservers=n_distinct(user_id),
            nobservations=n()) %>% 
  ungroup()

# Build data
data <- mpas_df %>% 
  select(region, name, name_short) %>% 
  rename(mpa_name=name, mpa_name_short=name_short) %>% 
  # Recode region
  mutate(region=recode_factor(region,
                              "NCCSR"="North Coast",   
                              "NCSR"="North Central Coast",   
                              "SFBSR"="North Central Coast",
                              "CCSR"="Central Coast",
                              "SCSR"="South Coast")) %>% 
  # Add iNaturalist data
  left_join(inaturalist, by=c("mpa_name"="mpa")) %>% 
  # Add othter
  mutate(pop_dens=NA,
         mpa_watch_rec=NA,
         mpa_watch_fish=NA,
         sandy_beach=NA,
         boats_sar=NA,
         fishing_comm=NA,
         fishing_rec=NA) %>% 
  # Gather
  gather(key="metric", value="value", 4:ncol(.)) %>% 
  # Recode metrics (potentially organize by theme later)
  mutate(metric=recode_factor(metric,
                              "nobservers"="iNat observers",
                              "nobservations"="iNat observations",
                              "pop_dens"="Population density",
                              "mpa_watch_rec"="MPA Watch (recreation)",
                              "mpa_watch_fish"="MPA Watch (fishing)",
                              "sandy_beach"="Beach visits",
                              "boats_sar"="Boat activity (SAR)",
                              "fishing_comm"="Commercial fishing",
                              "fishing_rec"="Recreational fishing")) %>% 
  # Scale metrics
  group_by(metric) %>% 
  mutate(value_scaled=value/max(value, na.rm=T)) %>% 
  ungroup()

# Derive MPA order
mpa_order <- data %>% 
  group_by(region, mpa_name) %>% 
  summarize(metric_avg=mean(value, na.rm=T)) %>% 
  ungroup() %>% 
  arrange(region, desc(metric_avg))

# Order data
data_ordered <- data %>% 
  mutate(mpa_name=factor(mpa_name, levels=mpa_order$mpa_name))
