
# Setup --------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)

# Set directories 
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"
tabdir <- here::here("analyses","1performance_eco","tables")

# Load data
surf_biomass <- read.csv(file.path(datadir, "surf_zone_fish_biomass_updated.csv"))
kelp_biomass <- read.csv(file.path(datadir, "kelpforest_fish_biomass_updated.csv")) 
ccfrp_biomass <- read.csv(file.path(datadir, "ccfrp_fish_biomass_updated.csv"))
deep_biomass <- read.csv(file.path(datadir, "deep_reef_fish_biomass_updated.csv"))

################################################################################
#get all unique MPAs and types

surf_mpas <- surf_biomass %>% select(affiliated_mpa, bioregion, year, mpa_state_class, 
                                     mpa_defacto_class) %>% unique() %>%
  mutate(Ecosystem = "Surf zone")


kelp_mpas <- kelp_biomass %>% select(affiliated_mpa, bioregion, year, mpa_state_class, 
                                     mpa_defacto_class) %>% unique() %>%
  mutate(Ecosystem = "Kelp forest")

ccfrp_mpas <- ccfrp_biomass %>% select(affiliated_mpa, bioregion,year,  mpa_state_class, 
                                       mpa_defacto_class) %>% unique() %>%
  mutate(Ecosystem = "Shallow reef")

deep_mpas <- deep_biomass %>% select(affiliated_mpa, bioregion,year,  mpa_state_class, 
                                     mpa_defacto_class) %>% unique() %>%
  mutate(Ecosystem = "Deep reef")


#join
mpa_type <- rbind(surf_mpas, kelp_mpas, ccfrp_mpas, deep_mpas) %>%
  #organize
  select(Ecosystem, everything())%>%
  mutate(affiliated_mpa = str_to_title(affiliated_mpa),
         affiliated_mpa = str_replace(affiliated_mpa, "\\b(\\w+)$", function(x) toupper(x))) %>%
  drop_na() %>%
  mutate(mpa_state_class = toupper(mpa_state_class),
         mpa_defacto_class = toupper(mpa_defacto_class)) %>%
  select(Ecosystem, affiliated_mpa, year)


# Summarize the years into a single string for each affiliated_mpa and Ecosystem combination
mpa_type_summary <- mpa_type %>%
  group_by(affiliated_mpa, Ecosystem) %>%
  summarise(years = toString(unique(year)), .groups = 'drop')

# Pivot the data wider
mpa_type_wide <- mpa_type_summary %>%
  pivot_wider(names_from = Ecosystem, values_from = years)


write.csv(mpa_type_wide, file.path(tabdir, "TableS4_survey_effort.csv"),row.names = FALSE)





