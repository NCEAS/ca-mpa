# Substrat sums QA/QC
# Julien -- brun@nceas.ucsb.edu


# Setup ------------------------------------------------------------------------
# Packages
library(tidyverse)


# Directories
data.dir <- file.path(getwd(), "analyses", "7habitat", "intermediate_data")


### Read raw data ----

# Read Attribute (Habitat) Data
att_raw <- readxl::read_excel(file.path(data.dir, "mpa-attributes-2022Oct17-raw.xlsx"),
                              sheet = 1, skip = 4, na = ".") %>% 
  janitor::clean_names()

names(att_raw)


# Read The ROMS data
roms_raw <- readxl::read_excel(file.path(data.dir, "ROMS_habitat_totals_CA_Baja_OR_221016_final.xlsx"),
                              sheet = 2, na = "ND") %>% 
  janitor::clean_names()

names(roms_raw)


### Compute the sums

# Attribute data
att_raw_sum <- att_raw %>% 
  select(mpa_name, rocky_reef_mapped_0_30m_km2, rocky_reef_predicted_0_30m_km2, max_kelp_canopy_cdfw_km2) %>%  #reduce the number of columns to ease visual check
  mutate(shallow_rock_total_att = rocky_reef_mapped_0_30m_km2 + rocky_reef_predicted_0_30m_km2 + max_kelp_canopy_cdfw_km2) # sum


# ROMS database
roms_sum <- roms_raw %>%
  select(cell, mpa, mapped_rock_0_30m_km2, predicted_rock_0_30m_km2, max_kelp_odfw_or_cdfw_km2, shallow_rock_total_mapped_predicted_0_30m_rock_max_kelp_no_doublecounting_km2) %>%  #reduce the number of columns to ease visual check
  filter(!is.na(mpa)) %>% # remoce non MPA cells (At least most of them)
  group_by(mpa) %>%
  summarise_all(.,sum) %>% # Merge multi cells MPA
  mutate(shallow_rock_total_roms = mapped_rock_0_30m_km2 + predicted_rock_0_30m_km2 + max_kelp_odfw_or_cdfw_km2) %>%  # compute sum
  mutate(diffsum_data_computed = shallow_rock_total_mapped_predicted_0_30m_rock_max_kelp_no_doublecounting_km2 - shallow_rock_total_roms) # diff provided in the data  -  what we just computed

