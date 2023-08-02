#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, janitor, stringr, vegan)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

# Load biomass data 
biomass_raw <- read.csv(file.path(biomass_dat,"targeted_nontargeted_biomass_MPA_means.csv"))

# Load MPA traits
#load habitat data
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")


################################################################################
#join biomass data with moderators

#step 1 - merge habitat gen and diversity
mpa_traits1 <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name")
mpa_traits2 <- left_join(mpa_traits1, mpa_attributes_hab_div, by="name")

#step 2 - merge habitat and proportion rock
mpa_traits3 <- left_join(mpa_traits2, prop_rock, by="name")

#step 3 - merge habitat and fishing effort
mpa_traits4 <- left_join(mpa_traits3, fishing_effort, by="name")

#step 4 - clean up
mpa_traits <- mpa_traits4 %>%
  #select variables of interest
  dplyr::select(affiliated_mpa, implementation_date, size=size_km2.x,
                habitat_richness, habitat_diversity=habitat_diversity_sw, 
                prop_rock, fishing_pressure = annual_avg_lb_sqkm_20002006
  )

#step 5 - join traits biomass
biomass_with_mods <- left_join(biomass_raw, mpa_traits, by="affiliated_mpa") %>%
  mutate(
    implementation_year = as.numeric(format(implementation_date,'%Y')),
    age_at_survey = year - implementation_year) 
#


saveRDS(biomass_with_mods, file = file.path(dat_path,"biomass_with_moderators.Rds"))






