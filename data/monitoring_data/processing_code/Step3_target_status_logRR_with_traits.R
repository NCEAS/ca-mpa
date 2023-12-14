#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith", Cori Lopazanski
#date: "8/1/2023"

# About --------------------------------------------------------------------------------
# This script takes the biomass values calculated to the MPA year from Step 3, including 
# the mean targeted biomass, mean nontargeted biomass, n, sd, se, and log response ratio, and
# joins with the moderators used in subsequent analyses. 

# Main Input: target_status_biomass_MPA_means.csv

# Setup --------------------------------------------------------------------------------

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
# biomass_raw <- read.csv(file.path(biomass_dat,"targeted_nontargeted_biomass_MPA_means.csv"))
biomass_raw <- read_csv(file.path(biomass_dat, "target_status_biomass_MPA_means_updated.csv"))

# Load MPA traits
# load habitat data
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","data","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")
mpas_orig <- readRDS(file.path(data_path, "mpa_traits/processed", "CA_mpa_metadata.Rds")) %>% 
  dplyr::select(name = mpa, region) %>% 
  mutate(name = str_replace(name, " \\s*\\([^\\)]+\\)", "")) # fix name to match join

#load connectivity dat
settle_dat <- readRDS(file.path(data_path, "/connectivity/processed/settlement_by_mpa.Rds"))

# Join biomass data with moderators ---------------------------------------------------

# Join attributes with habitat and fishing information
mpa_traits <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name") %>% 
  left_join(., mpa_attributes_hab_div, by="name") %>% 
  left_join(., prop_rock, by="name") %>% 
  left_join(., fishing_effort, by="name") %>% 
  left_join(., mpas_orig) %>% 
  select(state_region = region, affiliated_mpa, implementation_date, size=size_km2.x,
         habitat_richness, habitat_diversity=habitat_diversity_sw, 
         prop_rock, fishing_pressure = annual_avg_lb_sqkm_20002006)
  

# Create habitat specific settlement for join
settlement_join <- settle_dat %>%
  mutate(shallow_settlement = settlement_kelp_forest_shallow_reef,
         kelp_settlement = settlement_kelp_forest_shallow_reef)%>% #kelp forest and shallow reef share settlement, so make unique colunns
  dplyr::select(-settlement_kelp_forest_shallow_reef)%>%
  pivot_longer(cols = c(settlement_rocky_intertidal, kelp_settlement, shallow_settlement, settlement_deep_reef),
               names_to = "habitat",
               values_to = "settlement_habitat") %>%
  #assign habitat var for join
  mutate(habitat = case_when(
    habitat == "settlement_rocky_intertidal" ~ "Surf zone", 
    habitat == "kelp_settlement" ~ "Kelp forest",
    habitat == "shallow_settlement" ~ "Shallow reef",
    habitat == "settlement_deep_reef" ~ "Deep reef",
    TRUE ~ as.factor(habitat)  ))


# Join traits biomass
biomass_with_mods <- left_join(biomass_raw, mpa_traits, by="affiliated_mpa") %>%
  mutate(implementation_year = as.numeric(format(implementation_date,'%Y')),
         age_at_survey = year - implementation_year,
         affiliated_mpa = factor(str_to_title(affiliated_mpa) %>% 
                                   str_replace(" Smr$", " SMR") %>% 
                                   str_replace(" Smca$", " SMCA"))) %>%
  dplyr::select(habitat, year, state_region, everything()) %>%
  #join settlement data
  left_join(settlement_join, by = c("habitat","affiliated_mpa"))  %>%
  #set up yi and vi for meta analytic language
  mutate(yi = logRR,
         vi = ((sd_mpa^2) / (n_rep_mpa*((biomass_mpa + scalar_mpa)^2))) + #the scalar comes from 10% of the mean we calculated back in Step2
           ((sd_ref^2) / (n_rep_ref*((biomass_ref + scalar_ref)^2)))) %>% #this is the within-study variance. See Eq. 2 in paper
  dplyr::select(-logRR) %>%
  #drop sites that do not have associate variance
  filter(!(is.na(vi) | vi == 0))

saveRDS(biomass_with_mods, file.path(dat_path, "biomass_with_moderators_new2.Rds"))
# last write 13 Dec 2023

#saveRDS(biomass_with_mods, file.path(dat_path, "biomass_with_moderators_new.Rds"))
# last write 26 oct 2023


# Old Version - Do Not Overwrite/Keeping for file tracking
#saveRDS(biomass_with_mods, file = file.path(dat_path,"biomass_with_moderators.Rds"))

