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
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","data","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")
mpas_orig <- readRDS(file.path(data_path, "mpa_traits/processed", "CA_mpa_metadata.Rds")) %>% dplyr::select(mpa, region)

#load connectivity dat
settle_dat <- readRDS(file.path(data_path, "/connectivity/processed/settlement_by_mpa.Rds"))

################################################################################
#join biomass data with moderators

#step 1 - merge habitat gen and diversity
mpa_traits1 <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name")
mpa_traits2 <- left_join(mpa_traits1, mpa_attributes_hab_div, by="name")

#step 2 - merge habitat and proportion rock
mpa_traits3 <- left_join(mpa_traits2, prop_rock, by="name")

#step 3 - merge habitat and fishing effort
mpa_traits4 <- left_join(mpa_traits3, fishing_effort, by="name")

#step 3 - merge habitat and fishing effort
mpa_traits5 <- left_join(mpa_traits4, mpas_orig, by=c("name"="mpa"))

#step 4 - clean up
mpa_traits <- mpa_traits5 %>%
  #select variables of interest
  dplyr::select(state_region = region, affiliated_mpa, implementation_date, size=size_km2.x,
                habitat_richness, habitat_diversity=habitat_diversity_sw, 
                prop_rock, fishing_pressure = annual_avg_lb_sqkm_20002006
  )


# Step 5 - create habitat specific settlement for join
settlement_join <- settle_dat %>%
  mutate(shallow_settlement = settlement_kelp_forest_shallow_reef,
         kelp_settlement = settlement_kelp_forest_shallow_reef)%>% #kelp forest and shallow reef share settlement, so make unique colunns
  dplyr::select(-settlement_kelp_forest_shallow_reef)%>%
  pivot_longer(
    cols = c(settlement_rocky_intertidal, kelp_settlement, shallow_settlement, settlement_deep_reef),
    names_to = "habitat",
    values_to = "settlement_habitat"
  ) %>%
  #assign habitat var for join
  mutate(habitat = case_when(
    habitat == "settlement_rocky_intertidal" ~ "Surf zone", 
    habitat == "kelp_settlement" ~ "Kelp forest",
    habitat == "shallow_settlement" ~ "Shallow reef",
    habitat == "settlement_deep_reef" ~ "Deep reef",
    TRUE ~ as.factor(habitat)  
  ))


  

#step 6 - join traits biomass
biomass_with_mods <- left_join(biomass_raw, mpa_traits, by="affiliated_mpa") %>%
  mutate(
    implementation_year = as.numeric(format(implementation_date,'%Y')),
    age_at_survey = year - implementation_year,
    affiliated_mpa = factor(affiliated_mpa),
    state_region = factor(state_region),
    # Fix state region 
    state_region = case_when(
      affiliated_mpa == "campus point smca" ~ "South Coast",
      affiliated_mpa == "point vicente smca" ~ "South Coast",
      affiliated_mpa == "blue cavern onshore smca" ~"South Coast",
      TRUE ~ as.character(state_region)  
    )
  ) %>%
  dplyr::select(habitat, year, state_region, everything()) %>%
  #fix mpa name
  mutate(target_status = ifelse(habitat == "Shallow reef","Targeted",target_status),
         affiliated_mpa = str_to_title(affiliated_mpa) %>% 
           str_replace(" Smr$", " SMR") %>% 
           str_replace(" Smca$", " SMCA")) %>%
  #join settlement data
left_join(settlement_join, by = c("habitat","affiliated_mpa"))  %>%
  #set up yi and vi for meta analytic language
  mutate(yi = logRR,
         vi = ((sd_smr^2) / (n_rep_smr*((biomass_smr + scalar_smr)^2))) + #the scalar comes from 10% of the mean we calculated back in Step2
           ((sd_ref^2) / (n_rep_ref*((biomass_ref + scalar_ref)^2)))) %>% #this is the within-study variance. See Eq. 2 in paper
  dplyr::select(-logRR) %>%
  #drop sites that do not have associate variance
  filter(!(is.na(vi) | vi == 0))




saveRDS(biomass_with_mods, file = file.path(dat_path,"biomass_with_moderators.Rds"))






