#Joshua G. Smith
#December 13, 2022

rm(list=ls())

#require packages
require(dplyr)
require(here)
require(MASS)
require(ggeffects)

#set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/statewide_data"
figdir <-  here::here("analyses", "5community_climate_ecology", "figures")


#load MPA traits
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")

#load model output
mod_out_raw <- read.csv(file.path(datadir,"mpa_betadisp_mod.csv"))

#connectivity



###############################################################################
#PART 1 ---- DISTANCE BASED RESPONSE RATIOS

resist_RR_dat <- mod_out_raw %>% filter(habitat =="Kelp forest fishes" |
                                   habitat=="Kelp forest inverts and algae",
                                   period_2 =="during") %>%
                          dplyr::select(habitat, MPA, MPA_type, distance) %>%
                          pivot_wider(names_from = MPA_type, values_from = distance) %>%
                          drop_na() %>%
                          mutate(logRR = log(smr/ref),
                                 prop_shift = (ref-smr)/ref,
                                 period = "resistance") 
                         

recover_RR_dat <- mod_out_raw %>% filter(habitat =="Kelp forest fishes" |
                                          habitat=="Kelp forest inverts and algae",
                                        period_2 =="after") %>%
  dplyr::select(habitat, MPA, MPA_type, distance) %>%
  pivot_wider(names_from = MPA_type, values_from = distance) %>%
  drop_na() %>%
  mutate(logRR = log(smr/ref),
         prop_shift = (ref-smr)/ref, 
         period = "recovery") 
        

RR_dat <- rbind(resist_RR_dat, recover_RR_dat)

#Join model output with MPA traits

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
  )%>%
  mutate(affiliated_mpa = recode(affiliated_mpa,
                                 "año nuevo smr" = "ano nuevo smr"))


#step 5 - join traits and mod

RR_dat_full <- left_join(RR_dat, mpa_traits, by=c("MPA"="affiliated_mpa")) %>%
  mutate(implementation_year = as.numeric(format(implementation_date,'%Y')))





###############################################################################
#build model for kelp forest fishes

kf_fish_resist <- RR_dat_full %>%
  filter(period=="resistance",
         habitat == 'Kelp forest fishes')

kf_fish_resil <- RR_dat_full %>%
  filter(period=="recovery",
         habitat == "Kelp forest fishes")

#resistance 
kf_fish_resist_mod <-glm(logRR ~ size + habitat_richness +
                                    habitat_diversity + prop_rock + fishing_pressure, data = kf_fish_resist, #add region or lat?
                                  family="gaussian", 
                                  na.action = na.exclude)
summary(kf_fish_resist_mod)

#resilience
kf_fish_resil_mod <- glm(logRR ~ size + habitat_richness +
                           habitat_diversity + prop_rock + fishing_pressure, data = kf_fish_resil, #add region or lat?
                         family="gaussian", 
                         na.action = na.exclude)

summary(kf_fish_resil_mod)



###############################################################################
#build model for kelp forest inverts and algae


###############################################################################
#build model for intertidal


###############################################################################
#create table of output


###############################################################################
#MODEL 2 - --- RAW DISTANCE REGRESSION 













