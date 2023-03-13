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

#load habitat data
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")


#View#load model output
mod_out <- read.csv(file.path(datadir,"mpa_betadisp_mod.csv"))



###############################################################################
#calcualte response ratio

mod_wide_during <- mod_out %>%
            filter(period_2=="during") %>%
            dplyr::select(habitat, MPA, MPA_type, distance)%>%
            pivot_wider(values_from="distance", names_from="MPA_type")%>%
            drop_na()%>%
            #calculate response ratio
            mutate(logRR = log10(smr/ref),
                   process = "Resistance")

mod_wide_after <- mod_out %>%
  filter(period_2=="after") %>%
  dplyr::select(habitat, MPA, MPA_type, distance)%>%
  pivot_wider(values_from="distance", names_from="MPA_type")%>%
  drop_na()%>%
  #calculate response ratio
  mutate(logRR = log10(smr/ref),
         process = "Resilience")


mod_dat <- rbind(mod_wide_during, mod_wide_after) 


###############################################################################
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

logit_dat <- left_join(mod_dat, mpa_traits, by=c("MPA"="affiliated_mpa")) %>%
  mutate(
         implementation_year = as.numeric(format(implementation_date,'%Y')))
#

#write.csv(logit_dat, file.path(datadir, "MPA_centroid_distances_with_traits.csv"), row.names = FALSE)


###############################################################################
#Build logistic regression

resist_all <- logit_dat %>%
  filter(process == "Resistance",
         habitat=="Kelp forest fishes")%>%
  filter(!(is.na(habitat_diversity)))

resist_mod_all <- lm(logRR ~ size + habitat_diversity + prop_rock + fishing_pressure +
                        habitat_richness,
                      data = resist_all, 
                       na.action = na.exclude)

summary(resist_mod_all)



resist_all <- logit_dat %>%
  filter(process == "Resilience",
         habitat == "Kelp forest fishes")%>%
  filter(!(is.na(habitat_diversity)))

resist_mod_all <- lm(logRR ~ size + habitat_diversity + prop_rock + fishing_pressure +
                       habitat_richness,
                     data = resist_all, 
                     na.action = na.exclude)

summary(resist_mod_all)










resist_all <- logit_dat %>%
  filter(process == "Resistance",
         habitat=="Kelp forest inverts and algae")%>%
  filter(!(is.na(habitat_diversity)))

resist_mod_all <- lm(logRR ~ size + habitat_diversity + prop_rock + fishing_pressure +
                       habitat_richness,
                     data = resist_all, 
                     na.action = na.exclude)

summary(resist_mod_all)



resist_all <- logit_dat %>%
  filter(process == "Resilience",
         habitat == "Kelp forest inverts and algae")%>%
  filter(!(is.na(habitat_diversity)))

resist_mod_all <- lm(logRR ~ size + habitat_diversity + prop_rock + fishing_pressure +
                       habitat_richness,
                     data = resist_all, 
                     na.action = na.exclude)

summary(resist_mod_all)







