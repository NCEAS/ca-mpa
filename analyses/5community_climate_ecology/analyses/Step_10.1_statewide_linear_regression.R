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
conn_path <- "/home/shares/ca-mpa/data/sync-data/connectivity"
input_file <- "Settlement_connectivity_by_habitat.csv"  
settle_dat <- read.csv(file.path(conn_path, input_file))



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
        

RR_dat1 <- rbind(resist_RR_dat, recover_RR_dat)

rocky_join <- mod_out_raw %>% filter(habitat=="Rocky intertidal") %>%
              filter(MPA_type == "smr")%>%
                mutate(ref = NA,
                       logRR=NA,
                       prop_shift = NA)%>%
               dplyr::select(habitat, MPA, ref, smr = "distance", logRR,
                      prop_shift, period = "period_2") %>%
                mutate(period = recode(period, during = "resistance",
                                      after = "recovery"))
RR_dat  = rbind(RR_dat1, rocky_join)

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

RR_dat_join <- left_join(RR_dat, mpa_traits, by=c("MPA"="affiliated_mpa")) %>%
  mutate(implementation_year = as.numeric(format(implementation_date,'%Y')),
         habitat_short = ifelse(habitat =="Kelp forest fishes"|
                                  habitat =="Kelp forest inverts and algae","Kelp forest",habitat))


#step 6 - join settlement dat

settle_dat1 <- settle_dat %>% mutate(MPA = tolower(MPA)) %>%
                  dplyr::rename("Rocky intertidal" = "Rocky_Intertidal",
                                "Kelp forest" = `Shallow.Rocky.Reef..Kelp.and.Rock.`,
                                "Shallow rocky reef" = `Rock.30_100m`,
                                "Deep reef" = `Rock.100_200m`) %>%
                  pivot_longer(cols = c("Rocky intertidal","Kelp forest","Shallow rocky reef","Deep reef"),
                               values_to = "settlement", names_to = "habitat_short") %>%
                  mutate(MPA = recode(MPA, "campus point smca (no-take)" = "campus point smca",
                         "point vicente smca (no-take)" = "point vicente smca",
                         "blue cavern onshore smca (no-take)" = "blue cavern onshore smca"))

RR_dat_full <- left_join(RR_dat_join, settle_dat1, by=c("MPA","habitat_short"))


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
                                    habitat_diversity + prop_rock + fishing_pressure + settlement, data = kf_fish_resist, #add region or lat?
                                  family="gaussian", 
                                  na.action = na.exclude)
summary(kf_fish_resist_mod)

#resilience
kf_fish_resil_mod <- glm(smr ~ size + habitat_richness +
                           habitat_diversity + prop_rock + fishing_pressure + settlement, data = kf_fish_resil, #add region or lat?
                         family="gaussian", 
                         na.action = na.exclude) 

summary(kf_fish_resil_mod)

ggplot(RR_dat_full %>% filter(
                              #period == "recovery"
                              ) %>% mutate(period = factor(period, levels=c("resistance","recovery"))) ,aes(x = settlement, y=smr))+
    geom_point()+
    #geom_line()+
  stat_poly_line() +
  stat_poly_eq(use_label(c("P","R2")), label.x.npc="right") +
  facet_wrap(habitat~period, ncol=2, scales="free")+
  theme_classic()+
  labs(y="Distance (Bray-Curtis)")

lm <- lm(smr ~ settlement, data = RR_dat_full %>% filter(habitat=="Kelp forest inverts and algae",
                                                         period=="recovery"))
summary(lm)

###############################################################################
#build model for kelp forest inverts and algae


###############################################################################
#build model for intertidal


###############################################################################
#create table of output


###############################################################################
#MODEL 2 - --- RAW DISTANCE REGRESSION 













