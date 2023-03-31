#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 22, 2023

rm(list=ls())

#load required packages
require(dplyr)
require(stringr)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"

surf_zone_raw <- read.csv(file.path(datadir, "surf_zone_fish_biomass.csv"))
kelp_raw <- read.csv(file.path(datadir, "kelpforest_fish_biomass.csv"))
rocky_reef_raw <- read.csv(file.path(datadir, "ccfrp_fish_biomass.csv"))
deep_reef_raw <- read.csv(file.path(datadir, "deep_reef_fish_biomass.csv"))

################################################################################
# calculate lnRRs for total biomass 

#-------------------SURF ZONE--------------------------------------------------#
#calculate total biom for each rep unit
surf_zone_build1 <- surf_zone_raw %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           haul_number)%>%
  dplyr::summarize(total_biomass = sum(total_weight_kg))

#calculate MPA average and error

surf_zone_build2 <- surf_zone_build1 %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation) %>%
  summarize(haul_avg = mean(total_biomass, na.rm=TRUE),
            n_hauls = n(),
            haul_sd = sd(total_biomass, na.rm=TRUE),
            haul_se = haul_sd/sqrt(n_hauls))

#calculate response ratio

surf_RR <- surf_zone_build2 %>% 
  #drop smcas
  filter(!(mpa_defacto_designation == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(haul_avg, n_hauls, haul_sd, haul_se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_hauls_ref)|is.na(n_hauls_smr)))%>%
  mutate(log_RR = log(haul_avg_smr/haul_avg_ref))%>%
  rename("biomass_ref"= haul_avg_ref,
         "biomass_smr" = haul_avg_smr,
         "n_rep_ref" = n_hauls_ref,
         "n_rep_smr" = n_hauls_smr,
         "sd_ref" = haul_sd_ref,
         "sd_smr" = haul_sd_smr,
         "se_ref" = haul_se_ref,
         "se_smr" = haul_se_smr) %>%
  mutate(habitat = "Surf zone")

#-------------------kelp forest------------------------------------------------#

#calculate total biom for each rep unit
kelp_build1 <- kelp_raw %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           zone, level, transect)%>%
  dplyr::summarize(total_biomass = sum(total_biom_kg))


#calculate MPA average and error

kelp_build2 <- kelp_build1 %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation) %>%
  summarize(biomass = mean(total_biomass, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass, na.rm=TRUE),
            se = sd/sqrt(n_rep))

#calculate response ratio

kelp_RR <- kelp_build2 %>% 
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))%>%
  mutate(log_RR = log(biomass_smr/biomass_ref)) %>%
  mutate(habitat = "Kelp forest")



#------------------------CCFRP------------------------------------------------#

#calculate total biom for each rep unit
ccfrp_build1 <- rocky_reef_raw %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           grid_cell_id)%>%
  dplyr::summarize(total_biomass = sum(cell_bpue_kg))


#calculate MPA average and error

ccfrp_build2 <- ccfrp_build1 %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation) %>%
  summarize(biomass = mean(total_biomass, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass, na.rm=TRUE),
            se = sd/sqrt(n_rep))


#calculate response ratio

ccfrp_RR <- ccfrp_build2 %>% 
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))%>%
  mutate(log_RR = log(biomass_smr/biomass_ref)) %>%
  mutate(habitat = "Rocky reef")


#------------------------DEEP REEF---------------------------------------------#

#calculate total biom for each rep unit
deep_reef_build1 <- deep_reef_raw %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           line_id)%>%
  dplyr::summarize(total_biomass = sum(total_biom_kg)) 


#calculate MPA average and error

deep_reef_build2 <- deep_reef_build1 %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation) %>%
  summarize(biomass = mean(total_biomass, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass, na.rm=TRUE),
            se = sd/sqrt(n_rep))

#calculate response ratio

deep_RR <- deep_reef_build2 %>% 
  mutate(mpa_defacto_designation = tolower(mpa_defacto_designation),
         mpa_defacto_class = tolower(mpa_defacto_class))%>%
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))%>%
  mutate(log_RR = log(biomass_smr/biomass_ref))%>%
  mutate(habitat ="Deep reef")


################################################################################
# join data

biomass_RR_full <- rbind(surf_RR, kelp_RR, ccfrp_RR, deep_RR) %>%
  dplyr::select(habitat, everything())

#write.csv(row.names = FALSE, target_RR_full, file.path(datadir, "biomass_lnRR_MPA_means"))

