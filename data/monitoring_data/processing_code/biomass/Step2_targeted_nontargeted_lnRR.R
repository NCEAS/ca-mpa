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
# Some site pairs do not appear in the data because they were either not sampled, 
#or because no species were observed. True zeros should be included, so we need 
#to use the site tables to identify where true zeros might exist. 

#

sites <- readRDS("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sites_clean.Rds")


################################################################################
# calculate lnRRs for targeted vs nontargeted

#-------------------SURF ZONE--------------------------------------------------#
#calculate total biom for each rep unit
surf_zone_build1 <- surf_zone_raw %>%
                      group_by(year, bioregion, region4, affiliated_mpa,
                               mpa_state_class, mpa_state_designation,
                               mpa_defacto_class, mpa_defacto_designation,
                               haul_number, target_status)%>%
                      dplyr::summarize(total_biomass = sum(total_weight_kg)) %>%
                  #make wider to fill NAs with true zeros 
                  pivot_wider(names_from = target_status, values_from = total_biomass) %>%
                  #drop NA column
                  dplyr::select(!("NA")) %>%
                  #replace NAs with 0s, since these are true zeros
                  replace_na(list(Targeted = 0, Nontargeted = 0)) %>%
                  #make longer
                  pivot_longer(cols = c(Targeted, Nontargeted), names_to = "target_status", values_to = "total_biomass")
              

#calculate MPA average and error
surf_zone_build2 <- surf_zone_build1 %>%
                      #leave out haul number to take the average across hauls for a given MPA
                      group_by(year, bioregion, region4, affiliated_mpa,
                               mpa_defacto_class, mpa_defacto_designation,
                              target_status) %>%
                      summarize(haul_avg = mean(total_biomass, na.rm=TRUE),
                                n_hauls = n(),
                                haul_sd = sd(total_biomass, na.rm=TRUE),
                                haul_se = haul_sd/sqrt(n_hauls)) 

#Reshape
surf_targeted_RR <- surf_zone_build2 %>% filter(target_status == "Targeted") %>%
                      #drop smcas. We only want to include no-take SMRs in our analysis
                      filter(!(mpa_defacto_class == "SMCA"))%>%
                      pivot_wider(names_from = "mpa_defacto_designation",
                                  values_from = c(haul_avg, n_hauls, haul_sd, haul_se)) 

                      
surf_nontargeted_RR <- surf_zone_build2 %>% filter(target_status == "Nontargeted") %>%
  #drop smcas
  filter(!(mpa_defacto_class == "SMCA"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(haul_avg, n_hauls, haul_sd, haul_se))


surf_target_RR <- rbind(surf_targeted_RR, surf_nontargeted_RR) %>%
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
           zone, level, transect, target_status)%>%
  dplyr::summarize(total_biomass = sum(total_biom_kg)) %>%
#make wider to fill NAs with true zeros 
pivot_wider(names_from = target_status, values_from = total_biomass)%>%
  #replace NAs with 0s, since these are true zeros
  replace_na(list(Targeted = 0, Nontargeted = 0)) %>%
  #make longer
  pivot_longer(cols = c(Targeted, Nontargeted), names_to = "target_status", values_to = "total_biomass")


#calculate MPA average and error
kelp_build2 <- kelp_build1 %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  summarize(biomass = mean(total_biomass, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass, na.rm=TRUE),
            se = sd/sqrt(n_rep)) 


#Reshape

kelp_targeted_RR <- kelp_build2 %>% filter(target_status == "Targeted") %>%
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))


kelp_nontargeted_RR <- kelp_build2 %>% filter(target_status == "Nontargeted") %>%
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))

kelp_target_RR <- rbind(kelp_targeted_RR, kelp_nontargeted_RR) %>%
                        mutate(habitat = "Kelp forest")


#------------------------CCFRP------------------------------------------------#

#calculate total biom for each rep unit
ccfrp_build1 <- rocky_reef_raw %>%
  #excluding target_status for CCFRP, since they are all targeted 
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
            se = sd/sqrt(n_rep)) %>%
  mutate(target_status=NA)


#REshape

ccfrp_targeted_RR <- ccfrp_build2 %>% 
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  mutate(habitat = "Rocky reef")



#------------------------DEEP REEF---------------------------------------------#

#calculate total biom for each rep unit
deep_reef_build1 <- deep_reef_raw %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           line_id, target_status)%>%
  dplyr::summarize(total_biomass = sum(total_biom_kg)) %>%
  #make wider to fill NAs with true zeros 
  pivot_wider(names_from = target_status, values_from = total_biomass)%>%
  #drop NA column
  dplyr::select(!("NA"))%>%
  #replace NAs with 0s, since these are true zeros
  replace_na(list(Targeted = 0, Nontargeted = 0)) %>%
  #make longer
  pivot_longer(cols = c(Targeted, Nontargeted), names_to = "target_status", values_to = "total_biomass")


#calculate MPA average and error
deep_reef_build2 <- deep_reef_build1 %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  summarize(biomass = mean(total_biomass, na.rm=TRUE),
            n_rep = n(),
            sd = sd(total_biomass, na.rm=TRUE),
            se = sd/sqrt(n_rep))


#Reshape

deep_reef_targeted_RR <- deep_reef_build2 %>% filter(target_status == "Targeted") %>%
  mutate(mpa_defacto_designation = tolower(mpa_defacto_designation),
         mpa_defacto_class = tolower(mpa_defacto_class))%>%
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se))%>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))

deep_reef_nontargeted_RR <- deep_reef_build2 %>% filter(target_status == "Nontargeted") %>%
  mutate(mpa_defacto_designation = tolower(mpa_defacto_designation),
         mpa_defacto_class = tolower(mpa_defacto_class))%>%
  #drop smcas
  filter(!(mpa_defacto_class == "smca"))%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(biomass, n_rep, sd, se)) %>%
  #drop sites that do not have pairs
  filter(!(is.na(n_rep_ref)|is.na(n_rep_smr)))

deep_reef_target_RR <- rbind(deep_reef_targeted_RR, deep_reef_nontargeted_RR) %>%
                        mutate(habitat ="Deep reef")

################################################################################
# join data

target_RR_full <- rbind(surf_target_RR, kelp_target_RR, ccfrp_targeted_RR, deep_reef_target_RR) %>%
                    dplyr::select(habitat, everything())


################################################################################
# calculate response ratio

target_status_RR <- target_RR_full %>%
                    #deal with the 0s by adding 10% of the mean for each group. 
                    group_by(year, habitat)%>% #Adding year really changes the shape of the histogram, so need to check on this. 
                    mutate(scalar_smr = mean(biomass_smr)*.10, #determine 10% of the mean biomass for each habitat inside MPAs
                           scalar_ref = mean(biomass_ref)*.10)%>% #determine 10% of the mean biomass for each habitat outside MPAs
                    ungroup()%>%
                    mutate(logRR = log((biomass_smr+scalar_smr) / (biomass_ref + scalar_ref))) #add appropriate scalar back to bioass estimate and calculate RR

hist(target_status_RR$logRR)
                    
#write.csv(row.names = FALSE, target_status_RR, file.path(datadir, "targeted_nontargeted_biomass_MPA_means.csv"))


