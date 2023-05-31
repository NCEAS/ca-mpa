#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "5/30/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, janitor, stringr)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

# Load biomass data 
surf_biomass <- read.csv(file.path(biomass_dat,"surf_zone_fish_biomass.csv"))
kelp_biomass <- read.csv(file.path(biomass_dat,"kelpforest_fish_biomass.csv"))
ccfrp_biomass <- read.csv(file.path(biomass_dat,"ccfrp_fish_biomass.csv"))
deep_reef_biomass <- read.csv(file.path(biomass_dat,"deep_reef_fish_biomass.csv"))

################################################################################
#calculate site-level means for each year and associated error

#surf
surf_build1 <- surf_biomass %>% 
  #use only defacto smr and ref
  filter(mpa_defacto_class=='SMR') %>%
                          group_by(year, bioregion, region4, affiliated_mpa,
                          #mpa_state_class, mpa_state_designation,
                                         mpa_defacto_class, mpa_defacto_designation,
                                         target_status, haul_number)%>%
                          #determine total kg target and nontargeted per haul
                                dplyr::summarise(total_kg = sum(total_weight_kg,na.rm=TRUE))%>%
                          #drop unknown target status
                            filter(!(is.na(target_status)))%>%
                                #calculate mean
                          group_by(year, bioregion, region4, affiliated_mpa,
                          #mpa_state_class, mpa_state_designation,
                                mpa_defacto_class, mpa_defacto_designation,
                                target_status) %>%
                          dplyr::summarise(mean_kg = mean(total_kg, na.rm=TRUE),
                                           sd_kg = sd(total_kg),
                                           n = n(),
                                           se_kg = sd_kg / sqrt(n))%>%
                          pivot_wider(names_from = mpa_defacto_designation,
                                      values_from = c(mean_kg, sd_kg, n, se_kg)) %>%
                          #clean up
                          #per methods, there were always 6 tows per site, so replace missing 0s with true zeros and adjust effort. 
                          #file:///Users/jossmith/Downloads/MPA_BeachSurfZoneSites_Methods_dataone.pdf
                          mutate(n_ref = 6,
                                 n_smr = 6) %>%
                          mutate_all(~ replace_na(.,0)) %>%
  #clean up
  janitor::clean_names()  %>%
  mutate(across(everything(), str_to_sentence),
         habitat = "Surf zone")%>%
  dplyr::select(habitat, everything())


                                  
#kelp
kelp_build1 <- kelp_biomass %>% 
  #use only defacto smr and ref
  filter(mpa_defacto_class=='smr') %>%
  #drop 1999
  filter(!(year == 1999))%>%
  mutate(rep = paste(zone, level, transect))%>%
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           target_status, rep)%>%
  #determine total kg target and nontargeted per transect
  dplyr::summarise(total_kg = sum(total_biom_kg,na.rm=TRUE))%>%
  #drop unknown target status
  filter(!(is.na(target_status)))%>%
  #calculate mean per MPA across transects
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  dplyr::summarise(mean_kg = mean(total_kg, na.rm=TRUE),
                   sd_kg = sd(total_kg),
                   n = n(),
                   se_kg = sd_kg / sqrt(n))%>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(mean_kg, sd_kg, n, se_kg)) %>%
  #drop sites with missing data
  filter(!(is.na(mean_kg_ref) | is.na(mean_kg_smr))) %>%
  #clean up
  janitor::clean_names()  %>%
  mutate(across(everything(), str_to_sentence),
         habitat = "Kelp forest")%>%
  dplyr::select(habitat, everything())





#ccfrp 
ccfrp_build1 <- ccfrp_biomass %>% 
  #use only defacto smr and ref
  filter(mpa_defacto_class=='smr') %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           target_status, grid_cell_id)%>%
  #determine total kg target and nontargeted per transect
  dplyr::summarise(total_kg = sum(cell_bpue_kg,na.rm=TRUE))%>%
  #drop unknown target status
  filter(!(is.na(target_status)))%>%
  #calculate mean per MPA across transects
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  dplyr::summarise(mean_kg = mean(total_kg, na.rm=TRUE),
                   sd_kg = sd(total_kg),
                   n = n(),
                   se_kg = sd_kg / sqrt(n))%>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(mean_kg, sd_kg, n, se_kg)) %>%
  #drop sites with missing data
  filter(!(is.na(n_ref) | is.na(n_smr))) %>%
  #ccfrp is targeted only
  filter(target_status == "Targeted")%>%
  #clean up
  janitor::clean_names()  %>%
  mutate(across(everything(), str_to_sentence),
         habitat = "Shallow reef")%>%
  dplyr::select(habitat, everything())



#deep reef
#ccfrp 
deep_reef_build1 <- deep_reef_biomass %>% 
  #use only defacto smr and ref
  filter(mpa_defacto_class=='SMR') %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           target_status, line_id)%>%
  #determine total kg target and nontargeted per transect
  dplyr::summarise(total_kg = sum(total_biom_kg,na.rm=TRUE))%>%
  #drop unknown target status
  filter(!(is.na(target_status)))%>%
  #calculate mean per MPA across transects
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           target_status) %>%
  dplyr::summarise(mean_kg = mean(total_kg, na.rm=TRUE),
                   sd_kg = sd(total_kg),
                   n = n(),
                   se_kg = sd_kg / sqrt(n))%>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(mean_kg, sd_kg, n, se_kg)) %>%
  #drop sites with missing data
  filter(!(is.na(n_REF) | is.na(n_SMR))) %>%
  mutate_all(~replace_na(.,0)) %>%
  #clean up
  janitor::clean_names()  %>%
  mutate(across(everything(), str_to_sentence),
         habitat = "Deep reef")%>%
  dplyr::select(habitat, everything())


#merge

biomass_dat <- rbind(surf_build1, kelp_build1, ccfrp_build1, deep_reef_build1)

write.csv(biomass_dat, file.path(dat_path,"biomass_dat.csv"), row.names = FALSE)


################################################################################







