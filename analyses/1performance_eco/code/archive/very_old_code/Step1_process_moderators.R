#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "5/30/2023"

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
surf_biomass <- read.csv(file.path(biomass_dat,"surf_zone_fish_biomass.csv"))
kelp_biomass <- read.csv(file.path(biomass_dat,"kelpforest_fish_biomass.csv"))
ccfrp_biomass <- read.csv(file.path(biomass_dat,"ccfrp_fish_biomass.csv"))
deep_reef_biomass <- read.csv(file.path(biomass_dat,"deep_reef_fish_biomass.csv"))

# Load MPA traits
#load habitat data
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")


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

#write.csv(biomass_dat, file.path(dat_path,"biomass_dat.csv"), row.names = FALSE)


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

#step 5 - join traits and mod

biomass_with_mods <- left_join(biomass_dat, mpa_traits, by="affiliated_mpa") %>%
  mutate(
    implementation_year = as.numeric(format(implementation_date,'%Y')),
    age_at_survey = year - implementation_year) 
#


################################################################################
#calculate diversity

surf_diversity <- surf_biomass %>%
  dplyr::mutate(habitat = "Surf zone")%>%
  filter(mpa_defacto_class == "SMR")%>%
  group_by(habitat, year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, haul_number) %>%
  mutate(proportion = count / sum(count)) %>%
  dplyr::summarize(Shannon = -sum(proportion * log(proportion)),
            species_richness = n_distinct(species_code),
            total_abundance = sum(count)) %>%
  group_by(habitat, year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation)  %>%
  dplyr::summarize(mean_Shannon = mean(Shannon),
            sd_Shannon = sd(Shannon),
            n_reps = 6, #all sites had 6 hauls per report
            mean_species_richness = mean(species_richness, na.rm=TRUE),
            total_abundance = mean(total_abundance, na.rm=TRUE)) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(mean_Shannon, sd_Shannon, mean_species_richness, total_abundance)) %>%
  mutate_all(~ replace_na(.,0)) #missing values are true zeros




kelp_diversity <- kelp_biomass %>%
  dplyr::mutate(habitat = "Kelp forest")%>%
  filter(mpa_defacto_class == "smr")%>%
  group_by(habitat, year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, zone, level, transect) %>%
  mutate(proportion = count / sum(count)) %>%
  dplyr::summarize(Shannon = -sum(proportion * log(proportion)),
                   species_richness = n_distinct(classcode),
                   total_abundance = sum(count)) %>%
  group_by(habitat, year, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation)  %>%
  dplyr::summarize(mean_Shannon = mean(Shannon),
                   sd_Shannon = sd(Shannon),
                   n_reps = n(), #all sites had 6 hauls per report
                   mean_species_richness = mean(species_richness, na.rm=TRUE),
                   total_abundance = mean(total_abundance, na.rm=TRUE)) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(mean_Shannon, sd_Shannon, mean_species_richness, total_abundance, n_reps)) 







