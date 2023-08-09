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
# processing notes

#1. use NO_ORG to keep track of effort when no species were observed. 

#2. we can only use species-level data for diversity, so drop higher taxa. All habitats recorded 
#   fish to the spp level unless they were unsure of an ID. 

#3. Diversity and richness are calculated at the MPA, NOT the transect level, since
#   some groups used depth-stratified sampling, and we ultimately care about H' and richness
#   at the MPA level. However we still need to account for effort

#4. Since effort could vary by MPA (e.g., ome sites have more transects than others),
#   we will weight each diversity and richness estimate by the number of replicates 
#   at that location. 


################################################################################
# Surf zone

#calculate effort for each MPA
surf_effort <- surf_zone_raw %>%
  #keep track of hauls with no spp, but these get dropped below
  mutate(species = ifelse(species_code == "NOSP","NO_ORG",species))%>%
  #drop species with NA
  filter(!(is.na(species))) %>%
  #find number of hauls for each MPA
  distinct(year, bioregion, region4, affiliated_mpa,
          mpa_state_class, mpa_state_designation,
          mpa_defacto_class, mpa_defacto_designation, haul_number)%>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(n_hauls = n())%>%
  ungroup()


#find total counts of each species per MPA
surf_diversity_build1 <- surf_zone_raw %>%
  #drop anything not ID to species level
  filter(!(is.na(species))) %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           species_code, class, order, family, genus, species)%>%
  #summarize counts of individuals for each species 
  dplyr::summarize(count_of_individuals = sum(count)) %>% 
  #calculate MPA-level diversity
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(shannon_unweighted = diversity(count_of_individuals, index = "shannon"), #n MPAs should match with surf_effort
                   richness_unweighted = length(unique(species_code)))%>%
  #join effort
  left_join(surf_effort, by=c("year", "bioregion", "region4", "affiliated_mpa",
           "mpa_state_class", "mpa_state_designation",
           "mpa_defacto_class", "mpa_defacto_designation")) %>%
  #calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_hauls,
         richness_weighted = richness_unweighted / n_hauls)


#check if there were any MPAs where no species were observed
surf_diversity_MPAs <- surf_diversity %>% 
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation) %>% distinct()

nrow(surf_effort) - nrow(surf_diversity_MPAs) #one MPA has no fish

no_fish_MPA <- anti_join(surf_effort, surf_diversity_MPAs)

surf_diversity_build2 <- rbind(surf_diversity_build1, no_fish_MPA) %>%
                          #replace with true zeros
                            mutate(
                              shannon_unweighted = coalesce(shannon_unweighted, 0),
                              richness_unweighted = coalesce(richness_unweighted, 0),
                              shannon_weighted = coalesce(shannon_weighted, 0),
                              richness_weighted = coalesce(richness_weighted, 0)
                            )

#yay!

#Reshape
surf_RR <- surf_diversity_build2 %>% 
  #drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_class == "SMCA"))%>%
  ungroup()%>%
  dplyr::select(!(c(mpa_state_class, mpa_state_designation, mpa_defacto_class)))%>%
  ungroup()%>%
  group_by(year, bioregion, region4, affiliated_mpa)%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_hauls" ,"shannon_weighted", "richness_weighted"))


surf_H_R <-surf_RR %>%
  rename("n_rep_ref" = n_hauls_ref,
         "n_rep_smr" = n_hauls_smr) %>%
  mutate(habitat = "Surf zone")



################################################################################
# Kelp forest

#calculate effort
kelp_effort <- kelp_raw %>%
  separate(sciname, into = c("genus", "species"), sep = " ")%>%
  #rename dummy var for any species to get dropped. Keep for now to calculate effort
  mutate(species = ifelse(species == "spp" | is.na(species),"NO_ORG",species))%>%
  #find number of transects for each MPA
  distinct(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation, zone, level, transect)%>%
  #determine n_transects for each MPA
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(n_transects = n())%>%
  ungroup()


#find total count of each species per MPA 
kelp_diversity <- kelp_raw %>%
  separate(sciname, into = c("genus", "species"), sep = " ")%>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           classcode, genus, species)%>%
  dplyr::summarize(count_of_individuals = sum(count)) %>% 
  #calculate MPA-level diversity
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(shannon_unweighted = diversity(count_of_individuals, index = "shannon"), #n MPAs should match with surf_effort
                   richness_unweighted = length(unique(classcode))) %>%
  #join effort
  left_join(kelp_effort, by=c("year", "bioregion", "region4", "affiliated_mpa",
                              "mpa_state_class", "mpa_state_designation",
                              "mpa_defacto_class", "mpa_defacto_designation"))%>%
  #calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_transects,
         richness_weighted = richness_unweighted / n_transects)

#check if there were any MPAs where no species were observed
kelp_diversity_MPAs <- kelp_diversity %>% 
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation) %>% distinct()

nrow(kelp_effort) - nrow(kelp_diversity_MPAs) #all MPAs had at least one fish. Yay!


#Reshape
kelp_RR <- kelp_diversity %>% 
  #drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_class == "smca"))%>%
  ungroup()%>%
  dplyr::select(!(c(mpa_state_class, mpa_state_designation, mpa_defacto_class)))%>%
  ungroup()%>%
  group_by(year, bioregion, region4, affiliated_mpa)%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_transects" ,"shannon_weighted", "richness_weighted"))

###What about NAs? Need to figure out effort


kelp_H_R <-kelp_RR %>%
  rename("n_rep_ref" = n_transects_ref,
         "n_rep_smr" = n_transects_smr) %>%
  mutate(habitat = "Kelp forest") %>%
  #some years have missing pairs, so these get dropped
  filter(!(is.na(n_rep_ref) | is.na(n_rep_smr)))


################################################################################
# For CCFRP, need to calculate CPUE and then use CPUE for diversity

#Everything below is copy over and needs to be complete 8/9/2023

#------------------------CCFRP------------------------------------------------#

#calculate total biom for each rep unit
ccfrp_effort <- rocky_reef_raw %>%
  #keep track of cells with no spp, but these get dropped below
  mutate(species = ifelse(species_code == "NOSP","NO_ORG",species))%>%
  #drop species with NA
  filter(!(is.na(species))) %>%
  #find number of hauls for each MPA
  distinct(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation, haul_number)%>%
  
  #excluding target_status for CCFRP, since they are all targeted 
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_defacto_class, mpa_defacto_designation,
           grid_cell_id)%>%
  dplyr::summarize(total_biomass = sum(cell_bpue_kg)) 

#calculate effort for each MPA
surf_effort <- surf_zone_raw %>%
  #keep track of hauls with no spp, but these get dropped below
  mutate(species = ifelse(species_code == "NOSP","NO_ORG",species))%>%
  #drop species with NA
  filter(!(is.na(species))) %>%
  #find number of hauls for each MPA
  distinct(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation, haul_number)%>%
  group_by(year, bioregion, region4, affiliated_mpa,
           mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(n_hauls = n())%>%
  ungroup()

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


