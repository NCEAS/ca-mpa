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
deep_reef_raw <- read.csv(file.path(datadir, "deep_reef_fish_biomass.csv"))
shallow_reef <- read.csv(file.path(datadir, "ccfrp_fish_biomass.csv"))


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
surf_diversity_MPAs <- surf_diversity_build1 %>% 
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
#shallow reef

#calculate effort for each MPA
shallow_effort <- shallow_reef %>%
  #drop species with NA
  filter(!(is.na(sciname))) %>%
  #find number of cells for each MPA
  distinct(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation, grid_cell_id)%>%
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(n_cells = n())%>%
  ungroup()


#find total counts of each species per MPA
shallow_diversity_build1 <- shallow_reef %>%
  #drop anything not ID to species level
  filter(!(is.na(sciname))) %>%
  group_by(year, bioregion, region4, affiliated_mpa,
          # mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           species_code, sciname)%>%
  #summarize counts of individuals for each species 
  dplyr::summarize(count_of_individuals = sum(cpue)) %>% 
  #calculate MPA-level diversity
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(shannon_unweighted = diversity(count_of_individuals, index = "shannon"), #n MPAs should match with surf_effort
                   richness_unweighted = length(unique(species_code)))%>%
  #join effort
  left_join(shallow_effort, by=c("year", "bioregion", "region4", "affiliated_mpa",
                             # "mpa_state_class", "mpa_state_designation",
                              "mpa_defacto_class", "mpa_defacto_designation")) %>%
  #calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_cells,
         richness_weighted = richness_unweighted / n_cells)


#check if there were any MPAs where no species were observed
shallow_diversity_MPAs <- shallow_diversity_build1 %>% 
  group_by(year, bioregion, region4, affiliated_mpa,
          #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation) %>% distinct()

nrow(shallow_effort) - nrow(shallow_diversity_MPAs) #one MPA has no fish

no_fish_MPA <- anti_join(shallow_effort, shallow_diversity_MPAs)

shallow_diversity_build2 <- rbind(shallow_diversity_build1, no_fish_MPA) %>%
  #replace with true zeros
  mutate(
    shannon_unweighted = coalesce(shannon_unweighted, 0),
    richness_unweighted = coalesce(richness_unweighted, 0),
    shannon_weighted = coalesce(shannon_weighted, 0),
    richness_weighted = coalesce(richness_weighted, 0)
  )

#Reshape
shallow_RR <- shallow_diversity_build2 %>% 
  #drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_class == "SMCA"))%>%
  ungroup()%>%
  dplyr::select(!(c( mpa_defacto_class)))%>%
  ungroup()%>%
  group_by(year, bioregion, region4, affiliated_mpa)%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_cells" ,"shannon_weighted", "richness_weighted"))


shallow_H_R <-shallow_RR %>%
  rename("n_rep_ref" = n_cells_ref,
         "n_rep_smr" = n_cells_smr) %>%
  mutate(habitat = "Shallow reef")


################################################################################
#Deep reef

#calculate effort for each MPA
deep_effort <- deep_reef_raw %>%
  separate(sciname, into = c("genus", "species"), sep = " ")%>%
  #keep track of lines with no spp, but these get dropped below
  mutate(species_new = ifelse(species == "spp" | is.na(species),"NO_ORG",species))%>%
  #drop species with NA
  #filter(!(is.na(species))) %>%
  #find number of hauls for each MPA
  distinct(year, bioregion, region4, affiliated_mpa,
          # mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation, line_id)%>%
  group_by(year, bioregion, region4, affiliated_mpa,
          # mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(n_lines = n())%>%
  ungroup()


#find total counts of each species per MPA
deep_diversity_build1 <- deep_reef_raw %>%
  separate(sciname, into = c("genus", "species"), sep = " ")%>%
  #keep track of lines with no spp, but these get dropped below
  mutate(species_new = ifelse(species == "spp" | is.na(species),"NO_ORG",species))%>%
  #drop anything not ID to species level
  filter(!(species_new == "NO_ORG")) %>%
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation,
           habitat_specific_code, genus, species)%>%
  #summarize counts of individuals for each species 
  dplyr::summarize(count_of_individuals = sum(count)) %>% 
  #calculate MPA-level diversity
  group_by(year, bioregion, region4, affiliated_mpa,
           #mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(shannon_unweighted = diversity(count_of_individuals, index = "shannon"), #n MPAs should match with surf_effort
                   richness_unweighted = length(unique(habitat_specific_code)))%>%
  #join effort
  left_join(deep_effort, by=c("year", "bioregion", "region4", "affiliated_mpa",
                             # "mpa_state_class", "mpa_state_designation",
                              "mpa_defacto_class", "mpa_defacto_designation")) %>%
  #calculate weighted diversity and richness
  mutate(shannon_weighted = shannon_unweighted / n_lines,
         richness_weighted = richness_unweighted / n_lines)


#check if there were any MPAs where no species were observed
deep_diversity_MPAs <- deep_diversity_build1 %>% 
  group_by(year, bioregion, region4, affiliated_mpa,
          # mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation) %>% distinct()

nrow(deep_effort) - nrow(deep_diversity_MPAs) #one MPA has no fish

no_fish_MPA <- anti_join(deep_effort, deep_diversity_MPAs)

deep_diversity_build2 <- rbind(deep_diversity_build1, no_fish_MPA) %>%
  #replace with true zeros
  mutate(
    shannon_unweighted = coalesce(shannon_unweighted, 0),
    richness_unweighted = coalesce(richness_unweighted, 0),
    shannon_weighted = coalesce(shannon_weighted, 0),
    richness_weighted = coalesce(richness_weighted, 0)
  )

#yay!

#Reshape
deep_RR <- deep_diversity_build2 %>% 
  #drop smcas. We only want to include no-take SMRs in our analysis
  filter(!(mpa_defacto_class == "SMCA"))%>%
  ungroup()%>%
  dplyr::select(!(c(mpa_defacto_class)))%>%
  ungroup()%>%
  group_by(year, bioregion, region4, affiliated_mpa)%>%
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c("shannon_unweighted", "richness_unweighted", "n_lines" ,"shannon_weighted", "richness_weighted"))%>%
  janitor::clean_names()


deep_H_R <-deep_RR %>%
  rename("n_rep_ref" = n_lines_ref,
         "n_rep_smr" = n_lines_smr) %>%
  mutate(habitat = "Deep reef")%>%
  #some years have missing pairs, so these get dropped
  filter(!(is.na(n_rep_ref) | is.na(n_rep_smr)))%>%
  filter(!(is.na(year)))


################################################################################
# join data

richness_diversity_full <- rbind(surf_H_R, kelp_H_R, shallow_H_R, deep_H_R) %>%
  dplyr::select(habitat, everything())


################################################################################
# calculate response ratio


# Step 1: Pivot longer
pivot_longer_result <- richness_diversity_full %>%
  pivot_longer(
    cols = starts_with(c("shannon_unweighted_ref", "shannon_weighted_smr", 
                         "richness_unweighted_ref", "richness_unweighted_smr", 
                         "shannon_weighted_ref", "shannon_weighted_smr", 
                         "richness_weighted_ref", "richness_weighted_smr")),
    names_to = "factor",
    values_to = "value"
  )

# Step 2: Calculate 10% of the mean and create 'scalar'
pivot_longer_result <- pivot_longer_result %>%
  group_by(year, habitat, factor) %>%
  mutate(scalar = 0.1 * mean(value, na.rm = TRUE))

# Step 3: Add 'scalar' to each factor level
pivot_longer_result <- pivot_longer_result %>%
  mutate(value = value + scalar) %>%
  dplyr::select(-scalar)

# Step 4: Pivot wider back to the original format
pivot_wider_result <- pivot_longer_result %>%
  pivot_wider(
    names_from = factor,
    values_from = value
  )

original_order <- c("habitat", "year", "bioregion", "region4", "affiliated_mpa")
pivot_wider_result <- pivot_wider_result %>%
  dplyr::select(original_order, everything())

# Step 5: calculate response ratio


richness_diversity_RR <- pivot_wider_result %>%
                          mutate(shannon_weighted_logRR  = log(shannon_weighted_smr/shannon_unweighted_ref),
                                 shannon_unweighted_logRR = log(shannon_unweighted_smr/shannon_unweighted_ref),
                                 richness_weighted_logRR = log(richness_weighted_smr/richness_weighted_ref),
                                 richness_unweighted_logRR = log(richness_unweighted_smr/richness_unweighted_ref))
  

write.csv(row.names = FALSE, richness_diversity_RR, file.path(datadir, "richness_diversity_MPA_means.csv"))









