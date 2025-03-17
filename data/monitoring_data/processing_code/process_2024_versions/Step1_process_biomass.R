# Processing monitoring data for mpa-year level analyses
# Joshua G Smith; joshsmith@nceas.ucsb.edu; March 17, 2023

# About --------------------------------------------------------------------------------
# Updated by Cori Lopazanski August 2023

# This script uses the cleaned monitoring data (outputs from Step0) to
# calculate biomass from length/weight parameters (output from species_traits/Step4). 

# Important Notes:
# - Species w/o size data cannot be converted to biomass, but leave these for now since we 
#   want to track effort (true zeros).

# - We need to track true zeros. Standardize convention by replacing spp code with "NO_ORG" 
#   for all replicates where nothing was observed.

# - Don't drop any species at this stage, including unidentified/unknown species. We will 
#   do this in the next processing step.

# - For biomass conversion estimates, we elected to use parameters listed by the rocky reef
#   monitoring group, who conducted a literature review for dozens of species. Since the 
#   list is not comprehensive for species found in all other habitats, we added parameters
#   from fish base. The processing script for this is available here:
#   https://github.com/NCEAS/ca-mpa/tree/main/data/species_traits

# - Notes about key variables:
#     * mpa_class = whether the MPA is a SMR or SMCA (or something else)
#     * mpa_designation = whether the SITE was inside or outside
#     * habitat_specific_spp_name = used to join species_key to raw monitoring data (in Step0)
#     * sciname = corrected name for joining length-weight parameters

# - Size units in raw data should all be cm. Conversion parameters are all in cm and g.

# Updated by Cori Lopazanski July 2024
# Added the updated Kelp Forest Monitoring Data (from 2020-2024)
# Added the updated CCFRP Monitoring Data (from 2020-2024)
# No update to surf or deep reef.

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(stringr)

# Directories
traitsdir <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read monitoring data
surf  <- read_csv(file.path(datadir, "update_2024/surf_zone_processed.csv")) %>% clean_names()
deep  <- read_csv(file.path(datadir, "update_2024/deep_reef_processed.csv")) 
ccfrp <- read_csv(file.path(datadir, "update_2024/ccfrp_processed.2024.csv")) # updated CL 
kelp  <- read_csv(file.path(datadir, "update_2024/kelp_processed.6.csv"))  # updated CL

# Read length-weight parameters
params <- read_csv(file.path(traitsdir, "lw_parameters_fish.csv"))

# Read species key
species_key <- read.csv(file.path(traitsdir, "species_key.csv")) %>%
  clean_names() %>% 
  #reassign target_status_standardized for downstream code
  dplyr::select(-target_status)%>%
  rename(target_status = target_status_standardized)

# Biomass conversion function ----------------------------------------------------------------

bio_fun <- function(params, data) {
  
  data %>% 
    # Join data with parameters
    left_join(params, multiple = "all") %>% 
    
    # Compute from the cm inputs the type of length and units needed by the formula 
    mutate(length_to_use = 
             case_when(
               # Use given length (TL in cm)
               lw_type %in% c("TL", "WD") ~ tl_cm,
               lw_type == "FL" & is.na(slope_ll) ~ tl_cm,
               
               # For surf zone: use given SL when necessary & available
               lw_type == "SL" & !is.na(sl_cm) ~ sl_cm,
               
               # Convert from TL to SL using length-length conversion
               lw_type == "FL" & !is.na(slope_ll) ~ tl_cm*slope_ll+intercept_ll,
               lw_type == "SL" & is.na(sl_cm) ~ tl_cm*slope_ll+intercept_ll,
               
               # Data not available
               is.na(lw_type) ~ NA,
               TRUE~-9999),
           
           # Calculate weight in grams = a*L^b
           weight_g = a*length_to_use^b*count,
           
           # Correct NA to true zeros for NO_ORG observations
           weight_g = case_when(species_code == "NO_ORG" ~ 0,
                                T ~ weight_g),
           
           weight_kg = weight_g/1000
    )
}


# Calculate biomass  ----------------------------------------------------------------

ccfrp_biomass <- bio_fun(params, ccfrp)

deep_biomass <- bio_fun(params, deep)

kelp_biomass <- bio_fun(params, kelp)

surf_biomass <- bio_fun(params, surf)


# Drop pelagics species and large 'biomass buster' sharks ----------------------

#see selection criteria here: https://docs.google.com/spreadsheets/d/1SUalEkWFKEgB46bqCcSNLqCzBHGj6FHNfs1iFOiMLww/edit?gid=1028839899#gid=1028839899

ccfrp_biomass1 <- ccfrp_biomass %>%
  filter(!(sciname %in% c("Alopias vulpinus", 
                          "Aulorhynchus flavidus", 
                          "Carcharhinus obscurus", 
                          "Engraulis mordax", 
                          "Galeorhinus galeus", 
                          "Oncorhynchus tshawytscha", 
                          "Sardinops sagax"))) %>% 
  filter(!species_code == "BID") # drop bc only 3 fish, unclear if blue or something else



deep_biomass1 <- deep_biomass %>%
  filter(!(sciname %in% c("Entosphenus tridentatus", 
                          "Hexanchus griseus", 
                          "Mola mola",
                          "Apristurus brunneus"))) %>% 
  filter(!species_code == "RFYOY")


kelp_biomass1 <- kelp_biomass %>%
  filter(!(sciname %in% c("Alopias vulpinus", 
                          "Carcharhinus obscurus", 
                          "Decapterus macarellus", 
                          "Engraulis mordax", 
                          "Galeorhinus galeus", 
                          "Hexanchus griseus", 
                          "Mola mola", 
                          "Notorynchus cepedianus", 
                          "Prionace glauca", 
                          "Sarda chiliensis", 
                          "Sardinops sagax", 
                          "Scomber japonicus", 
                          "Thaleichthys pacificus")))

surf_biomass1 <- surf_biomass 

#surf_biomass1 <- surf_biomass %>%
#                 filter(!(sciname %in% c("Engraulis mordax", "Sardinops sagax")))
#sardines and anchovies are characteristic of the Central Coast,
#so we will retain them here. see: https://caseagrant.ucsd.edu/sites/default/files/importedFiles/FinalMPAReportBeachesSurfZonesJan2022.pdf


# Write to csv ----------------------------------------------------------------
write.csv(surf_biomass1, row.names = F,  file.path(datadir,"/update_2024/surf_zone_fish_biomass_updated.csv"))  #last write 7 Jan 2025
write.csv(kelp_biomass1, row.names = F,  file.path(datadir,"/update_2024/kelpforest_fish_biomass_updated.6.csv")) #last write 13 Mar 2025
write.csv(ccfrp_biomass1, row.names = F, file.path(datadir,"/update_2024/ccfrp_fish_biomass_updated.2024.csv")) #last write  2 Mar 2025
write.csv(deep_biomass1, row.names = F,  file.path(datadir,"/update_2024/deep_reef_fish_biomass_updated.csv")) #last write  3 Mar 2025

# IN PROGRESS: Explore everything that's going wrong -----------------------------------

# CCFRP
# Check cases where there is length and species info but no biomass calculation
# No problems so far!
test_ccfrp <- ccfrp_biomass %>% 
  filter(is.na(weight_g) & !is.na(tl_cm)) %>% 
  distinct(sciname, species_code, target_status) # now there are 9 in 2024 version


# KELP KELP
# See kelp forest processing code for outstanding concerns --
# Check cases where there is length and species info but no biomass calculation
# Added these species to list 
test_kelp <- kelp_biomass %>% 
  filter(is.na(weight_g) & ! is.na(tl_cm)) %>% 
  distinct(sciname, species_code, target_status)


# DEEP REEF
# Waiting for revised target status from Rick -- update will go in processing
# Waiting for revised mpa_designation from Rick -- update will go in processing

# Waiting for info about duplication from Rick:
# Some of the deep reef transects seem duplicated -- reference sites for
# two mpas with same species/length list
deep_duplicate_list <- deep %>% 
  distinct(year, affiliated_mpa, mpa_state_class, mpa_state_designation,
           mpa_defacto_class, mpa_defacto_designation, dive, line_id) %>% 
  mutate(multiple = if_else(line_id %in% line_id[duplicated(line_id)], T, F),
         year_id = paste(year, line_id, sep = "_"),
         multiple2 = if_else(year_id %in% year_id[duplicated(year_id)], T, F)) %>% 
  arrange(-multiple, line_id) %>% 
  filter(multiple2)

deep_duplicate_obs <- deep %>% 
  filter(line_id %in% deep_duplicate_list$line_id) %>% 
  dplyr::select(year, affiliated_mpa, mpa_state_designation, line_id,
         sciname, count, tl_cm) %>% 
  arrange(year, line_id, sciname, count, tl_cm, affiliated_mpa)

# Check cases where there is length and species info but no biomass calculation
# Added these species to the list
test_deep <- deep_biomass %>% 
  filter(is.na(weight_g) & ! is.na(tl_cm)) %>% 
  distinct(sciname, species_code, target_status)


# Exploring some things for step 2 -----------------------------------------------------------------------------

# Calculated biomass per unit effort (trip-cell)
ccfrp_bpue <- ccfrp_biomass %>% 
  dplyr::select(year, affiliated_mpa, 
         mpa_defacto_class, mpa_defacto_designation,
         id_cell_per_trip, species_code, sciname, target_status,
         total_angler_hrs_cell, weight_g) %>% 
  filter(!is.na(weight_g)) %>% 
  # Calculate biomass per unit effort (trip-cell)
  mutate(bpue = weight_g/total_angler_hrs_cell)

# Calculate the total targeted/non-targeted BPUE for each trip-cell
ccfrp_trip <- ccfrp_bpue %>%
  filter(!is.na(target_status)) %>% # drop for now
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, 
           id_cell_per_trip, target_status) %>% 
  # Calculated total bpue for targeted and nontargeted species for each trip
  summarize(total_bpue = sum(bpue, na.rm = T)) %>% ungroup()

ccfrp_summary <- ccfrp_trip %>%    
  group_by(year, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
           target_status) %>% 
  summarize(mean_bpue = mean(total_bpue),
            n_rep = n(),
            sd = sd(total_bpue, na.rm=TRUE),
            se = sd/sqrt(n_rep)) %>% 
  filter(!(mpa_defacto_class == "smca")) %>%  # drop smcas
  filter(!(target_status == "Nontargeted")) %>% # drop nontargeted (for now)
  filter(!(affiliated_mpa == "trinidad NA")) %>% # drop trinidad
  pivot_wider(names_from = "mpa_defacto_designation",
              values_from = c(mean_bpue, n_rep, sd, se)) %>% 
  mutate(habitat = "shallow reef")

# OLD FILE NAMES: NOT YET OVERWRITTEN
#write.csv(surf_zone_build3, row.names = F, file.path(outdir,"/biomass_processed/surf_zone_fish_biomass.csv"))  
#write.csv(kelp_fish_counts_final, row.names = F, file.path(outdir,"/biomass_processed/kelpforest_fish_biomass.csv"))
#write.csv(ccfrp_build12, row.names = F, file.path(outdir,"/biomass_processed/ccfrp_fish_biomass.csv"))         
#write.csv(deep_reef_build10, row.names = F, file.path(outdir,"/biomass_processed/deep_reef_fish_biomass.csv"))  


#Note: the unit of replication for CCFRP is cell
#CCFRP did their own length weight conversion which is included in ccfrp_effort
#that is already loaded. We are going to process their data and apply our own biomass
#conversion params to make sure the same params are applied for species shared 
#between habitats (kelp forest and deep reef). We can use the ccfrp_effort table
#to QAQC our processed data. cpue and no. caught fishes should match exactly. 
#bpue might be slightly different since we are using our own conversion params. 
#it would be worthwhile at some point to compare how well our estimates align with theirs. 


