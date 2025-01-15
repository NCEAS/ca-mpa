#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 22, 2023

rm(list=ls())

#load required packages
require(dplyr)
require(stringr)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"

biomass <- readRDS(file.path(here::here("analyses","1performance_eco","output"), "biomass_with_moderators_new2.Rds")) %>%
                mutate(affiliated_mpa = tolower(affiliated_mpa))

rich_div <- read.csv(file.path(datadir, "richness_diversity_MPA_means_updated.csv"))


################################################################################
#merge

merged_dat <- biomass %>%
                  filter(target_status == 'Targeted') %>%
                  dplyr::select(habitat, year, state_region,affiliated_mpa,
                                yi,vi, age_at_survey) %>%
                  rename(target_biomass_logRR = yi,
                         target_biomass_sd = vi) %>%
                  left_join(rich_div, by = c("habitat","year","affiliated_mpa")) %>%
                  filter(!(is.na(year)))


saveRDS(merged_dat, file = file.path(here::here("analyses","1performance_eco","output"),"biomass_richness_diversity2.Rds"))

#last write 22 Oct 2024










