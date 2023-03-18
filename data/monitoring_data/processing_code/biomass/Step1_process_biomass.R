#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 17, 2023

rm(list=ls())

#load required packages
require(dplyr)
require(stringr)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

ccfrp_raw <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_derived_data_tables_DataONE/CCFRP_derived_effort_table.csv"))
kelp_forest_raw <- read.csv(file.path(datadir, "/monitoring_kelp/MLPA_kelpforest_fish.4.csv"))

#load taxonomy lookup table
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv")

################################################################################
#process ccfrp biomass data

################################################################################
#process kelp forest

kelp_code <- taxon_tab %>% filter(habitat=="Kelp forest")

kelp_forest_process1 <- left_join(kelp_forest_raw, kelp_code, by=(c("classcode"="habitat_specific_code")))













                                  