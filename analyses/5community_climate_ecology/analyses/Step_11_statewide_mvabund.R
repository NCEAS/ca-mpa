#Joshua G. Smith
#December 19, 2022

rm(list=ls())

#required packages
require(dplyr)
require(mvabund)
require(here)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")
tabledir <- here::here("analyses", "5community_climate_ecology", "tables")

mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")

mpa_traits1 <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name")
mpa_traits <- left_join(mpa_traits1, mpa_attributes_hab_div, by="name")

comm_data <- load(file.path(data_path, "comm_data.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
