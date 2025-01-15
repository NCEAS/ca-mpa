
# Setup --------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)

# Set directories 
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"
tabdir <- here::here("analyses","1performance_eco","tables")

# Load data
surf_biomass <- read.csv(file.path(datadir, "surf_zone_fish_biomass_updated.csv"))
kelp_biomass <- read.csv(file.path(datadir, "kelpforest_fish_biomass_updated.csv")) 
ccfrp_biomass <- read.csv(file.path(datadir, "ccfrp_fish_biomass_updated.csv"))
deep_biomass <- read.csv(file.path(datadir, "deep_reef_fish_biomass_updated.csv"))

################################################################################
#get focal table elements

surf_table <- surf_biomass %>% dplyr::select(sciname, target_status)%>%
              mutate(ecosystem = "Surf zone") %>% distinct() %>% drop_na()

kelp_table <- kelp_biomass %>% dplyr::select(sciname, target_status)%>%
  mutate(ecosystem = "Kelp forest") %>% distinct() %>% drop_na()

ccrfp_table <- ccfrp_biomass %>% dplyr::select(sciname, target_status)%>%
  mutate(ecosystem = "Shallow reef") %>% distinct() %>% drop_na()

deep_table <- deep_biomass %>% dplyr::select(sciname, target_status)%>%
  mutate(ecosystem = "Deep reef") %>% distinct() %>% drop_na()

#merge

taxon_table <- rbind(surf_table, kelp_table, ccrfp_table, deep_table) %>%
                dplyr::select(Ecosystem = ecosystem, "Scientific name" = sciname, 
                              "Target status" = target_status) %>%
                mutate(dumy_var = "X") %>%
                pivot_wider(names_from = Ecosystem, values_from = dumy_var, values_fill = NA) %>%
                mutate(across(3:6, ~ ifelse(.=="NULL",NA,"X"))) %>%
                arrange(`Scientific name`)
################################################################################
#Create table

write.csv(taxon_table, file.path(tabdir, "AppendixS4_taxonomy_table.csv"),row.names = FALSE)










