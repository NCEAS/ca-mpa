#Joshua G. Smith
#December 13, 2022

rm(list=ls())

#require packages
require(dplyr)
require(here)

#set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/statewide_data"
figdir <-  here::here("analyses", "5community_climate_ecology", "figures")

mpa_trait <- read.csv("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_clean.csv")

#load model output
mod_out_raw <- read.csv(file.path(datadir,"mpa_betadisp_mod.csv"))


###############################################################################
#Join model output with MPA traits

#Step 1 - select traits of interest

trait_drivers <- mpa_trait %>%
                 select(name, long, lat, implementation_date, size_km2, shore_span_km,
                        min_depth_m, max_depth_m, protection, depth_range)


mpa_dat <- left_join(mod_out_raw, trait_drivers, by=c("MPA"="name"))%>%
            ## set target as 'no change' == stable
            mutate(stable = ifelse(p.value < 0.05,"no","yes"))



###############################################################################
#Build logistic regression







