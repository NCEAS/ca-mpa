
# Setup --------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)

# Set directories 
dat_path <- here::here("analyses","1performance_eco","output")
tabdir <- here::here("analyses","1performance_eco","tables")

# Load data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators_new2.Rds")) 

################################################################################
#get all unique MPAs and types

mpa_effort <- biomass_mod %>% select(habitat, year, affiliated_mpa) %>% unique() %>%
              rename(Ecosystem = habitat)
             
# Summarize the years into a single string for each affiliated_mpa and Ecosystem combination
mpa_type_summary <- mpa_effort %>%
  group_by(affiliated_mpa, Ecosystem) %>%
  summarise(years = toString(unique(year)), .groups = 'drop')

# Pivot the data wider
mpa_type_wide <- mpa_type_summary %>%
  pivot_wider(names_from = Ecosystem, values_from = years)


write.csv(mpa_type_wide, file.path(tabdir, "TableS4_survey_effort.csv"),row.names = FALSE)





