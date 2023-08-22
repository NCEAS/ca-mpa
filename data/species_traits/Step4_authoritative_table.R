
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/home/shares/ca-mpa/data/sync-data/" 
datadir <- file.path(basedir, "species_traits/")

# Read data
fishbase_params <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fishbase_lw_parameters_by_species.csv") #FishBase params from Step3

# file sent to us by Rachel Brooks, rachel.brooks@sjsu.edu, on 11/30/2022
ccfrp_params <- read_xlsx(file.path(datadir, "raw/CCFRP_Biomass_Conversion_20220206.xlsx"), sheet = "CCFRP_Biomass_Conversion_Table_") %>%
  mutate(source = "ccfrp") %>% 
  rename(units = ...16)%>%  # fix column with no header in original file
  mutate(source = "Brooks") #add source of params


################################################################################
#workflow:

# Brooks et al. conducted a lit review for relevant params in this system. 
#  Use params from Brooks et al. first, then fishbase. 


################################################################################
#prep fishbase params for join with Brooks 
fishbase_params1 <- fishbase_params %>% 
  mutate(source = "fishbase",
         WL_W_units = "g",
         WL_L_units = "cm",
         WL_input_length = "TL")%>%
  dplyr::select(ScientificName_accepted = sciname,
                WL_a = a, WL_b = b,
                WL_W_units,
                WL_L_units,
                WL_input_length, source)

################################################################################
#prep Brooks for join
ccfrp_params1 <- ccfrp_params %>%
                  #drop species not ID to species level. We'll use the genus level 
                  #params for these instead when we merge with fishbase_params1.
                  filter(!(ScientificName_accepted %in% c("Sebastes serranoides/flavidus",
                                                          "Sebastes spp.")))%>%
                  filter(!(is.na(ScientificName_accepted)))

################################################################################
#drop species from fishbase params that are already in Brooks
brooks_join <- ccfrp_params1 %>% dplyr::select(ScientificName_accepted) 
fishbase_join <- fishbase_params1 %>% dplyr::select(ScientificName_accepted)

drop_spp <- inner_join(brooks_join, fishbase_join)  

fishbase_params2 <- anti_join(fishbase_params1, drop_spp, by="ScientificName_accepted")


################################################################################
#merge unique params
params_tab <- merge(ccfrp_params1,fishbase_params2,all=TRUE)


################################################################################
#Inspect

#check for any duplicates
params_dup <- params_tab[duplicated(params_tab[, 1:6]) | duplicated(params_tab[, 1:6], fromLast = TRUE), ]


################################################################################
#

write.csv(params_tab, file.path(datadir, "processed/fish_lw_parameters_by_species.csv"))










