# Process Attributes for Figures and Community Analyses
# Cori Lopazanski


# Setup ------------------------------------------------------------------------
# Packages
library(tidyverse)

# Clear workspace
rm(list = ls())

# Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
out.dir <- file.path(getwd(), "analyses", "7habitat", "intermediate_data")

# Read Attribute (Habitat) Data
att_raw <- read_csv(file.path(base.dir, "mpa_traits", 
                              "processed", "mpa_attributes_clean.csv"))


# Process  ---------------------------------------------------------------------

# Process
att_data <- att_raw %>% 
  # Combine predicted and mapped columns for 0-30m
  mutate(hard_substrate_0_30m_km2_comb = 
           hard_substrate_predicted_0_30m_km2 + hard_substrate_mapped_0_30m_km2,
         soft_substrate_0_30m_km2_comb = 
           soft_substrate_0_30m_km2 + soft_substrate_predicted_0_30m_km2) %>% 
  # Drop the columns that are combined
  select(-hard_substrate_predicted_0_30m_km2, -hard_substrate_mapped_0_30m_km2,
         -soft_substrate_0_30m_km2, -soft_substrate_predicted_0_30m_km2)

## Change big river estuary smca to an estuary
att_data$coastal_estuary[att_data$name == "big river estuary smca"] <- "estuary"

## Change ten mile beach smca to a coastal
att_data$coastal_estuary[att_data$name == "ten mile beach smca"] <- "coastal"


## Classify offshore MPAs (no coastline) ----
offshore_mpas <- c("blue cavern offshore smca", 
                   "farnsworth offshore smca",
                   "begg rock smr",
                   "richardson rock smr",
                   "footprint smr",
                   "south la jolla smca",
                   "soquel canyon smca",
                   "portuguese ledge smca",
                   "point lobos smca",
                   "point sur smca",
                   "big creek smca",
                   "piedras blancas smca",
                   "point buchon smca",
                   "point st. george reef offshore smca",
                   "reading rock smca",
                   "point arena smca",
                   "point reyes smca",
                   "southeast farallon island smca",
                   "mattole canyon smr",
                   "carmel pinnacles smr")  

att_data <- att_data %>% 
  ## Create new "mpa habitat type" coastal, offshore, estuary ----
  mutate(mpa_habitat_type = if_else(name %in% offshore_mpas, "offshore", coastal_estuary)) %>% 
  mutate(mpa_habitat_type = recode_factor(mpa_habitat_type,
                                          "estuary" = "Estuary",
                                          "coastal" = "Coastal",
                                          "offshore" = "Offshore")) %>% 
  # Reorder the variables
  select(name:coastal_estuary, mpa_habitat_type, everything()) %>% 
  # Make region a factor
  mutate(bioregion = recode_factor(bioregion,
                                   "north" = "North",
                                   "central" = "Central",
                                   "south" = "South")) %>% 
  mutate(four_region_north_ci = recode_factor(four_region_north_ci,
                                              "north" = "North",
                                              "central" = "Central",
                                              "north islands" = "N. Channel Islands",
                                              "south" = "South")) %>% 
  # Fix names
  mutate(name = str_to_title(name)) %>% 
  mutate(name = str_replace(name, "Smrma", "SMRMA")) %>% 
  mutate(name = str_replace(name, "Smr", "SMR")) %>% 
  mutate(name = str_replace(name, "Smca", "SMCA"))

# Export Main Processed Data ---------------------------------------------------
#saveRDS(att_data, file.path(out.dir, "mpa_attributes_processed.Rds"))

# Define Attribute Labels ------------------------------------------------------

att_short <- c("Size", "Sandy beach", "Rocky intertidal", "Offshore rock",
               "Kelp canopy", "Kelp landsat", "Hard substrate 30-100m",
               "Hard substrate 100-200m", "Hard substrate 200-3000m",
               "Soft substrate 30-100m", "Soft substrate 100-200m", "Soft substrate 200-3000m",
               "Submarine canyon 0-30m", "Submarine canyon 30-100m", "Submarine canyon 100-200m",
               "Submarine canyon 200-3000m", "Estuary", "Surfgrass", "Eelgrass", "Coastal marsh",
               "Coastal marsh (km2)", "Tidal flats", "Hardened/armored shore", "Hard substrate 0-30m",
               "Soft substrate 0-30m")


att_labels <- data.frame(attribute = 
                           names(att_data %>% select(size_km2, 
                                                     sandy_beach_km:hardened_armored_shore_km,
                                                     hard_substrate_0_30m_km2_comb, 
                                                     soft_substrate_0_30m_km2_comb))) %>% 
  cbind(att_label = att_short)

#saveRDS(att_labels, file.path(out.dir, "mpa_attributes_labels.Rds"))

        