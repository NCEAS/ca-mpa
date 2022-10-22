# Process Attributes for Figures and Community Analyses
# Cori Lopazanski
# Updated Oct 2022 with New Habitat Attribute Data

# Setup ------------------------------------------------------------------------
# Packages
library(tidyverse)

# Clear workspace
rm(list = ls())

# Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
data.dir <- file.path(getwd(), "analyses", "7habitat", "intermediate_data")

# Read Attribute (Habitat) Data
att_raw <- readxl::read_excel(file.path(data.dir, "mpa-attributes-2022Oct17-raw.xlsx"),
                              sheet = 1, skip = 4, na = ".") %>% janitor::clean_names()

# Identify Northern Channel Islands MPAs
n_ci <- c("Anacapa Island SMCA", "Anacapa Island SMR",
          "Begg Rock SMR", "Carrington Point SMR", "Footprint SMR",
          "Gull Island SMR", "Harris Point SMR", "Judith Rock SMR", 
          "Painted Cave SMCA", "Richardson Rock SMR", "Santa Barbara Island SMR",
          "Scorpion SMR", "Skunk Point SMR", "South Point SMR")
# Clean ------------------------------------------------------------------------
att_clean <- att_raw %>% 
  # Remove entries for "-old" calculations
  filter(!(str_detect(mpa_name, "-old"))) %>% 
  # Remove the remaining "-new" tag from the MPA names
  mutate(mpa_name = str_remove(mpa_name, "-new")) %>% 
  # Drop the special closures
  filter(!(designation == "Special Closure")) %>% 
  # Rename "rcky_inter_km" to "rocky_inter_km"
  rename(rocky_inter_km = rcky_inter_km) %>% 
  # Rename the "rocky_reef" columns to hard substrate
  rename_with(~ gsub("rocky_reef", "hard_substrate", .x, fixed = TRUE)) %>% 
  # Rename the "soft_bottom" columns to soft substrate
  rename_with(~ gsub("soft_bottom", "soft_substrate", .x, fixed = TRUE)) %>% 
  # Correct depths so that all say "m" 
  rename_with(~ gsub("100_k", "100m_k", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("200_k", "200m_k", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("3000_k", "3000m_k", .x, fixed = TRUE)) %>% 
  rename_with(~ gsub("200m_3", "200_3", .x, fixed = TRUE)) %>% 
  # Create full name (name + designation) 
  mutate(name = paste(mpa_name, designation, sep = " ")) %>% 
  #Combine predicted and mapped columns for 0-30m
  mutate(hard_substrate_0_30m_km2_comb = 
           hard_substrate_predicted_0_30m_km2 + hard_substrate_mapped_0_30m_km2,
         soft_substrate_0_30m_km2_comb = 
           soft_substrate_0_30m_km2 + soft_substrate_predicted_0_30m_km2) %>% 
  # Drop the columns that are combined
  select(-hard_substrate_predicted_0_30m_km2, -hard_substrate_mapped_0_30m_km2,
         -soft_substrate_0_30m_km2, -soft_substrate_predicted_0_30m_km2) %>% 
  # Make region a factor and correct the name
  mutate(bioregion = recode_factor(bioregion,
                                   "NorCal" = "North",
                                   "CenCal" = "Central",
                                   "SoCal" = "South")) %>% 
  # Identify channel islands MPAs 
  mutate(four_region_north_ci = factor(if_else(name %in% n_ci, 
                                               "N. Channel Islands", 
                                              as.character(bioregion)),
         levels = c("North", "Central", "N. Channel Islands", "South"))) 

# Build ---------------------------------------------------------------------
## Specify habitat lists ----
## These are the ones that were updated in the newer calculations by E. Saarman,
## whereas the other variables were from the Paulo orginial planning era calcs
linear_habitats <- c("sandy_beach_km",
                     "rocky_inter_km",
                     "coastal_marsh_km",
                     "tidal_flats_km",
                     "hardened_armored_shore_km")

area_habitats <- c("hard_substrate_0_30m_km2_comb", 
                   "hard_substrate_30_100m_km2", 
                   "hard_substrate_100_200m_km2",
                   "hard_substrate_200_3000m_km2",
                   "soft_substrate_0_30m_km2_comb", 
                   "soft_substrate_30_100m_km2", 
                   "soft_substrate_100_200m_km2", 
                   "soft_substrate_200_3000m_km2",
                   "max_kelp_canopy_cdfw_km2")

## Identify habitat types ----
estuaries <- att_clean %>% 
  filter_at(vars(linear_habitats), any_vars(. > 0)) %>% 
  filter_at(vars(area_habitats), all_vars(. %in% c(0, NA)))

offshore <- att_clean %>% 
  filter_at(vars(area_habitats), any_vars(. > 0)) %>% 
  filter_at(vars(linear_habitats), all_vars(. %in% c(0, NA)))

# Casino Point = Shoreline all hardened/armored shore but values are zeroes
# Moro Cojo Slough = "Shoreline mapping doesn't extend into embayment"

coastal <- att_clean %>% 
  filter(!(name %in% c(estuaries$name, offshore$name)))
  
## Create new habitat type column ----
att_data <- att_clean %>% 
  mutate(mpa_habitat_type = case_when(name %in% estuaries$name ~ "Estuary",
                                      name %in% offshore$name ~ "Offshore",
                                      name %in% coastal$name ~ "Coastal")) %>% 
  mutate(mpa_habitat_type = factor(mpa_habitat_type,
                                   levels = c("Estuary", "Coastal", "Offshore"))) %>% 
  ## Reorder the variables ----
  select(name, bioregion, four_region_north_ci, size_km2, mpa_habitat_type,
         all_of(area_habitats), all_of(linear_habitats))

## Correct some values
att_data$mpa_habitat_type[att_data$name == "Moro Cojo Slough SMR"] <- "Estuary"
  

# Export Main Processed Data ---------------------------------------------------
saveRDS(att_data, file.path(data.dir, "mpa_attributes_processed.Rds"))

# Define Attribute Labels ------------------------------------------------------
att_short <- c("Size", 
               "Hard substrate 0-30m", "Hard substrate 30-100m",
               "Hard substrate 100-200m", "Hard substrate 200-3000m",
               "Soft substrate 0-30m", "Soft substrate 30-100m", 
               "Soft substrate 100-200m", "Soft substrate 200-3000m", "Kelp canopy",
               "Sandy beach", "Rocky intertidal", "Coastal marsh",
               "Tidal flats", "Hardened/armored shore")

att_labels <- data.frame(attribute = 
                           names(att_data %>% select(size_km2, all_of(area_habitats), 
                                                     all_of(linear_habitats)))) %>% 
  cbind(att_label = att_short)

saveRDS(att_labels, file.path(data.dir, "mpa_attributes_labels.Rds"))

        