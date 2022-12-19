# Process Attributes for Figures and Community Analyses
# Cori Lopazanski
# Updated Oct 2022 with New Habitat Attribute Data

# ------------------------------------------------------------------------------
# This script cleans and processes the MPA attribute data, which contains data 
# of MPA-level traits. The original data files were supplied to the NCEAS 
# working group by CDFW in 2021. An updated version was provided by CDFW in 
# May 2022. Issues with the habitat attributes were noticed in August 2022. 
# An updated, corrected version of the habitat values were provided to the 
# NCEAS working group and CDFW in Oct 2022 by Emily Saarman.
# 
# This script combines the updated habitat attributes with the other MPA
# traits that had been provided across previous versions. Use with caution lol.
#
# Contact Cori Lopazanski with questions (lopazanski@ucsb.edu)
# ------------------------------------------------------------------------------


# Setup ------------------------------------------------------------------------
# Packages
library(tidyverse)

# Clear workspace
rm(list = ls())

# Directories
base.dir <- "/Volumes/GoogleDrive-105151121202188525604/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data" # Cori Local
raw.dir <- file.path(base.dir, "mpa_traits", "raw")
clean.dir <- file.path(base.dir, "mpa_traits", "processed")
#data.dir <- file.path(getwd(), "analyses", "7habitat", "intermediate_data")


# Read Data Files --------------------------------------------------------------
## Updated Attribute (Habitat/MPA Trait) Data (version provided October 2022)
att_new <- readxl::read_excel(file.path(raw.dir, "mpa-attributes-2022Oct17-raw.xlsx"),
                              sheet = 1, skip = 4, na = ".") %>% janitor::clean_names()

## Original Attribute (Habitat/MPA Trait) Data (version provided May 2022)
# Note that this has been previously modified and updated outside of script
# because was saved as a google sheet that could be modified. Will try to
# only keep columns that are verified but YMMV, use with caution.
att_old <- readxl::read_excel(file.path(raw.dir, "mpa-attributes-2021.xlsx"),
                              sheet = 1, na = "na") %>% janitor::clean_names()

## MPA Age Data
# Information about previous protection prior to MLPA implementation
mpa_age <- readxl::read_excel(file.path(raw.dir, "mpa-attributes-2021.xlsx"),
                              sheet = 3) %>% janitor::clean_names()
## MPA Class Data
# Information supplied by the long-term monitoring PIs indicating their 
# perspective on the 'actual' level of protection within the MPA for their 
# habitat type, e.g. if there is an SMCA that allows limited take of an 
# organism that does not use that habitat, the habitat group may consider the
# SMCA to be a de-facto SMR. 
mpa_class <- readxl::read_excel(file.path(raw.dir, "mpa-attributes-2021.xlsx"),
                                sheet = 5) %>% janitor::clean_names()

# Definitions ------------------------------------------------------------------
# Identify Northern Channel Islands MPAs ----
n_ci <- c("Anacapa Island SMCA", "Anacapa Island SMR",
          "Begg Rock SMR", "Carrington Point SMR", "Footprint SMR",
          "Gull Island SMR", "Harris Point SMR", "Judith Rock SMR", 
          "Painted Cave SMCA", "Richardson Rock SMR", "Santa Barbara Island SMR",
          "Scorpion SMR", "Skunk Point SMR", "South Point SMR")


# Clean ------------------------------------------------------------------------
## att_raw (updated version) ----
att_new_clean <- att_new %>% 
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
  # Rename name column to name_short
  rename(name_short = mpa_name) %>% 
  # Create affiliated mpa column (name + designation, all lowercase)
  mutate(affiliated_mpa = str_to_lower(name)) %>% 
  # Combine predicted and mapped columns for 0-30m
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

## att_old (original version) ----
att_old_clean <- att_old %>% 
  select(affiliated_mpa = name, longitude_centroid = long,
         mpa_class, cluster, tier, historical_protection_overlap)
  
# Build General Metadata -------------------------------------------------------
all_data <- att_new_clean %>% 
  left_join(att_old_clean) %>% 
  select(
    # Variations of name + designation
    name, name_short, designation, affiliated_mpa, 
    # Regions
    bioregion, four_region_north_ci, 
    # De-facto mpa class
    mpa_class,
    # Implementation date
    implementation_date,
    # Size and basic location stats
    size_km2, latitude_centroid, longitude_centroid,
    # MLPA protection level (qualitative; e.g. "very high") and score
    level_of_protection, level_of_protection_score, 
    # Historical overlap score
    historical_protection_overlap,
    # Cluster and tier
    cluster, tier) 

# Export
saveRDS(all_data, file.path(clean.dir, "mpa_attributes_general.Rds")) # shared


# Build Habitat ----------------------------------------------------------------
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
estuaries <- att_new_clean %>% 
  filter_at(vars(linear_habitats), any_vars(. > 0)) %>% 
  filter_at(vars(area_habitats), all_vars(. %in% c(0, NA)))

offshore <- att_new_clean %>% 
  filter_at(vars(area_habitats), any_vars(. > 0)) %>% 
  filter_at(vars(linear_habitats), all_vars(. %in% c(0, NA)))

# Casino Point = Shoreline all hardened/armored shore but values are zeroes
# Moro Cojo Slough = "Shoreline mapping doesn't extend into embayment"

coastal <- att_new_clean %>% 
  filter(!(name %in% c(estuaries$name, offshore$name)))
  
## Create new habitat type column ----
att_data <- att_new_clean %>% 
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
#saveRDS(att_data, file.path(data.dir, "mpa_attributes_processed.Rds")) #local
saveRDS(att_data, file.path(clean.dir, "mpa_attributes_habitat.Rds")) # shared


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

#saveRDS(att_labels, file.path(data.dir, "mpa_attributes_labels.Rds")) # local

        