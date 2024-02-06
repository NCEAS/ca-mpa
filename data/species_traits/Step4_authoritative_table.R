# Step 4 Create Authoritative Length-Weight Parameter Table
# JS, CL, CF
# Aug 2023

# The CCFRP team conducted a thorough literature review to identify length-weight
# parameters most appropriate for each species in their database. However, their 
# species list does not capture all observed species in the entire monitoring datasets
# across all habitats. Therefore, we will use the CCFRP-identified length-weight 
# parameters when they exist, and will otherwise use parameters pulled from fishbase.

# This script combines the CCFRP parameter table with the length-weight 
# parameters that were pulled from fishbase in Step 3.

# There are a few species that are not in the CCFRP dataset OR fishbase. For these,
# we use the length-weight parameters provided from literature reviews by the kelp forest
# monitoring group. There are 9 species that use KF parameters.

# Setup ---------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(janitor)

# Directories
basedir <- "/home/shares/ca-mpa/data/sync-data" 
taxadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables"
datadir <- file.path(basedir, "species_traits")

# Read Data ---------------------------------------------------------------------------
# Length-weight parameters pulled from fishbase in Step 3
fishbase_params <- read.csv(file.path(datadir, "processed/fishbase_lw_parameters.csv")) %>% 
  mutate(source = "Fishbase")

# Read the cleaned species key to match the taxonomy to that in fishbase
spp_orig <- read.csv(file.path("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv"), as.is=T) %>% 
  clean_names() 

# Length-weight parameters from CCFRP literature review
# File sent to us by Rachel Brooks, rachel.brooks@sjsu.edu, on 11/30/2022
ccfrp_params <- readxl::read_excel(file.path(datadir, "raw/CCFRP_Biomass_Conversion_20220206.xlsx"), 
                                   sheet = "CCFRP_Biomass_Conversion_Table_") %>%
  mutate(source = "Rocky reef") %>% 
  rename(units = ...16) %>%  # fix column with no header in original file
  clean_names() 

# Length-weight parameters from Kelp Forest literature review
kelp_params <-readxl::read_excel(file.path(taxadir, "Kelp-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA") %>% 
  clean_names() %>% select(pisco_classcode, scientific_name_accepted, wl_a:ll_equation_for_wl) %>% 
  mutate(source = "Kelp forest")

# Length weight parameters from surf zone literature review
surf_params <- read.csv(file.path(taxadir,"surf_zone_fish_seine_species.csv")) %>% 
  clean_names() %>% 
  select(species_code, scientific_name_accepted, wl_a:ll_equation_for_wl) %>% 
  mutate(source = "Surf zone")

# Length-weight parameters from deep reef literature review
deep_params <- readxl::read_excel(file.path(taxadir, "DeepReef-ROV-Taxonomy.xlsx"),
  sheet = 1, na = c("NA", "N/A", "", " ")) %>% clean_names() %>% 
  select(scientific_name, wl_a:lc_type_for_wl) %>% 
  mutate(source = "Deep reef")

  
# Build Habitat Data ---------------------------------------------------------------------------
# Conversion to a prime:
# Parameters are unit-specific. Since fishbase converts all parameters to a_prime with length in 
# cm and weight in g, we will convert the a values provided by the groups to match those units.
# Converting here simplifies the eventual biomass conversion function and will allow us to examine
# potential errors more closely here, rather than with the full dataset.
# Equations used are here: https://fishbase.de/manual/fishbasethe_length_weight_table.htm

# Fixing length-length conversion: 
# For CCFRP, there are two entries that have additional "cm" or "*10" written in their equations,
# but testing either mm input or cm input with those given equations makes it seem like 
# this possible extra step of unit conversion is not necessary. Using the provided slope 
# and intercept with either cm or mm input (and no additional *10) provides a reasonable 
# conversion from SL > TL, therefore we use the provided values with no added conversion.

# Across all values, regardless of the wl_input_length (cm vs mm), the given length-length 
# slope and intercept appear to already be converted such that the given equation takes cm
# as input, and produces cm as output. This is confirmed once adjusting the 
# parameters for "REVERSE" below (explanation in next note). See ccfrp_params1, last two 
# columns test converting a hypothetical 25cm (250mm) species measured in TL to the SL 
# needed for the length-weight conversion. These values look reasonable. 

# Reversing equation: Several of the group tables length-length conversions have REVERSE
# for input type, which means that instead of taking TL as the input and producing 
# SL as the output ("TYPICAL"), the given slope and intercept are for the formula where SL is
# the input and TL is the output. Since we are always using TL as our input, we will
# adjust the slope and intercept based on rearranging the TL = int + slope * SL 
# equation:
# Given slope and intercept from Rachel are appropriate for this equation
# TL = int + slope * SL
# 
#       Rearrange such that: 
#       SL = (TL - int) / slope
#       SL = (1/slope) * TL - (int/slope)
#           
#     So, slope' = 1/slope
#           int' = -1*(int/slope)


## Build CCFRP  ------------------------------------------------------------------------
ccfrp_params1 <- ccfrp_params %>% 
  # Correct some missing scientific names 
  mutate(scientific_name_accepted = case_when(common_name == "Finescale Triggerfish" ~ "Balistes polylepis", 
                                              common_name == "Mackerel (Family Scombridae)" ~ "Scombridae spp",
                                              common_name == "Ocean Sunfish" ~ "Mola mola",
                                              T~scientific_name_accepted)) %>% 
  mutate(scientific_name_accepted = str_replace_all(scientific_name_accepted, '\\.', '')) %>% 
  mutate(scientific_name_accepted = str_replace_all(scientific_name_accepted, '\\/', ' or ')) %>% 
  # Drop incomplete entries 
  filter(!is.na(scientific_name_accepted) & !is.na(wl_a) & !is.na(wl_b)) %>% 
  select(sciname = scientific_name_accepted,
         a = wl_a,
         b = wl_b,
         units_w = wl_w_units,
         units_l = wl_l_units,
         type = wl_input_length,
         slope_ll = lc_a,
         intercept_ll = lc_b,
         conversion_type = lc_type_for_wl,
         conversion_units = units,
         source) %>% 
  distinct() # a few duplicates; this df should be 91 obs
  
ccfrp_params2 <- ccfrp_params1 %>% 
  # Adjust length-length conversions to NA when type == TL (no conv. needed)
  mutate(intercept_ll = if_else(type %in% c("TL", "DW"),  NA, intercept_ll),
         slope_ll = if_else(type %in% c("TL", "DW"), NA, slope_ll),
         conversion_type = if_else(type %in% c("TL", "DW"), NA, conversion_type)) %>% 
  # Convert a' to correct units (g, cm) when needed
  mutate(a_prime = case_when(units_w == "g" & units_l == "cm" ~ a,
                             units_w == "g" & units_l == "mm" ~ a*10^b,
                             units_w == "kg" & units_l == "cm" ~ a*1000,
                             units_w == "kg" & units_l == "mm" ~ a*10^b*1000,
                             T~NA)) %>% 
  # Fix the length-length conversion 
  mutate(slope_ll_prime = if_else(conversion_type == "REVERSE", 1/slope_ll, slope_ll),
         intercept_ll_prime = if_else(conversion_type == "REVERSE", -1*intercept_ll/slope_ll, intercept_ll)) %>% 
  # Demonstrate that regardless of cm/mm input, conversion will produce appropriate output:
  mutate(test_25_cm = 25*slope_ll_prime + intercept_ll_prime, # 25 cm TL > convert to SL
         test_250_mm = 250*slope_ll_prime + intercept_ll_prime) # 250 cm TL > convert to SL
 
# Clean up and save appropriate columns to join with fishbase parameters
ccfrp_params3 <- ccfrp_params2 %>% 
  mutate(type = recode(type, "DW" = "WD")) %>% 
  # Drop cases where conversion is missing
  filter(!(type == "SL" & is.na(slope_ll_prime))) %>% 
  # Fix scientific names
  mutate(sciname = recode(sciname,
                          "Rhacochilus vacca" = "Phanerodon vacca",
                          "Xenistius californiensis" = "Brachygenys californiensis",
                          "Citharichthys" = "Citharichthys spp",
                          "Sebastes serranoides or flavidus" = "Sebastes serranoides")) %>% 
  left_join(fishbase_params %>% select(sciname, family, genus)) %>% # add family and genus from fishbase taxa
  mutate(level = case_when(str_detect(sciname, "spp") & !is.na(genus) ~"genus", 
                               str_detect(sciname, "spp") & is.na(genus) ~ "family",
                               T~"species")) %>% 
  select(family, genus, sciname, level,
         a = a_prime, # a_prime has been converted to cm and g as in fishbase
         b, 
         lw_type = type, 
         slope_ll = slope_ll_prime,
         intercept_ll = intercept_ll_prime, 
         source)

## Build Kelp  -----------------------------------------------------
kelp_params1 <- kelp_params %>% 
  # drop rows with no parameters
  filter_at(vars(wl_a:ll_equation_for_wl), any_vars(!is.na(.))) %>% 
  # join taxonomy from species table
  left_join(spp_orig, by = c("scientific_name_accepted" = "habitat_specific_spp_name", 
                             "pisco_classcode" = "habitat_specific_code")) %>% 
  filter(!is.na(sciname)) %>% 
  select(sciname, family, genus, species, level,
        a = wl_a,
        b = wl_b,
        units_w = wl_w_units,
        units_l = wl_l_units,
        type = wl_input_length,
        slope_ll = lc_a_for_wl,
        intercept_ll = lc_b_for_wl,
        conversion_type = lc_type_for_wl,
        ll_equation = ll_equation_for_wl,
        source,
        notes = wl_reference_notes) %>% 
  distinct()

kelp_params2 <- kelp_params1 %>% 
  # Convert a' to correct units (g, cm) when needed
  mutate(a_prime = case_when(units_w == "g" & units_l == "cm" ~ a,
                             units_w == "g" & units_l == "mm" ~ a*10^b,
                             units_w == "kg" & units_l == "cm" ~ a*1000,
                             units_w == "kg" & units_l == "mm" ~ a*10^b*1000,
                             T~NA)) %>% 
  # Fix the length-length conversion 
  mutate(slope_ll_prime = if_else(conversion_type == "REVERSE", 1/slope_ll, slope_ll),
         intercept_ll_prime = if_else(conversion_type == "REVERSE", -1*intercept_ll/slope_ll, intercept_ll)) %>% 
  # Demonstrate that regardless of cm/mm input, conversion will produce appropriate output:
  mutate(test_25_cm = 25*slope_ll_prime + intercept_ll_prime, # 25 cm TL > convert to SL
         test_250_mm = 250*slope_ll_prime + intercept_ll_prime) # 250 cm TL > convert to SL
  
# Clean up and save appropriate columns to join with fishbase parameters
kelp_params3 <- kelp_params2 %>% 
  mutate(type = recode(type, "DW" = "WD")) %>% 
  # Drop cases where conversion is missing (there are none)
  #filter(!(type == "SL" & is.na(slope_ll_prime))) %>% 
  select(family, genus, sciname, level,
         a = a_prime, # a_prime has been converted to cm and g as in fishbase
         b, 
         lw_type = type, 
         slope_ll = slope_ll_prime,
         intercept_ll = intercept_ll_prime, 
         source)

## Build Surf -------------------------------------------------------------------
surf_params1 <- surf_params %>% 
  # go ahead and drop rows with no parameters
  filter_at(vars(wl_a:ll_equation_for_wl), any_vars(!is.na(.))) %>% 
  left_join(spp_orig, by = c("scientific_name_accepted" = "habitat_specific_spp_name", 
                             "species_code" = "habitat_specific_code")) %>% 
  filter(!is.na(sciname))

surf_params2 <- surf_params1 %>% 
  select(sciname, family, genus, species, level,
         a = wl_a,
         b = wl_b,
         units_w = wl_w_units,
         units_l = wl_l_units,
         type = wl_input_length,
         slope_ll = lc_a_for_wl,
         intercept_ll = lc_b_for_wl,
         conversion_type = lc_type_for_wl,
         ll_equation = ll_equation_for_wl,
         source,
         lw_source = source_lw_lc,
         notes = wl_reference_notes) %>% 
  # Drop those not identified to species level; will use FB instead
  #filter(level == "species") %>% 
  # Convert a' to correct units (g, cm) when needed
  mutate(a_prime = case_when(units_w == "g" & units_l == "cm" ~ a,
                             units_w == "g" & units_l == "mm" ~ a*10^b,
                             units_w == "kg" & units_l == "cm" ~ a*1000,
                             units_w == "kg" & units_l == "mm" ~ a*10^b*1000,
                             T~NA)) %>% 
  # Fix the length-length conversion 
  mutate(slope_ll_prime = if_else(conversion_type == "REVERSE", 1/slope_ll, slope_ll),
         intercept_ll_prime = if_else(conversion_type == "REVERSE", -1*intercept_ll/slope_ll, intercept_ll)) %>% 
  # Demonstrate that regardless of cm/mm input, conversion will produce appropriate output:
  mutate(test_25_cm = 25*slope_ll_prime + intercept_ll_prime, # 25 cm TL > convert to SL
         test_250_mm = 250*slope_ll_prime + intercept_ll_prime) # 250 cm TL > convert to SL

# Clean up and save appropriate columns to join with fishbase parameters
surf_params3 <- surf_params2 %>% 
  # Drop cases where conversion is missing (there are none)
  filter(!(type == "SL" & is.na(slope_ll_prime))) %>% 
  mutate(lw_source = "species",
         source = "surf") %>% 
  select(family, genus, sciname, lw_source,
         a = a_prime, # a_prime has been converted to cm and g as in fishbase
         b, 
         lw_type = type, 
         slope_ll = slope_ll_prime,
         intercept_ll = intercept_ll_prime, 
         source)

## Build Deep ----------------------------------------------------------------------
deep_params1 <- deep_params %>% 
  # go ahead and drop rows with no parameters
  filter_at(vars(wl_a:lc_type_for_wl), any_vars(!is.na(.))) %>% 
  left_join(spp_orig, by = c("scientific_name" = "habitat_specific_spp_name")) %>% 
  filter(!is.na(sciname)) %>% mutate(source = "deep")

deep_params2 <- deep_params1
  select(sciname, family, genus, species, level,
         a = wl_a,
         b = wl_b,
         units_w = wl_w_units,
         units_l = wl_l_units,
         type = wl_input_length,
         slope_ll = lc_a_for_wl,
         intercept_ll = lc_b_for_wl,
         conversion_type = lc_type_for_wl,
         ll_equation = ll_equation_for_wl, 
         source)




# Combine parameter datasets -------------------------------------------------
# Drop species from fishbase parameters that are already in CCFRP
fishbase_params_subset <- fishbase_params %>% 
  filter(!sciname %in% ccfrp_params2$sciname) %>% 
  filter(!sciname %in% kelp_params2$sciname) %>% 
  filter(!lw_type == "Unknown")

# Combine fishbase subset with CCFRP data
params <- full_join(ccfrp_params2, fishbase_params_subset)

# Drop the kelp forest species that are already in the above dataset
kelp_subset <- kelp_params2 %>% 
  filter(!sciname %in% params$sciname |
           sciname == "Amphistichus rhodoterus")

# Add the kelp forest species to the params dataset
params2 <- full_join(params, kelp_subset)

# Drop the surf species that are already in the above dataset
surf_subset <- surf_params2 %>% 
  filter(!sciname %in% params2$sciname)


# Write to csv
#write.csv(params, file.path(datadir, "processed/lw_parameters_fish.csv"), row.names = F)
# last write Oct 11 2023

# Still not overwriting original files
#write.csv(params_tab, file.path(datadir, "processed/fish_lw_parameters_by_species.csv"),row.names = FALSE)










