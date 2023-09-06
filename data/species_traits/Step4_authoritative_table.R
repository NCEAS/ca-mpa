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

# Setup ---------------------------------------------------------------------------
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/home/shares/ca-mpa/data/sync-data/" 
datadir <- file.path(basedir, "species_traits/")

# Read Data ---------------------------------------------------------------------------
# Length-weight parameters pulled from fishbase in Step 3
fishbase_params <- read.csv(file.path(data.dir, "processed/fishbase_lw_parameters.csv")) %>% 
  mutate(source = "fishbase")

# Length-weight parameters from CCFRP literature review
# File sent to us by Rachel Brooks, rachel.brooks@sjsu.edu, on 11/30/2022
ccfrp_params <- readxl::read_excel(file.path(datadir, "raw/CCFRP_Biomass_Conversion_20220206.xlsx"), 
                                   sheet = "CCFRP_Biomass_Conversion_Table_") %>%
  mutate(source = "ccfrp") %>% 
  rename(units = ...16) %>%  # fix column with no header in original file
  clean_names()

# Build CCFRP Data ---------------------------------------------------------------------------
# Conversion to a prime:
# Parameters are unit-specific. Since fishbase converts all parameters to a_prime with length in 
# cm and weight in g, we will convert the a values provided by CCFRP to match those units.
# Converting here simplifies the eventual biomass conversion function and will allow us to examine
# potential errors more closely here, rather than with the full dataset.
# Equations used are here: https://fishbase.de/manual/fishbasethe_length_weight_table.htm

# Fixing length-length conversion: 
# There are two entries that have additional "cm" or "*10" written in their equations,
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

# Reversing equation: Several of the CCFRP length-length conversions have REVERSE
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


# Process CCFRP params to match fishbase dataframe structure
ccfrp_params1 <- ccfrp_params %>% 
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
  # Drop species not ID to species level. We'll use the genus level 
  # params for these instead when we merge with fishbase_params1.
  filter(!(sciname %in% c("Sebastes serranoides/flavidus", "Sebastes spp.", "Citharichthys"))) %>%  # NOTE FOR JS: added Cith. to this list b/c not species level - ok? 
  filter(!is.na(sciname) & !is.na(a) & !is.na(b)) %>% 
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
ccfrp_params2 <- ccfrp_params1 %>% 
  # Fix scientific names
  mutate(sciname = recode(sciname,
                          "Rhacochilus vacca" = "Phanerodon vacca",
                          "Xenistius californiensis" = "Brachygenys californiensis")) %>% 
  left_join(fishbase_params %>% select(sciname, family, genus)) %>% 
  mutate(lw_source = "species") %>% 
  select(family, genus, sciname, lw_source,
         a = a_prime, # a_prime has been converted to cm and g as in fishbase
         b, 
         lw_type = type, 
         slope_ll = slope_ll_prime,
         intercept_ll = intercept_ll_prime, 
         source)

# Combine parameter datasets -------------------------------------------------
# Drop species from fishbase parameters that are already in CCFRP
fishbase_params_subset <- fishbase_params %>% 
  filter(!sciname %in% ccfrp_params2$sciname)

# Combine fishbase subset with CCFRP data
params <- full_join(ccfrp_params2, fishbase_params_subset)

# Write to csv
#write.csv(params, file.path(datadir, "processed/lw_parameters_fish.csv"))

# Still not overwriting original files
#write.csv(params_tab, file.path(datadir, "processed/fish_lw_parameters_by_species.csv"),row.names = FALSE)










