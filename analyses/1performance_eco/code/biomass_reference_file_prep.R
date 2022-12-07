# Prepare reference table for the length - Weight relationship to compute biomass from raw data
# Julien Brun, brun@nceas.ucsb.edu

# Relationship reference: https://www.fishbase.de/manual/fishbasethe_length_weight_table.htm



# Load packages
# install.packages(librarian)
librarian::shelf(tidyverse, readxl)

# Set the path names
data_dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/raw"
kelp_file <- "kelp_forest_biomass_params.xlsx"
ccfrp_file_old <- "old/CCFRP_biomass_params.csv"         # file we got through Shelby
ccfrp_file <- "CCFRP_Biomass_Conversion_20220206.xlsx"   # file sent to us by Rachel Brooks, rachel.brooks@sjsu.edu, on 11/30/2022


# Read the data in
raw_data_kf <- read_xlsx(file.path(data_dir, kelp_file), sheet = "species_attribute_table") %>%
  mutate(source = "kelp")

raw_data_ccfrp_old <- read_csv(file.path(data_dir, ccfrp_file_old)) %>%
  mutate(source = "ccfrp")

raw_data_ccfrp <- read_xlsx(file.path(data_dir, ccfrp_file), sheet = "CCFRP_Biomass_Conversion_Table_") %>%
  mutate(source = "ccfrp") %>% 
  rename(units = ...16)  # fix column with no header in original file

# all_equal(raw_data_ccfrp_old, raw_data_ccfrp)

# filter for fish on keep columns of interest
fish_data_kf <- raw_data_kf %>% 
  filter(group == "fish") %>%
  select(data_type, group, pisco_classcode, genus, species, common_name, ScientificName_accepted, 
         juv_size_cutoff, contains("WL"), contains("LC"), Max_Length_fishbase, Size_Mature_cm) %>%
  mutate(source = "kf")

# Check for sie difference
raw_diff <- anti_join( raw_data_ccfrp, fish_data_kf) 

raw_all <- full_join(raw_data_ccfrp, fish_data_kf)

# We have several main cases:
#  - We have the total length (TL):
#     - We have the right unit for a and b: g and cm => done
#     - We have the wrong units for a and b => need to transform using formulas
#  - We have another type of length:
#     - We need to first transform the length into the total one using the formula in column "LL_Equation_for_WL"
# 
# Main formula: W = a * L^b

# Unit transformation:
# a'(cm, g) = a (mm, g)*10^b
# a'(cm, g) = a (cm, kg)*1000
# a'(cm, g) = a (mm, mg)*10^b/1000
# a'(cm, g) = a (mm, kg)*10^b*1000


#  Code from Shelby
# 
# fishes.caught.bpue = fishes.caught.cpue %>%
#   na.omit() %>%
#   #Get rid of fishes without lengths - this will change results
#   #slightly, but can't be avoided
#   merge(., length.weight, by='Common.Name') %>%
#   mutate(
#     biomass.kg =ifelse(WL_L_units=='cm' & WL_input_length=='TL'
#                        & WL_W_units=='kg', WL_a*((Length.cm)^WL_b),
#                        ifelse(WL_L_units=='mm' & WL_input_length=='TL'
#                               & WL_W_units=='kg', WL_a*((Length.cm*10)^WL_b),
#                               ifelse(WL_L_units=='cm' & WL_input_length=='TL'
#                                      & WL_W_units=='g', WL_a*((Length.cm)^WL_b)/1000,
#                                      ifelse(WL_L_units=='mm' & WL_input_length=='TL'
#                                             & WL_W_units=='g', WL_a*((Length.cm*10)^WL_b)/1000,
#                                             ifelse(WL_L_units=='mm' & WL_input_length=='SL'
#                                                    & WL_W_units=='g' & LC_type_for_WL=='TYPICAL',
#                                                    WL_a*((LC_a*(Length.cm*10)+LC_b)^WL_b)/1000,
#                                                    ifelse(WL_L_units=='mm' & WL_input_length=='SL'
#                                                           & WL_W_units=='g' & LC_type_for_WL=='REVERSE',
#                                                           WL_a*((((Length.cm*10)-LC_b)/LC_a)^WL_b)/1000,NA))))))) %>%
#   droplevels()


## CCFRP

#  Subset the ready to go data (#20)
ready_data_ccfrp <-  raw_data_ccfrp %>%
  filter(WL_input_length == "TL") %>%
  filter(WL_W_units == "g" & WL_L_units == "cm")






## KELP

#  Subset the ready to go data (#34)
ready_data <- fish_data_kf %>%
  filter(WL_input_length == "TL") %>%
  filter(WL_W_units == "g" & WL_L_units == "cm")

#  Subset the with a different length
notTL_data <- fish_data %>%
  filter(!(WL_input_length == "TL")) %>%
  filter(WL_W_units == "g" & WL_L_units == "cm")

unique(notTL_data$LL_Equation_for_WL)


# Data with not transformation provided
notTL_data_no_formula <- notTL_data %>%
  filter(is.na(LL_Equation_for_WL))








