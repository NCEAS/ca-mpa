# Prepare reference table for the length - Weight relationship to compute biomass from raw data
# Julien Brun, brun@nceas.ucsb.edu

# Relationship reference: https://www.fishbase.de/manual/fishbasethe_length_weight_table.htm



# Load packages
# install.packages(librarian)
librarian::shelf(tidyverse, readxl)

# Set the path and file names
data_dir <- "/home/shares/ca-mpa/data/sync-data/species_traits/raw"
kelp_file <- "kelp_forest_biomass_params.xlsx"
ccfrp_file_old <- "old/CCFRP_biomass_params.csv"         # file we got through Shelby
ccfrp_file <- "CCFRP_Biomass_Conversion_20220206.xlsx"   # file sent to us by Rachel Brooks, rachel.brooks@sjsu.edu, on 11/30/2022


#### Read the data in ####

# Kelp data
raw_data_kf <- read_xlsx(file.path(data_dir, kelp_file), sheet = "species_attribute_table") %>%
  mutate(source = "kelp")

# old CCFRP (Shelby)
raw_data_ccfrp_old <- read_csv(file.path(data_dir, ccfrp_file_old)) %>%
  mutate(source = "ccfrp")

# Lastest CCFRP (Rachel)
raw_parameters_ccfrp <- read_xlsx(file.path(data_dir, ccfrp_file), sheet = "CCFRP_Biomass_Conversion_Table_") %>%
  mutate(source = "ccfrp") %>% 
  rename(units = ...16)  # fix column with no header in original file


# Create a fake length data set  -- TEMPORARY to be replaced by real data
length_data <- tibble(
  species_scientificname = sample(na.omit(raw_parameters_ccfrp$ScientificName_accepted), 100, replace = TRUE),
  TL_cm = rnorm(100, mean = 20, sd = 5)
  )





#### DATA CONVERSION #####

# Data conversion
# We have several main cases:
#  - We have the total length (TL):
#     - We have the right unit for a and b: g and cm => done
#     - We have the wrong units for a and b => need to transform using formulas
#  - We have another type of length:
#     - We need to first transform the length into the total one using the formula in column "LL_Equation_for_WL"
# 
# Main formula: W = a * L^b

# Unit transformation (source: Fishbase
# a'(cm, g) = a (mm, g)*10^b
# a'(cm, g) = a (cm, kg)*1000
# a'(cm, g) = a (mm, mg)*10^b/1000
# a'(cm, g) = a (mm, kg)*10^b*1000

# Create a tibble from that
a_prime_conversion <- tribble(
  ~unit_length, ~unit_weight,  ~a_coeff, ~b_coeff,
  "mm", "g",  10, 1,
  "cm", "kg", 1000, NA,
  "mm", "mg", 10, 1/1000,
  "mm", "kg", 10, 1000
)


#### Length conversion ####


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


# filter the necessary parameters
conversion_parameters <- raw_parameters_ccfrp %>%
  filter(!(is.na(WL_a))) %>% # remove rows without conversion
  filter(ScientificName_accepted %in% unique(length_data$species_scientificname))

# Join the parameters to the data (might be expensive with large data sets)
data_ccfrg_param <- length_data %>% 
  left_join(conversion_parameters, by = c("species_scientificname"="ScientificName_accepted"))

# Compute from the cm inputs the type of length and units needed by the formula 
data_length_ccfrg <-  data_ccfrg_param %>%
  mutate(length_to_use = case_when(
    WL_input_length == "TL" & WL_L_units == "cm" ~ TL_cm,
    WL_input_length == "TL" & WL_L_units == "mm" ~ TL_cm *10,
    WL_input_length == "SL" & WL_L_units == "cm" ~ (TL_cm - LC_b)/LC_a,
    WL_input_length == "SL" & WL_L_units == "mm" ~ (TL_cm - LC_b)/LC_a *10,
    WL_input_length == "FL" & WL_L_units == "cm" ~ TL_cm,
    WL_input_length == "FL" & WL_L_units == "mm" ~ TL_cm *10,
    WL_input_length == "DW" & WL_L_units == "cm" ~ TL_cm,
    WL_input_length == "DW" & WL_L_units == "mm" ~ TL_cm *10,
    TRUE ~ NA   #-9999 # for debugging, to be switched to NA
  ),
  # Compute the weight
  weight_g = case_when(
    WL_W_units == "g" ~ WL_a*length_to_use^WL_b,
    WL_W_units == "kg" ~ (WL_a*length_to_use^WL_b)/1000,
    TRUE ~ NA #-9999 # for debugging, to be switched to NA
  )
)













#### KELP ####

# #  Subset the ready to go data (#34)
# ready_data <- fish_data_kf %>%
#   filter(WL_input_length == "TL") %>%
#   filter(WL_W_units == "g" & WL_L_units == "cm")
# 
# #  Subset the with a different length
# notTL_data <- fish_data %>%
#   filter(!(WL_input_length == "TL")) %>%
#   filter(WL_W_units == "g" & WL_L_units == "cm")
# 
# unique(notTL_data$LL_Equation_for_WL)
# 
# 
# # Data with not transformation provided
# notTL_data_no_formula <- notTL_data %>%
#   filter(is.na(LL_Equation_for_WL))

# # Quick comparison checks
# 
# # all_equal(raw_data_ccfrp_old, raw_data_ccfrp)
# 
# # filter for fish on keep columns of interest
# fish_data_kf <- raw_data_kf %>% 
#   filter(group == "fish") %>%
#   select(data_type, group, pisco_classcode, genus, species, common_name, ScientificName_accepted, 
#          juv_size_cutoff, contains("WL"), contains("LC"), Max_Length_fishbase, Size_Mature_cm) %>%
#   mutate(source = "kf")
# 
# # Check for sie difference
# raw_diff <- anti_join( raw_data_ccfrp, fish_data_kf) 
# 
# raw_all <- full_join(raw_data_ccfrp, fish_data_kf)






