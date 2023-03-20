#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 17, 2023

rm(list=ls())

#load required packages
require(dplyr)
require(stringr)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

ccfrp_raw <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_derived_data_tables_DataONE/CCFRP_derived_effort_table.csv"))
kelp_forest_raw <- read.csv(file.path(datadir, "/monitoring_kelp/MLPA_kelpforest_fish.4.csv"))

#load taxonomy lookup table
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv")

#load lw params
params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv")

################################################################################
#prep conversion function

a_prime_conversion <- tribble(
  ~unit_length, ~unit_weight,  ~a_coeff, ~b_coeff,
  "mm", "g",  10, 1,
  "cm", "kg", 1000, NA,
  "mm", "mg", 10, 1/1000,
  "mm", "kg", 10, 1000
)


# filter the necessary parameters

convert_dat <- function(params, data){
  
  convert_params <- params %>%
  filter(!(is.na(WL_a))) %>% # remove rows without conversion
  filter(ScientificName_accepted %in% unique(data$sciname))

  data %>% left_join(convert_params, by = c("sciname"="ScientificName_accepted"), multiple = "all")
}


# Compute from the cm inputs the type of length and units needed by the formula 
bio_fun <- function(data_with_params) {
  data_with_params %>%
  rename(TL_cm = fish_tl)%>%
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
}


################################################################################
#process kelp forest

kelp_code <- taxon_tab %>% filter(habitat=="Kelp forest")

kelp_forest_process1 <- left_join(kelp_forest_raw, kelp_code, by=(c("classcode"="habitat_specific_code")))

#estimate biomass

kelp_dat <- convert_dat(params_tab, kelp_forest_process1)
kelp_out <- bio_fun(kelp_dat)









