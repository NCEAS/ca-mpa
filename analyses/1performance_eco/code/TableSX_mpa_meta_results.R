#title: "MPA-level meta analysis results table"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
mpa_meta_results <- readRDS(file.path(dat_path, "mpa_level_meta_results.Rds")) 


################################################################################
#extract relevant model output
library(dplyr)
library(purrr)

# Define a function to safely extract 'habitat'
extract_habitat <- safely(function(x) x[[1]][["data"]][["habitat"]])

# Apply the function to each element in 'meta_results'
mpa_meta_out <- mpa_meta_results %>%
  #get the habitats included in the analysis for each MPA
  mutate(tau2 = map_dbl(meta_result, ~ pluck(.x, "tau2")),
         Q = map_dbl(meta_result, ~ pluck(.x, "QE")),
         habitats = map(seq_along(meta_result), ~ meta_result[[.x]][["data"]][["habitat"]]),
         #convert habitats to string
         habitats = map_chr(habitats, ~ str_c(.x, collapse = ", ")),
         #extract years
         years = map(seq_along(meta_result), ~ meta_result[[.x]][["data"]][["year"]]),
         #convert habitats to string
         years = map_chr(years, ~ str_c(.x, collapse = ", "))
         )%>%
  arrange(state_region, affiliated_mpa, target_status)%>%
  #numerate each unique MPA
  mutate(MPA_number = group_indices(., affiliated_mpa))%>%
  dplyr::select(MPA_number, everything())
 
