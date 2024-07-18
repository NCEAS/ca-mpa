# Combine PMEP and MLPA Species Tables

# About --------------------------------------------------------------------------------




# Setup --------------------------------------------------------------------------------
library(tidyverse)


# Read the final authoritative species table
mlpa_sp <- read_csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>% 
  filter(kingdom == "Animalia") %>% 
  select(family, genus, sciname, level, target_status = target_status_standardized) %>% 
  distinct() %>% 
  filter(!is.na(target_status)) %>% 
  filter(!is.na(sciname))

mlpa_lw <- read_csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/lw_parameters_fish.csv")



# Build --------------------------------------------------------------------------------
pmep_assemblage_sp <- pmep_assemblage %>% 
  select(assemblage, sciname = species) %>% 
  distinct() %>% 
  mutate(sciname = recode(sciname,
                          "Hypocritichthys analis" = "Hyperprosopon anale", # https://www.fishbase.se/summary/3630
                          "Seriola dorsalis" = "Seriola lalandi", # https://www.fishbase.se/summary/Seriola-lalandi.html
                          "Urobatis helleri" = "Urobatis halleri", # https://www.fishbase.se/summary/Urobatis-halleri.html
                          "Embiotoca caryi" = "Hypsurus caryi", # https://www.fishbase.se/summary/Hypsurus-caryi.html        
                          "Bodianus pulcher" = "Semicossyphus pulcher", # https://www.fishbase.se/summary/Semicossyphus-pulcher.html
                          "Halichoeres californicus" = "Oxyjulis californica", # https://www.fishbase.se/summary/Oxyjulis-californica.html
                          "Carangoides vinctus" = "Caranx vinctus", # https://www.fishbase.se/summary/Caranx-vinctus.html
                          "Lobotes pacificus" = "Lobotes pacifica" # https://www.fishbase.se/summary/Lobotes-pacifica.html
  ))



mlpa_pmep <- mlpa_sp %>% 
  left_join(pmep_assemblage_sp) %>% 
  # Drop NA values for now
  filter(!is.na(assemblage))



no_match <- pmep_assemblage_sp  %>% 
  filter(!(sciname %in% mlpa_sp$sciname))
