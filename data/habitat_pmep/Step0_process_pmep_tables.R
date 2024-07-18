# Explore & Process PMEP Habitat Data (Biotic Component)
# Cori Lopazanski
# June 2024


# About --------------------------------------------------------------------------------
# Read and clean the Pacific Marine and Estuary Partnership data

# Appendices from PMEP Habitat Report provided by Joe Bizarro

# Appendix 4. Source datasets used to develop Nearshore Zones and CMECS Biotic and Substrate component datasets.
# PMEP Region Key: Salish Sea = SS ; Pacific Northwest = PNW; Central California = CC; Southern California Bight = SCB
# Data Quality Ranking Key: Low= Resolution >1:100,000 map scale/100m resolution, data digitized from expert input/reports, 
# no ground-truthing; Moderate=> 12-100 m gridded or > 1:24,000-100,000 scale, high resolution with no ground-truthing, low 
# resolution with ground-truthing, data compilation with varying resolution data, volunteer field data collection with 
# ground-truthing; High=Resolution<1:24,000 or 12m resolution, ground-truthing. Mixed=Data compilation with mixed levels of 
# data quality; Modeled=modeled dataset, not from remote sensing or ground-truthed information.

# Appendix 7. Qualitative assessment of relative abundance among documented fishes off the Pacific Northwest based on a 
# review of relevant field guides (e.g., Ebert 2003, Love et al. 2002, Lamb and Edgell 2010, Love 2011, Butler et al. 2012, 
# Kells et al. 2016), primary literature, and landings or survey data. Included are: Taxon (Order, Family, Genus, Species), 
# Common Name, and abundance estimates throughout the ecoregion (Overall), in the Core and Seaward Zones, and among the Pacific 
# Northwest subregions. Only fishes with multiple documented records in the ecoregion are included. Blank entries indicate that 
# the species has not been documented in a particular subregion. CAPG = Canadian Border to Point Grenville, PGCL = Point Grenville 
# to Cape Lookout, CLCB = Cape Lookout to Cape Blanco, CBCM = Cape Blanco to Cape Mendocino. A = abundant, C = common, R = rare.
# -- only the final region, CBCM "Cape Blanco to Cape Mendocino" is in CA

# Appendix 8. Depth and habitat associations of documented fishes in the Pacific Northwest Ecoregion based on Love (2011), 
# Lamb and Edgell (2010), Kells et al. (2016), Love et al. (2005), Butler et al. (2012), Ebert (2002), and a review of primary
# literature, fishery independent survey catches, and museum records. Included are: Vertical Zonation (Benthic = benthic and 
# demersal, Midwater, Pelagic, and WC = found throughout the water column), Depth Range (Min-Max), Common Depth, and
# and seafloor habitats. Gravel includes pebbles. SFMI = structure forming marine invertebrates. 1= primary habitat or similar 
# utilization of habitat types for generalist species. 2 = secondary habitat. Habitat associations  are for combined juvenile 
# and adult life stages. Only benthic habitat associations are indicated. Fishes are primarily marine with the following 
# exceptions: Exotic = nonnative species, FW = freshwater usage (X = yes, A = anadramous). S = surface depth. 0 = bottom depth.

# Appendix 9. Qualitative assessment of relative abundance among documented fishes off the Central California based on a review 
# of relevant field guides (e.g., Miller and Lea 1972, Ebert 2003, Love et al. 2002, Love 2011, Butler et al. 2012, Kells et al. 2016), 
# primary literature, and landings or survey data. Included are: Taxon (Order, Family, Genus, Species), Common Name, and abundance 
# estimates throughout the ecoregion (Overall), in the Core and Seaward Zones, and among the Central California subregions. Only fishes 
# with multiple documented records in the ecoregion are included. Blank entries indicate that the species has not been documented in 
# a particular subregion. CMPR = Cape Mendocino to Point Reyes, PRPS = Point Reyes to Point Sur, Point Sur to Point Arguello, 
# PAS = Point Arguello South. A = abundant, C = common, R = rare.

# Appendix 10. Depth and habitat associations of documented fishes in the Central California Ecoregion based on Love (2011), Kells 
# et al. (2016), Love et al. (2005), Butler et al. (2012), Ebert (2002), and a review of primary literature, fishery independent 
# survey catches, and museum records. Included are: Vertical Zonation (Benthic = benthic and demersal, Midwater, Pelagic, and 
# WC = found throughout the water column), Depth Range (Min-Max), Common Depth, and  seafloor habitats. Gravel includes pebbles. 
# SFMI = structure forming marine invertebrates. 1= primary habitat or similar utilization of habitat types for generalist species. 
# 2 = secondary habitat. Habitat associations  are for combined juvenile and adult life stages. Only benthic habitat associations are 
# indicated. Fishes are primarily marine with the following exceptions: Exotic = nonnative species, FW = freshwater usage 
# (X = yes, A = anadramous). S = surface depth. 0 = bottom depth.

# Appendix 11. Qualitative assessment of relative abundance among documented fishes in the Southern California Bight based on a review of 
# relevant field guides (e.g., Miller and Lea 1972, Ebert 2003, Love et al. 2002, Love 2011, Butler et al. 2012, Kells et al. 2016), primary 
# literature, and landings or survey data. Included are: Taxon (Order, Family, Genus, Species), Common Name, and abundance estimates throughout the 
# ecoregion (Overall), in the Core and Seaward Zones, and between the Southern California Bight subregions. Only fishes with multiple documented
# records in the ecoregion are included. Blank entries indicate that the species has not been documented in a particular subregion. PCPV = Point 
# Conception to Palos Verde, PVMX = Palos Verde to Mexican Border. A = abundant, C = common, R = rare.

# Appendix 12. Depth and habitat associations of documented fishes in the Southern California Bight based on Love (2011), 
# Kells et al. (2016), Love et al. (2005), Butler et al. (2012), Ebert (2002), and a review of primary literature, fishery independent
# survey catches, and museum records. Included are: Vertical Zonation (Benthic = benthic and demersal, Midwater, Pelagic, and WC = found 
# throughout the water column), Depth Range (Min-Max), Common Depth, and  seafloor habitats. Gravel includes pebbles. SFMI = structure 
# forming marine invertebrates. 1= primary habitat or similar utilization of habitat types for generalist species. 2 = secondary habitat. 
# Habitat associations  are for combined juvenile and adult life stages. Only benthic habitat associations are indicated. Fishes are 
# primarily marine with the following exceptions: Exotic = nonnative species, FW = freshwater usage (X = yes, A = anadramous). 
# S = surface depth. 0 = bottom depth.

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)
library(purrr)
library(readxl)

# Directories
pmep.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw"
dat.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Appendix_Tables" # Aurora
out.dir <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/processed"

# Read Tables --------------------------------------------------------------------------------
# Read and bind the main tables
bind_assemblages <- function(directory) {
  file_list <- list.files(directory, pattern = "*.csv", full.names = TRUE)
  
  combined_data <- map_df(file_list, function(file) {
    region <- gsub(".*(scb|pnw|cce).*\\.csv", "\\1", basename(file))
    read.csv(file) %>% 
      mutate(region = region) %>% 
      mutate(across(where(is.character), str_trim))
    })

  return(combined_data)
}

directory_path <- "/home/shares/ca-mpa/data/sync-data/habitat_pmep/raw/PMEP_Report_Tables"

pmep_assemblage <- bind_assemblages(directory_path) %>% 
  clean_names() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = region, values_from = value) %>% 
  rename(species = scientific_name)


## Appendix 4. Source datasets ----
a4 <- read_xlsx(file.path(dat.dir, "Appendix_4.xlsx"), skip = 4, trim_ws = T) %>% 
  janitor::clean_names() %>% 
  select(dataset_number:input_into) %>% 
  # Table is split into two - remove the second header set and white space
  filter(!(dataset_number == "Dataset Number")) %>%
  mutate(dataset_number = as.numeric(dataset_number))
  
## Appendix 7. PNW relative abundance ----
a7 <- read_xlsx(file.path(dat.dir, "Appendix_7.xlsx"), skip = 7, trim_ws = F) %>% 
  clean_names() %>% 
  filter(!(taxon == "Taxon")) %>% # drop additional headers
  mutate(indent = str_extract(taxon, "^\\s*") %>% str_length()) %>% 
  mutate(taxon = str_to_sentence(str_trim(taxon)),
         common_name = str_to_sentence(str_trim(common_name))) %>% 
  mutate(order = if_else(indent == 0, taxon, NA_character_),
         family = if_else(indent == 2, taxon, NA_character_)) %>% 
  fill(order, family) %>% 
  filter(indent == 4) %>% select(!indent) %>% 
  select(order, family, species = taxon, common_name, everything()) %>% 
  mutate(pnw = 1)

## Appendix 8. PNW Habitat ----
a8 <- read_xlsx(file.path(dat.dir, "Appendix_8.xlsx"), skip = 5, trim_ws = F) %>% 
  clean_names() %>% 
  filter(!(taxon == "Taxon")) %>% # drop additional headers
  mutate(indent = str_extract(taxon, "^\\s*") %>% str_length()) %>% 
  mutate(taxon = str_to_sentence(str_trim(taxon)),
         common_name = str_to_sentence(str_trim(common_name))) %>% 
  mutate(order = if_else(indent == 0, taxon, NA_character_),
         family = if_else(indent == 2, taxon, NA_character_)) %>% 
  fill(order, family) %>% 
  filter(indent == 4) %>% select(!indent) %>% 
  select(order, family, species = taxon, common_name, everything()) %>% 
  mutate(pnw = 1)

## Appendix 9. Central relative abundance ----
a9 <- read_xlsx(file.path(dat.dir, "Appendix_9.xlsx"), skip = 7, trim_ws = F) %>% 
  clean_names() %>% 
  select(taxon:pas) %>% 
  filter(!(taxon == "Taxon")) %>% # drop additional headers
  mutate(indent = str_extract(taxon, "^\\s*") %>% str_length()) %>% 
  mutate(taxon = str_to_sentence(str_trim(taxon)),
         common_name = str_to_sentence(str_trim(common_name))) %>% 
  mutate(taxon = case_when(taxon == "Scorpionfishes" ~ "Scorpaeniformes",
                           taxon == "Bigeyes" ~ "Priacanthidae",
                           taxon == "Wrasses" ~ "Labridae",
                           taxon == "Tube blennies" ~ "Chaenopsidae",
                           TRUE ~ taxon)) %>% 
  mutate(order = if_else(indent == 0, taxon, NA_character_),
         family = if_else(indent == 2, taxon, NA_character_)) %>% 
  fill(order, family) %>% 
  filter(indent >= 4) %>% select(!indent) %>% 
  select(order, family, species = taxon, common_name, everything()) %>% 
  mutate(cce = 1)

## Appendix 10. Central habitat ----
a10 <- read_xlsx(file.path(dat.dir, "Appendix_10.xlsx"), skip = 5, trim_ws = F) %>% 
  clean_names() %>% 
  filter(!(taxon == "Taxon")) %>% # drop additional headers
  mutate(indent = str_extract(taxon, "^\\s*") %>% str_length()) %>% 
  mutate(taxon = str_to_sentence(str_trim(taxon)),
         common_name = str_to_sentence(str_trim(common_name))) %>% 
  mutate(order = if_else(indent == 0, taxon, NA_character_),
         family = if_else(indent == 2, taxon, NA_character_)) %>% 
  fill(order, family) %>% 
  filter(indent == 4) %>% select(!indent) %>% 
  select(order, family, species = taxon, common_name, everything()) %>% 
  mutate(cce = 1)

## Appendix 11. South relative abundance ----
a11 <- read_xlsx(file.path(dat.dir, "Appendix_11.xlsx"), skip = 7, trim_ws = F) %>% 
  clean_names() %>% 
  filter(!(taxon == "Taxon")) %>% # drop additional headers
  mutate(indent = str_extract(taxon, "^\\s*") %>% str_length()) %>% 
  mutate(taxon = str_to_sentence(str_trim(taxon)),
         common_name = str_to_sentence(str_trim(common_name))) %>% 
  mutate(order = if_else(indent == 0, taxon, NA_character_),
         family = if_else(indent == 2, taxon, NA_character_)) %>% 
  fill(order, family) %>% 
  filter(indent == 4) %>% select(!indent) %>% 
  select(order, family, species = taxon, common_name, everything()) %>% 
  mutate(scb = 1)


## Appendix 12. South habitat ----
a12 <- read_xlsx(file.path(dat.dir, "Appendix_12.xlsx"), skip = 5, trim_ws = F) %>% 
  clean_names() %>% 
  filter(!(taxon == "Taxon")) %>% # drop additional headers
  mutate(indent = str_extract(taxon, "^\\s*") %>% str_length()) %>% 
  mutate(taxon = str_to_sentence(str_trim(taxon)),
         common_name = str_to_sentence(str_trim(common_name))) %>% 
  mutate(order = if_else(indent == 0, taxon, NA_character_),
         family = if_else(indent == 2, taxon, NA_character_)) %>% 
  fill(order, family) %>% 
  filter(indent == 4) %>% select(!indent) %>% 
  select(order, family, species = taxon, common_name, everything()) %>% 
  mutate(scb = 1)

# Compare habitats across regions ---------------------------------------------------------------
# A8, A10, A12

# Combine habitat tables
pmep_habitat <- full_join(a8, a10) %>% 
  full_join(a12)

# Extract any duplicates to explore - mostly depth differences but some habitat discrepancies
habitat_mismatch <- pmep_habitat %>% 
  group_by(species) %>% 
  filter(n() > 1)

# Join the assemblage information
pmep_habitat2 <- pmep_habitat %>% 
  left_join(pmep_assemblage, by = c("species"))







# Combine the abundance and habitat tables
pnw <- a7 %>% 
  select(order:seaward, cbcm, region) %>% 
  full_join(., a8) %>% 
  filter(!is.na(cbcm)) # remove species never documented in CBCM

cce <- full_join(a9, a10)

scb <- full_join(a11, a12)

# Explore overlapping species
pnw_sp <- pnw %>% select(order, family, species, common_name)
cce_sp <- cce %>% select(order, family, species, common_name)
scb_sp <- scb %>% select(order, family, species, common_name)

pmep_sp <- full_join(pnw_sp, cce_sp) %>% 
  full_join(scb_sp) %>% 
  distinct() 


pnw_only <- anti_join(pnw_sp, cce_sp) %>% 
  anti_join(scb_sp)
cce_only <- anti_join(cce_sp, pnw_sp) %>% 
  anti_join(scb_sp)
scb_only <- anti_join(scb_sp, cce_sp) %>% 
  anti_join(pnw_sp)

# Verify taxonomy ----------------------------------------------------------------------

# Read taxa from fishbase and sealifebase
sp_fb <- rfishbase::load_taxa("fishbase") %>% 
  mutate(database = "fishbase") %>% 
  dplyr::select(database, SpecCode, Species, Genus, Family, Order, Class) %>% 
  as.data.frame()

sp_slb <- rfishbase::load_taxa("sealifebase") %>% 
  mutate(database = "sealifebase") %>% 
  dplyr::select(database, SpecCode, Species, Genus, Family, Order, Class)

fb_all <- full_join(sp_fb, sp_slb) 

rm(sp_fb, sp_slb)

# Format species_key ---------------------------------------------------------------

# Identify the species that are not in the full fishbase/sealifebase list
species_fix <- pmep_sp %>%
  filter(!(species %in% fb_all$Species)) 

# Correct species names
pmep_sp <- pmep_sp %>% 
  mutate(species = recode(species,
    "Hypocritichthys analis" = "Hyperprosopon anale", # https://www.fishbase.se/summary/3630
    "Seriola dorsalis" = "Seriola lalandi", # https://www.fishbase.se/summary/Seriola-lalandi.html
    "Urobatis helleri" = "Urobatis halleri", # https://www.fishbase.se/summary/Urobatis-halleri.html
    "Embiotoca caryi" = "Hypsurus caryi", # https://www.fishbase.se/summary/Hypsurus-caryi.html        
    "Bodianus pulcher" = "Semicossyphus pulcher", # https://www.fishbase.se/summary/Semicossyphus-pulcher.html
    "Halichoeres californicus" = "Oxyjulis californica", # https://www.fishbase.se/summary/Oxyjulis-californica.html
    "Carangoides vinctus" = "Caranx vinctus", # https://www.fishbase.se/summary/Caranx-vinctus.html
    "Lobotes pacificus" = "Lobotes pacifica", # https://www.fishbase.se/summary/Lobotes-pacifica.html
  ))

# Two species remain that are verified as correct or entry doesn't exist
# "Sebastes crocotulus"
# "Cephalopholis colonus" # https://www.fishbase.se/summary/Cephalopholis-colonus.html

# Identify family incorrect
family_fix <- pmep_sp %>% 
  filter(!(family %in% fb_all$Family))

pmep_sp <- pmep_sp %>% 
  mutate(family = recode(family,
                         "Scorpaenichthyidae" = "Jordaniidae", 
                         "Platyrhynidae" = "Platyrhinidae")) %>% 
  mutate(genus = sub("([A-Za-z]+).*", "\\1", species))



