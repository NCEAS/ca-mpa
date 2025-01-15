# Visualize the post-MPA catch

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(janitor)
library(sf)
library(rfishbase)

# Directories
basedir <- "/Users/lopazanski/Library/CloudStorage/GoogleDrive-lopazanski@ucsb.edu/Shared drives/NCEAS MPA network assessment/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
gisdir <- file.path(basedir, "gis_data/processed")
datadir <- "analyses/2performance_fisheries/analyses/blocks/data"
fishdir <- "/Users/lopazanski/Documents/github/nceas/CDFW-fishing-data"

# Read data
#mpas <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
#blocks <- readRDS(file.path(datadir, "blocks_by_mlpa_region_w_mpa_stats.Rds"))
#landings_raw <- readRDS(file.path(fishdir, "CDFW_2000_2020_landings_receipts.Rds"))
#sp_key <- readRDS(file.path(fishdir, "CDFW_species_key.Rds"))# This seems incomplete
sp_key <- read_csv(file.path(fishdir, "CDFW_species_key.csv")) %>% # Loads species key & joins fishbase taxa info
  left_join(., rfishbase::load_taxa(collect = T), by = c("sci_name" = "Species")) %>% 
  janitor::clean_names()

# Fishery groups
groups <- data.frame(fishery_group_num = c(1:9),
                     fishery_group = c("coastal pelagic species",
                                       "salmonids",
                                       "groundfish",
                                       "gamefish",
                                       "highly migratory species",
                                       "market squid",
                                       "echinoderms",
                                       "dungeness crab",
                                       "other crustaceans"))


# Create species groupings dataframe
data <- sp_key %>% 
  mutate(fishery = case_when(
    # 1. Coastal Pelagic Species
    grepl("anchovy|sardine|mackerel", comm_name) ~ "cps", #anchovy, sardine, mackerel
    # 2. Salmonids (including trout as written)
    grepl("Oncorhynchus", sci_name) ~ "salmonid", # salmonids
    # 3. Groundfish
    spp_code_num == 153 ~ "leopard_shark",
    grepl("Sanddab|sanddab", comm_name) ~ "sanddab", 
    grepl("Flounder|flounder|Turbot|turbot", comm_name) ~ "flounder_turbot", # Flounder & turbots
    grepl("Sole|sole", comm_name) ~ "soles", # All soles
    spp_code_num == 222 ~ "ca_halibut", 
    grepl("Cabezon|scorp|sculp", comm_name) ~ "cabezon", # Cabezon, scorpionfish, sculpin
    spp_code_num == 195 ~ "lingcod", 
    spp_code_num == 209 ~ "petrale_sole", 
    spp_code_num == 221 ~ "pacific_halibut", 
    spp_code_num == 495 ~ "north_pacific_hake", # (whiting)
    grepl("rockfish", comm_name) ~ "rockfish", # (Sebastes spp)
    spp_code_num == 190 ~ "sablefish",
    spp_code_num == 152 ~ "spiny_dogfish", 
    grepl("Dover|Rex", comm_name) ~ "dover_rex_sole", # Dover & Rex Sole
    grepl("Thorny|thorny", comm_name) ~ "thornyheads", 
    # 4. Gamefish
    grepl("Serranid|Epinephelid", family)|grepl("Paralabrax", sci_name)|grepl("rouper",comm_name) ~ "seabass", # (Serranidae, Epinephelinae)
    spp_code_num == 3 ~ "bonito",
    spp_code_num == 145 ~ "sheephead",
    spp_code_num == 130 ~ "barracuda",
    # 5. Highly Migratory Species
    grepl("Swordfish", comm_name) ~ "swordfish",
    grepl("tuna", comm_name)|grepl("Thun", sci_name) ~ "tunas",
    grepl("Alop", sci_name) ~ "thresher_sharks", # Alopias spp.
    spp_code_num == 151 ~ "shortfin_mako", # (Isurus oxyrinchus)
    spp_code_num == 167 ~ "blue_shark", # (Prionace glauca)
    spp_code_num == 158 ~ "hammerhead_sharks", # (Sphyrna spp.) 
    # 6. Market Squid
    spp_code_num == 711 ~ "market_squid",
    # 7. Echinoderms
    grepl("urch", comm_name) ~ "urchin", 
    grepl("cucu", comm_name) ~ "sea_cucumber",
    # 8. Dungeness Crab
    spp_code_num == 800 ~ "dungeness_crab",
    # 9. Other Crustaceans
    grepl("Cancer", sci_name) ~ "rock_crab",
    spp_code_num == 820 ~ "spiny_lobster",
    spp_code_num == 815 ~ "spot_prawn",
    spp_code_num == 812 ~ "pink_shrimp")) %>% 
  select(spp_code_num, comm_name, sci_name, family, fishery) %>% 
  mutate(fishery_group_num = case_when(
    fishery == "cps" ~ 1,
    fishery == "salmonid" ~ 2,
    fishery %in% c("leopard_shark", "sanddab", "flounder_turbot", "soles", "ca_halibut", 
                   "cabezon", "lingcod", "petrale_sole","pacific_halibut", 
                   "north_pacific_hake", "rockfish","sablefish", "spiny_dogfish",  
                   "dover_rex_sole", "thornyheads") ~ 3,
    fishery %in% c("seabass", "bonito", "sheephead", "barracuda") ~ 4,
    fishery %in% c("swordfish", "tunas", "thresher_sharks", "shortfin_mako",
                   "blue_shark", "hammerhead_sharks") ~ 5,
    fishery == "market_squid" ~ 6,
    fishery %in% c("urchin", "sea_cucumber") ~ 7,
    fishery == "dungeness_crab" ~ 8,
    fishery %in% c("rock_crab", "spiny_lobster", "spot_prawn", "pink_shrimp") ~ 9)) %>% 
  left_join(., groups)


saveRDS(data, file = file.path(datadir, "species_fishery_group_key.Rds"))
