# Gather and process taxonomy tables for each habitat
# Cori Lopazanski
# August 2023

# About --------------------------------------------------------------------------------
# This script gathers all of the taxanomoy tables from Surf Zone, Rocky Intertidal,
# Kelp Forest, CCFRP, and Deep Reef. 

# The purpose of this script is to create a single lookup table with the full
# taxonomy of all unique species recorded by each habitat with the habitat-specific
# codes that link the observation data with the taxonomy. 

# Taxa should not be duplicated within a habitat (distinct rows only), but
# they may be duplicated across habitats at this stage. We need to keep track
# of habitat specific codes so that we can join the taxonomy with each habitat's raw data. 
# Note that there may appear to be duplicates, but these would be to correct misspelings and
# other character issues.

# OUTPUT: full_taxon_table_new.csv

# Setup --------------------------------------------------------------------------------

rm(list=ls())

library(janitor)
require(dplyr)
require(stringr)
require(tidyr)

export_path <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

# Load taxonomy tables ------------------------------------------------------------------

# Surf zone has multiple sampling methods each with a unique taxonomy table. 
# We are only using the seine data so will load that table only. 

surf_taxon1 <- read.csv(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","surf_zone_fish_seine_species.csv"))

# Rocky intertidal has two survey types (biodiversity and long term) each with 
# a unique taqxonomy table. We are only using the long term data here so will
# not load the biodiversity table for now. 
rocky_taxon1 <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","RockyIntertidal-LongTerm-Taxonomy.xlsx"), 
  sheet=1, skip = 0, na=c("NA", "", "NULL"))

# Full taxonomy for all methods
kelp_taxon1 <-readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables/Kelp-Taxonomy.xlsx"), 
  sheet=1, skip = 0, na="NA")

# Full taxonomy for all methods
ccfrp_taxon1 <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables/CCFRP_Taxonomy.xlsx"), 
  sheet=2, skip = 0, na=c("NA", "?"))

# Full taxonomy for all methods
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables"
input_file <- "DeepReef-ROV-Taxonomy.xlsx"  
deep_reef_taxon1 <- readxl::read_excel(
  file.path(data_path,input_file),
  sheet = 1, na = c("NA", "N/A", "", " ")) %>%
  janitor::clean_names()

# Many species in deep reef dataset are not in the taxonomy table -- read here to complete
deep_reef_data <- read.csv(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/ROV_Dataset/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), 
  na = c("N/A", "", " ")) %>% clean_names() 


# Clean surf zone ------------------------------------------------------------------

surf_taxon <- surf_taxon1 %>%
  mutate(habitat = "Surf Zone") %>%
  dplyr::select(habitat, habitat_specific_code = "species_code", habitat_specific_spp_name = "ScientificName_accepted",
                Kingdom, Phylum, Class, Order, Family, Genus = genus, Species = species, target_status = Targeted) %>%
  distinct() %>% 
  # Fix incorrect species code
  mutate(habitat_specific_code = if_else(habitat_specific_spp_name == "Genyonemus lineatus", "GELI", habitat_specific_code)) %>% 
  mutate(habitat_specific_spp_name = case_when(
    habitat_specific_code == "FFUN" ~ "Unidentified flatfish", 
    habitat_specific_code == "HALI" ~ "Unidentified halibut",
    habitat_specific_code == "RFYOY" ~"Rockfish young of year",
    TRUE ~ habitat_specific_spp_name)) %>% 
  # Drop the NA row with no species info
  filter(!is.na(habitat_specific_code)) 

# add rows that are in the raw data but not in the taxonomy table
HALI <-  data.frame(
    habitat = "Surf Zone", 
    habitat_specific_code = "HALI",
    habitat_specific_spp_name = "Unidentified halibut",
    Kingdom = "Animalia",
    Phylum = "Chordata",
    Class = "Actinopteri",
    Order = "Pleuronectimformes",
    Family = "Paralichthyidae",
    Genus = NA,
    Species = NA,
    target_status = NA
  )

unspecified <-  data.frame(
  habitat = "Surf Zone", 
  habitat_specific_code = "unspecified",
  habitat_specific_spp_name = "unidentified",
  Kingdom = "Animalia",
  Phylum = "Chordata",
  Class = NA,
  Order = NA,
  Family = NA,
  Genus = NA,
  Species = NA,
  target_status = NA
)

RFYOY <-  data.frame(
  habitat = "Surf Zone", 
  habitat_specific_code = "RFYOY",
  habitat_specific_spp_name = "Rockfish young of year",
  Kingdom = "Animalia",
  Phylum = "Chordata",
  Class = "Actinopteri",
  Order = "Perciformes",
  Family = "Sebastidae",
  Genus = "Sebastes",
  Species = NA,
  target_status = NA
)

FFUN <-   data.frame(
  habitat = "Surf Zone", 
  habitat_specific_code = "FFUN",
  habitat_specific_spp_name = "Unidentified flatfish",
  Kingdom = "Animalia",
  Phylum = "Chordata",
  Class = "Actinopteri",
  Order = "Pleuronectimformes",
  Family = NA,
  Genus = NA,
  Species = NA,
  target_status = NA
)

surf_taxon <- rbind(surf_taxon, HALI, unspecified, RFYOY, FFUN)

surf_taxon$Species[surf_taxon$Species %in% c("sp.", "spp")] <- NA
surf_taxon$Genus[surf_taxon$Genus == surf_taxon$Family] <- NA

# Clean rocky intertidal ------------------------------------------------------------------

rocky_taxon <- rocky_taxon1 %>%
  mutate(habitat = "Rocky intertidal",
         target_status = NA) %>%
  dplyr::select(habitat, habitat_specific_code = "marine_species_code", habitat_specific_spp_name = "marine_species_name",
                Kingdom, Phylum, Class, Order, Family, Genus, Species, target_status) %>% 
  mutate(Species = str_to_lower(word(Species, -1))) %>% 
  mutate(Species = case_when(habitat_specific_spp_name == "haliclona cinerea" ~ "cinerea", 
                             habitat_specific_spp_name == "halichondria panicea" ~ "panicea", 
                             habitat_specific_spp_name == "norrisia norrisi" ~ "norrisi",
                             TRUE ~ Species)) 

rocky_taxon$Phylum <-  gsub("[()]", "", rocky_taxon$Phylum)  
rocky_taxon$Species <-  gsub("[()]", "", rocky_taxon$Species)  


# Clean kelp forest -----------------------------------------------------------------
kelp_taxon <- kelp_taxon1 %>%
              mutate(habitat = "Kelp forest") %>%
                dplyr::select(habitat, habitat_specific_code = "pisco_classcode", habitat_specific_spp_name = "ScientificName_accepted",
                Kingdom, Phylum, Class, Order, Family, Genus = genus, Species = species, target_status = Targeted) %>%
              distinct() %>% 
  # Fix Sarda chiliensis 
  mutate(Species = recode(Species, "chiliensis chiliensis" = "chiliensis")) %>%
  # Fix entries with dual names
  mutate(Species = if_else(str_detect(Genus, "/"), sub("\\/.*", "", Species), Species)) %>% # if there's a / in Genus, remove everything after the / in Species
  mutate(Genus = sub("\\/.*", "", Genus)) %>%  # remove everything after the / in Genus
  # All remaining species names with special characters convert to spp
  mutate(Species = if_else(grepl("\\s|[^A-Za-z0-9]", Species), "spp", Species))

kelp_taxon$Species[kelp_taxon$Species == "spp"] <- NA
kelp_taxon$Genus[kelp_taxon$Genus == kelp_taxon$Family] <- NA
kelp_taxon$Genus[kelp_taxon$Genus == kelp_taxon$Order] <- NA
kelp_taxon$Genus[kelp_taxon$Genus == kelp_taxon$Class] <- NA



# Clean CCFRP ----------------------------------------------------------------------

CCFRP_taxon <- ccfrp_taxon1%>%
  mutate(habitat = "Rocky reef")%>%
  dplyr::select(habitat, habitat_specific_code = "Species_Code", habitat_specific_spp_name = "Scientific_Name",
                Kingdom, Phylum, Class, Order, Family, Genus, Species = species, target_status=Fished) %>%
  distinct() %>% 
  mutate(across(everything(), str_trim)) %>% 
  mutate(Species = recode(Species, "diaconua" = "diaconus")) %>% 
  mutate(target_status = case_when(habitat_specific_spp_name == "Atherinops affinis" ~ "targeted", # targeted in surf zone
                                   habitat_specific_spp_name == "Atherinopsidae spp" ~ "nontargeted", # nontargeted in kelp forest
                                   habitat_specific_spp_name == "Atherinopsis californiensis" ~ "targeted", #not in other habitats
                                   habitat_specific_spp_name == "Pteroplatytrygon violate" ~ "nontargeted", # not in other habitats but its a stingray...
                                   habitat_specific_spp_name == "Squalus acanthias" ~ "targeted", # targeted in deep and kelp
                                   T~target_status))


# Clean Deep reef ---------------------------------------------------------------------

# Extract taxonomy from deep reef data that's missing in taxon table
# (Some are new species, some are misspellings of known species)
deep_reef_data_taxa <- deep_reef_data %>% 
  mutate(across(everything(), str_trim)) %>% # this is the one step that must be repeated everywhere for deep
  select(habitat_specific_spp_name = scientific_name) %>% distinct() %>% 
  filter(!(habitat_specific_spp_name %in% deep_reef_taxon1$scientific_name)) %>% 
  mutate(habitat = "Deep reef") %>% 
  # Build out taxa information 
  mutate(scientific_name = str_trim(str_to_sentence(str_replace_all(habitat_specific_spp_name, "[^[:alnum:]]", " ")))) %>% 
  # Correct misspellings
  mutate(scientific_name = recode(scientific_name,
                                  "Attractoscion nobilis" =  "Atractoscion nobilis",
                                  "Hexagrammus decagrammus" =  "Hexagrammos decagrammus",            
                                  "Hydrolagus collei" = "Hydrolagus colliei",
                                  "Oxylebius rictus" = "Oxylebius pictus",
                                  "Racochilus vacca" = "Rhacochilus vacca",
                                  "Rhacocohils vacca" = "Rhacochilus vacca",
                                  "Sebases serranoides" = "Sebastes serranoides",                     
                                  "Sebaste miniatus"  = "Sebastes miniatus",    
                                  "Sebastes dalii" = "Sebastes dallii",
                                  "Sebastes hopskini" = "Sebastes hopkinsi",                    
                                  "Sebastes letiginosus" = "Sebastes lentiginosus", 
                                  "Sebastes minatus"  = "Sebastes miniatus",                        
                                  "Sebastes paucipinis" = "Sebastes paucispinis",                   
                                  "Sebastes pauscipinis" = "Sebastes paucispinis",
                                  "Sebastes services" =  "Sebastes serriceps", 
                                  "Starry rockfish" =  "Sebastes constellatus", 
                                  "Torpedo california" = "Torpedo californica",
                                  "Unidentified sebastes sp" = "Sebastes spp")) %>% 
  separate(scientific_name, into = c("genus", "species"), sep = " ", extra = "merge", remove = F) %>% 
  # Add other taxonomy from main taxon table if it exists
  left_join(deep_reef_taxon1) %>% 
  # Fix the family
  mutate(family = case_when(habitat_specific_spp_name == "Unidentified Macrouridae"~ "Macrouridae",
                            habitat_specific_spp_name == "Pleuronectidae spp."~ "Pleuronectidae",
                            TRUE ~ family)) %>% 
  mutate(genus = if_else(habitat_specific_spp_name %in% c("Unidentified Macrouridae", "Pleuronectidae spp."), NA, genus),
         species = if_else(habitat_specific_spp_name %in% c("Unidentified Macrouridae", "Pleuronectidae spp."), NA, species)) %>% 
  select(habitat, habitat_specific_spp_name, 
         Kingdom = kingdom, Phylum = phylum, Class = class, Order = order, Family = family, 
         Genus = genus, Species = species, target_status=targeted)

# Process main taxon table
deep_reef_taxon2 <- deep_reef_taxon1%>%
  mutate(across(everything(), str_trim)) %>% # this is the one step that must be repeated everywhere for deep
  mutate(habitat = "Deep reef")%>%
  mutate(habitat_specific_code = NA) %>% # the original ones were pisco anyway, and we wont use this for matching
  mutate(across(everything(), str_trim)) %>% 
  dplyr::select(habitat, habitat_specific_spp_name = "scientific_name", habitat_specific_code,
                Kingdom = kingdom, Phylum = phylum, Class = class, Order = order, Family = family, 
                Genus = genus, Species = species, target_status=targeted) %>%
  distinct() %>% 
  filter(!is.na(habitat_specific_spp_name)) %>% 
  # Add missing taxa to taxon table 
  full_join(deep_reef_data_taxa) %>% 
  # Remove synonyms in species names
  mutate(Species = if_else(str_detect(Species, "/syn./"), word(Species, 1), Species)) %>% 
  # Recode "Synodus lucioceps or Ophiodon elongatus" (species listed as lucioceps)
  mutate(Species = if_else(habitat_specific_spp_name == "Synodus lucioceps or Ophiodon elongatus", NA, Species)) %>% 
  # Recode "Metacarcinus magister /syn./ Cancer magister - genus is incorrect 
  mutate(Genus = if_else(Species == "magister", "Metacarcinus", Genus)) %>% 
  # Recode "Octopus rubescens" - genus, order, family is incorrect 
  mutate(Genus =  if_else(habitat_specific_spp_name == "Octopus rubescens", "Octopus", Genus),
         Family = if_else(habitat_specific_spp_name == "Octopus rubescens", "Octopodidae", Family),
         Order =  if_else(habitat_specific_spp_name == "Octopus rubescens", "Octopoda", Order)) %>% 
  # Correct misspellings
  mutate(Species = if_else(habitat_specific_spp_name == "Psolus chitonoides", "chitinoides", Species)) %>% 
  mutate(Family = recode(Family,
                         "Dendrotidae" = "Dendronotidae",
                         "Haliperidae" = "Halipteridae",
                         "Fisurellidae" = "Fissurellidae",
                         "Pleurobranchaeidae" = "Pleurobranchidae",
                         "Poranidae" = "Poraniidae",
                         "Epaultidae" = "Epialtidae")) %>% 
  # Update target status (from previous script)
  mutate(target_status = if_else(Genus == "Sebastes" &
                                   Species %in% c("serranoides", "caurinus", "carnatus",
                                                  "diaconus",
                                                  "melanops or mystinus",
                                                  "melanops or mystinus or diaconus",
                                                  "mystinus or diagonus",
                                                  "pinniger or miniatus",
                                                  "serranoides or flavidus"), "Targeted", target_status))

# Test to confirm remaining fixes - should be 16 here
deep_reef_fix <- deep_reef_taxon2 %>% 
  filter(grepl("\\s|[^A-Za-z0-9]", Species)) %>% 
  mutate(test_species = if_else(Genus %in% c("Sebastes", "Solaster", "Pisaster") &
                          grepl("\\s|[^A-Za-z0-9]", Species), "spp", Species)) %>% 
  mutate(test_genus = if_else(grepl("\\s|[^A-Za-z0-9]", test_species), NA, Genus)) %>% 
  mutate(test_species = if_else(grepl("\\s|[^A-Za-z0-9]", test_species), NA, test_species))

# -- All cases where Genus is (Sebastes, Solaster, Pisaster) can be converted to NA (will ultimately be sciname: Genus spp))
# -- All other remaining cases are not confirmed to one genus and should revert to family (Genus & Species go to NA, will become Family spp)

deep_reef_taxon3 <- deep_reef_taxon2 %>% 
  mutate(Species = if_else((Genus %in% c("Sebastes", "Solaster", "Pisaster") & grepl("\\s|[^A-Za-z0-9]", Species)), NA, Species)) %>% 
  mutate(Genus = if_else(grepl("\\s|[^A-Za-z0-9]", Species), NA, Genus)) %>% 
  mutate(Species =  if_else(grepl("\\s|[^A-Za-z0-9]", Species), NA, Species)) 

# Confirm that there are no more changes to fix
deep_reef_fix <- deep_reef_taxon3 %>% 
  filter(grepl("\\s|[^A-Za-z0-9]", Genus)|grepl("\\s|[^A-Za-z0-9]", Species))

deep_reef_taxon3$Species[deep_reef_taxon3$Species == "spp"] <- NA

# Check missing target status - several are missing target status, this data will show
# 170, but many of these are not fishes and therefore not included in the analyses. We 
# shared the list of missing target status for fishes with Rick Starr, and he provided the
# correction for each of the species in September 2023. 
deep_missing_target <- deep_reef_taxon3 %>% filter(is.na(target_status))

# List nontargeted species as classified by Rick - these names will match those provided
# in the deep reef raw dataset (may be incorrect/misspelled - LEAVE AS IS)
nontargeted_list <- c("Apristurus brunneus", "Entosphenus tridentatus", "Gibbonsia metzi",
                      "Rhinogobiops nicholsii", "Torpedo california",
                      "Torpedo californica", "Unidentified Agonidae", "Unidentified Cottidae",
                      "Unidentified fish", "Unidentified Gadidae", "Unidentified Gobiidae",
                      "Unidentified Macrouridae", "Unidentified small benthic fish",
                      "Unidentified Zoarcidae", "Zaniolepis frenata or latipinnis",
                      "Zaniolepis Spp.", "Zaniolepis spp.")

# List targeted species as classified by Rick
targeted_list <- c("Beringraja binoculata","Beringraja rhina", "Hyperprosopon ellipticum",
                   "Lycodes pacificus", "Pegusa lascaris", "Pleuronectidae spp.",
                   "Raja inornata", "Schooling (10-15 cm Sebastes sp.)", "Scorpaena guttata", 
                   "Sebastes aurora", "Sebastes aurora or diploproa", "Sebastes borealis", 
                   "Sebastes crameri", "Sebastes crocotulus", "Sebastes diploproa", 
                   "Sebastes ensifer", "Sebastes goodei", "Sebastes hopkinsi or entomelas", 
                   "Sebastes lentiginosus", "Sebastes letiginosus", "Sebastes melanops or mystinus or diaconus", 
                   "Sebastes melanostomus", "Sebastes mystinus or diaconus", "Sebastes simulator", 
                   "Sebastolobus alascanus or altivelis", "Synodus lucioceps or Ophiodon elongatus", 
                   "Unidentified Citharichthys sp.", "Unidentified Elasmobranchii (ray)", 
                   "Unidentified Embiotocidae", "Unidentified Hexagrammos sp.", "Unidentified Osmeridae", 
                   "Unidentified Pleuronectidae", "Unidentified Raja sp.", "Unidentified schooling pelagic fish", 
                   "Unidentified Sciaenidae", "Unidentified Scorpaenidae", "Unidentified Sebastes sp.", 
                   "Unidentified Sebastomus sp.")
  
# Update the target status 
deep_reef_taxon <- deep_reef_taxon3 %>% 
  mutate(target_status = case_when(is.na(target_status) & habitat_specific_spp_name %in% nontargeted_list ~ "nontargeted",
                                   is.na(target_status) & habitat_specific_spp_name %in% targeted_list ~ "targeted",
                                   T~target_status))

deep_missing_target <- deep_reef_taxon %>% filter(is.na(target_status)) # 115 left

################################################################################
#merge

taxon_tab <- rbind(surf_taxon, rocky_taxon, kelp_taxon, CCFRP_taxon, deep_reef_taxon) %>% 
  mutate(across(everything(), str_trim))

taxon_tab[taxon_tab == "NA"] <- NA
taxon_tab[taxon_tab == "?"] <- NA
taxon_tab[taxon_tab == "Non-targeted"] <- "Nontargeted"



#export
write.csv(taxon_tab, file.path(export_path, "full_taxon_table_new.csv"), row.names = FALSE)







