rm(list=ls())

require(dplyr)
require(stringr)
require(tidyr)

export_path <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

################################################################################
# This script gathers all of the taxanomoy tables from Surf Zone, Rocky Intertidal,
# Kelp Forest, CCFRP, and Deep Reef. 

# The purpose of this script is to create a single lookup table with the full
# taxonomy of all unique species recorded by each habitat with the habitat-specific
# codes that link the observation data with the taxonomy. 

# Taxa should not be duplicated within a habitat (distinct rows only), but
# they may be duplicated across habitats at this stage. We need to keep track
# of habitat specific codes so that we can join the taxonomy with each habitat's raw data. 

################################################################################
#load taxonomy tables

# Surf zone has multiple sampling methods each with a unique taxonomy table. 
# We are only using the seine data so will load that table only. 

surf_taxon1 <- read.csv(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","surf_zone_fish_seine_species.csv"))

##Rocky intertidal has two survey types (biodiversity and long term) each with 
#a unique taqxonomy table. We are only using the long term data here so will
#not load the biodiversity table for now. 
rocky_taxon1 <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","RockyIntertidal-LongTerm-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")

# Full taxonomy for all methods
kelp_taxon1 <-readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables/Kelp-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")

# Full taxonomy for all methods
ccfrp_taxon1 <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables/CCFRP_Taxonomy.xlsx"), sheet=2, skip = 0, na="NA")


# Full taxonomy for all methods
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables"
input_file <- "DeepReef-ROV-Taxonomy.xlsx"  
deep_reef_taxon1 <- readxl::read_excel(file.path(data_path,input_file),sheet = 1, na = "NA")%>%
  janitor::clean_names()

# Many species in deep reef dataset are not in the taxonomy table -- read here to complete
deep_reef_data <- read.csv(file.path("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/ROV_Dataset/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), 
                          na = c("N/A", "", " ")) %>% clean_names() 


################################################################################
#clean surf zone

surf_taxon <- surf_taxon1 %>%
  mutate(habitat = "Surf Zone") %>%
  dplyr::select(habitat, habitat_specific_code = "species_code", habitat_specific_spp_name = "ScientificName_accepted",
                Kingdom, Phylum, Class, Order, Family, Genus = genus, Species = species, target_status = Targeted) %>%
  distinct() %>% 
  # Fix incorrect species code
  mutate(habitat_specific_code = if_else(habitat_specific_code == "CLIN" &
                                           habitat_specific_spp_name == "Genyonemus lineatus", "GELI", habitat_specific_code)) %>% 
  mutate(habitat_specific_spp_name = case_when(
    habitat_specific_code == "FFUN" ~ "Unidentified flatfish", 
    habitat_specific_code == "HALI" ~ "Unidentified halibut",
    TRUE ~ habitat_specific_spp_name))

################################################################################
#clean rocky intertidal
rocky_taxon <- rocky_taxon1 %>%
                mutate(habitat = "Rocky intertidal",
                       target_status = NA)%>%
                dplyr::select(habitat, habitat_specific_code = "marine_species_code", habitat_specific_spp_name = "marine_species_name",
                              Kingdom, Phylum, Class, Order, Family, Genus, Species, target_status) 
rocky_taxon[rocky_taxon == "NULL"] <- NA
rocky_taxon <- rocky_taxon %>% mutate(Species = word(Species, -1))

rocky_taxon$Phylum <-  gsub("[()]", "", rocky_taxon$Phylum)  
rocky_taxon$Species <-  gsub("[()]", "", rocky_taxon$Species)  

################################################################################
#clean kelp forest
kelp_taxon <- kelp_taxon1 %>%
              mutate(habitat = "Kelp forest")%>%
                dplyr::select(habitat, habitat_specific_code = "pisco_classcode", habitat_specific_spp_name = "ScientificName_accepted",
                Kingdom, Phylum, Class, Order, Family, Genus = genus, Species = species, target_status = Targeted) %>%
              distinct()
kelp_taxon[kelp_taxon == "spp"] <- NA
kelp_taxon[kelp_taxon == "spp."] <- NA

################################################################################
#clean CCFRP

CCFRP_taxon <- ccfrp_taxon1%>%
  mutate(habitat = "Rocky reef")%>%
  dplyr::select(habitat, habitat_specific_code = "Species_Code", habitat_specific_spp_name = "Scientific_Name",
                Kingdom, Phylum, Class, Order, Family, Genus, Species = species, target_status=Fished) %>%
  distinct()

################################################################################
#clean Deep reef
# Extract taxonomy from deep reef data that's missing in taxon table
# (Some are new species, some are misspellings of known species)
deep_reef_data_taxa <- deep_reef_data %>% 
  select(habitat_specific_spp_name = scientific_name) %>% distinct() %>% 
  filter(!(habitat_specific_spp_name %in% deep_reef_taxon1$scientific_name)) %>% 
  mutate(habitat = "Deep Reef") %>% 
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
deep_reef_taxon <- deep_reef_taxon1%>%
  mutate(habitat = "Deep reef")%>%
  mutate(habitat_specific_code = NA) %>% 
  dplyr::select(habitat, habitat_specific_spp_name = "scientific_name", habitat_specific_code,
                Kingdom = kingdom, Phylum = phylum, Class = class, Order = order, Family = family, 
                Genus = genus, Species = species, target_status=targeted) %>%
  distinct() %>% 
  # Add missing taxa to taxon table 
  full_join(deep_reef_data_taxa) %>% 
  # Remove synonyms in species names
  mutate(Species = if_else(str_detect(Species, "/syn./"), word(Species, 1), Species)) %>% 
  # Update target status
  mutate(target_status = if_else(Genus == "Sebastes" &
                                   Species %in% c("serranoides", "caurinus", "carnatus",
                                                  "diaconus",
                                                  "melanops or mystinus",
                                                  "melanops or mystinus or diaconus",
                                                  "mystinus or diagonus",
                                                  "pinniger or miniatus",
                                                  "serranoides or flavidus"), "Targeted", target_status))
           



################################################################################
#merge

taxon_tab <- rbind(surf_taxon, rocky_taxon, kelp_taxon, CCFRP_taxon, deep_reef_taxon)
taxon_tab[taxon_tab == "NA"] <- NA
taxon_tab[taxon_tab == "?"] <- NA
taxon_tab[taxon_tab == "Non-targeted"] <- "Nontargeted"



#export
write.csv(taxon_tab, file.path(export_path, "full_taxon_table_new.csv"), row.names = FALSE)










