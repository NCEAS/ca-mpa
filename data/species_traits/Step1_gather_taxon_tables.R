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
deep_reef_taxon1 <- readxl::read_excel(file.path(data_path,input_file),sheet = 1)%>%
  janitor::clean_names()



################################################################################
#clean surf zone

surf_taxon <- surf_taxon1 %>%
  mutate(habitat = "Surf Zone")%>%
  dplyr::select(habitat, habitat_specific_code = "species_code", habitat_specific_spp_name = "ScientificName_accepted",
                Kingdom, Phylum, Class, Order, Family, Genus = genus, Species = species, target_status = Targeted) %>%
  distinct() %>% 
  # Fix incorrect species code
  mutate(habitat_specific_code = if_else(habitat_specific_code == "CLIN" &
                                           habitat_specific_spp_name == "Genyonemus lineatus", "GELI", habitat_specific_code))

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

deep_reef_taxon <- deep_reef_taxon1%>%
  mutate(habitat = "Deep reef")%>%
  dplyr::select(habitat, habitat_specific_code = "pisco_code", habitat_specific_spp_name = "scientific_name",
                Kingdom = kingdom, Phylum = phylum, Class = class, Order = order, Family = family, 
                Genus = genus, Species = species, target_status=targeted) %>%
  distinct()
deep_reef_taxon[deep_reef_taxon == "NA"] <- NA


################################################################################
#merge

taxon_tab <- rbind(surf_taxon, rocky_taxon, kelp_taxon, CCFRP_taxon, deep_reef_taxon)
taxon_tab[taxon_tab == "NA"] <- NA
taxon_tab[taxon_tab == "?"] <- NA
taxon_tab[taxon_tab == "Non-targeted"] <- "Nontargeted"



#export
write.csv(taxon_tab, file.path(export_path, "full_taxon_table_new.csv"), row.names = FALSE)










