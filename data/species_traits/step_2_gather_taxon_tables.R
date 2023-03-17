rm(list=ls())

require(dplyr)
require(stringr)
require(tidyr)

export_path <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

################################################################################
#load taxonomy tables

rocky_taxon1 <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","RockyIntertidal-LongTerm-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")


kelp_taxon1 <-readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables/Kelp-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")


ccfrp_taxon1 <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables/CCFRP_Taxonomy.xlsx"), sheet=2, skip = 0, na="NA")


data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables"
input_file <- "DeepReef-ROV-Taxonomy.xlsx"  
deep_reef_taxon1 <- readxl::read_excel(file.path(data_path,input_file),sheet = 1)%>%
  clean_names()



################################################################################
#clean tables

#rocky intertidal
rocky_taxon <- rocky_taxon1 %>%
                mutate(habitat = "Rocky intertidal",
                       target_status = NA)%>%
                dplyr::select(habitat, habitat_specific_code = "marine_species_code", habitat_specific_spp_name = "marine_species_name",
                              Kingdom, Phylum, Class, Order, Family, Genus, Species, target_status) 
rocky_taxon[rocky_taxon == "NULL"] <- NA
rocky_taxon <- rocky_taxon %>% mutate(Species = word(Species, -1))

rocky_taxon$Phylum <-  gsub("[()]", "", rocky_taxon$Phylum)  
rocky_taxon$Species <-  gsub("[()]", "", rocky_taxon$Species)  


#kelp forest
kelp_taxon <- kelp_taxon1 %>%
              mutate(habitat = "Kelp forest")%>%
                dplyr::select(habitat, habitat_specific_code = "pisco_classcode", habitat_specific_spp_name = "ScientificName_accepted",
                Kingdom, Phylum, Class, Order, Family, Genus = genus, Species = species, target_status = Targeted) %>%
              distinct()
kelp_taxon[kelp_taxon == "spp"] <- NA
kelp_taxon[kelp_taxon == "spp."] <- NA

#CCFRP

CCFRP_taxon <- ccfrp_taxon1%>%
  mutate(habitat = "Rocky reef")%>%
  dplyr::select(habitat, habitat_specific_code = "Species_Code", habitat_specific_spp_name = "Scientific_Name",
                Kingdom, Phylum, Class, Order, Family, Genus, Species = species, target_status=Fished) %>%
  distinct()

#Deep reef

deep_reef_taxon <- deep_reef_taxon1%>%
  mutate(habitat = "Deep reef")%>%
  dplyr::select(habitat, habitat_specific_code = "pisco_code", habitat_specific_spp_name = "scientific_name",
                Kingdom = kingdom, Phylum = phylum, Class = class, Order = order, Family = family, 
                Genus = genus, Species = species, target_status=targeted) %>%
  distinct()
deep_reef_taxon[deep_reef_taxon == "NA"] <- NA


################################################################################
#merge

taxon_tab <- rbind(rocky_taxon, kelp_taxon, CCFRP_taxon, deep_reef_taxon)
taxon_tab[taxon_tab == "NA"] <- NA
taxon_tab[taxon_tab == "?"] <- NA
taxon_tab[taxon_tab == "Non-targeted"] <- "Nontargeted"





#export
write.csv(taxon_tab, file.path(export_path, "full_taxon_table_new.csv"), row.names = FALSE)










