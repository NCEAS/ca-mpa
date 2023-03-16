rm(list=ls())

require(dplyr)
require(stringr)
require(tidyr)

export_path <- "/home/shares/ca-mpa/data/sync-data/species_traits/processed"

################################################################################
#load taxonomy tables

rocky_taxon <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","RockyIntertidal-LongTerm-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")%>%
  dplyr::select(marine_species_code, marine_species_name, Genus, Species)
rocky_taxon$marine_species_code <- tolower(rocky_taxon$marine_species_code)


data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_kelp"
input_file <- "MLPA_kelpforest_taxon_table.4.csv"  
kelp_taxon <- read.csv(file.path(data_path, input_file)) %>%
  janitor::clean_names() %>%
  dplyr::select(classcode, species_definition, genus, species, common_name) %>%
  distinct() #remove duplicates

kelp_taxon$species_definition <- tolower(kelp_taxon$species_definition)
kelp_taxon$common_name <- tolower(kelp_taxon$common_name)



data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_ccfrp/CCFRP_database/CCFRP_database_2007-2020_csv"
input_file <- "fish_species.csv"  

ccfrp_taxon <- read.csv(file.path(data_path, input_file)) %>%
  janitor::clean_names() %>%
  dplyr::select(common_name, genus, species) %>%
  distinct() %>%#remove duplicates
  mutate(species_definition = paste(genus, species))

ccfrp_taxon$common_name <- tolower(ccfrp_taxon$common_name)
ccfrp_taxon$species_definition <- tolower(ccfrp_taxon$species_definition)


data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables"
input_file <- "DeepReef-ROV-Taxonomy.xlsx"  

deep_reef_taxon <- readxl::read_excel(file.path(data_path,input_file),sheet = 1)%>%
  clean_names()

deep_reef_taxon$common_name <- tolower(deep_reef_taxon$common_name)
deep_reef_taxon$scientific_name <- tolower(deep_reef_taxon$scientific_name)


################################################################################
#merge


rocky_merge <- rocky_taxon %>% dplyr::select(genus = Genus, species = Species, common_name = marine_species_name)

kelp_merge <- kelp_taxon %>% dplyr::select(genus, species, common_name)

ccfrp_merge <- ccfrp_taxon %>% dplyr::select(genus, species, common_name)  

deep_reef_merge <- deep_reef_taxon %>% dplyr::select(genus, species, common_name)  


taxon_tab <- rbind(rocky_merge, kelp_merge, ccfrp_merge, deep_reef_merge) 


#clean ip
taxon_tab[taxon_tab == "NA"] <- NA
taxon_tab[taxon_tab == "NULL"] <- NA
taxon_tab[taxon_tab == "spp"] <- NA


taxon_tab1 <- taxon_tab %>% mutate(genus = tolower(genus),
                                   species = tolower(species))


#export
write.csv(taxon_tab1, file.path(export_path, "full_taxon_table.csv"), row.names = FALSE)










