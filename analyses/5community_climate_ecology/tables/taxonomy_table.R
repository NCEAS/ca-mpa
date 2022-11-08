rm(list=ls())

require(tidyverse)

#input_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"
input_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
export_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/cc_species_tables"

comm_data <- load(file.path(input_path, "comm_data.rda"))
group_vars <- load(file.path(input_path, "group_vars.rda"))


CCFRP_dat <- cbind(CCFRP_group_vars, CCFRP_ord_data)
kelp_invalg_dat <- cbind(kelp_invalg_group_vars, kelp_invalg_ord_data)
kelp_fish_dat <- cbind(kelp_fish_group_vars, kelp_fish_ord_data)
deep_reef_dat <- cbind(deep_reef_group_vars, deep_reef_ord_data)
rocky_dat <- cbind(rocky_group_vars, rocky_ord_data)

################################################################################
#load taxonomy tables

rocky_taxon <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","RockyIntertidal-LongTerm-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")%>%
  dplyr::select(marine_species_code, marine_species_name)
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





################################################################################
#select distinct species

CCFRP_spp <- CCFRP_dat %>%
              dplyr::select(10:ncol(.))%>%
              pivot_longer(cols=everything(), names_to ="Species",
                           values_to = "Values")%>%
              mutate(group = "CCFRP")%>%
              distinct(Species, group)%>%
  mutate(Species = gsub("_", " ", Species))

kelp_invalg <- kelp_invalg_dat %>%
  dplyr::select(10:ncol(.))%>%
  pivot_longer(cols=everything(), names_to ="Species",
               values_to = "Values")%>%
  mutate(group = "kelp invertebrates and algae")%>%
  distinct(Species, group)%>%
  mutate(Species = gsub("_", " ", Species))

kelp_fish <- kelp_fish_dat %>%
  dplyr::select(10:ncol(.))%>%
  pivot_longer(cols=everything(), names_to ="Species",
               values_to = "Values")%>%
  mutate(group = "kelp forest fish")%>%
  distinct(Species, group)%>%
  mutate(Species = gsub("_", " ", Species))

deep_reef <- deep_reef_dat %>%
  dplyr::select(11:ncol(.))%>%
  pivot_longer(cols=everything(), names_to ="Species",
               values_to = "Values")%>%
  mutate(group = "deep reef fish")%>%
  distinct(Species, group)%>%
  mutate(Species = gsub("_", " ", Species))


rocky <- rocky_dat %>%
  dplyr::select(10:ncol(.))%>%
  pivot_longer(cols=everything(), names_to ="Species",
               values_to = "Values")%>%
  mutate(group = "rocky intertidal")%>%
  distinct(Species, group)%>%
  mutate(Species = gsub("_", " ", Species))

################################################################################
#add common names, correct spelling inconsistencies


###join to kelp taxonomy table first, then habitat-specific for unmatched pairs

#kelp invalg corrections
kelp_invalg1 <- anti_join(kelp_invalg, kelp_taxon, by=c("Species"="species_definition"))

#kelp fish corrections
kelp_fish1 <-  anti_join(kelp_fish, kelp_taxon, by=c("Species"="species_definition"))
kelp_fish$Species <- recode_factor(kelp_fish$Species, "sebastes chrysomelas carnatus" = "sebastes chrysomelas/carnatus")
kelp_fish$Species <- recode_factor(kelp_fish$Species, "sebastes serranoides flavidus" = "sebastes serranoides/flavidus")
kelp_fish$Species <- recode_factor(kelp_fish$Species, "sebastes serranoides flavidus melanops" = "sebastes serranoides/flavidus/melanops")

#CCFRP corrections
CCFRP1 <-  anti_join(CCFRP_spp, kelp_taxon, by=c("Species"="common_name"))
CCFRP_spp$Species <- recode_factor(CCFRP_spp$Species, "jack smelt" = "grunion, topsmelt or jacksmelt")
CCFRP_spp$Species <- recode_factor(CCFRP_spp$Species, "mackerel family" = "pacific mackerel, greenback mackerel")
CCFRP_spp$Species <- recode_factor(CCFRP_spp$Species, "olive rockfish" = "olive or yellowtail rockfish")
CCFRP_spp$Species <- recode_factor(CCFRP_spp$Species, "pacific bonito" = "eastern pacific bonito")
CCFRP_spp$Species <- recode_factor(CCFRP_spp$Species, "scalyhead sculpin" = "scaleyhead sculpin")
CCFRP_spp$Species <- recode_factor(CCFRP_spp$Species, "striped seaperch" = "striped surfperch")
CCFRP_spp$Species <- recode_factor(CCFRP_spp$Species, "yellowtail rockfish" = "olive or yellowtail rockfish")

#deep reef corrections
deep_reef1 <-  anti_join(deep_reef, kelp_taxon, by=c("Species"="species_definition"))


####do first join
kelp_invalg1 <- left_join(kelp_invalg, kelp_taxon, by=c("Species"="species_definition"))
kelp_fish1 <-  left_join(kelp_fish, kelp_taxon, by=c("Species"="species_definition"))
CCFRP1 <-  left_join(CCFRP_spp, kelp_taxon, by=c("Species"="common_name"))
deep_reef1 <-  left_join(deep_reef, kelp_taxon, by=c("Species"="species_definition"))


###finish joins using habitat-specific tables
CCFRP2 <- CCFRP1 %>%
           filter_all(any_vars(is.na(.))) %>%
            dplyr::select(Species, group)
CCFRP3 <- left_join(CCFRP2, ccfrp_taxon, by=c("Species"="common_name"))

CCFRP_list <- merge(CCFRP1, CCFRP3)



anti_join(CCFRP1, ccfrp_taxon, by=c("Species"="common_name"))








rocky_final <- left_join(rownames_to_column(rocky_t), rocky_taxon, by=c("rowname" = "marine_species_code"))



library(xlsx)
write.xlsx(CCFRP_t, file.path(export_path, "species_table.xlsx"), sheetName="CCFRP", row.names=TRUE)
write.xlsx(kelp_invalg_t, file.path(export_path, "species_table.xlsx"), sheetName="kelp_invalg", append=TRUE, row.names=TRUE) 
write.xlsx(kelp_fish_t, file.path(export_path, "species_table.xlsx"), sheetName="kelp_fish", append=TRUE, row.names=TRUE) 
write.xlsx(deep_reef_t, file.path(export_path, "species_table.xlsx"), sheetName="deep_reef", append=TRUE, row.names=TRUE)  
write.xlsx(rocky_final, file.path(export_path, "species_table.xlsx"), sheetName="rocky_intertidal", append=TRUE, row.names=TRUE)  



