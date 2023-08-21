#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 17, 2023

rm(list=ls())

#load required packages
require(dplyr)
require(stringr)
require(knitr)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"
tabdir <- here::here("analyses","1performance_eco","tables")

#load taxonomy lookup table (combined for all habitats)
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv")

#oad data
mondir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"
surf_zone_raw <- read.csv(file.path(mondir, "surf_zone_fish_biomass.csv"))
kelp_raw <- read.csv(file.path(mondir, "kelpforest_fish_biomass.csv"))
rocky_reef_raw <- read.csv(file.path(mondir, "ccfrp_fish_biomass.csv"))
deep_reef_raw <- read.csv(file.path(mondir, "deep_reef_fish_biomass.csv"))


################################################################################
#determine spp list from data, not taxonomy table, since we only want to include
#the species that were observed in the data

surf_zone_spp <- surf_zone_raw %>% mutate(habitat = "Surf zone") %>% 
                                  dplyr::select(habitat, class, order, 
                                                 family, genus, species,
                                                 target_status)%>%
                                  distinct() %>% filter(!(is.na(target_status)))

kelp_taxa <- taxon_tab %>% filter(habitat == "Kelp forest")
kelp_spp <- kelp_raw %>% mutate(habitat = "Kelp forest")%>%
                          dplyr::select(habitat, classcode, target_status) %>%
                          distinct() %>% filter(!(is.na(target_status))) %>%
                          left_join(kelp_taxa, by = c("classcode" = "habitat_specific_code")) %>%
            dplyr::select(habitat = habitat.x, class = Class, order = Order,
                          family = Family, genus = Genus, species = Species, target_status = target_status.x
                          )

shallow_reef_taxa <- taxon_tab %>% filter(habitat == "Rocky reef")
shallow_reef_spp <- rocky_reef_raw %>% 
  mutate(habitat = "Rocky reef")%>%
  dplyr::select(habitat, sciname, target_status) %>%
  distinct() %>% filter(!(is.na(target_status))) %>%
  left_join(shallow_reef_taxa, by = c("sciname" = "habitat_specific_spp_name")) %>%
  dplyr::select(habitat = habitat.x, class = Class, order = Order,
                family = Family, genus = Genus, species = Species, target_status = target_status.x
  ) %>% filter(!(is.na(class))) %>% mutate(habitat = "Shallow reef")

deep_reef_taxa <- taxon_tab %>% filter(habitat == "Deep reef")
deep_reef_spp <- deep_reef_raw %>% 
  mutate(habitat = "Deep reef")%>%
  dplyr::select(habitat, habitat_specific_id, target_status) %>%
  distinct() %>% filter(!(is.na(target_status))) %>%
  left_join(deep_reef_taxa, by = c("habitat_specific_id" = "habitat_specific_spp_name")) %>%
  dplyr::select(habitat = habitat.x, class = Class, order = Order,
                family = Family, genus = Genus, species = Species, target_status = target_status.x
  ) %>% filter(!(is.na(class)))


#merge

spp_long <- rbind(surf_zone_spp, kelp_spp, shallow_reef_spp, deep_reef_spp)

################################################################################
# clean up

spp_clean1 <- spp_long %>% mutate(class = ifelse(class == "Actinopteri","Actinopterygii",class),
                                  order = ifelse(order == "Ovalentaria incertae sedis","Perciformes",order))

spp_wide <- spp_clean1 %>%
  mutate(dumy_var = "X") %>%
  pivot_wider(names_from = habitat, values_from = dumy_var, values_fill = NA) %>%
  mutate(across(7:10, ~ ifelse(.=="NULL",NA,"X")))
  
################################################################################
#Check and clean up

duplicate_spp <- spp_wide[duplicated(spp_wide$species) | duplicated(spp_wide$species, fromLast = TRUE), ]

#fix cases

spp_wide_fixed <- spp_wide %>%
                    #fix class
                    mutate(class = ifelse(order == "Lamniformes","Actinopterygii",class),
                           #fix order
                           order = ifelse(order == "Perciformes" & family == "Sebastidae","Scorpaeniformes",order),
                           order = ifelse(order == "Eupercaria incertae sedis" | order == "Centrarchiformes","Perciformes",order),
                           order = ifelse(order == "Perciformes" & family == "Hexagrammidae","Scorpaeniformes",order)) %>%
                    #drop missing species
                    filter(!is.na(species))%>%
                    #remove leading and trailing spaces
                    mutate(across(c(class, order, family, genus, species), str_trim)) %>%
  group_by(class, order, family, genus, species, target_status) %>%
  summarize_at(vars(`Surf zone`, `Kelp forest`, `Shallow reef`, `Deep reef`),
               function(x) paste(unique(na.omit(x)), collapse = ", ")) %>%
  ungroup() %>%
  mutate(dup_spp = paste(genus, species))

duplicate_spp <- spp_wide_fixed[duplicated(spp_wide_fixed$dup_spp) | duplicated(spp_wide_fixed$dup_spp, fromLast = TRUE), ]


write.csv(duplicate_spp, file.path(tabdir, "target_discrep.csv"),row.names = FALSE)

###need to reconcile species that were considered targeted by one habitat and
#not by another

################################################################################
#Create table

write.csv(spp_wide, file.path(tabdir, "TableS1_taxonomy.csv"),row.names = FALSE)


