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
# get full taxonomy from FishBase

spp_taxa <- spp_long %>%
              #drop missing genus or species
              filter(!(is.na(genus) | is.na(species))) %>%
              filter(!(species %in% c("spp")))%>%
              mutate(spp = paste(genus, species))


FB_taxa <- freeR::taxa(spp_taxa$spp)


# Perform a left join based on matching 'spp' and 'sciname'
joined_data <- left_join(spp_taxa, FB_taxa, by = c("spp" = "sciname"))

# Update columns in 'spp_taxa' with corresponding values from 'FB_taxa'
spp_taxa <- joined_data %>%
  mutate(
    class = coalesce(class.y, class.x),
    order = coalesce(order.y, order.x),
    family = coalesce(family.y, family.x),
    genus = coalesce(genus.y, genus.x),
    species = coalesce(species.y, species.x)
  ) %>%
  select(-starts_with("class."), -starts_with("order."), -starts_with("family."), -starts_with("genus."), -starts_with("species."))


################################################################################


spp_wide <- spp_taxa %>%
  mutate(dumy_var = "X") %>%
  pivot_wider(names_from = habitat, values_from = dumy_var, values_fill = NA) %>%
  mutate(across(9:12, ~ ifelse(.=="NULL",NA,"X"))) %>%
  dplyr::select(-type)
  
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


#write.csv(duplicate_spp, file.path(tabdir, "target_discrep.csv"),row.names = FALSE)

################################################################################
#fix target status
#If a habitat called a species 'targeted' then make targeted for all

#make list of targeted species
target_spp <- duplicate_spp %>% distinct(dup_spp)

#fix target status
target_status_fixed <- spp_wide_fixed %>%
  mutate(target_status = case_when(
    dup_spp %in% target_spp$dup_spp ~ "Targeted",
    TRUE ~ target_status
  )) %>% distinct(class, order, family, genus, species, target_status, .keep_all = TRUE)

################################################################################
#check for taxonomy inconsistencies from observerations not in FishBase

duplicate_rows <- target_status_fixed %>%
  filter(duplicated(dup_spp) | duplicated(dup_spp, fromLast = TRUE))

#fix
target_status_final <- target_status_fixed %>%
                        mutate(order = ifelse(dup_spp == "Pseudobatos productus", "Rajiformes",order)) %>%
  distinct(class, order, family, genus, species, target_status, .keep_all = TRUE)

################################################################################
#Create table

write.csv(target_status_final, file.path(tabdir, "TableS1_taxonomy.csv"),row.names = FALSE)


