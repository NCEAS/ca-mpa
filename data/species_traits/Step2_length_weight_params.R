# Build Taxa & Length Weight Parameters
# CF, JS, CL
# 28 Aug 2023

# This script brings the full taxon table created in the previous step, which includes
# taxa from all habitats (rocky intertidal, kelp forest, rocky reef, deep reef).
# - Compares the full table to taxonomy drawn from Fishbase and Sealife Base
# - Applies corrections to ensure taxonomy is correct
# - Identify taxononmic level for each entry (species, genus, family)
# - Export as species_key.csv


# Setup ------------------------------------------------------------------------------

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(rfishbase)
library(freeR)

# Directories
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
basedir <- "/home/shares/ca-mpa/data/sync-data/" #Josh
datadir <- file.path(basedir, "species_traits/processed") 

# Read data
spp_orig <- read.csv(file.path(datadir, "full_taxon_table_new.csv"), na = c("", "NA", "N/A"))

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

# 1. Genus should be one word to pull from FishBase. Identify Genus entries
#    that contain special characters or are not in the FB/SLB list
genus_fix <- spp_orig %>%
  filter(Kingdom == "Animalia") %>% 
  filter(!is.na(Genus)) %>% 
  filter(grepl("\\s|[^A-Za-z0-9]", Genus) |
           !(Genus %in% fb_all$Genus)) %>% distinct(Genus, Species, habitat)
# Note: currently 8 obs

# 2. Species should be one word to pull from FishBase. Identify Species entries
#    that contain special characters or are not in the FB/SLB list
species_fix <- spp_orig %>%
  filter(Kingdom == "Animalia") %>% 
  filter(!is.na(Species)) %>% 
  mutate(sciname = if_else(!is.na(Genus) & !is.na(Species), paste(Genus, Species), NA)) %>% 
  filter(grepl("\\s|[^A-Za-z0-9]", Species) | !(sciname %in% fb_all$Species)) %>% distinct(Genus, Species, sciname, habitat)
# Note: currently 32 obs

# 3. Format data and apply corrections
spp <- spp_orig %>% 
  mutate(across(everything(), str_trim)) %>% # Remove leading and trailing whitespace across all entries
  # Create scientific name at lowest taxonomic level
  mutate(sciname = case_when(!is.na(Genus) & !is.na(Species) ~ paste(Genus, Species),
                             !is.na(Genus) & is.na(Species) ~ paste(Genus, "spp"),
                             !is.na(Family) & is.na(Genus) & is.na(Species) ~ paste(Family, "spp"))) %>% 
  mutate(sciname = recode(sciname, # Correct to newest accepted name via FB/SLB
                          "Anthomastus ritteri" = "Heteropolypus ritteri", #still not found: https://www.marinespecies.org/aphia.php?p=taxdetails&id=724715
                          "Crassostrea gigas" = "Magallana gigas", #https://www.marinespecies.org/aphia.php?p=taxdetails&id=836033
                          "Loligo opalescens" = "Doryteuthis opalescens",   
                          "Cyanoplax hartwegii" = "Lepidochitona hartwegii",
                          "Haemulon californiensis" = "Brachygenys californiensis", #https://www.fishbase.se/summary/3570
                          "Hermosilla azurea" = "Kyphosus azureus", #https://www.fishbase.se/summary/Kyphosus-azureus
                          "Kyphosus azurea" = "Kyphosus azureus", #https://www.fishbase.se/summary/Kyphosus-azureus
                          "Lithopoma undosum" = "Megastraea undosa", #https://www.marinespecies.org/aphia.php?p=taxdetails&id=528084
                          "Okenia rosacea" = "Hopkinsia rosacea", # https://www.sealifebase.se/summary/Okenia-rosacea.html
                          "Raja binoculata" = "Beringraja binoculata",
                          "Raja inornata" = "Caliraja inornata", 
                          "Raja rhina" = "Beringraja rhina",
                          "Raja stellulata" = "Beringraja stellulata",
                          "Rhacochilus vacca" = "Phanerodon vacca",
                          "Stylissa stipitata" = "Semisuberites cribrosa", # https://www.marinespecies.org/aphia.php?p=taxdetails&id=168379
                          "Tethya aurantia" = "Tethya aurantium", # https://www.marinespecies.org/aphia.php?p=taxdetails&id=134311
                          "Torpedo californica" = "Tetronarce californica", # https://www.fishbase.se/summary/Tetronarce-californica
                          "Urolophus halleri" = "Urobatis halleri", # https://www.fishbase.se/summary/urobatis-halleri
                          "Xenistius californiensis" = "Brachygenys californiensis" #https://www.fishbase.se/summary/3570
                          )) %>% 
  mutate(Family = recode(Family,
                         "Scopalinidae" = "Esperiopsidae",
                         "Cottidae/Gobiesocidae" = "Cottidae",
                         "Onuphidae, Chaetopteridae" = "Onuphidae"))

# 4. Test for wrong names from fishbase/sealifebase after above corrections -- 13 confirmed ok
check_names <- spp %>% 
  filter(Kingdom == "Animalia"|is.na(Kingdom)) %>% 
  filter(!str_detect(sciname, "spp")) %>% 
  filter(!(sciname %in% fb_all$Species))%>% 
  distinct(sciname, Family, Genus, Species, habitat) %>% 
  arrange(sciname)

## These names come up as incorrect but were validated as the correct name on 25 Aug 2023 (CL)
##  Cribrinopsis albopunctata https://www.marinespecies.org/aphia.php?p=taxdetails&id=240769
##  Diaperoforma californica https://www.marinespecies.org/aphia.php?p=taxdetails&id=472584
##  Epitonium tinctum https://www.marinespecies.org/aphia.php?p=taxdetails&id=524008
##  Evasterias troschelii https://www.marinespecies.org/aphia.php?p=taxdetails&id=255040
##  Heteropolypus ritteri (corrected above; still not found)
##  Lirobittium munitum https://www.marinespecies.org/aphia.php?p=taxdetails&id=580440
##  Mexacanthina lugubris https://www.marinespecies.org/aphia.php?p=taxdetails&id=404505
##  Neobernaya spadicea
##  Peltodoris nobilis https://www.marinespecies.org/aphia.php?p=taxdetails&id=594422
##  Pugettia foliata https://www.marinespecies.org/aphia.php?p=taxdetails&id=851288
##  Sebastes crocotulus 
##  Caliraja inornata https://www.fishbase.se/summary/2558

# Check for incorrect families -- should be 0
check_family <- spp %>% 
  filter(Kingdom == "Animalia"|is.na(Kingdom)) %>% # dont bother with algae (sorry algae friends)
  distinct(Family, Genus, Species, sciname, habitat) %>% 
  filter(!(Family %in% fb_all$Family))

# Check for incorrect genus -- 8 obs here confirmed OK
check_genus <- spp %>% 
  filter(Kingdom == "Animalia"|is.na(Kingdom)) %>% # dont bother with algae (sorry algae friends)
  distinct(Family, Genus, Species, sciname, habitat) %>% 
  filter(!is.na(Genus)) %>% 
  filter(!(Genus %in% fb_all$Genus))


## 5. Correct those remaining species to "Genus spp" and add taxonomic level
spp <- spp %>% 
  mutate(sciname = if_else(sciname %in% check_names$sciname, paste(word(sciname, 1), "spp"), sciname)) %>% 
  # Create level
  mutate(level = case_when(!is.na(Species) & !is.na(Genus) & !(str_detect(sciname, "spp")) ~ "species",
                           is.na(Species) & !is.na(Genus) ~ "genus",
                           is.na(Species) & is.na(Genus) & !is.na(Family) ~ "family",
                           !is.na(Species) & !is.na(Genus) & str_detect(sciname, "spp") ~ "genus", # fixes level for the species above without overwriting the correct species name 
                           TRUE ~ NA)) %>% 
  # Add higher order values for key missing species
  mutate(Family = case_when(Genus == "Zaniolepis" ~ "Zaniolepididae",
                            Genus == "Sebastes" ~ "Sebastidae",
                            Genus == "Diaperoforma" ~ "Cyclostomatida",
                            Genus == "Raja" ~ "Rajidae",
                            TRUE~Family))

## 6. Examine discrepancies between our taxonomic list and FB/SLB
taxa_match <- spp %>% 
  select(Kingdom:Species, sciname) %>% distinct() %>% 
  left_join(fb_all %>% 
              select(Genus, Family, Order, Class, Species) %>% 
              mutate(Order = sub("\\/.*", "", Order)),  # remove everyting after / in Order,
            by = c("sciname" = "Species")) %>% 
  mutate(Genus = coalesce(Genus.y, Genus.x),
         Family = coalesce(Family.y, Family.x),
         Order = coalesce(Order.y, Order.x),
         Class = coalesce(Class.y, Class.x)) %>% 
  mutate(Genus.match = if_else(Genus.x == Genus.y, TRUE, FALSE),
         Family.match = if_else(Family.x == Family.y, TRUE, FALSE), 
         Order.match = if_else(Order.x == Order.y, TRUE, FALSE), 
         Class.match = if_else(Class.x == Class.y, TRUE, FALSE)) %>% 
  select(sciname, Genus, Genus.x, Genus.y, Genus.match,
         Family, Family.x, Family.y, Family.match,
         Order, Order.x, Order.y, Order.match,
         Class, Class.x, Class.y, Class.match) 

match_class <- taxa_match %>% filter(!Class.match) # 277 will update
match_family <- taxa_match %>% filter(!Family.match) # 43 will update
match_genus <- taxa_match %>% filter(!Genus.match)  # 16 will update
match_order <- taxa_match %>% filter(!Order.match) # 155 will update

## Fix corrections -- any changes beyond are reflected above
spp_corrected <- spp %>% 
  left_join(fb_all %>% select(Genus, Family, Order, Class, Species) %>% 
              mutate(Order = sub("\\/.*", "", Order)), by = c("sciname" = "Species")) %>% 
  # Fill in NAs with values from FB/SLB. If there is a discrepancy, default to FB/SLB.
  mutate(Genus = coalesce(Genus.y, Genus.x),
         Family = coalesce(Family.y, Family.x),
         Order = coalesce(Order.y, Order.x),
         Class = coalesce(Class.y, Class.x)) %>% 
  select(habitat, habitat_specific_code, habitat_specific_spp_name,
         Kingdom, Phylum, Class, Order, Family, Genus, Species,
         sciname, target_status, level)

## Examine NA for sciname (currently 109 obs; most algae/higher order/unknown species)
## Leaving for review -- these will mostly be excluded from analyses.. ok?
spp_na <- spp %>% 
  filter(is.na(sciname))


# Export species key to csv -----------------------------------------------------------
write.csv(spp_corrected, file=file.path(datadir, "species_key.csv"), row.names = F)



# Get length weight data -----------------------------------------------------------

# Get length weight tables from fishbase
lw_fb <-  rfishbase::length_weight(server = "fishbase") %>% 
  mutate(database = "fishbase") %>% select(database, everything()) 

lw_slb <- rfishbase::length_weight(server = "sealifebase") %>% 
  mutate(database = "sealifebase") %>% select(database, everything()) 

lw <- plyr::rbind.fill(lw_fb, lw_slb) %>% 
  select(database, SpecCode, LengthMin, LengthMax, Type, a, aTL, b, EsQ) %>% 
  rename(length_min_cm=LengthMin, length_max_cm=LengthMax, a_tl=aTL, doubtful=EsQ) %>% 
  # Join with taxa from fb/slb
  left_join(fb_all, by = c("database", "SpecCode")) %>% 
  arrange(Species) %>% 
  # Filter for only the family, genus, species in our data
  filter(Family %in% spp$Family |
           Genus %in% spp$Genus |
           Species %in% spp$sciname) %>%
  select(database, Class, Order, Family, Genus, Species, everything())

# Write length weight parameters to csv ----------------------------------------------------------
#write.csv(lw, file=file.path(datadir, "species_lw_parameters_from_fishbase_full_new.csv"), row.names = F)

