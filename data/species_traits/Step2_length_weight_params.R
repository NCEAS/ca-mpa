# Build Taxa & Length Weight Parameters
# CF, JS, CL
# 28 Aug 2023

# This script brings the full taxon table created in the previous step, which includes
# taxa from all habitats (rocky intertidal, kelp forest, rocky reef, deep reef).
# - Compares the full table to taxonomy drawn from Fishbase and Sealife Base
# - Applies corrections to ensure taxonomy is correct
# - Identify taxononmic level for each entry (species, genus, family)
# - 


# Read data
################################################################################

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
  select(database, SpecCode, Species, Genus, Family, Order, Class) 

sp_slb <- rfishbase::load_taxa("sealifebase") %>% 
  mutate(database = "sealifebase") %>% 
  select(database, SpecCode, Species, Genus, Family, Order, Class)

fb_all <- full_join(sp_fb, sp_slb) 
rm(sp_fb, sp_slb)

# Format taxa key
################################################################################

###Steps for formatting

# 1. Genus should be one word to pull from FishBase. Identify Genus entries
#    that contain special characters or are not in the FB/SLB list
genus_fix <- spp_orig %>%
  filter(Kingdom == "Animalia") %>% 
  filter(!is.na(Genus)) %>% 
  filter(grepl("\\s|[^A-Za-z0-9]", Genus) |
           !(Genus %in% fb_all$Genus)) %>% distinct(Genus, Species, habitat)

# 2. Species should be one word to pull from FishBase. Identify Species entries
#    that contain special characters or are not in the FB/SLB list
species_fix <- spp_orig %>%
  filter(Kingdom == "Animalia") %>% 
  filter(!is.na(Species)) %>% 
  mutate(sciname = if_else(!is.na(Genus) & !is.na(Species), paste(Genus, Species), NA)) %>% 
  filter(grepl("\\s|[^A-Za-z0-9]", Species) | !(sciname %in% fb_all$Species)) %>% distinct(Genus, Species, sciname, habitat)

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
                          "Raja rhina" = "Beringraja rhina",
                          "Raja stellulata" = "Beringraja stellulata",
                          "Rhacochilus vacca" = "Phanerodon vacca",
                          "Stylissa stipitata" = "Semisuberites cribrosa", # https://www.marinespecies.org/aphia.php?p=taxdetails&id=168379
                          "Tethya aurantia" = "Tethya aurantium", # https://www.marinespecies.org/aphia.php?p=taxdetails&id=134311
                          "Urolophus halleri" = "Urobatis halleri", # https://www.fishbase.se/summary/urobatis-halleri
                          "Xenistius californiensis" = "Brachygenys californiensis" #https://www.fishbase.se/summary/3570
                          ))

# 4. Test for wrong names from fishbase/sealifebase after above corrections
check_names <- spp %>% 
  filter(Kingdom == "Animalia") %>% 
  filter(!str_detect(sciname, "spp")) %>% 
  filter(!(sciname %in% fb_all$Species))%>% 
  distinct(sciname, Genus, Species, habitat) %>% 
  arrange(sciname)

## These names come up as incorrect but were validated as the correct name on 25 Aug 2023 (CL)
##  Cribrinopsis albopunctata https://www.marinespecies.org/aphia.php?p=taxdetails&id=240769
##  Diaperoforma californica https://www.marinespecies.org/aphia.php?p=taxdetails&id=472584
##  Epitonium tinctum https://www.marinespecies.org/aphia.php?p=taxdetails&id=524008
##  Evasterias troschelii https://www.marinespecies.org/aphia.php?p=taxdetails&id=255040
##  Heteropolypus ritteri (corrected above; still not found)
##  Lirobittium munitum https://www.marinespecies.org/aphia.php?p=taxdetails&id=580440
##  Mexacanthina lugubrishttps://www.marinespecies.org/aphia.php?p=taxdetails&id=404505
##  Neobernaya spadicea
##  Peltodoris nobilis https://www.marinespecies.org/aphia.php?p=taxdetails&id=594422
##  Pugettia foliata https://www.marinespecies.org/aphia.php?p=taxdetails&id=851288

## 5. Correct those remaining species to "Genus spp" and add taxonomic level
spp <- spp %>% 
  mutate(sciname = if_else(sciname %in% check_names$sciname, paste(Genus, "spp"), sciname)) %>% 
  # Create level
  mutate(level = case_when(!is.na(Species) & !is.na(Genus) ~ "species",
                           is.na(Species) & !is.na(Genus) ~ "genus",
                           is.na(Species) & is.na(Genus) & !is.na(Family) ~ "family",
                           TRUE ~ NA
    ))

## 6. Get correct taxonomy from Fishbase/Sealifebase 

  
  
# Inspect
sort(unique(spp$Genus))

# Export
write.csv(spp, file=file.path(datadir, "species_key.csv"), row.names = F)

## NOTE: SCRIPT NOT FINALIZED BELOW

# Get length weight data
################################################################################

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

write.csv(lw, file=file.path(datadir, "species_lw_parameters_from_fishbase_full_new.csv"), row.names = F)


### BELOW: OLD RECODING LIST FROM CHRIS'S FUNCTION:
# mutate(sciname = recode(sciname,  
#                         "Beringraja binoculata"  = "Beringraja spp",                             
#                         "Beringraja stellulata" = "Beringraja spp", 
#                         "Californiconus californicus" = "Californiconus spp",                        
#                         "Ceramium flaccidum"="Gayliella flaccida",      
#                         "Clupea pallasii" = "Clupea spp",
#                         "Cribrinopsis albopunctata" = "Cribrinopsis spp",              
#                         "Cyanoplax hartwegii"   = "Cyanoplax spp",               
#                         "Diaperoforma californica" = "Diaperoforma spp",             
#                         "Epitonium tinctum"    = "Epitonium spp",                               
#                         "Evasterias troschelii"    = "Evasterias spp",      
#                         "Hedophyllum sessile"   = "Hedopphyllum spp",                             
#                         "Kyphosus azurea" = "Kyphosus spp",     
#                         "Lirobittium munitum"     = "Lirobittium spp",                           
#                         "Lithopoma undosum"        = "Lithopoma spp",        
#                         "Mexacanthina lugubris"          = "Mexacanthina spp",   
#                         "Nemalion elminthoides"  = "Nemalion spp",                            
#                         "Neoagarum fimbriatum"     = "Neoagarum spp",                           
#                         "Neobernaya spadicea"   = "Neobernaya spp",                             
#                         "Neogastroclonium subarticulatum"    = "Neogastroclonium spp", 
#                         "Norrisia norrisii"      = "Norrisia spp",                             
#                         "Okenia rosacea"   = "Okenia spp",                               
#                         "Phyllospadix scouleri"    = "Phyllospadix spp",                          
#                         "Phyllospadix serrulatus"       = "Phyllospadix spp",                      
#                         "Phyllospadix torreyi"    = "Phyllospadix spp" ,                                
#                         "Plocamium violaceum"       = "Plocamium spp",                         
#                         "Pseudobatos productus"    = "Pseudobatos spp",                           
#                         "Psolus chitonoides"    = "Psolus spp",                             
#                         "Pugettia foliata"   = "Pugettia spp",
#                         "Sebastes diaconus"   = "Sebastes spp",
#                         "Sebastes diaconua" = "Sebastes spp",                    
#                         "Stephanocystis osmundacea"   = "Stephanocystis spp",                        
#                         "Taonia lennebackerae"     = "Taonia spp",                          
#                         "Tethya aurantia"  = "Tethya spp",                                   
#                         "Tetronarce californica"  = "Tetronarce spp",    
#                         "Urobatis halleri" = "Urobatis spp",                     
#                         "Xenistius californiensis" = "Xenistius spp",                           
#                         "Xystreurys rubescens" = "Xystreurys spp"
