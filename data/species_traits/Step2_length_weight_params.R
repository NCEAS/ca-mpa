
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(freeR)

# Directories
#basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
basedir <- "/home/shares/ca-mpa/data/sync-data/" #Josh
datadir <- file.path(basedir, "species_traits/processed") 

# Read data
spp_orig <- read.csv(file.path(datadir, "full_taxon_table_new.csv"), na = c("", "NA", "N/A"))

freeR::complete(spp_orig)


# Format taxa key
################################################################################

###Steps for formatting

# 1. Genus should be one word to pull from FishBase. Identify Genus entries
#    that need fix
genus_fix <- spp_orig %>%
  mutate(genus_new = str_trim(Genus)) %>%   # Remove leading and trailing whitespace
  filter(grepl("\\s|[^A-Za-z0-9]", genus_new))

# 2. Species should be one word to pull from FishBase. Identify Species entries
#    that need fix
species_fix <- spp_orig %>%
  #mutate(Species_new = if_else(is.na(Species) & !is.na(Genus), "spp", Species)) %>% 
  filter(grepl("\\s|[^A-Za-z0-9]", Species)) %>% distinct(Species) %>% 
  filter(!(Species == "chiliensis chiliensis")) # This will be corrected manually

# 3. Format data and apply corrections
spp <- spp_orig %>% 
  mutate(across(everything(), str_trim)) %>% # Remove leading and trailing whitespace across all entries
  #Fix Genus
  mutate(genus_new = recode(Genus, 
                            "Thylacodes/Petaloconchus" = "Thylacodes",
                            "Synchirus/Rimicola" = "Synchirus",
                            "Loxorhynchus/Scyra" = "Loxorhynchus",
                            "Diopatra/Chaetopterus" = "Diopatra")) %>%
  # Add "spp" when missing
  mutate(species_new = if_else(is.na(Species) & !is.na(genus_new), "spp", Species)) %>% 
  # Correct any Species from species_fix to "spp"
  mutate(species_new = if_else(species_new %in% species_fix$Species, "spp", species_new)) %>% 
  mutate(species_nwords = freeR::nwords(species_new), # Keep as check -- only one is Sarda chiliensis chiliensis which we will keep
         sciname = if_else(!is.na(genus_new) & !is.na(species_new), paste(genus_new, species_new), NA)) %>% 
  # Fix scientific name
  # NOTE: a lot of these are already fixed the Genus and Species above. 
  # But saving in case we need this later since it was a lot of work!
  mutate(sciname = recode(sciname,  
                          "Beringraja binoculata"  = "Beringraja spp",                             
                          "Beringraja stellulata" = "Beringraja spp", 
                          "Californiconus californicus" = "Californiconus spp",                        
                          "Ceramium flaccidum"="Gayliella flaccida",      
                          "Clupea pallasii" = "Clupea spp",
                          "Cribrinopsis albopunctata" = "Cribrinopsis spp",              
                          "Cyanoplax hartwegii"   = "Cyanoplax spp",               
                          "Diaperoforma californica" = "Diaperoforma spp",             
                          "Epitonium tinctum"    = "Epitonium spp",                               
                          "Evasterias troschelii"    = "Evasterias spp",      
                          "Halichondria Halichondria" = "Halichondria panicea",
                          "Haliclona Reniera" = "Haliclona spp",
                          "Hedophyllum sessile"   = "Hedopphyllum spp",                             
                          "Kyphosus azurea" = "Kyphosus spp",     
                          "Lirobittium munitum"     = "Lirobittium spp",                           
                          "Lithopoma undosum"        = "Lithopoma spp",                                      
                          "Loligo opalescens"="Doryteuthis opalescens",            
                          "Mexacanthina lugubris"          = "Mexacanthina spp",   
                          "Nemalion elminthoides"  = "Nemalion spp",                            
                          "Neoagarum fimbriatum"     = "Neoagarum spp",                           
                          "Neobernaya spadicea"   = "Neobernaya spp",                             
                          "Neogastroclonium subarticulatum"    = "Neogastroclonium spp", 
                          "Norrisia norrisii"      = "Norrisia spp",                             
                          "Okenia rosacea"   = "Okenia spp",                               
                          "Phyllospadix scouleri"    = "Phyllospadix spp",                          
                          "Phyllospadix serrulatus"       = "Phyllospadix spp",                      
                          "Phyllospadix torreyi"    = "Phyllospadix spp" ,                                
                          "Plocamium violaceum"       = "Plocamium spp",                         
                          "Pseudobatos productus"    = "Pseudobatos spp",                           
                          "Psolus chitonoides"    = "Psolus spp",                             
                          "Pugettia foliata"   = "Pugettia spp",
                          "Sarda chiliensis chiliensis" = "Sarda chiliensis",
                          "Sebastes diaconus"   = "Sebastes spp",
                          "Sebastes diaconua" = "Sebastes spp",                    
                          "Stephanocystis osmundacea"   = "Stephanocystis spp",                        
                          "Taonia lennebackerae"     = "Taonia spp",                          
                          "Tethya aurantia"  = "Tethya spp",                                   
                          "Tetronarce californica"  = "Tetronarce spp",    
                          "Urobatis halleri" = "Urobatis spp",                     
                          "Xenistius californiensis" = "Xenistius spp",                           
                          "Xystreurys rubescens" = "Xystreurys spp"
  )) %>% 
  # Mark level
  mutate(level=ifelse(grepl("spp|/|,", sciname) | is.na(Genus), "group", "species"))

# Inspect
sort(unique(spp$genus_new))

# Check names
spp_names <- spp$sciname[spp$level=="Species"] %>% unique() %>% sort() 
wrong_names <- freeR::check_names(spp_names)
wrong_names 

# Check names with punctuation
gen_names <- spp$sciname[grepl("/", spp$sciname)] %>% unique() %>% sort()
gen_names

# Export
write.csv(spp, file=file.path(datadir, "species_key.csv"), row.names = F)



# Get length weight data
################################################################################

# Retrieve taxa
taxa <- freeR::taxa(species=spp_names)

# Retrieve length-weight
lw <- freeR::fishbase(dataset="lw", species=spp_names, level="family", cleaned=T, add_taxa = F)

# Export
write.csv(lw, file=file.path(datadir, "species_lw_parameters_from_fishbase_full.csv"), row.names = F)



