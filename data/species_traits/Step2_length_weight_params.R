
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
datadir <- file.path(basedir, "species_traits/processed")

# Read data
spp_orig <- read.csv(file.path(datadir, "full_taxon_table.csv"))
freeR::complete(spp_orig)


# Format taxa key
################################################################################

# Format data
spp <- spp_orig %>% 
  # Rename
  rename(species_id=common_name) %>% 
  # Arrange
  select(species_id, everything()) %>% 
  # Format genus
  mutate(genus=stringr::str_to_sentence(genus),
         genus=ifelse(genus=="", NA, genus)) %>% 
  # Add "spp" when missing
  mutate(species=ifelse(is.na(species) & !is.na(genus), "spp", species)) %>% 
  # Add scientific name
  mutate(species_nwords=freeR::nwords(species),
         sciname=ifelse(species_nwords==1, paste(genus, species), species)) %>% 
  # Remove missing scientific names
  mutate(sciname=ifelse(sciname=="NA NA", NA, sciname),
         sciname=ifelse(sciname=="", NA, sciname)) %>% 
  # Sentence case
  mutate(sciname=sciname %>% stringr::str_squish() %>% stringr::str_to_sentence()) %>% 
  # Simplify
  select(species_id, genus, sciname) %>% 
  unique() %>% 
  # Fix scientific name
  mutate(sciname=recode(sciname,
                        "<10 cm sebastes sp."="Sebastes spp",
                        # "Actinostella bradleyi"                              
                        # "Antiopella barbarensis"                              
                        # "Atrimitra idae"                                     
                        # "Beringraja binoculata"                               
                        # "Beringraja stellulata"                              
                        "Brevispinus, ochraceus, or giganteus"="Pisaster Brevispinus/ochraceus/giganteus",               
                        # "Californiconus californicus"                        
                        # "Californicus /syn./ parastichopus californicus"      
                        "Californicus or pycnopodia helianthoides"="Rathbunaster californicus, Pycnopodia helianthoides",       
                        "Carnatus, caurinus"="Sebastes carnatus/caurinus",
                        "Ceramium flaccidum"="Gayliella flaccida",                              
                        "Chiliensis chiliensis"="Sarda chiliensis",
                        "Chrysomelas/carnatus young of year"="Sebastes chrysomelas/carnatus",               
                        # "Coriacea /syn./ tealia coriacea"                     
                        # "Cribrinopsis albopunctata"                          
                        # "Cyanoplax hartwegii"                                 
                        "Dawsoni or stimpsoni"="Solaster dawsoni/stimpsoni",
                        # "Diaperoforma californica"                            
                        # "Dictyoneurum californicum/reticulatum"              
                        # "Dodecaceria fewkesi/concharum"                       
                        # "Eopsetta  jordani"                                  
                        # "Epitonium tinctum"                                   
                        # "Evasterias troschelii"                              
                        # "Felimare californiensis"                             
                        # "Felimare porterae"                                  
                        # "Felimida macfarlandi"                                
                        # "Flabellinopsis iodinea"                             
                        "Foliolata or astropecten armatus"="Luidia foliolata, Astropecten armatus",                
                        # "Franciscanus /syn./ strongylocentrotus franciscanus"
                        "Franciscanus or strongylocentrotus purpuratus"="Mesocentrotus franciscanus, Strongylocentrotus purpuratus",
                        "Halichondria (halichondria)"="Halichondria panicea",
                        "Haliclona (reniera)"="Haliclona cinerea",
                        # "Hedophyllum sessile"                                
                        "Holothuria (vaneyothuria) zacae"="Holothuria zacae",
                        "Inflata or hippasteria spinosa"="Poraniopsis inflata, Hippasteria spinosa",
                        # "Leukothele /syn./ parastichopus leukothele"          
                        # "Lirobittium munitum"                                
                        # "Lithopoma undosum"                                   
                        # "Lofotensis /syn./ tealia lofotensis"                
                        "Loligo opalescens"="Doryteuthis opalescens",                                   
                        # "Lopholithodes mandtii/foraminatus"                  
                        # "Loxorhynchus/scyra crispatus/acutifrons"             
                        "Magister /syn./ cancer magister"="Metacarcinus magister",                    
                        "Melanops / mystinus/ diaconus"="Sebastes melanops/mystinus/diaconus",                      
                        "Melanops or mystinus"="Sebastes melanops/mystinus",                               
                        # "Mexacanthina lugubris"                               
                        "Miniata or mediaster aequalis"="Patiria miniata, Mediaster aequalis",
                        "Mystinus or diaconus"="Sebastes mystinus/diaconus",
                        # "Nemalion elminthoides"                              
                        # "Neoagarum fimbriatum"                                
                        # "Neobernaya spadicea"                                
                        # "Neogastroclonium subarticulatum"                     
                        # "Nobilis /syn./ anisodoris nobilis"                  
                        # "Norrisia norrisii"                                   
                        # "Okenia rosacea"                                     
                        "Opalescens (eggs)"="Doryteuthis opalescens",
                        # "Parvimensis /syn./ parastichopus parvimensis"       
                        # "Peltodoris nobilis"                                  
                        # "Phyllospadix scouleri"                              
                        # "Phyllospadix serrulatus"                             
                        # "Phyllospadix torreyi"                               
                        "Pinniger or miniatus"="Sebastes pinniger/miniatus",                              
                        # "Plocamium violaceum"                                
                        # "Pseudobatos productus"                               
                        # "Psolus chitonoides"                                 
                        # "Pugettia foliata"                                    
                        # "Ritteri /syn./ heteropolypus ritteri"               
                        # "Scyra/oregonia acutifrons/gracilis"                  
                        "Sebastes (10-15"="Sebastes spp",                                  
                        "Sebastes atrovirens,carnatus,chrysomelas,caurinus"="Sebastes atrovirens/carnatus/chrysomelas/caurinus",   
                        "Sebastes dalli"="Sebastes dallii",                                   
                        # "Sebastes diaconus"                                   
                        "Sebastes n/a"="Sebastes spp",                                        
                        "Sebastes serranoides,flavidus"="Sebastes serranoides/flavidus",                         
                        "Sebastes serranoides,flavidus,melanops"="Sebastes serranoides/flavidus/melanops",         
                        "Serranoides or flavidus"="Sebastes serranoides/flavidus",                        
                        # "Stephanocystis dioica"                              
                        # "Stephanocystis osmundacea"                           
                        # "Taonia lennebackerae"                               
                        # "Tethya aurantia"                                     
                        "Tetilla sp"="Tetilla spp",                                        
                        # "Tetronarce californica"                              
                        # "Thylacodes/petaloconchus squamigerus/montereyensis" 
                        # "Trikentrion helium"                                  
                        # "Urticina columbiana/mcpeaki"                        
                        # "Xenistius californiensis"                            
                        # "Xystreurys rubescens"
                        )) %>% 
  # Mark level
  mutate(level=ifelse(grepl("spp|/|,", sciname) | is.na(genus), "group", "species"))

# Inspect
sort(unique(spp$genus))


# Check names
spp_names <- spp$sciname[spp$level=="species"] %>% unique() %>% sort()
wrong_names <- freeR::check_names(spp_names)
wrong_names 

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



