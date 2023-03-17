
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
spp_orig <- read.csv(file.path(datadir, "full_taxon_table_new.csv"))
freeR::complete(spp_orig)


# Format taxa key
################################################################################

# Format data
spp <- spp_orig %>% 
  # Rename
  #rename(species_id=common_name) %>% 
  # Arrange
  #select(species_id, everything()) %>% 
  # Format genus
  mutate(Genus_orig=stringr::str_to_sentence(Genus),
         Genus_orig=ifelse(Genus=="", NA, Genus)) %>% 
  #Fix Genus
  mutate(Genus = recode(Genus_orig, 
                        "Thylacodes/petaloconchus" = "Thylacodes",
                        "Synchirus/rimicola" = "Synchirus",
                        "Loxorhynchus/scyra" = "Loxorhynchus",
                        "Diopatra/chaetopterus" = "Diopatra",
                        ))%>%
  # Add "spp" when missing
  mutate(Species_orig=ifelse(is.na(Species) & !is.na(Genus), "spp", Species)) %>% 
  #Fix Species name 
  mutate(Species=recode(Species_orig,
                        "<10 cm sebastes sp."="spp",
                        "(10-15" = "spp",
                        "atrovirens,carnatus,chrysomelas,caurinus" = "spp",
                        "brevispinus, ochraceus, or giganteus" = "spp",
                        "californicum/reticulatum" = "spp",
                        "californicus /syn./ Parastichopus californicus" = "spp",
                        "californicus or Pycnopodia helianthoides" = "spp",
                        "carnatus, caurinus" = "spp",
                        "chrysomelas/carnatus young of year" = "spp",
                        "columbiana/mcpeaki" = "spp",
                        "coriacea /syn./ Tealia coriacea" = "spp",
                        "crispatus/acutifrons" = "spp",
                        "dawsoni or stimpsoni" = "spp",
                        "foliolata or Astropecten armatus" = "spp",
                        "franciscanus /syn./ Strongylocentrotus franciscanus" = 'spp',
                        "franciscanus or Strongylocentrotus purpuratus" = "spp",
                        "inflata or Hippasteria spinosa" = "spp",
                        "leukothele /syn./ Parastichopus leukothele" = "spp",
                        "lofotensis /syn./ Tealia lofotensis" = "spp",
                        "magister /syn./ Cancer magister" = "spp",
                        "melanops / mystinus/ diaconus" = "spp",
                        "melanops or mystinus" = "spp",
                        "miniata or Mediaster aequalis" = "spp",
                        "mystinus or diaconus" = "spp",
                        "N/A" = "spp",
                        "nobilis /syn./ Anisodoris nobilis" = "spp",
                        "parvimensis /syn./ Parastichopus parvimensis"="spp",
                        "pinniger or miniatus" = "spp",
                        "ritteri /syn./ Heteropolypus ritteri"="spp",
                        "serranoides or flavidus" = "spp",
                        "serranoides,flavidus" = "spp",
                        "serranoides,flavidus,melanops"= "spp",
                        "squamigerus/montereyensis" = "spp",
                        ))%>%
  # Add scientific name
  mutate(species_nwords=freeR::nwords(Species),
         sciname=ifelse(species_nwords==1, paste(Genus, Species), Species)) %>% 
  # Remove missing scientific names
  mutate(sciname=ifelse(sciname=="NA NA", NA, sciname),
         sciname=ifelse(sciname=="", NA, sciname)) %>% 
  # Sentence case
  mutate(sciname=sciname %>% stringr::str_squish() %>% stringr::str_to_sentence()) %>% 
  # Simplify
  #select(species_id, genus, sciname) %>% 
  unique() %>% 
  # Fix scientific name
  mutate(sciname=recode(sciname,
                        "<10 cm sebastes sp." = "Sebastes spp",
                        # "Actinostella bradleyi"                              
                        # "Antiopella barbarensis"                              
                        # "Atrimitra idae"                                     
                        "Beringraja binoculata"  = "Beringraja spp",                             
                        "Beringraja stellulata" = "Beringraha spp",  
                        "Diopatra/chaetopterus spp" = "Diopatra spp",
                        "Loxorhynchus/scyra spp" = "Loxorhynchus spp",
                        "Synchirus/rimicola spp" = "Synchirus",
                        "Thylacodes/petaloconchus spp" = "Thylacodes spp",
                        "Brevispinus, ochraceus, or giganteus"="Pisaster brevispinus/ochraceus/giganteus",               
                        "Californiconus californicus" = "Californiconus spp",                        
                        "Californicus /syn./ parastichopus californicus"="Apostichopus californicus",    
                        "Californicus or pycnopodia helianthoides"="Rathbunaster californicus, Pycnopodia helianthoides",       
                        "Carnatus, caurinus"="Sebastes carnatus/caurinus",
                        "Ceramium flaccidum"="Gayliella flaccida",                              
                        "Chiliensis chiliensis"="Sarda chiliensis",
                        "Chrysomelas/carnatus young of year"="Sebastes chrysomelas/carnatus",               
                        "Coriacea /syn./ tealia coriacea"="Urticina coriacea",                    
                         "Cribrinopsis albopunctata" = "Cribrinopsis spp",                       
                         "Cyanoplax hartwegii"   = "Cyanoplax spp",                              
                        "Dawsoni or stimpsoni"="Solaster dawsoni/stimpsoni",
                         "Diaperoforma californica" = "Diaperoforma spp",                           
                        # "Dictyoneurum californicum/reticulatum"              
                        # "Dodecaceria fewkesi/concharum"                       
                         "Epitonium tinctum"    = "Epitonium spp",                               
                         "Evasterias troschelii"    = "Evasterias spp",                          
                        # "Felimare californiensis"                             
                        # "Felimare porterae"                                  
                        # "Felimida macfarlandi"                                
                        # "Flabellinopsis iodinea"                             
                        "Foliolata or astropecten armatus"="Luidia foliolata, Astropecten armatus",                
                        "Franciscanus /syn./ strongylocentrotus franciscanus"="Mesocentrotus franciscanus",
                        "Franciscanus or strongylocentrotus purpuratus"="Mesocentrotus franciscanus, Strongylocentrotus purpuratus",
                        "Halichondria (halichondria)"="Halichondria panicea",
                        "Haliclona (reniera)"="Haliclona cinerea",
                        "Haliclona reniera" = "Haliclona spp",
                        "Halichondria halichondria" = "Halichondria spp",
                        "Hedophyllum sessile"   = "Hedopphyllum spp",                             
                        "Holothuria (vaneyothuria) zacae"="Holothuria zacae",
                        "Inflata or hippasteria spinosa"="Poraniopsis inflata, Hippasteria spinosa",
                        "Leukothele /syn./ parastichopus leukothele"="Apostichopus leukothele",        
                        "Lirobittium munitum"     = "Lirobittium spp",                           
                         "Lithopoma undosum"        = "Lithopoma spp",                           
                        "Lofotensis /syn./ tealia lofotensis"="Urticina lofotensis",              
                        "Loligo opalescens"="Doryteuthis opalescens",   
                        "Synchirus" = "Synchirus spp",
                        # "Lopholithodes mandtii/foraminatus"                  
                        # "Loxorhynchus/scyra crispatus/acutifrons"             
                        "Magister /syn./ cancer magister"="Metacarcinus magister",                    
                        "Melanops / mystinus/ diaconus"="Sebastes melanops/mystinus/diaconus",                      
                        "Melanops or mystinus"="Sebastes melanops/mystinus",                               
                         "Mexacanthina lugubris"          = "Mexacanthina spp",                     
                        "Miniata or mediaster aequalis"="Patiria miniata, Mediaster aequalis",
                        "Mystinus or diaconus"="Sebastes mystinus/diaconus",
                         "Nemalion elminthoides"  = "Nemalion spp",                            
                         "Neoagarum fimbriatum"     = "Neoagarum spp",                           
                         "Neobernaya spadicea"   = "Neobernaya spp",                             
                         "Neogastroclonium subarticulatum"    = "Neogastroclonium spp",                 
                        "Nobilis /syn./ anisodoris nobilis"="Montereina nobilis",
                         "Norrisia norrisii"      = "Norrisia spp",                             
                         "Okenia rosacea"   = "Okenia spp",                                  
                        "Opalescens (eggs)"="Doryteuthis opalescens",
                        "Parvimensis /syn./ parastichopus parvimensis"="Apostichopus parvimensis",       
                        # "Peltodoris nobilis"                                  
                         "Phyllospadix scouleri"    = "Phyllospadix spp",                          
                         "Phyllospadix serrulatus"       = "Phyllospadix spp",                      
                         "Phyllospadix torreyi"    = "Phyllospadix spp" ,                         
                        "Pinniger or miniatus"="Sebastes pinniger/miniatus",                              
                         "Plocamium violaceum"       = "Plocamium spp",                         
                         "Pseudobatos productus"    = "Pseudobatos spp",                           
                         "Psolus chitonoides"    = "Psolus spp",                             
                         "Pugettia foliata"   = "Pugettia spp",                                 
                        "Ritteri /syn./ heteropolypus ritteri"="Heteropolypus ritteri",               
                        # "Scyra/oregonia acutifrons/gracilis"                  
                        "Sebastes (10-15"="Sebastes spp",                                  
                        "Sebastes atrovirens,carnatus,chrysomelas,caurinus"="Sebastes atrovirens/carnatus/chrysomelas/caurinus",   
                        "Sebastes dalli"="Sebastes dallii",                                   
                         "Sebastes diaconus"   = "Sebastes spp",
                        "Sebastes diaconua" = "Sebastes spp",
                        "Sebastes n/a"="Sebastes spp",                                        
                        "Sebastes serranoides,flavidus"="Sebastes serranoides/flavidus",                         
                        "Sebastes serranoides,flavidus,melanops"="Sebastes serranoides/flavidus/melanops",         
                        "Serranoides or flavidus"="Sebastes serranoides/flavidus",                        
                        # "Stephanocystis dioica"                              
                         "Stephanocystis osmundacea"   = "Stephanocystis spp",                        
                         "Taonia lennebackerae"     = "Taonia spp",                          
                         "Tethya aurantia"  = "Tethya spp",                                   
                        "Tetilla sp"="Tetilla spp",                                        
                         "Tetronarce californica"  = "Tetronarce spp",                            
                        # "Thylacodes/petaloconchus squamigerus/montereyensis" 
                        # "Trikentrion helium"                                  
                        # "Urticina columbiana/mcpeaki"                        
                         "Xenistius californiensis" = "Xenistius spp",                           
                         "Xystreurys rubescens" = "Xystreurys spp"
                        )) %>% 
  # Mark level
  mutate(level=ifelse(grepl("spp|/|,", sciname) | is.na(Genus), "group", "species"))

# Inspect
sort(unique(spp$Genus))

# Check names
spp_names <- spp$sciname[spp$level=="species"] %>% unique() %>% sort() 
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



