# Process Deep Reef Data
# Cori Lopazanski
# August 21, 2023

# About --------------------------------------------------------------------------------
# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Goal: Separate processing code from biomass conversion code, and review all steps
# for errors and potential concerns that could propagate.

# Summary of key changes from original code:
# - 

# Note potential concerns for next steps:
# - 

# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)

# Directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring//monitoring_deep-reef/ROV_Dataset"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read deep reef monitoring data ---------------------------------------------
deep_reef_raw <- read.csv(file.path(datadir, "/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), na = c("N/A", "")) %>% 
  clean_names() 

deep_reef_sites <- readxl::read_excel(file.path(datadir, "/MidDepth_ROV_Site_Table.xlsx")) %>% clean_names()

# Read additional data ----------------------------------------------------------------
# Read taxonomy table 
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>% 
  filter(habitat == "Deep reef")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))

# Read de-facto SMRs
defacto_smr_deep_reef <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>% 
  filter(group=="deep_reef") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

# Read length-weight parameters
params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv") %>%
  mutate(ScientificName_accepted = recode(ScientificName_accepted, "Sebastes spp." = "Sebastes spp")) %>%
  filter(!is.na(ScientificName_accepted))


# Process Data ------------------------------------------------------------------------


data <- deep_reef_raw %>% 
  # Per PI instructions: Only keep transects affiliated with MPAs and that do not cross boundaries
  filter(type %in% c("SMR", "SMCA", "Reference"))

# Still needs review:
# - "Farallon Island" site (for which MPA?)
# - Confirm treatment of type == reference and designation == reference

# Secondary/Tertiary Naming Correction:
# Names are not always consistently listed in order -- create primary, secondary, tertiary as follows:
# - When there is only one mpa_name given, it always matches the mpa_group + type listed 
# - If no mpa_name is given, the affiliated_mpa is the mpa_group + type listed with no secondary
# - When multiple mpa_names are given,
#      * If the first mpa_name listed matches the mpa_group + type listed, the affiliated_mpa
#         remains the mpa_group + type listed, and the second mpa_name becomes the secondary_mpa 
#      * If the first mpa_name listed does not match the mpa_group + type listed, 
#         the affiliated_mpa remains the mpa_group + type listed, and the first mpa_name 
#         becomes the secondary_mpa 

data2 <- data %>% 
  # Drop the "_REF" from the names
  mutate(mpa_name = str_remove_all(mpa_name, "_REF"),
         mpa_name = str_replace_all(mpa_name, "_", " ")) %>% 
  # Correct naming across all columns
  mutate(across(location:designation, str_replace, 'Ano Nuevo','Año Nuevo')) %>%
  mutate(across(location:designation, str_replace, 'Islands','Island')) %>%
  mutate(across(location:designation, str_replace, 'SE ','Southeast ')) %>%
  mutate(across(location:designation, str_replace, 'Bodega Bay','Bodega Head')) %>%
  mutate(across(location:designation, str_replace, 'Point St. George','Point St. George Reef Ofshore')) %>%
  # Correct Ano Nuevo to SMR (incorrectly listed as SMCA) 
  mutate(type = if_else(mpa_group == 'Año Nuevo', "SMR", type)) %>% 
  # Create affiliated_mpa variable 
  mutate(affiliated_mpa = 
           case_when(type %in% c("SMCA", "SMR") ~ paste(mpa_group, type, sep = " "),
                     # Provide the full affiliated MPA name for sites called "Reference"
                     type == "Reference" &
                       mpa_group %in% c("Campus Point", "Farallon Islands", "Pillar Point", 
                                        "Point St. George Reef Ofshore", "Portuguese Ledge") ~ paste(mpa_group, "SMCA", sep = " "),
                     type == "Reference" & 
                       mpa_group %in% c("Año Nuevo", "Carrington Point", "Gull Island", "Harris Point", 
                                        "Point Buchon", "Point Conception", "Point Lobos", 
                                        "Sea Lion Gulch", "South Point", "Ten Mile") ~ paste(mpa_group, "SMR", sep = " "),
                     # These MPAs have both SMR and SMCA - will select SMR as the primary affiliated MPA and later list SMCA as secondary
                     type == "Reference" & 
                       mpa_group %in% c("Big Creek", "Bodega Bay", "Point Sur") ~  paste(mpa_group, "SMR", sep = " "))) %>% 
  separate(mpa_name, into=c("primary_mpa", "secondary_mpa","tertiary_mpa"),sep = ", ", convert = TRUE) %>% 
  # Secondary/tertiary naming correction (described above)
  mutate(secondary_mpa = if_else(!(affiliated_mpa == primary_mpa), primary_mpa, secondary_mpa)) %>% 
  # These MPAs have both SMR and SMCA - select SMCA as secondary affiliated MPA
  mutate(secondary_mpa = if_else(type == "Reference" & mpa_group %in% c("Big Creek", "Bodega Bay", "Point Sur"),
                                 paste(mpa_group, "SMCA", sep = " "), secondary_mpa)) %>% 
  select(year = survey_year, mpa_group:common_name, 
         count, estimated_length_cm, 
         affiliated_mpa, secondary_mpa, tertiary_mpa) %>% 
  mutate(across(affiliated_mpa:tertiary_mpa), str_to_lower)



################################################################################



sites_josh <- deep_reef_build1 %>% 
  select(location:designation,site_type, mpa_name1, primary_final, secondary_final, tertiary_final) %>% distinct() %>% 
  arrange(mpa_group, type, designation)


#process deep reef

#Get ready ... 

deep_reef_build1 <- deep_reef_raw %>%
  #remove transects that crossed MPA boundaries -- per PI
  filter(!(type == ""| #Random transect not affiliated with a MPA
             type == "SMR/SMCA"| #this means the transect crossed from the SMR into the SMCA
             type == "SMCA/SMR"| #this means the transect cross from the SMCA into the SMCA
             type == "N/A")) %>% #Random transect not affiliated with a MPA
  #clean up mpa name
  mutate(
    #assign missing MPAs a name
    mpa_name = ifelse(mpa_name == "",paste(mpa_group, type),mpa_name),
    #clean up
    mpa_name1 = str_replace_all(mpa_name, "_", " "),
    #determine whether site was in an MPA or ref
    site_type = word(mpa_name1,-1),
    site_type = ifelse(site_type == "","REF",site_type),
    mpa_name2 = trimws(str_replace_all(mpa_name1, "REF", "")))%>%
  #separate MPAs 
  separate(mpa_name2, into=c("primary_mpa", "secondary_mpa","tertiary_mpa"),sep = ",", convert = TRUE)%>%
  #dplyr::select(!c(secondary_mpa, tertiary_mpa))%>%
  #trim ws
  mutate(primary_mpa = trimws(primary_mpa)) %>%
  #recode reference MPAs to match pair
  mutate(primary_mpa2 = recode(primary_mpa, 
                               "Ano Nuevo Reference" = "Ano Nuevo SMCA",
                               "Big Creek Reference" = "Big Creek SMCA, Big Creek SMR",
                               "Campus Point Reference" = "Campus Point SMCA",
                               "Carrington Point Reference" = "Carrington Point SMR",
                               "Farallon Islands Reference" = "Farallon Islands SMCA",
                               "Gull Island Reference" = "Gull Island SMR",
                               "Harris Point Reference" = "Harris Point SMR",
                               "Pillar Point Reference" = "Pillar Point SMCA",
                               "Point Buchon Reference" = "Point Buchon SMR",
                               "Point Conception Reference" = "Point Conception SMR",
                               "Point Lobos Reference" = "Point Lobos SMR",
                               "Point St. George Reference" = "Point St. George SMCA",
                               "Point Sur Reference" = "Point Sur SMCA, Point Sur SMR",
                               "Portuguese Ledge Reference" = "Portuguese Ledge SMCA",
                               "Sea Lion Gulch Reference" = "Sea Lion Gulch SMR",
                               "South Point Reference" = "South Point SMR",
                               "Ten Mile Reference" = "Ten Mile SMR",
                               "Bodega Bay Reference" = "Bodega Bay SMR, Bodega Bay SMCA"))%>%
  separate(primary_mpa2, into=c("primary_mpa2", "secondary_mpa2","tertiary_mpa2"),sep = ",", convert = TRUE)%>%
  mutate(primary_final = primary_mpa2,
         secondary_final = paste(secondary_mpa, secondary_mpa2),
         tertiary_final = paste(tertiary_mpa, tertiary_mpa2),
         secondary_final = trimws(str_replace_all(secondary_final, "NA", "")),
         tertiary_final = trimws(str_replace_all(tertiary_final, "NA", "")))

unique(sort(deep_reef_build1$primary_final))
unique(sort(deep_reef_build1$secondary_final))
unique(sort(deep_reef_build1$tertiary_final))

#clean up

deep_reef_build2 <- deep_reef_build1 %>%
  dplyr::select(year= survey_year, 
                primary_mpa = primary_final,
                secondary_mpa = secondary_final, 
                tertiary_mpa = tertiary_final,
                site_type, line_id, line, dive, scientific_name,
                common_name, count, estimated_length_cm) %>%
  mutate(secondary_mpa = ifelse(secondary_mpa=="",NA,secondary_mpa),
         tertiary_mpa = ifelse(tertiary_mpa == "",NA,tertiary_mpa),
         site_type = recode(site_type, "Reference"="REF")) %>%
  #create single affiliated_mpa -- reference sites with multiple affiliated mpas will have duplicates
  pivot_longer(cols=c("primary_mpa","secondary_mpa","tertiary_mpa"),
               names_to = "mpa_order", values_to = "affiliated_mpa",
               values_drop_na = TRUE) %>%
  mutate(MPA_type = word(affiliated_mpa, -1),
         affiliated_mpa = tolower(affiliated_mpa),
         #correct spelling to match mpa attribute table
         affiliated_mpa = recode(affiliated_mpa, 
                                 "point buchon smr" = "point buchon smca",
                                 "southeast farallon islands smca"= "southeast farallon island smca",
                                 "southeast farallon islands smr"="southeast farallon island smr",
                                 "ano nuevo smca"= "ano nuevo smr",
                                 "bodega bay smr"="bodega head smr",
                                 "bodega bay smca"="bodega head smca",
                                 "farallon islands smca"="southeast farallon island smca",
                                 "point st. george smca" = "point st. george reef offshore smca"
         )
  )

unique(deep_reef_build2$MPA_type)

#add defacto SMRs

deep_reef_build3 <- left_join(deep_reef_build2, defacto_smr_deep_reef, by="affiliated_mpa") %>%
  mutate(mpa_defacto_designation = ifelse(site_type == "REF","REF",mpa_defacto_class),
         affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr"))

#add regions
deep_reef_build4 <- left_join(deep_reef_build3, regions, by=c("affiliated_mpa"="name"))%>%
  dplyr::rename(fish_tl = estimated_length_cm)


#add taxa and correct species spellings
deep_reef_taxa <- taxon_tab %>% filter(habitat=="Deep reef") %>%
  #fix target status
  mutate(target_status = ifelse(habitat_specific_spp_name == "Sebastes melanops or mystinus", "Targeted",target_status),
         target_status = ifelse(habitat_specific_spp_name == "Sebastes melanops or mystinus or diaconus", "Targeted",target_status),
         target_status = ifelse(habitat_specific_spp_name == "Sebastes mystinus or diaconus", "Targeted",target_status),
         target_status = ifelse(habitat_specific_spp_name == "Sebastes pinniger or miniatus", "Targeted",target_status),
         target_status = ifelse(habitat_specific_spp_name == "Sebastes diaconus", "Targeted",target_status),
         target_status = ifelse(habitat_specific_spp_name == "Sebastes serranoides or flavidus", "Targeted",target_status),
         target_status = ifelse(habitat_specific_spp_name == "Sebastes serranoides or flavidus", "Targeted",target_status)
  )

deep_reef_build5 <- left_join(deep_reef_build4, deep_reef_taxa, by=c("scientific_name"="habitat_specific_spp_name"), multiple="all")%>%
  filter(is.na(sciname)) #identify species that are spelled incorrectly

deep_reef_build6 <- deep_reef_build4 %>%
  #fix spelling
  mutate(scientific_name = recode(scientific_name,
                                  "Pleuronectidae spp." = "Pleuronectidae spp",
                                  "sebastes miniatus" = "Sebastes miniatus",
                                  "Hexagrammus decagrammus" = "Hexagrammos decagrammus",
                                  "Sebastes paucipinis" = "Sebastes paucispinis",
                                  "Sebastes minatus"="Sebastes miniatus",
                                  "Sebastes letiginosus"="Sebastes lentiginosus",
                                  "Sebases serranoides" = "Sebastes serranoides",
                                  "Rhinogobiops nicholsii" = "Rhinogobiops nicholsii",
                                  "Chromis Punctipinnis" = "Chromis punctipinnis",
                                  "Sebastes Caurinus" = "Sebastes caurinus",
                                  "Oxylebius rictus†"= "Oxylebius rictus",
                                  "Sebastes hopskini" = "Sebastes hopkinsi",
                                  "Sebaste miniatus" = "Sebastes miniatus",
                                  "Hexagrammos decagrammus†" = "Hexagrammos decagrammus",
                                  "Sebastes lentiginosus" = "Sebastes lentiginosus",
                                  "Sebastes umbrosus†" = "Sebastes umbrosus",
                                  "Sebastes dalii"="Sebastes dallii",
                                  "Sebastes pauscipinis" = "Sebastes paucispinis",
                                  "Sebastes ensifer" = "Sebastes ensifer",
                                  "Hydrolagus collei" = "Hydrolagus collei",
                                  "Zaniolepis Spp." = "Zaniolepis spp",
                                  "Starry Rockfish" = "Sebastes constellatus",
                                  "Sebastes carnatus " = "Sebastes carnatus",
                                  "Zaniolepis spp." ="Zaniolepis spp",
                                  "Sebastes nebulosus " ="Sebastes nebulosus",
                                  "Hyperprosopon ellipticum" = "Hyperprosopon ellipticum",
                                  "Sebastes mystinus " = "Sebastes mystinus",
                                  "Hexagrammos decagrammus " = "Hexagrammos decagrammus",
                                  "Sebastes caurinus " = "Sebastes caurinus",
                                  "Sebastes miniatus " = "Sebastes miniatus",
                                  "sebastes maliger" = "Sebastes maliger",
                                  "Sebastes pinniger " = "Sebastes pinniger",
                                  "Ophiodon elongatus " = "Ophiodon elongatus",
                                  "Unidentified Sebastes sp." = "Sebastes spp"
  ))

deep_reef_build7 <- left_join(deep_reef_build6, deep_reef_taxa, by=c("scientific_name"="habitat_specific_spp_name"), multiple="all")


#calculate biomass
deep_reef_build8 <- convert_dat(params_tab, deep_reef_build7)
deep_reef_build9 <- bio_fun(deep_reef_build8) %>%
  #use 'NO_ORG' to keep track of effort
  mutate(habitat_specific_code = ifelse(scientific_name == "","NO_ORG",habitat_specific_code))
#filter(!(is.na(weight_g)))

#clean up
deep_reef_build10 <- deep_reef_build9 %>%
  mutate(total_biom_g = weight_g*count,
         total_biom_kg = total_biom_g/1000)%>%
  dplyr::select(year, bioregion, region4, affiliated_mpa,
                site_type, mpa_defacto_class, mpa_defacto_designation,
                line_id, line, dive, habitat_specific_code, habitat_specific_id = scientific_name,
                sciname, target_status, count, TL_cm, weight_g, total_biom_kg)


#write.csv(deep_reef_build10, row.names = F, file.path(outdir,"/biomass_processed/deep_reef_fish_biomass.csv"))  