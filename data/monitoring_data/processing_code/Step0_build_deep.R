# Process Deep Reef Data
# Cori Lopazanski
# August 21, 2023

# About --------------------------------------------------------------------------------
# Reading and processing steps adapted from Josh Smith, Step1_process_biomass.R

# Goal: Separate processing code from biomass conversion code, and review all steps
# for errors and potential concerns that could propagate. This script contains all major
# cleaning of the raw deep reef ROV data.

# Summary of key changes from original code:
# - Corrected treatment of NA values 
# - Several species still weren't matching with taxonomy table - fixed
# - Removed step that added duplicates for multiple affiliated MPAs (until can confirm - see below)
# - Updated target status for those that were not listed in original target status table
# - There are several combinations of group-site-designation that aren't in the main
#   deep reef site table - these have been discussed with the PI (RS) and confirmed 
#   treatment for each. Many of these cases are type == Reference and designation == Reference,
#   which we assume to be cases where a Reference site was sampled, and the type was incorrectly 
#   listed as "Reference" when it should have been either "SMR" or "SMCA." These were confirmed 
#   individually. 
#     * Farallon Island is associated with Southeast Farallon Island SMCA
#     * The mpa_name sometimes has multiple sites lislted; we do not use this and only
#       consider the MPA from the mpa_group column


# Setup --------------------------------------------------------------------------------
rm(list=ls())

# Load required packages
library(tidyverse)
library(janitor)

# Directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/ROV_Dataset"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

# Read deep reef monitoring data ---------------------------------------------
deep_reef_raw <- read_csv(file.path(datadir, "/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), 
                          na = c("N/A", "", " ")) %>% clean_names() 

# This is their site table from DataOne but it is incomplete given sites in the data
#deep_reef_sites <- readxl::read_excel(file.path(datadir, "/MidDepth_ROV_Site_Table.xlsx")) %>% clean_names()

# Read additional data ----------------------------------------------------------------
# Read taxonomy table 
taxon_deep <- read_csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>% 
  filter(habitat == "Deep reef")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))

# Read de-facto SMRs
defacto_smr_deep_reef <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>% 
  filter(group=="deep_reef") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class) %>% 
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr"),
         mpa_defacto_class = tolower(mpa_defacto_class)) %>%
  #add missing mpa_defacto_class
  bind_rows(tibble(affiliated_mpa = "point buchon smr", mpa_defacto_class = "smr"))

# Build Data ------------------------------------------------------------------------
# Note: keeping this description for clarity but have confirmed will only use primary (Sept 2023)
# Names are not always consistently listed in order -- create primary, secondary, tertiary as follows:
# - When there is only one mpa_name given, it always matches the mpa_group + type listed 
# - If no mpa_name is given, the affiliated_mpa is the mpa_group + type listed with no secondary
# - When multiple mpa_names are given,
#      * If the first mpa_name listed matches the mpa_group + type listed, the affiliated_mpa
#         remains the mpa_group + type listed, and the second mpa_name becomes the secondary_mpa 
#      * If the first mpa_name listed does not match the mpa_group + type listed, 
#         the affiliated_mpa remains the mpa_group + type listed, and the first mpa_name 
#         becomes the secondary_mpa 

data <- deep_reef_raw %>% 
  # Trim white space across all columns
  mutate(across(everything(), str_trim)) %>% 
  # Per PI instructions: Only keep transects affiliated with MPAs and that do not cross boundaries
  filter(type %in% c("SMR", "SMCA", "Reference")) %>% 
  # Drop the "_REF" from the names
  mutate(mpa_name = str_remove_all(mpa_name, "_REF"),
         mpa_name = str_replace_all(mpa_name, "_", " ")) %>% 
  # Correct naming across all columns
  mutate(across(location:designation, str_replace, 'Ano Nuevo','Año Nuevo')) %>%
  mutate(across(location:designation, str_replace, 'Islands','Island')) %>%
  mutate(across(location:designation, str_replace, 'SE ','Southeast ')) %>%
  mutate(across(location:designation, str_replace, 'Bodega Bay','Bodega Head')) %>%
  mutate(across(location:designation, str_replace, 'Point St. George','Point St. George Reef Offshore')) %>%
  # Correct Farallon Island to Southeast Farallon Island SMCA (Confirmed with RS Sept 2023)
  mutate(mpa_name = if_else(mpa_group == "Farallon Island", "Southeast Farallon Island SMCA", mpa_name),
         mpa_group = if_else(mpa_group == "Farallon Island", "Southeast Farallon Island", mpa_group),
         ) %>% 
  # Correct Ano Nuevo to SMR (incorrectly listed as SMCA) 
  mutate(type = if_else(mpa_group == 'Año Nuevo', "SMR", type)) %>% 
  # Create affiliated_mpa variable - confirmed in Sept 2023 that will only use primary!
  mutate(affiliated_mpa = 
           case_when(type %in% c("SMCA", "SMR") ~ paste(mpa_group, type, sep = " "),
                     # Provide the full affiliated MPA name for sites called "Reference"
                     type == "Reference" &
                       mpa_group %in% c("Campus Point", "Southeast Farallon Islands", "Pillar Point", 
                                        "Point St. George Reef Offshore", "Portuguese Ledge") ~ paste(mpa_group, "SMCA", sep = " "),
                     type == "Reference" & 
                       mpa_group %in% c("Año Nuevo", "Carrington Point", "Gull Island", "Harris Point", 
                                        "Point Buchon", "Point Conception", "Point Lobos", 
                                        "Sea Lion Gulch", "South Point", "Ten Mile") ~ paste(mpa_group, "SMR", sep = " "),
                     # These MPAs have both SMR and SMCA - will select SMR as the primary affiliated MPA and later list SMCA as secondary
                     type == "Reference" & 
                       mpa_group %in% c("Big Creek", "Bodega Head", "Point Sur") ~  paste(mpa_group, "SMR", sep = " "))) %>% 
  separate(mpa_name, into=c("primary_mpa", "secondary_mpa","tertiary_mpa"),sep = ", ", convert = TRUE) %>% 
  # Secondary/tertiary naming correction (described above)
  mutate(secondary_mpa = if_else(!(affiliated_mpa == primary_mpa), primary_mpa, secondary_mpa)) %>% 
  # These MPAs have both SMR and SMCA - select SMCA as secondary affiliated MPA
  mutate(secondary_mpa = if_else(type == "Reference" & mpa_group %in% c("Big Creek", "Bodega Bay", "Point Sur"),
                                 paste(mpa_group, "SMCA", sep = " "), secondary_mpa)) %>% 
  # Create date columns
  # Note: day not used b/c recorded in GMT and therefore sometimes crosses over to next day
  mutate(survey_date = mdy(case_when(dive == 320 ~ "8/19/2006", # Use date instead of survey_year b/c sometimes survey_year is NA
                                     dive == 332 ~ "9/1/2006",
                                     dive == 341 ~ "9/26/2006",
                                     TRUE~survey_date)),
         year = year(survey_date),
         month = month(survey_date)) %>%
  # Convert affiliated mpa columns to lowercase 
  mutate((across(c(affiliated_mpa, secondary_mpa, tertiary_mpa), \(x) str_to_lower(x)))) %>% 
  # Add defacto SMRs based on affiliated_mpa
  left_join(defacto_smr_deep_reef, by = "affiliated_mpa") %>% 
  mutate(mpa_defacto_class = if_else(is.na(mpa_defacto_class) & type == "SMR", "SMR", mpa_defacto_class),
         mpa_defacto_designation = if_else(type == "Reference" | designation == "Reference", "REF", mpa_defacto_class)) %>% 
  # Add official mpa_state_class and mpa_state_designation to match other datasets 
  mutate(mpa_state_class = if_else(type == "Reference", str_to_upper(word(affiliated_mpa, -1)), type),
         mpa_state_designation = if_else(designation == "Reference", "REF", mpa_state_class)) %>% 
  # Fix
  mutate(mpa_defacto_designation = if_else(is.na(mpa_defacto_designation) & mpa_state_class == "SMR", "SMR", mpa_defacto_designation)) %>% 
  # Add taxa
  left_join(taxon_deep, by = c("scientific_name" = "habitat_specific_spp_name")) %>% 
  # Assign NO_ORG for empty transects
  mutate(species_code = case_when(is.na(scientific_name) & is.na(estimated_length_cm) ~ "NO_ORG", 
                                  !is.na(scientific_name) & is.na(sciname) ~ "UNKNOWN", 
                                  is.na(scientific_name) & is.na(sciname) & !is.na(estimated_length_cm) ~ "UNKNOWN", # length recorded but not identified
                                  T ~ NA)) %>% 
  mutate(sl_cm = NA,# create to match other habitats
         mpa_defacto_designation = str_to_lower(mpa_defacto_designation), # lower to match other habitats
         mpa_defacto_class = str_to_lower(mpa_defacto_class)) %>% 
  # Fix dive with error
  mutate(dive = if_else(scientific_name == "Sebastes mystinus or diaconus" & line_id == "143_1", "143", dive)) %>% 
  mutate(dive = if_else(scientific_name == "Young of year (<10 cm Sebastes sp.)" & line_id == "141_780", "141", dive)) %>% 
  select(year, month,
         mpa_group, type, designation, # keep these for now until confirm site treatment
         affiliated_mpa, secondary_mpa, tertiary_mpa,
         mpa_state_class, mpa_state_designation, mpa_defacto_class, mpa_defacto_designation,
         line_id, dive, line, scientific_name, count, tl_cm = estimated_length_cm, sl_cm,
         class, order, family, genus, species, 
         sciname, species_code, target_status, level) %>%
  #assign reference/reference sites that do not have an affiliated_mpa 
  mutate(affiliated_mpa = case_when(
    mpa_group == "Southeast Farallon Island" & type == "Reference" & designation == "Reference" ~ "southeast farallon island smr",
    TRUE ~ affiliated_mpa
  ),
  secondary_mpa = case_when(
    mpa_group == "Southeast Farallon Island" & type == "Reference" & designation == "Reference" ~ "southeast farallon island smca",
    TRUE ~ secondary_mpa
  ))%>%
  #fix missing mpa_state_class
  mutate(mpa_state_class = ifelse(is.na(mpa_state_class), toupper(word(affiliated_mpa, -1)),mpa_state_class)) %>%
  #rejoin defacto class to fill NAs
  select(-mpa_defacto_class)%>%
  left_join(defacto_smr_deep_reef, by = "affiliated_mpa") %>%
  # Add regions
  left_join(regions, by = "affiliated_mpa")%>%
  #arrange
  select(year, month,
                mpa_group, type, designation, # keep these for now until confirm site treatment
                bioregion, region4, affiliated_mpa, secondary_mpa, tertiary_mpa,
                mpa_state_class, mpa_state_designation, mpa_defacto_class, mpa_defacto_designation,
                line_id, dive, line, scientific_name, count, tl_cm, sl_cm,
                class, order, family, genus, species, 
                sciname, species_code, target_status, level) 
  

# Note: Warning "Expected 3 Pieces" is OK (not every entry has secondary & tertiary mpa)

# Check for entries with missing sciname (6 ok)
taxa_na <- data %>% 
  filter(is.na(sciname)) %>% 
  distinct(scientific_name)

# Write to CSV
#write.csv(data, row.names = F, file.path(outdir,"/deep_reef_processed.csv"))  
# last write 13 Sept 2023

### EXPLORING ISSUES BELOW

dives <- data %>% 
  distinct(year, month, dive, line_id)

duplicates <- data %>% 
  mutate(transect_id = paste(year, line_id, sep = "_")) %>% 
  distinct(year, month, line_id, dive, transect_id, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  mutate(multiple = if_else(transect_id %in% transect_id[duplicated(transect_id)], T, F)) %>% 
  filter(multiple)

test <- data %>% 
  filter(species_code == "NO_ORG")



#Fix duplicates ----------------------------------------------------------------

#NOTE: per PI, when a single reference was used for more than one MPA, the fish
#observations (rows) were duplicated. We are going to drop the duplicates and 
#ensure that each MPA site has a single paired reference sites. 

#Steps:

#check structure
str(data)
str(duplicates)

#step 1: identify unique levels of 'type' for each mpa_group in the duplicates and
# drop the reference site for the SMCA if there is both an SMCA and SMR. 

drop_levels <- duplicates %>%
  mutate(mpa_group = str_replace(affiliated_mpa, "\\s\\w+$", ""),
    type = toupper(word(affiliated_mpa, -1)),
    transect_id_desig = paste0(transect_id,"_",mpa_defacto_designation)) %>%
  filter(mpa_defacto_class == "smca") %>%
  dplyr::select(-multiple) 
  

#drop duplicates
data2 <- data %>%
  mutate(transect_id_desig = paste0(year,"_",line_id,"_",mpa_defacto_designation))%>%
  filter(!(transect_id_desig %in% drop_levels$transect_id_desig))


#recheck duplicates
duplicates2 <- data2 %>% 
  mutate(transect_id = paste(year, line_id, sep = "_")) %>% 
  distinct(year, month, line_id, dive, transect_id, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(multiple = if_else(transect_id %in% transect_id[duplicated(transect_id)], T, F)) %>% 
  filter(multiple)

unique(duplicates2$affiliated_mpa)

#step 2: balance reference sites that were used for multiple smrs

dup_transects <- duplicates2 %>%
  mutate(transect_id_desig = paste0(year,"_",line_id,"_",mpa_defacto_designation))%>%
  group_by(transect_id_desig) %>%
  distinct(affiliated_mpa)

#find south point / gull island transects
sp_gi <- dup_transects %>% filter(affiliated_mpa %in% c("south point smr","gull island smr"))

#find point sur/ piedras blancas transects
pbr_pb <- dup_transects %>% filter(affiliated_mpa %in% c("point buchon smr","piedras blancas smr"))

g <- as.vector(unique(sp_gi$transect_id_desig))
p <- as.vector(unique(pbr_pb$transect_id_desig))

transect_drop_levels <- dup_transects %>%
  #case-by-case corrections
  filter(
    #point arena smca / point arena smr duplicate -- drop smca
    (transect_id_desig == "2011_207_1990_smr" & affiliated_mpa == "point arena smca") |
          #point sur / point buchon -- drop point sur smca
           affiliated_mpa == "point sur smca" |
      #alternate drop between gull island and south point to balance samples
      (transect_id_desig == g[1] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[2] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[3] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[4] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[5] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[6] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[7] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[8] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[9] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[10] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[11] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[12] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[13] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[14] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[15] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[16] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[17] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[18] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[19] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[20] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[21] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[22] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[23] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[24] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[25] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[26] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[27] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[28] & affiliated_mpa == "gull island smr")|
      (transect_id_desig == g[29] & affiliated_mpa == "south point smr")|
      (transect_id_desig == g[30] & affiliated_mpa == "gull island smr")|
      #now balance samples for point buchon / piedras blancas
      (transect_id_desig == p[1] & affiliated_mpa == "point buchon smr")|
      (transect_id_desig == p[2] & affiliated_mpa == "piedras blancas smr")|
      (transect_id_desig == p[3] & affiliated_mpa == "point buchon smr"))
      

data3 <- data2 %>%
          filter(!(transect_id_desig %in% transect_drop_levels$transect_id_desig &
                     affiliated_mpa %in% transect_drop_levels$affiliated_mpa &
                     mpa_defacto_designation == "ref")) %>%
          #drop final duplicate
          filter(!(transect_id_desig == "2011_207_1990_smr" & affiliated_mpa == "point arena smca"))


#recheck duplicates
duplicates3 <- data3 %>% 
  mutate(transect_id = paste(year, line_id, sep = "_")) %>% 
  distinct(year, month, line_id, dive, transect_id, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(multiple = if_else(transect_id %in% transect_id[duplicated(transect_id)], T, F)) %>% 
  filter(multiple)

#check
nrow(data) - nrow(data2)



################################################################################
#export

write.csv(data3, row.names = F, file.path(outdir,"/deep_reef_processed.csv"))  

# last write 13 Dec 2023





