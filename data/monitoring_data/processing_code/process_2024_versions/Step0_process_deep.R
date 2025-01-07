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
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring"
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/ROV_Dataset"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

# Read deep reef monitoring data ---------------------------------------------
deep_reef_raw <- read_csv(file.path(datadir, "/ROVLengths2005-2019Merged-2021-02-02SLZ.csv"), 
                          col_select = c(1:19),
                          col_types = c("dcccccccccddccccddc"),
                          na = c("N/A", "", " ")) %>% clean_names()

# This is their site table from DataOne but it is incomplete given sites in the data
#deep_reef_sites <- readxl::read_excel(file.path(datadir, "/MidDepth_ROV_Site_Table.xlsx")) %>% clean_names()

# Locations of each dive/line ID provided by SLZ to CL on Jan 1 2025 via email.
# NOTE: Several missing from either dataset. Assuming filtering applied appropriately. 
deep_sites <- readxl::read_xlsx(file.path(ltm.dir, "monitoring_deep-reef/DeepwaterROVlocations.xlsx"),
                                na = c("N/A", "", " ")) %>% janitor::clean_names()


# Read additional data ----------------------------------------------------------------
# Read taxonomy table 
taxon_deep <- read_csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>%
  #reassign target_status_standardized for downstream code
  select(-target_status) %>%
  rename(target_status = target_status_standardized)%>%
  filter(habitat == "Deep reef")

# Read regions from MPA attributes table
regions <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds") %>% 
  dplyr::select(affiliated_mpa = name, bioregion, region4 = four_region_north_ci) %>%
  mutate(affiliated_mpa = tolower(affiliated_mpa))

# Read de-facto SMRs
defacto_smr_deep_reef <- readxl::read_excel("/home/shares/ca-mpa/data/sync-data/mpa_traits/mpa-attributes.xlsx", sheet = 5, skip = 0, na = "NA") %>% 
  filter(group=="deep_reef") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class) %>% 
  mutate(mpa_defacto_class = tolower(mpa_defacto_class)) %>%
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
  mutate(across(where(is.character), str_trim)) %>% 
  # Per PI instructions: Only keep transects affiliated with MPAs and that do not cross boundaries
  # The only two MPA groups that cross boundaries are for Farallon Islands and Point Sur (both defacto SMR)
  # so will keep those for this study.
  filter(type %in% c("SMR", "SMCA", "Reference", "SMR/SMCA", "SMCA/SMR")) %>% 
  mutate(type = if_else(type %in% c("SMR/SMCA", "SMCA/SMR"), "SMR", type)) %>% 
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
                       mpa_group %in% c("Año Nuevo", "Carrington Point", "Gull Island", "Harris Point", 
                                        "Point Buchon", "Point Conception", "Point Lobos", 
                                        "Sea Lion Gulch", "South Point", "Ten Mile") ~ paste(mpa_group, "SMR", sep = " "),
                     type == "Reference" &
                       mpa_group %in% c("Campus Point", "Southeast Farallon Island", "Pillar Point", 
                                        "Point St. George Reef Offshore", "Portuguese Ledge") ~ paste(mpa_group, "SMCA", sep = " "),
                     # These MPAs have both SMR and SMCA - will select SMR as the primary affiliated MPA and later list SMCA as secondary
                     type == "Reference" & 
                       mpa_group %in% c("Big Creek", "Bodega Head", "Point Sur") ~  paste(mpa_group, "SMR", sep = " "))) %>% 
  # Create date columns
  # Note: day not used b/c recorded in GMT and therefore sometimes crosses over to next day
  # Use date instead of survey_year b/c sometimes survey_year is NA
  mutate(survey_date = mdy(case_when(dive == "332" & mpa_group == "Gull Island" ~ "9/1/2006",
                                     dive == "341" & mpa_group == "Carrington Point"~ "9/26/2006",
                                     TRUE~survey_date)),
         year = year(survey_date),
         month = month(survey_date)) %>%
  # Convert affiliated mpa columns to lowercase 
  mutate((across(c(affiliated_mpa), \(x) str_to_lower(x)))) %>% 
  # Add defacto SMRs based on affiliated_mpa
  left_join(defacto_smr_deep_reef, by = "affiliated_mpa") %>% 
  mutate(mpa_defacto_class = if_else(is.na(mpa_defacto_class) & type == "SMR", "smr", mpa_defacto_class),
         mpa_defacto_designation = if_else(type == "Reference" | designation == "Reference", "ref", mpa_defacto_class)) %>% 
  # Add official mpa_state_class and mpa_state_designation to match other datasets 
  mutate(mpa_state_class = if_else(type == "Reference", str_to_upper(word(affiliated_mpa, -1)), type),
         mpa_state_designation = if_else(designation == "Reference", "REF", mpa_state_class)) %>% 
  # Add taxa
  left_join(taxon_deep, by = c("scientific_name" = "habitat_specific_spp_name")) %>% 
  # Assign NO_ORG for empty transects
  mutate(species_code = case_when(is.na(scientific_name) & is.na(estimated_length_cm) ~ "NO_ORG", 
                                  !is.na(scientific_name) & is.na(sciname) ~ "UNKNOWN", 
                                  is.na(scientific_name) & is.na(sciname) & !is.na(estimated_length_cm) ~ "UNKNOWN", # length recorded but not identified
                                  T ~ NA)) %>% 
  mutate(sl_cm = as.numeric(NA)) %>% # create to match other habitats
  # Fix dives with errors
  mutate(dive = if_else(scientific_name == "Sebastes mystinus or diaconus" & line_id == "143_1", "143", dive)) %>% 
  mutate(dive = if_else(scientific_name == "Young of year (<10 cm Sebastes sp.)" & line_id == "141_780", "141", dive)) %>% 
  mutate(dive = if_else(line_id == "88_70" & dive == "89", "88", dive)) %>% 
  select(year, month,
         mpa_group, type, designation, # keep these for now until confirm site treatment
         affiliated_mpa, mpa_state_class, mpa_state_designation, mpa_defacto_class, mpa_defacto_designation,
         line_id, dive, line, scientific_name, count, tl_cm = estimated_length_cm, sl_cm,
         class, order, family, genus, species, 
         sciname, species_code, target_status, level) %>%
  # Add regions
  left_join(regions, by = "affiliated_mpa") %>%
  # Add transect ID
  mutate(transect_id = paste(year, line_id, sep = "_")) %>% 
  #arrange
  select(year, month,
         mpa_group, type, designation, # keep these for now until confirm site treatment
         bioregion, region4, affiliated_mpa, mpa_state_class, mpa_state_designation, mpa_defacto_class, mpa_defacto_designation,
         transect_id, line_id, dive, line, scientific_name, count, tl_cm, sl_cm,
         class, order, family, genus, species, 
         sciname, species_code, target_status, level) 

# Check for entries with missing sciname (6 ok)
taxa_na <- data %>% 
  filter(is.na(sciname)) %>% 
  distinct(scientific_name)


# Fix duplicates ----------------------------------------------------------------

#NOTE: per PI, when a single reference was used for more than one MPA, the fish
#observations (rows) were duplicated. We are going to drop the duplicates and 
#ensure that each MPA site has a single paired reference sites. 

dives <- data %>% 
  distinct(year, month, dive, line_id)

duplicates <- data %>% # 126
  distinct(year, month, line_id, dive, transect_id, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  mutate(multiple = if_else(transect_id %in% transect_id[duplicated(transect_id)], T, F)) %>% 
  filter(multiple) %>% 
  mutate(transect_id_class = paste0(transect_id,"_",mpa_defacto_class)) # UPDATED CL Jan 2025

# Step 1: identify unique levels of 'type' for each mpa_group in the duplicates and
# drop the reference site for the SMCA if there is both an SMCA and SMR. 

drop_levels <- duplicates %>%
  mutate(mpa_group = str_replace(affiliated_mpa, "\\s\\w+$", ""),
         type = toupper(word(affiliated_mpa, -1))) %>% # UPDATED CL Jan 2025
  filter(mpa_defacto_class == "smca") %>%
  dplyr::select(-multiple) # 26 transects to drop in this df

# drop duplicates
data2 <- data %>%
  anti_join(drop_levels, by = c("year", "line_id", "affiliated_mpa"))

# recheck duplicates
duplicates2 <- data2 %>% 
  distinct(year, month, line_id, dive, transect_id, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(multiple = if_else(transect_id %in% transect_id[duplicated(transect_id)], T, F)) %>% 
  filter(multiple)

# check difference in duplicates (-26*2 = 52)
nrow(duplicates) - nrow(duplicates2)

unique(duplicates2$affiliated_mpa)

# Step 2: Balance reference sites for multiple SMRs
# Identify the duplicated transects, arrange by transect and MPA pair
dup_transects <- duplicates2 %>%
  mutate(transect_id_desig = paste(year, line_id, mpa_defacto_designation, sep = "_")) %>%
  group_by(transect_id_desig) %>%
  distinct(affiliated_mpa) %>% 
  arrange(transect_id_desig, affiliated_mpa) # 74

# Alternately drop transects across each pair
balanced_transects <- dup_transects %>%
  group_by(transect_id_desig) %>% 
  summarize(pair_1 = affiliated_mpa[1],
            pair_2 = affiliated_mpa[2]) %>% 
  group_by(pair_1, pair_2) %>% 
  mutate(row_num = row_number()) %>% ungroup() %>% 
  mutate(mpa_drop = if_else(row_num %% 2 == 0,   # If row number is even
                     pair_1,              # Drop pair_1 for even rows
                     pair_2)) %>% 
  # always drop the point sur smca over the point sur smr
  mutate(mpa_drop = case_when(mpa_drop == "point sur smr" ~ "point sur smca", T~mpa_drop)) %>% 
  dplyr::select(transect_id_desig, mpa_drop)

# Filter out rows where the `affiliated_mpa` matches `mpa_drop`
drop_levels2 <- dup_transects %>%
  left_join(balanced_transects, by = "transect_id_desig") %>% 
  filter(affiliated_mpa == mpa_drop) %>%  # Keep only rows that match the `mpa_drop`
  select(transect_id_desig, affiliated_mpa)  # Keep only identifying columns

# Remove these rows from the `data2` df
data3 <- data2 %>%
  mutate(transect_id_desig = paste(year, line_id, mpa_defacto_designation, sep = "_")) %>% 
  anti_join(drop_levels2, by = c("transect_id_desig", "affiliated_mpa"))

#recheck duplicates
duplicates3 <- data3 %>% 
  distinct(year, month, line_id, dive, transect_id, affiliated_mpa, mpa_defacto_designation) %>% 
  mutate(multiple = if_else(transect_id %in% transect_id[duplicated(transect_id)], T, F)) %>% 
  filter(multiple) # 0

#check
nrow(data) - nrow(data2) # 222
nrow(data2) - nrow(data3) # 286
nrow(data) - nrow(data3) # 508

summary(data3)

# Apply kelp forest and CCFRP species codes for deep ID

taxon_kelp <- read_csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>%
  filter(habitat == "Kelp forest") %>% 
  dplyr::select(species_code = habitat_specific_code, name = habitat_specific_spp_name, sciname, target_status = target_status_standardized)

taxon_rock <- read_csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv") %>% 
  clean_names() %>%
  filter(habitat == "Rocky reef") %>% 
  dplyr::select(species_code = habitat_specific_code, name = habitat_specific_spp_name, sciname, target_status = target_status_standardized)

deep_codes <- data3 %>% 
  dplyr::select(sciname, name = scientific_name, target_status) %>% 
  distinct() %>% 
  filter(!target_status == "NO_ORG") %>% 
  # Start by filling in the ones that match both the sciname and the habitat-specific name in kelp
  left_join(taxon_kelp)

deep_codes2 <- deep_codes %>% 
  filter(is.na(species_code)) %>% 
  filter(!sciname == "Sebastes spp") %>% 
  left_join(taxon_kelp, by = c("sciname"), suffix = c("", "_kelp")) %>% 
  mutate(species_code = coalesce(species_code, species_code_kelp)) %>% 
  left_join(taxon_rock, by = c("sciname"), suffix = c("", "_rock")) %>% 
  mutate(species_code = coalesce(species_code, species_code_rock)) %>% 
  dplyr::select(sciname:species_code) 
  
deep_codes3 <- deep_codes %>% 
  filter(!is.na(species_code) | (sciname == "Sebastes spp" & is.na(species_code))) %>% 
  bind_rows(deep_codes2) # missing 5 are those unidentified

deep_codes4 <- deep_codes3 %>% 
  mutate(species_code = case_when(name == c("Sebastes serranoides or flavidus")  ~ "OYT",
                                  name %in% c("Unidentified Sebastes sp.") ~ "SEBSPP",
                                  name %in% c("Young of year (<10 cm Sebastes sp.)", "Schooling (10-15 cm Sebastes sp.)") ~ "RFYOY",
                                  is.na(species_code) & str_detect(sciname, "spp") ~ toupper(paste0(str_sub(sciname, 1, 3), "SPP")),
                                  is.na(species_code) ~ toupper(paste0(substr(sciname, 1, 1), substr(gsub("^\\S+\\s*(\\S+).*", "\\1", sciname), 1, 3))),
                                  T~species_code)) %>% 
  dplyr::select(scientific_name = name, species_code_new = species_code)

data4 <- data3 %>% 
  left_join(deep_codes4) %>% 
  mutate(species_code = coalesce(species_code, species_code_new)) %>% 
  dplyr::select(-species_code_new)


# Process sites and get transect information
# Get the individual transects from the site data and average the lat/lon and sum the distance/area
deep_latlon <- deep_sites %>% 
  # Per PI: Only keep transects affiliated with MPAs and that do not cross boundaries
  # Don't do this as it drops some transects from the cleaned dataset
  #filter(type %in% c("SMR", "SMCA", "Reference")) %>% 
  mutate(line = as.character(line),
         dive = as.character(dive)) %>% 
  rename(year = "survey_year") %>% 
  # Fix one instance where year is incorrectly coded
  mutate(year = case_when(project == "CIMPA 2009" ~ 2009, T~year)) %>% 
  group_by(year, dive, line, line_id) %>%
  summarize(distance_m = sum(distance_m),
            area_m2 = sum(area_m2),
            avg_lat = mean(avg_lat, na.rm = T),
            avg_lon = mean(avg_lon, na.rm = T), .groups = 'drop') %>% 
  dplyr::select(year, dive, line, line_id, distance_m, area_m2, avg_lat, avg_lon) %>% 
  arrange(line_id) # 1988 w/out filter

# Get the individual transects from the fish data
deep_transects <- data4 %>% 
  distinct(year, affiliated_mpa, mpa_defacto_designation, line_id, dive, line, transect_id, transect_id_desig) %>% 
  # Add the sites
  left_join(deep_latlon) %>% 
  filter(!is.na(avg_lat)) # 1533 that have data for lat/lon

# # Test the ones that don't match to see
# mismatch <- data4 %>%
#   filter(!line_id %in% deep_transects$line_id) # 893 observations
# 
# # See if it's a problem with the line IDs
# test_line_id <- deep_sites %>% 
#   mutate(line_id_new = paste(dive, line, sep = "_")) %>% 
#   filter(line_id != line_id_new) %>% 
#   filter(line_id_new %in% mismatch$line_id)

# No solution currently. Should ask RS.

# Add the transect information to the main data

data5 <- data4 %>% 
  left_join(deep_transects) %>% 
  dplyr::select(year, bioregion, affiliated_mpa:sl_cm, sciname:avg_lon) %>% 
  # Drop transects without location data (will be removed from data below)
  filter(!is.na(avg_lat))


################################################################################
# Export the main data and the site metadata
write.csv(data5, row.names = F, file.path(outdir,"/deep_reef_processed.csv"))  
saveRDS(deep_transects, file.path(ltm.dir,"processed_data/update_2024/deep_reef_transect_metadata.Rds"))

# last write 7 Jan 2024




