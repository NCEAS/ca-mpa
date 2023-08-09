#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 17, 2023

rm(list=ls())

#load required packages
require(dplyr)
require(stringr)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

#load raw monitoring data
ccfrp_caught_fishes <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_database/CCFRP_database_2007-2020_csv/4-Caught_Fishes.csv"))%>%
                          janitor::clean_names()
ccfrp_drift <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_database/CCFRP_database_2007-2020_csv/3-Drift_Information.csv"))%>%
  janitor::clean_names()
ccfrp_trip_info <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_database/CCFRP_database_2007-2020_csv/1-Trip_Information.csv"))%>%
  janitor::clean_names()
ccfrp_areas <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_database/CCFRP_database_2007-2020_csv/Monitoring_Areas.csv"))%>%
  janitor::clean_names()
ccfrp_effort <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_derived_data_tables_DataONE/CCFRP_derived_effort_table.csv"))%>%
  janitor::clean_names()

kelp_forest_raw <- read.csv(file.path(datadir, "/monitoring_kelp/MLPA_kelpforest_fish.4.csv"))

deep_reef_raw <- read.csv(file.path(datadir, "/monitoring_deep-reef/ROV_Dataset/ROVLengths2005-2019Merged-2021-02-02SLZ.csv")) %>%
  janitor::clean_names()

surf_zone_raw <- read.csv(file.path(datadir, "/monitoring_sandy-beach/surf_zone_fish_seine_data.csv")) %>%
  janitor::clean_names()

#load taxonomy lookup table (combined for all habitats)
##Note: if we modify target_status then we should do it in the processing script for species table, 
#which is in data/species_traits/Step2_length_weight_params.R. 
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv")


#load kelp forest site table
kelp_sites <- read.csv(file.path(datadir, "/monitoring_kelp/MLPA_kelpforest_site_table.4.csv")) %>%
  janitor::clean_names() %>%
  dplyr::select(site, ca_mpa_name_short, mpa_class=site_designation, mpa_designation=site_status)%>%
  distinct() #remove duplicates

#load habitat data (for regions only, we'll add habitat moderators in Step3)
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")

regions <- mpa_attributes_gen %>%
  dplyr::select(name, bioregion, region4 = four_region_north_ci) %>%
  mutate(name = tolower(name))

#load defacto SMRs 
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr_kelp <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")%>%
  filter(group=="kelp") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

defacto_smr_deep_reef <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")%>%
  filter(group=="deep_reef") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

defacto_smr_surf <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")%>%
  filter(group=="surf") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

#load lw params
params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv") %>%
                mutate(ScientificName_accepted = recode(ScientificName_accepted, "Sebastes spp." = "Sebastes spp")) %>%
                filter(!(is.na(ScientificName_accepted)))

################################################################################
################################################################################

#Important --- general steps for processing biomass. 
#1. Species w/o size data cannot be converted to biomass, but leave these for now since we 
#want to track effort (true zeros).

#2. We need to track true zeros. Standardize convention by replacing spp code with "NO_ORG" for all replicates where nothing was observed.

#3. Don't drop any species at this stage. We will do this in the next processing step. 

#4. For biomass conversion estimates, we elected to use parameters listed by the kelp
#forest monitoring group, who conducted a literature review for dozens of species. Since the 
#list is not comprehensive for species found in all other habitats, we added parameters
#from fish base. The processing script for this is available here https://github.com/NCEAS/ca-mpa/tree/main/data/species_traits

#5. In some cases, a single SMCA was used as a reference site for two SMRs. 
# We need to watch out for these. I am not sure how many there are. 

#6. Make sure that MPA pairs ('inside' and 'outside') are correctly matched in the site tables. 

#7. mpa_class = whether the MPA is a SMR or SMCA (or something else); mpa_designation = whether the SITE was inside or outside. 

#8. Size units in raw data should all be cm

################################################################################
################################################################################
#prep biomass conversion function

#define units for conversion
a_prime_conversion <- tribble(
  ~unit_length, ~unit_weight,  ~a_coeff, ~b_coeff,
  "mm", "g",  10, 1,
  "cm", "kg", 1000, NA,
  "mm", "mg", 10, 1/1000,
  "mm", "kg", 10, 1000
)


# This function will filter the necessary parameters
convert_dat <- function(params, data){
  convert_params <- params %>%
  filter(!(is.na(WL_a))) %>% # remove rows without conversion
  filter(ScientificName_accepted %in% unique(data$sciname))
  data %>% left_join(convert_params, by = c("sciname"="ScientificName_accepted"), multiple = "all")
}


# Compute from the cm inputs the type of length and units needed by the formula 
bio_fun <- function(data_with_params) {
  data_with_params %>%
  rename(TL_cm = fish_tl)%>%
  mutate(length_to_use = case_when(
    WL_input_length == "TL" & WL_L_units == "cm" ~ TL_cm,
    WL_input_length == "TL" & WL_L_units == "mm" ~ TL_cm *10,
    WL_input_length == "SL" & WL_L_units == "cm" ~ (TL_cm - LC_b)/LC_a,
    WL_input_length == "SL" & WL_L_units == "mm" ~ (TL_cm - LC_b)/LC_a *10,
    WL_input_length == "FL" & WL_L_units == "cm" ~ TL_cm,
    WL_input_length == "FL" & WL_L_units == "mm" ~ TL_cm *10,
    WL_input_length == "DW" & WL_L_units == "cm" ~ TL_cm,
    WL_input_length == "DW" & WL_L_units == "mm" ~ TL_cm *10,
    TRUE ~ NA   #-9999 # for debugging, to be switched to NA
  ),
  # Compute the weight
  weight_g = case_when(
    WL_W_units == "g" ~ WL_a*length_to_use^WL_b,
    WL_W_units == "kg" ~ (WL_a*length_to_use^WL_b)/1000,
    TRUE ~ NA #-9999 # for debugging, to be switched to NA
  )
  ) 
}

################################################################################
#process surf zone
#NOTE: surf zone weighed all caught individuals, so no need to estimate biomass. 
#Note: the unit of replication for surf zone is haul 

#identify MPA pairs
#surf zone habitat used a alpha naming naming convention('site_pair') to identify matched pairs (inside vs. out)
pairs <- surf_zone_raw %>% dplyr::select(site_code, site_type, mpa_name_short,
                                         affiliated_mpa, site_pair, mpa_status,
                                         mpa_type) %>% distinct() %>%
  #Surf zone called any SMR or SMCA a 'MPA', so use the affiliated_mpa name to identify the state class 
  mutate(mpa_state_class = word(affiliated_mpa, -1),
         #use typical naming
         mpa_state_designation = ifelse(mpa_status == "Reference","ref",tolower(mpa_state_class))) %>%
  dplyr::select(affiliated_mpa, mpa_state_class, mpa_state_designation, everything())

surf_zone_build1 <- surf_zone_raw %>%
  #Surf zone called any SMR or SMCA a 'MPA', so use the affiliated_mpa name to identify the state class 
  mutate(mpa_state_class = word(affiliated_mpa, -1),
         #use typical naming
         mpa_state_designation = ifelse(mpa_status == "Reference","ref",tolower(mpa_state_class))) %>%
  dplyr::select(affiliated_mpa, mpa_state_class, mpa_state_designation, everything())%>%
  #drop habitat-specific fields that are no longer needed
  dplyr::select(!(c(site_code, site_type, site_name, region, mpa_name_short, site_pair,
                    mpa_status, mpa_type)))%>%
  #follow typical naming
  dplyr::rename(weight_g = fish_weight_individual,
                total_weight_g = fish_weight) %>%
  mutate(
    #calculate weight in kg
    total_weight_kg = total_weight_g/1000,
    #convert mm to cm
    fish_length = fish_length/10,
    affiliated_mpa = tolower(affiliated_mpa),
    #affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr")
  )


#add defacto smrs
surf_zone_build2 <- left_join(surf_zone_build1, defacto_smr_surf, by="affiliated_mpa") %>%
  mutate(affiliated_mpa = recode(affiliated_mpa, "ano nuevo smr" = "año nuevo smr"))


#add regions
surf_zone_build3 <- left_join(surf_zone_build2, regions, by=c("affiliated_mpa"="name")) %>%
  #following naming
  mutate(mpa_defacto_designation = ifelse(mpa_state_designation == "ref","ref",tolower(mpa_defacto_class)))%>%
  #clean up
  dplyr::select(year, month, day, bioregion, region4, affiliated_mpa, 
                mpa_state_class, mpa_state_designation,
                mpa_defacto_class, mpa_defacto_designation,
                haul_number, species_code, class, order, 
                family, genus, species, target_status=targeted,
                fish_length, weight_g, total_weight_g, 
                count, total_weight_kg) %>%
  #replace no species with true zero
  mutate(total_weight_g = ifelse(species_code == "NOSP",0,total_weight_g),
         total_weight_kg = ifelse(species_code == "NOSP",0,total_weight_kg))

#write.csv(surf_zone_build3, row.names = F, file.path(outdir,"/biomass_processed/surf_zone_fish_biomass.csv"))  

################################################################################
#process kelp forest

#Note: the unit of replication for kelp forest is transect

#filter taxonomy table to kelp forest only
kelp_code <- taxon_tab %>% filter(habitat=="Kelp forest") %>% rename(taxon_group = level) 

#join taxonomy with data
kelp_forest_process1 <- left_join(kelp_forest_raw, kelp_code, by=(c("classcode"="habitat_specific_code"))) 

#estimate biomass
kelp_dat <- convert_dat(params_tab, kelp_forest_process1) #apply unit conversion function
#apply biomass conversion function
kelp_out <- bio_fun(kelp_dat) %>% 
  #calculate total biomass for unit (biomass of all invididuals of the same size)
  mutate(total_biom_g = weight_g*count)%>%
  #select interest vars
  dplyr::select(year, month, day, site, zone, level, transect,
                classcode, count, TL_cm, sciname, weight_g, total_biom_g, target_status)

#add regions
kelp_fish_counts <- kelp_out %>%
                    #fix site name for join
                    mutate(site = ifelse(site == "Swami's","SWAMIS",site))%>%
                      #join MPAs based on site name
                      left_join(kelp_sites, by="site") %>%
                      ungroup() %>%
                      #this MPA needs to be renamed to match the defacto_smr table
                      dplyr::select(year, month, day, affiliated_mpa=ca_mpa_name_short,mpa_class, mpa_designation, everything()) %>%
                      #following naming
                      mutate(mpa_designation = ifelse(mpa_designation=="reference","ref",mpa_class)) %>%
                      mutate(affiliated_mpa = ifelse(affiliated_mpa == "swamis smca","swami's smca",affiliated_mpa)) 

kelp_fish_counts$affiliated_mpa <- tolower(kelp_fish_counts$affiliated_mpa)
kelp_fish_counts <- left_join(kelp_fish_counts, regions, by=c("affiliated_mpa"="name")) 


#add defacto SMRs
kelp_fish_counts <- kelp_fish_counts %>%
                    left_join(defacto_smr_kelp, by="affiliated_mpa")

#clean up
kelp_fish_counts_final <- kelp_fish_counts %>% 
                      dplyr::select(year, month, day, affiliated_mpa, 
                                    mpa_state_class = mpa_class,
                                    mpa_defacto_class, bioregion, region4,
                                    everything()) %>%
                      mutate(mpa_defacto_class = tolower(mpa_defacto_class),
                             mpa_designation = tolower(mpa_designation),
                             #create defacto designation level. 
                             mpa_defacto_designation = ifelse(mpa_designation == "ref","ref",mpa_defacto_class), #either in MPA or reference
                             mpa_state_class = tolower(mpa_state_class),
                             #convert grams to kg
                             total_biom_kg = total_biom_g/1000)%>%
                      dplyr::rename(mpa_state_designation = mpa_designation)%>%
                      #select(!(mpa_class.y)) %>%
  dplyr::select(year, month, day, affiliated_mpa, 
                mpa_state_class, mpa_state_designation,
                mpa_defacto_class, mpa_defacto_designation, bioregion, region4,
                everything()) 

#write.csv(kelp_fish_counts_final, row.names = F, file.path(outdir,"/biomass_processed/kelpforest_fish_biomass.csv"))

################################################################################
#process CCFRP

#Note: the unit of replication for CCFRP is cell

#step 1 -- select variables of interest

#this is what was caught
ccfrp_caught_fishes1 <- ccfrp_caught_fishes %>% 
                        #drift_id is the common join field
                        dplyr::select(drift_id, species_code, length_cm)

#this is where they were at and effort (angler hrs)
ccfrp_drift1 <- ccfrp_drift %>% 
                    dplyr::select(drift_id, trip_id, id_cell_per_trip, grid_cell_id, site_mpa_ref,
                                  total_angler_hrs, total_fishes_caught ,excluded_drift_comment, drift_time_hrs)

#more location info
ccfrp_trip_info1 <- ccfrp_trip_info %>%
                    dplyr::select(trip_id, area, year = year_automatic, month, day)

#more location info
ccfrp_areas1 <- ccfrp_areas %>% dplyr::select(area_code, name, mpa_designation)

#join everything
ccfrp_build0 <- merge(ccfrp_caught_fishes1, ccfrp_drift1, by="drift_id", all=TRUE)
ccfrp_build1 <- merge(ccfrp_build1, ccfrp_trip_info1, by="trip_id", all=TRUE)
ccfrp_build2 <- left_join(ccfrp_build2, ccfrp_areas1, by=c("area"="area_code")) %>%
                  dplyr::select(year, month, day, trip_id, drift_id, id_cell_per_trip, grid_cell_id, name, mpa_designation, site_mpa_ref, 
                                 total_angler_hrs, species_code,
                                length_cm, excluded_drift_comment, drift_time_hrs) 

#step 2 -- select variables of interest, drop reps, and calculate effort

#per the instructions on DataONE, exclude these drifts below. 
#see pgs 2 and 3 for more info: https://opc.dataone.org/metacat/d1/mn/v2/object/urn:uuid:8fdbb007-c386-4371-bbe9-ba328c0f0477 
ccfrp_build3 <- ccfrp_build2 %>%
                #drop excluded drifts
                filter(excluded_drift_comment == "") %>% dplyr::select(!c(excluded_drift_comment))%>% 
                #drop excluded cells
                filter(!(grid_cell_id == "TDRR" |
                           grid_cell_id == "CMMM" |
                           grid_cell_id == "CMRR" |
                           grid_cell_id == "TMMM" |
                           grid_cell_id == "TMRR" |
                           grid_cell_id =="FNMM" |
                           grid_cell_id == "FNRR" |
                           grid_cell_id == "SPMM" |
                           grid_cell_id == "SPRR" |
                           grid_cell_id == "BHMM" |
                           grid_cell_id == "BHRR" |
                           grid_cell_id == "ANMM" |
                           grid_cell_id == "ANRR" |
                           grid_cell_id == "PLMM" |
                           grid_cell_id == "PLMN" |
                           grid_cell_id == "PLMO" |
                           grid_cell_id == "PLRR" |
                           grid_cell_id == "BLMM" |
                           grid_cell_id == "BLRR" | 
                           grid_cell_id == "PBMM" |
                           grid_cell_id == "PBRR" |
                           grid_cell_id == "PCMM" |
                           grid_cell_id == "PCRR" |
                           grid_cell_id == "CPMM" |
                           grid_cell_id == "CPRR" |
                           grid_cell_id == "AIMM" |
                           grid_cell_id =="AIRR" |
                           grid_cell_id == "LBMM" |
                           grid_cell_id == "LBRR" |
                           grid_cell_id == "SWMM" |
                           grid_cell_id == "SMRR" |
                           grid_cell_id == "LJMM" |
                           grid_cell_id == "LJRR")) %>%
                #drop drifts less than 2 min
                filter(drift_time_hrs > (2/60))

#calculate effort as the total angler hours per cell day
effort <- ccfrp_build3 %>%
  dplyr::select(year, month, day, trip_id, drift_id, id_cell_per_trip, grid_cell_id, total_angler_hrs) %>% distinct() %>% #USE ID CELL PER TRIP
  mutate(year = as.factor(year),
        month = as.factor(month),
         day = as.factor(day))%>%
  #some cells were sampled more than once in a given year, so take the total amount of effort in a year for that cell
  group_by(year, month, day, id_cell_per_trip, grid_cell_id) %>%
  summarize(cell_hours = sum(total_angler_hrs)) 

#filter taxon tab
ccfrp_taxa <- taxon_tab %>% filter(habitat =="Rocky reef")

#Join species ID
ccfrp_build4 <- left_join(ccfrp_build3, ccfrp_taxa, by=c("species_code" = "habitat_specific_code"),
                          na_matches="never") %>%
                #follow naming
                rename(fish_tl = length_cm) %>%
                #keep track of zeros for effort, replace with "NO_ORG"
                mutate(species_code = ifelse(is.na(species_code),"NO_ORG",species_code))


#step 3 -- calculate biomass at cell level for a given year
ccfrp_build5 <- convert_dat(params_tab, ccfrp_build4)
ccfrp_build6 <- bio_fun(ccfrp_build5) %>%
                dplyr::select(year, month, day, name, mpa_designation, site_mpa_ref, id_cell_per_trip, grid_cell_id,
                              species_code, TL_cm, sciname,
                              weight_g, target_status) 

#step 4 -- add regions

ccfrp_build7 <- ccfrp_build6 %>%
                  mutate(mpa_defacto_class = "smr", #all MPAs are defacto SMR for CCFRP
                         mpa_defacto_designation = tolower(site_mpa_ref),
                         mpa_defacto_designation = recode(mpa_defacto_designation, "mpa" = "smr"),
                         affiliated_mpa = paste(tolower(name),mpa_defacto_class),
                         #follow naming
                         affiliated_mpa = recode(affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr",
                                                 "SE farallon islands smr" = "southeast farallon island smr",
                                                 "swamis smr" = "swami's smca"
                                                 )
                         )%>%
                  #drop per PI recommendation
                  filter(!(name == "Trinidad"))

ccfrp_build8 <- left_join(ccfrp_build7, regions, by=c("affiliated_mpa"="name")) %>%
                dplyr::select(year, month, day, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation,
                              id_cell_per_trip, grid_cell_id, species_code, sciname,
                              TL_cm, weight_g, target_status) 


#step 5 -- calculate total species biomass per cell day
ccfrp_build9 <- ccfrp_build8 %>%
  mutate(weight_kg = weight_g/1000,
         year = as.character(year),
         month = as.character(month),
         day = as.character(day),
         grid_cell_id = trimws(as.character(grid_cell_id)),
         mpa_defacto_designation = as.character(mpa_defacto_designation)
         ) %>%
  group_by(year, month, day, bioregion, region4, affiliated_mpa, mpa_defacto_class,
           mpa_defacto_designation, id_cell_per_trip, grid_cell_id, species_code, sciname, target_status) %>%
  dplyr::summarize(total_biomass = sum(weight_kg))

#step 6 -- add effort
ccfrp_build10 <- left_join(ccfrp_build9, effort, by=c("year","month","day","id_cell_per_trip","grid_cell_id"), multiple = "all") %>%
                    #drop these cells per PI ... apparently there was an issue. 
                    filter(!(is.na(cell_hours)))%>%
                    mutate(bpue = total_biomass / cell_hours)


#step 8 -- calculate cpue and then join with biomass at the cell level
cpue <- ccfrp_build8 %>% group_by(year, month, day, bioregion, region4, 
                                 affiliated_mpa, mpa_defacto_class,id_cell_per_trip, grid_cell_id,
                                  species_code)%>%
                        dplyr::summarize(n_caught = n()) %>%
                        #join with effort
                        mutate(year = as.character(year),
                               month = as.character(month),
                               day = as.character(day))%>%
                       left_join(effort, by=c("year","month","day","id_cell_per_trip","grid_cell_id"), multiple = "all") %>%
                          mutate(cpue = n_caught / cell_hours) 


#step 9 -- join cpue with cpue
ccfrp_build11 <- ccfrp_build10 %>% left_join(cpue, by=c("year","month","day","id_cell_per_trip","grid_cell_id","species_code"))


#write.csv(ccfrp_build11, row.names = F, file.path(outdir,"/biomass_processed/ccfrp_fish_biomass.csv"))         


################################################################################
#process deep reef

#Get ready ... 

deep_reef_build1 <- deep_reef_raw %>%
                      #remove transects that crossed MPA boundaries -- per PI
                    filter(!(type == ""| #Random transect not affiliated with a MPA
                               type == "SMR/SMCA"| #this means the transect crossed from the SMR into the SMCA
                               type=="SMCA/SMR"| #this means the transect cross from the SMCA into the SMCA
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
                    dplyr::select(year= survey_year, primary_mpa = primary_final,
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
