#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; joshsmith@nceas.ucsb.edu; March 17, 2023

rm(list=ls())

#load required packages
require(dplyr)
require(stringr)

#set directories and load data

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/"
outdir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data"

ccfrp_raw <- read.csv(file.path(datadir, "/monitoring_ccfrp/CCFRP_derived_data_tables_DataONE/CCFRP_derived_effort_table.csv"))
kelp_forest_raw <- read.csv(file.path(datadir, "/monitoring_kelp/MLPA_kelpforest_fish.4.csv"))

#load taxonomy lookup table
taxon_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/species_key.csv")

#load site table
kelp_sites <- read.csv(file.path(datadir, "/monitoring_kelp/MLPA_kelpforest_site_table.4.csv")) %>%
  janitor::clean_names() %>%
  dplyr::select(site, ca_mpa_name_short, mpa_class=site_designation, mpa_designation=site_status)%>%
  distinct() #remove duplicates

#load habitat data
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")

regions <- mpa_attributes_gen %>%
  dplyr::select(name, bioregion, region4 = four_region_north_ci) %>%
  mutate(name = tolower(name))

#load defacto SMRs
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr_kelp <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")%>%
  filter(group=="kelp") %>%
  dplyr::select(affiliated_mpa, mpa_defacto_class = mpa_class)

#load lw params
params_tab <- read.csv("/home/shares/ca-mpa/data/sync-data/species_traits/processed/fish_lw_parameters_by_species.csv") %>%
                mutate(ScientificName_accepted = recode(ScientificName_accepted, "Sebastes spp." = "Sebastes spp"))

################################################################################
#prep conversion function

a_prime_conversion <- tribble(
  ~unit_length, ~unit_weight,  ~a_coeff, ~b_coeff,
  "mm", "g",  10, 1,
  "cm", "kg", 1000, NA,
  "mm", "mg", 10, 1/1000,
  "mm", "kg", 10, 1000
)


# filter the necessary parameters

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
#process kelp forest

kelp_code <- taxon_tab %>% filter(habitat=="Kelp forest")
kelp_forest_process1 <- left_join(kelp_forest_raw, kelp_code, by=(c("classcode"="habitat_specific_code")))

#estimate biomass

kelp_dat <- convert_dat(params_tab, kelp_forest_process1)
kelp_out <- bio_fun(kelp_dat) %>% 
  #drop species with missing size
  filter(!(is.na(TL_cm)))%>%
  #drop observations not identified to species level
  filter(!(
  classcode == "NO_ORG" |
    classcode == "UNID" | #unidentified fish
    classcode == "ATHE" |
    classcode == "UHAL" | #stingray
    classcode == "TCAL" | #torpedo ray
    classcode == "SYNG" | #tubesnout
    classcode == "STICH" |
    classcode == "RRIC" |
    classcode == "RJOR" |
    classcode =="RHYP" | #Ronquil
    classcode =="RALL" |#Ronquil
    classcode == "EMBI"|
    classcode == "BOTH"|
    classcode == "CITH" |
    classcode == "PHOL" |
    classcode =="PLEU"|
    classcode == "CLUP" |
    classcode == "BATH" |
    classcode == "GGAL" |
    classcode == "HEXA" |
    classcode == "PPRO"
    )) %>%
  #calculate total biomass
  mutate(total_biom_g = weight_g*count)%>%
  #select interest vars
  dplyr::select(year, month, day, site, zone, level = level.x, transect,
                classcode, count, TL_cm, sciname, weight_g, total_biom_g, target_status)

#add regions
kelp_fish_counts <- left_join(kelp_out, kelp_sites, by="site") %>%
  ungroup() %>%
  dplyr::select(year, month, day, affiliated_mpa=ca_mpa_name_short,mpa_class, mpa_designation, everything()) %>%
  mutate(mpa_designation = ifelse(mpa_designation=="reference","ref",mpa_class))

kelp_fish_counts$affiliated_mpa <- tolower(kelp_fish_counts$affiliated_mpa)
kelp_fish_counts <- left_join(kelp_fish_counts, regions, by=c("affiliated_mpa"="name"))


#add defacto SMRs
kelp_fish_counts <- left_join(kelp_fish_counts, defacto_smr_kelp, by="affiliated_mpa")

#clean up

kelp_fish_counts_final <- kelp_fish_counts %>% 
                      dplyr::select(year, month, day, affiliated_mpa, 
                                    mpa_state_class = mpa_class.x,
                                    mpa_defacto_class, bioregion, region4,
                                    everything()) %>%
                      filter(!(is.na(mpa_defacto_class)))%>%
                      mutate(mpa_defacto_class = tolower(mpa_defacto_class),
                             mpa_designation = tolower(mpa_designation),
                             mpa_defacto_designation = ifelse(mpa_defacto_class == "ref","ref",mpa_defacto_class),
                             mpa_state_class = tolower(mpa_state_class))%>%
                      dplyr::rename(mpa_state_designation = mpa_designation)%>%
                      select(!(mpa_class.y)) %>%
  dplyr::select(year, month, day, affiliated_mpa, 
                mpa_state_class, mpa_state_designation,
                mpa_defacto_class, mpa_defacto_designation, bioregion, region4,
                everything()) 

#write.csv(kelp_fish_counts_final, row.names = F, file.path(outdir,"/biomass_processed/kelpforest_fish_biomass.csv"))






