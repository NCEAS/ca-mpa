#Cleaning monitoring data for mpa-year level analyses
#Joshua G Smith; June 1, 2022


#load required packages
require(dplyr)
require(stringr)

# mid-depth rock / deep reef ----------------------------------------------

#load species level mean fish biomass 
data_path <- "/home/shares/ca-mpa/data/sync-data/Monitoring_data/Monitoring_deep-reef/ROV_Dataset"
input_file <- "MidDepth_ROV_Fish_Mean_Biomass.xlsx" 
biomass_raw <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

#clean up
biomass_raw <- biomass_raw %>%
                  mutate(Scientific_Name = str_replace(Scientific_Name, " ", "_"))#replace space in string with "_"
                       


biomass_raw$Scientific_Name <- tolower(biomass_raw$Scientific_Name) #make lower to correct inconsistencies 
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, sebastes_paucipinis = "sebastes_paucispinis") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, sebastes_pauscipinis = "sebastes_paucispinis") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, racochilus_vacca = "rhacochilus_vacca") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, sebastes_dalii = "sebastes_dallii") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, hexagrammus_decagrammus = "hexagrammos_decagrammus") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, sebastes_minatus = "sebastes_miniatus") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, starry_rockfish = "sebastes_constellatus") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, sebaste_miniatus = "sebastes_miniatus") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, sebastes_hopskini = "sebastes_hopkinsi") #correct mispelling
biomass_raw$Scientific_Name <- recode_factor(biomass_raw$Scientific_Name, sebastes_services = "sebastes_serriceps") #correct mispelling



#load taxonomy table
data_path <- "/home/shares/ca-mpa/data/sync-data/Taxonomy_traits/Taxonomy_deep-reef"
input_file <- "ROV_Taxonomic_Traits_NCEAS.xlsx" 
taxonomy <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")


#Filter targeted status and scientific name for join
targeted_taxonomy <- taxonomy %>%
                      dplyr::select(Scientific_Name, Common_name, targeted)%>%
                      mutate(targeted = str_remove_all(targeted, '-'),
                            Scientific_Name = str_replace(Scientific_Name, " ", "_"))


#clean up

targeted_taxonomy <- targeted_taxonomy %>%
                      mutate(targeted = ifelse(Common_name == "Blue/Deacon Rockfish", "targeted", targeted),
                             targeted = ifelse(Common_name == "Black/Blue/Deacon Rockfish complex", "targeted", targeted),
                             targeted = ifelse(Common_name == "Black/Blue Rockfish complex", "targeted", targeted),#All species in this complex are harvested
                             targeted = ifelse(Common_name == "California Scorpionfish", "targeted", targeted)) #targeted species

targeted_taxonomy$targeted <- tolower(targeted_taxonomy$targeted) #make lower
targeted_taxonomy$Scientific_Name <- tolower(targeted_taxonomy$Scientific_Name) #make lower


#Join targeted status with biomass data using scientific name

deep_reef_biomass <- left_join(biomass_raw,targeted_taxonomy,by="Scientific_Name") 
deep_reef_biomass <- deep_reef_biomass %>%
                        filter(!is.na(targeted))

#add column to create affiliated_mpa 

deep_reef_biomass <- deep_reef_biomass %>%
                        mutate(affiliated_mpa = paste(MPA_Group,Type))
deep_reef_biomass$affiliated_mpa <- tolower(deep_reef_biomass$affiliated_mpa) #make lower

#aggregate by MPA-year and targeted/nontargeted

deep_reef_biomass <- deep_reef_biomass %>%
                      group_by(Year, Region, affiliated_mpa, Type, Designation, targeted)%>%
                      summarize(sum_biomass = sum(Mean_Biomass))%>%
                      mutate(group='deep_reef')

#prep for joining reclassified defacto smrs
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")

#check for inconsistencies in mpa name spelling and correct designations
buchon_row <- c("Point Buchon","SMR","point buchon smr","No take.","deep_reef","SMR") 
defacto_smr <- rbind(defacto_smr,buchon_row) #add missing row

deep_reef_biomass$affiliated_mpa <- recode_factor(deep_reef_biomass$affiliated_mpa, "se farallon islands smca" = "southeast farallon island smca") #correct spelling to match affiliated
deep_reef_biomass$affiliated_mpa <- recode_factor(deep_reef_biomass$affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr") #correct spelling to match affiliated
deep_reef_biomass$affiliated_mpa <- recode_factor(deep_reef_biomass$affiliated_mpa, "farnsworth smca" = "farnsworth offshore smca") #correct spelling to match affiliated
deep_reef_biomass$affiliated_mpa <- recode_factor(deep_reef_biomass$affiliated_mpa, "point st. george smca" = "point st. george reef offshore smca") #correct spelling to match affiliated


deep_reef <- left_join(deep_reef_biomass, defacto_smr, by=c("group"="group","affiliated_mpa"="affiliated_mpa"))


#create new mpa_designation field to match other datasets
deep_reef <- deep_reef %>%
              mutate(mpa_designation = ifelse(Designation == "Reference","ref", Type))%>%
              ungroup() %>%
              dplyr::select(year=Year, group, region3=Region, affiliated_mpa, mpa_class, mpa_designation, targeted, sum_biomass)

deep_reef$mpa_designation <- toupper(deep_reef$mpa_designation)

#join new four subregions
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
                dplyr::select(name, region4 = four_region_north_ci)

deep_reef_biom <- left_join(deep_reef,four_region, by=c("affiliated_mpa"="name"))

deep_reef_biom <- deep_reef_biom %>%
                  dplyr::select(year, group, region3, region4, affiliated_mpa, mpa_class, mpa_designation, target_status=targeted, sum_biomass)



