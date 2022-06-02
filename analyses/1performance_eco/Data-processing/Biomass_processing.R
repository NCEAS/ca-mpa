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
                      summarize(sum_biomass = sum(Mean_Biomass, na.rm=TRUE))%>%
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





# ccfrp -------------------------------------------------------------------

#load species level mean fish BPUE
data_path <- "/home/shares/ca-mpa/data/sync-data/Monitoring_data/Monitoring_ccfrp/CCFRP_derived_data_tables_DataONE"
input_file <- "CCFRP_derived_effort_table.csv" 
biomass_raw <- read.csv(file.path(data_path, input_file))

#load taxonomy table
data_path <- "/home/shares/ca-mpa/data/sync-data/Taxonomy_traits/Taxonomy_ccfrp"
input_file <- "CCFRP_Species_TraitTable.xlsx" 
taxonomy<- readxl::read_excel(file.path(data_path, input_file), sheet=2, skip = 0, na="NA")

targeted_taxonomy <- taxonomy %>%
  dplyr::select(Scientific_Name, Common_Name, Fished)%>%
  mutate(Fished = str_remove_all(Fished, '-'),
         Scientific_Name = str_replace(Scientific_Name, " ", "_"))

targeted_taxonomy$Fished <- tolower(targeted_taxonomy$Fished)

#Join targeted status with bpue data using common name

ccfrp_biomass <- left_join(biomass_raw,targeted_taxonomy,by="Common_Name") 
ccfrp_biomass <- ccfrp_biomass %>%
  filter(!is.na(Fished))

#clean up
ccfrp_biomass <- ccfrp_biomass %>%
  mutate(affiliated_mpa = paste(Area,"smr"), #all ccfrp MPAs are defacto SMRs per PI
         mpa_designation = ifelse(MPA_Status == "MPA","smr", MPA_Status))
  
ccfrp_biomass$mpa_designation <- tolower(ccfrp_biomass$mpa_designation)
ccfrp_biomass$affiliated_mpa <- tolower(ccfrp_biomass$affiliated_mpa)

ccfrp_biomass <- ccfrp_biomass %>%
                  ungroup() %>%
                  dplyr::select(year=Year, affiliated_mpa, mpa_designation, Fished, Common_Name, BPUE_biomass.kg._per_angler_hour)%>%
                  mutate(mpa_class="smr", 
                         group="ccfrp")%>%
                  filter(Fished %in% c("targeted","nontargeted"))
                       


#aggregate by MPA-year and targeted/nontargeted

ccfrp_biomass.new <- ccfrp_biomass %>%
  group_by(group, year, affiliated_mpa, mpa_class, mpa_designation, Fished)%>%
  summarise(sum_biomass = sum(BPUE_biomass.kg._per_angler_hour, na.rm=TRUE))


#add regions

ccfrp_biomass.new$affiliated_mpa <- recode_factor(ccfrp_biomass.new$affiliated_mpa, "southeast farallon islands smr" = "southeast farallon island smr") #clean up names 
ccfrp_biomass.new$affiliated_mpa <- recode_factor(ccfrp_biomass.new$affiliated_mpa, "swamis smr" = "swami's smca") #clean up names 


data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

regions <- four_region %>%
  dplyr::select(name, region3=bioregion, region4 = four_region_north_ci)

ccfrp_biomass.new <- left_join(ccfrp_biomass.new, regions, by=c("affiliated_mpa"="name"))



#clean up and reorder to match other datasets

ccfrp_biom <- ccfrp_biomass.new %>%
  dplyr::select(year, group, region3, region4, affiliated_mpa, mpa_class, mpa_designation, target_status="Fished", sum_biomass)



# kelp forest -------------------------------------------------------------


data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring"
input_file <- "Ecol_perform_metrics_means_working.xlsx" 
ecol_metrics <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

ecol_metrics <- ecol_metrics %>%
                filter(group == "kelp forest-fish",
                       indicator == "biomass",
                       variable == "targeted" | variable =="nontargeted")

#add regions
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

regions <- four_region %>%
  dplyr::select(name, region3=bioregion, region4 = four_region_north_ci)

ecol_metrics.new <- left_join(ecol_metrics, regions, by=c("affiliated_mpa"="name"))


#select variables and reorder to match other datasets

kelp_biom <- ecol_metrics.new %>%
              mutate(group="kelp", sum_biomass=mean) %>%
              dplyr:: select (year, group,region3, region4, affiliated_mpa,mpa_class, mpa_designation, target_status = variable, sum_biomass)



# surf zone fishes --------------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring"
input_file <- "Ecol_perform_metrics_means_working.xlsx" 
ecol_metrics <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

ecol_metrics <- ecol_metrics %>%
  filter(group == "surf-zone",
         indicator == "bpue",
         variable == "targeted" | variable =="nontargeted")

#add regions
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

regions <- four_region %>%
  dplyr::select(name, region3=bioregion, region4 = four_region_north_ci)

ecol_metrics.new <- left_join(ecol_metrics, regions, by=c("affiliated_mpa"="name"))

#select variables and reorder to match other datasets

surf_biom <- ecol_metrics.new %>%
  mutate(group="surf", sum_biomass=mean) %>%
  dplyr:: select (year, group,region3, region4, affiliated_mpa,mpa_class, mpa_designation, target_status = variable, sum_biomass)



# combine all to form data table for export -------------------------------

biomass_data <- rbind(deep_reef_biom, ccfrp_biom, kelp_biom, surf_biom)


#export
#path_aurora <- "/home/shares/ca-mpa/data/sync-data/processed_data"
#write.csv(biomass_data,file.path(path_aurora, "targeted_nontargeted_fish_biomass.csv"), row.names = FALSE)



