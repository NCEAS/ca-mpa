#Joshua G. Smith, NCEAS
#November 1, 2022

rm(list=ls())

require(dplyr)
require(janitor)
require(tidyverse)

#set directories 

datadir <- "/home/shares/ca-mpa/data/sync-data/"
outdir <- "/analyses/1performance_eco/output"

deep_reef_biomass_raw <- read.csv(file.path(datadir,
  "monitoring/monitoring_deep-reef/ROV_Dataset/ROVLengths2005-2019Merged-2021-02-02SLZ.csv")) 


################################################################################
#clean 

dr_proc <- deep_reef_biomass_raw %>%
          clean_names()%>%
          #recode empty strings to NA
          mutate_all(list(~na_if(.,""))) %>%
          #drop rows with missing location information
          filter(!(is.na(mpa_name)&is.na(mpa_group)&
                     is.na(type)&is.na(designation))) %>%
          #drop counts with missing length data
          filter(!(is.na(estimated_length_cm)))

nrow(deep_reef_biomass_raw)-nrow(dr_proc)


################################################################################
#calculate total counts by size class

dr_proc1 <- dr_proc %>%
            group_by(survey_year, survey_date, location, mpa_name,
                     mpa_group, type, designation,line_id, dive, line,
                     scientific_name, common_name, estimated_length_cm)%>%
            dplyr::summarize(total = sum(count))


################################################################################
#match MPA name 


dr_proc1$mpa_group <- tolower(dr_proc1$mpa_group)
dr_proc1$type <- tolower(dr_proc1$type)

dr_proc2 <- dr_proc1 %>% 
            #select only SMCAs and SMRs
            filter(type=="smr"|type=="smca")%>%
            mutate(affiliated_mpa = paste(mpa_group, type))%>%
            select(affiliated_mpa, everything())%>%
            #reclassify designation 
            mutate(mpa_designation = ifelse(designation =="Reference",
                                            "ref",type))

unique(dr_proc2$affiliated_mpa)

################################################################################
#joni taxonomy 

#load taxonomy table
data_path <- "/home/shares/ca-mpa/data/sync-data/Taxonomy_traits/Taxonomy_deep-reef"
input_file <- "ROV_Taxonomic_Traits_NCEAS.xlsx" 
taxonomy <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")


#Filter targeted status and scientific name for join
targeted_taxonomy <- taxonomy %>%
  dplyr::select(Scientific_Name, Common_name, targeted)%>%
  mutate(targeted = str_remove_all(targeted, '-')
        # Scientific_Name = str_replace(Scientific_Name, " ", "_")
        )


#clean up

targeted_taxonomy <- targeted_taxonomy %>%
  mutate(targeted = ifelse(Common_name == "Blue/Deacon Rockfish", "targeted", targeted),
         targeted = ifelse(Common_name == "Black/Blue/Deacon Rockfish complex", "targeted", targeted),
         targeted = ifelse(Common_name == "Black/Blue Rockfish complex", "targeted", targeted),#All species in this complex are harvested
         targeted = ifelse(Common_name == "California Scorpionfish", "targeted", targeted),
         targeted = ifelse(Scientific_Name == "Sebastes diaconus", "targeted", targeted)) #targeted species

targeted_taxonomy$targeted <- tolower(targeted_taxonomy$targeted) #make lower
targeted_taxonomy$Scientific_Name <- tolower(targeted_taxonomy$Scientific_Name) #make lower
targeted_taxonomy$Common_name <- tolower(targeted_taxonomy$Common_name) 


#check NAs in taxonomy table
taxon_na <- targeted_taxonomy %>% filter(is.na(targeted))

write.csv(taxon_na, file="/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/taxon_na.csv", row.names=FALSE)

################################################################################
######drop taxa not identified to spp OR not listed in taxonomy table

dr_proc2 <- dr_proc2 %>%
            filter(!c(scientific_name == 'torpedo californica'|
                        scientific_name == 'torpedo california'|
                        scientific_name =='pleuronectidae spp.'|
                        scientific_name ==' ' |
                        scientific_name =='zaniolepis spp.'|
                        scientific_name == 'sebastes lentiginosus'|
                        scientific_name == 'sebastes ensifer'|
                        scientific_name == 'lycodes pacificus'|
                        scientific_name == 'sebastes letiginosus'|
                        scientific_name == 'rhinogobiops nicholsii'|
                        scientific_name == 'hyperprosopon ellipticum'|
                        scientific_name == "sebastes services"|
                        scientific_name == "beringraja binoculata"|
                        scientific_name == "raja inornata"|
                        scientific_name == "pegusa lascaris"|
                        scientific_name == "beringraja rhina"|
                        scientific_name == "unidentified sebastes sp."|
                        scientific_name == "apristurus brunneus"|
                        scientific_name == "sebastes aurora"|
                        scientific_name == "sebastes diploproa"|
                        scientific_name == "sebastes melanostomus"|
                        scientific_name == "sebastes aurora or diploproa"|
                        scientific_name == "sebastes crocotulus"|
                        scientific_name =="gibbonsia metzi"|
                        scientific_name =="sebastes crameri"|
                        scientific_name == "sebastes simulator"|
                        scientific_name == "sebastes borealis"
                     )) 
            #mutate(scientific_name = str_replace(scientific_name, " ", "_"))

################################################################################
######lots of mispelled spp names -- correct taxonomy table

targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'sebastes paucipinis' = "sebastes paucispinis") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'sebastes pauscipinis' = "sebastes paucispinis") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'racochilus vacca' = "rhacochilus vacca") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'sebastes dalii' = "sebastes dallii") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'hexagrammus decagrammus' = "hexagrammos decagrammus") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'sebastes minatus' = "sebastes miniatus") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'starry rockfish' = "sebastes constellatus") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'sebaste miniatus' = "sebastes miniatus") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'sebastes hopskini' = "sebastes hopkinsi") #correct mispelling
targeted_taxonomy$Scientific_Name <- recode_factor(targeted_taxonomy$Scientific_Name, 'sebastes services' = "sebastes serriceps") #correct mispelling

################################################################################
######correct spp names in data

dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "racochilus vacca" = "rhacochilus vacca")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes dalii" = "sebastes dallii")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "hexagrammus decagrammus" = "hexagrammos decagrammus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes pauscipinis" = "sebastes paucispinis")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "hydrolagus collei" = "hydrolagus colliei")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes paucipinis" = "sebastes paucispinis")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes minatus" = "sebastes miniatus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebases serranoides" = "sebastes serranoides")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes carnatus " = "sebastes carnatus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "starry rockfish" = "sebastes constellatus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes nebulosus " = "sebastes nebulosus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes mystinus " = "sebastes mystinus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebaste miniatus" = "sebastes miniatus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes hopskini" = "sebastes hopkinsi")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "oxylebius rictus†" = "oxylebius pictus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "hexagrammos decagrammus†" = "hexagrammos decagrammus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes umbrosus†" = "sebastes umbrosus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "hexagrammos decagrammus " = "hexagrammos decagrammus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes miniatus " = "sebastes miniatus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes caurinus " = "sebastes caurinus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "ophiodon elongatus " = "ophiodon elongatus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "oxylebius rictus" = "oxylebius pictus")
dr_proc2$scientific_name <- recode_factor(dr_proc2$scientific_name, "sebastes pinniger " = "sebastes pinniger")



################################################################################
#joni taxonomy 

#Join targeted status with count data using scientific name
dr_proc2$scientific_name <- tolower(dr_proc2$scientific_name)
dr_proc2$common_name <- tolower(dr_proc2$common_name)

dr_proc3 <- left_join(dr_proc2,targeted_taxonomy,by=c("scientific_name"="Scientific_Name"))
unique(dr_proc3$scientific_name)

#check join
anti_proc <- anti_join(dr_proc2,targeted_taxonomy,by=c("scientific_name"="Scientific_Name"))

unique(anti_proc$scientific_name)




















