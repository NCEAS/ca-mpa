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
          filter(!(is.na(survey_year)))%>%
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


##if reference is for both SMCA/SMR, then assign type = SMR, otherwise
#assign type = SMCA or type = SMR. The rationale for this is that SMCAs 
#will ultimately get dropped in the analyses. Some reference sites serve for 
#both the SMR and the SMCA

dr_proc1$mpa_group <- tolower(dr_proc1$mpa_group)
dr_proc1$type <- tolower(dr_proc1$type)

#check sites assigned as reference
type_ref <- dr_proc1%>% filter(type=="reference")
unique(type_ref$mpa_group)

#append names
dr_proc_ref <- dr_proc1 %>%
               mutate(type_2 = 
                        ifelse(type == "reference" & mpa_group == "campus point","smca",
                               ifelse(type == "reference" & mpa_group == "point conception","smr",
                                      ifelse(type == "reference" & mpa_group == "ten mile","smr",
                                             ifelse(type == "reference" & mpa_group == "harris point","smr",
                                                    ifelse(type == "reference" & mpa_group == "south point","smr",
                                                           ifelse(type == "reference" & mpa_group == "gull island","smr",
                                                                  ifelse(type == "reference" & mpa_group == "carrington point","smr",
                                                                         ifelse(type == "reference" & mpa_group == "point st. george","smca",
                                                                                ifelse(type == "reference" & mpa_group == "sea lion gulch","smr",
                                                                                       ifelse(type == "reference" & mpa_group == "pillar point","smca",
                                                                                              ifelse(type == "reference" & mpa_group == "farallon islands","smr",
                                                                                                     ifelse(type == "reference" & mpa_group == "ano nuevo","smca",
                                                                                                            ifelse(type == "reference" & mpa_group == "bodega bay","smr",
                                                                                                                   ifelse(type == "reference" & mpa_group == "big creek","smr",
                                                                                                                          ifelse(type == "reference" & mpa_group == "point buchon","smr",
                                                                                                                                 ifelse(type == "reference" & mpa_group == "portuguese ledge","smca",
                                                                                                                                        ifelse(type == "reference" & mpa_group == "point lobos","smca",
                                                                                                                                               ifelse(type == "reference" & mpa_group == "point sur","smr",
                                                                                                                                               type)))))))))))))))))))
                               


dr_proc2 <- dr_proc_ref %>% 
            filter(type_2=="smr"|type_2=="smca")%>%
            mutate(affiliated_mpa = paste(mpa_group, type_2))%>%
            select(affiliated_mpa, everything())%>%
            #reclassify designation 
            mutate(mpa_designation = ifelse(designation =="Reference",
                                            "ref",type_2))

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
         targeted = ifelse(Scientific_Name == "Sebastes diaconus", "targeted", targeted),
         targeted = ifelse(Scientific_Name == "Sebastes goodei", "targeted", targeted),
         targeted = ifelse(Scientific_Name == "Sebastes borealis", "targeted", targeted),
         targeted = ifelse(Scientific_Name == "Sebastes crameri", "targeted", targeted),
         targeted = ifelse(Scientific_Name == "Gibbonsia metzi", "targeted", targeted),
         ) #targeted species

targeted_taxonomy$targeted <- tolower(targeted_taxonomy$targeted) #make lower
targeted_taxonomy$Scientific_Name <- tolower(targeted_taxonomy$Scientific_Name) #make lower
targeted_taxonomy$Common_name <- tolower(targeted_taxonomy$Common_name) 


#check NAs in taxonomy table
taxon_na <- targeted_taxonomy %>% filter(is.na(targeted))

#write.csv(taxon_na, file="/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/taxon_na.csv", row.names=FALSE)

################################################################################
######drop taxa not identified to spp OR not listed in taxonomy table
dr_proc2$scientific_name <- tolower(dr_proc2$scientific_name)
dr_proc2$common_name <- tolower(dr_proc2$common_name)

dr_proc3 <- dr_proc2 %>%
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
                        scientific_name == "sebastes simulator"|
                        scientific_name == "sebastes crameri" |
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

dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "racochilus vacca" = "rhacochilus vacca")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes dalii" = "sebastes dallii")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "hexagrammus decagrammus" = "hexagrammos decagrammus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes pauscipinis" = "sebastes paucispinis")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "hydrolagus collei" = "hydrolagus colliei")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes paucipinis" = "sebastes paucispinis")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes minatus" = "sebastes miniatus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebases serranoides" = "sebastes serranoides")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes carnatus " = "sebastes carnatus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "starry rockfish" = "sebastes constellatus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes nebulosus " = "sebastes nebulosus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes mystinus " = "sebastes mystinus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebaste miniatus" = "sebastes miniatus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes hopskini" = "sebastes hopkinsi")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "oxylebius rictus†" = "oxylebius pictus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "hexagrammos decagrammus†" = "hexagrammos decagrammus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes umbrosus†" = "sebastes umbrosus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "hexagrammos decagrammus " = "hexagrammos decagrammus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes miniatus " = "sebastes miniatus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes caurinus " = "sebastes caurinus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "ophiodon elongatus " = "ophiodon elongatus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "oxylebius rictus" = "oxylebius pictus")
dr_proc3$scientific_name <- recode_factor(dr_proc3$scientific_name, "sebastes pinniger " = "sebastes pinniger")



################################################################################
#joni taxonomy 

#Join targeted status with count data using scientific name

dr_proc4 <- left_join(dr_proc3,targeted_taxonomy,by=c("scientific_name"="Scientific_Name"))
unique(dr_proc4$scientific_name)

#check join
anti_proc <- anti_join(dr_proc3,targeted_taxonomy,by=c("scientific_name"="Scientific_Name"))

unique(anti_proc$scientific_name)



################################################################################
#glance at data
#no. mpa sampled per year
n_mpa <- dr_proc4 %>%
        group_by(survey_year, affiliated_mpa, mpa_designation)%>%
        distinct(survey_year, affiliated_mpa, mpa_designation)%>%
        group_by(affiliated_mpa, mpa_designation)%>%
        summarize(n_years = n())
        


################################################################################
#join defacto smr

#prep for joining reclassified defacto smrs
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")

#check for inconsistencies in mpa name spelling and correct designations
buchon_row <- c("Point Buchon","SMR","point buchon smr","No take.","deep_reef","SMR") 
defacto_smr <- rbind(defacto_smr,buchon_row) #add missing row

dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "se farallon islands smca" = "southeast farallon island smca") #correct spelling to match affiliated
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr") #correct spelling to match affiliated
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "farallon islands smca" = "southeast farallon island smca") #correct spelling to match affiliated
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "farallon islands smr" = "southeast farallon island smr") 
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "farnsworth smca" = "farnsworth offshore smca") #correct spelling to match affiliated
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "point st. george smca" = "point st. george reef offshore smca") #correct spelling to match affiliated
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "bodega bay smr" = "bodega head smr")
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "bodega bay smca" = "bodega head smca")
dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "ano nuevo smca" = "ano nuevo smr") #listed as smr in cdfw master file
#dr_proc4$affiliated_mpa <- recode_factor(dr_proc4$affiliated_mpa, "anacapa island smca" = "anacapa island smr") #smca is reference site to the smr, recoding to match this framework.  
dr_proc4 <- dr_proc4 %>% mutate(group='deep_reef')

dr_proc5 <- left_join(dr_proc4, defacto_smr, by=c("group"="group","affiliated_mpa"="affiliated_mpa"))

#create new mpa_designation field to match other datasets
dr_proc6 <- dr_proc5 %>%
  ungroup() %>%
  dplyr::select(year=survey_year, group, affiliated_mpa, mpa_group, dfw_class, dfw_designation = mpa_designation,
                mpa_defacto_class = mpa_class, 
                line_id, dive, line, scientific_name,
                common_name, targeted, estimated_length_cm, total) %>%
          mutate(mpa_defacto_designation = ifelse(dfw_designation == "ref","ref",
                                                  mpa_defacto_class),
                 defacto_name = paste(mpa_group, mpa_defacto_class))%>%
          select(year, group, affiliated_mpa, defacto_name, dfw_class, dfw_designation,
                 mpa_defacto_class, mpa_defacto_designation, everything())
      

dr_proc6$mpa_defacto_designation <- toupper(dr_proc6$mpa_defacto_designation)

################################################################################
#check pairs

dr_proc7 %>%
  filter(year=='2008')%>%
  group_by(year, defacto_name, mpa_defacto_designation)%>%
  distinct(year, defacto_name, mpa_defacto_designation)%>%
  mutate(dummy_var =1)%>%
  ggplot(aes(x=defacto_name, y=dummy_var, fill=mpa_defacto_designation))+
  geom_bar(stat = "identity")+
  facet_wrap(~year)

###NOTE (1) anacapa island smca is the reference to the SMR, but the smca
###is a defacto SMR, therefore anacapa SMR does not have a true reference site. 
### Bodega bay smca is a true smca and is the reference to the SMR. 


#join new four subregions
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name,region3 = bioregion, region4 = four_region_north_ci, lat, long)

dr_proc7<- left_join(dr_proc6,four_region, by=c("affiliated_mpa"="name"))

dr_proc8 <- dr_proc7 %>%
  dplyr::select(group, year,region3,region4,lat,long,everything()
                )


################################################################################
#final cleaning

dr_proc9 <- dr_proc8 %>%
  filter(!is.na(targeted))%>%
  mutate(across(1:ncol(.), tolower))



#write.csv(dr_proc9, "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/deep_reef/deep_reef_counts.csv", row.names = FALSE)









