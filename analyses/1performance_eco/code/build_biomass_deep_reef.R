#Joshua G. Smith, NCEAS
#November 1, 2022

rm(list=ls())

require(dplyr)
require(janitor)

#set directories 

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring"
outdir <- "/analyses/1performance_eco/output"

deep_reef_biomass_raw <- read.csv(file.path(datadir,
  "/monitoring_deep-reef/ROV_Dataset/ROVLengths2005-2019Merged-2021-02-02SLZ.csv")) 



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
  mutate(targeted = str_remove_all(targeted, '-'),
        # Scientific_Name = str_replace(Scientific_Name, " ", "_")
        )


#clean up

targeted_taxonomy <- targeted_taxonomy %>%
  mutate(targeted = ifelse(Common_name == "Blue/Deacon Rockfish", "targeted", targeted),
         targeted = ifelse(Common_name == "Black/Blue/Deacon Rockfish complex", "targeted", targeted),
         targeted = ifelse(Common_name == "Black/Blue Rockfish complex", "targeted", targeted),#All species in this complex are harvested
         targeted = ifelse(Common_name == "California Scorpionfish", "targeted", targeted)) #targeted species

targeted_taxonomy$targeted <- tolower(targeted_taxonomy$targeted) #make lower
targeted_taxonomy$Scientific_Name <- tolower(targeted_taxonomy$Scientific_Name) #make lower
targeted_taxonomy$Common_name <- tolower(targeted_taxonomy$Common_name) 


################################################################################
#joni taxonomy 
##############################################################################
##############################################################################
###lots of misspellings using scientific_name as the join field. Try using common name. 

## ***common name is just as bad. Finishing for the day 11/1/2022

#Join targeted status with count data using scientific name
dr_proc2$scientific_name <- tolower(dr_proc2$scientific_name)
dr_proc2$common_name <- tolower(dr_proc2$common_name)

dr_proc3 <- left_join(dr_proc2,targeted_taxonomy,by=c("common_name"="Common_name"))

#check join
anti_proc <- anti_join(dr_proc2,targeted_taxonomy,by=c("common_name"="Common_name"))

deep_reef_biomass <- deep_reef_biomass %>%
  filter(!is.na(targeted))





