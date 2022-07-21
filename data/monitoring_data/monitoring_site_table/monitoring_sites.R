#author: "Joshua G. Smith"
#date: '2022-07-20'

rm(list=ls())

#required packages
require(dplyr)
require(tidyr)

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/"


# load data ---------------------------------------------------------------

#load CCFRP site table
input_file <- "/monitoring_ccfrp/CCFRP_derived_data_tables_DataONE/CCFRP_location_table.csv" 
CCFRP_sites <- read.csv(file.path(data_path, input_file))

#load deep reef site table
input_file <- "/monitoring_deep-reef/ROV_Dataset/MidDepth_ROV_Site_Table.csv" 
deep_reef_sites <- read.csv(file.path(data_path, input_file))

#load kelp site table
input_file <- "/monitoring_kelp/MLPA_kelpforest_site_table.4.csv" 
kelp_sites <- read.csv(file.path(data_path, input_file))

#load rocky site table
input_file <- "/monitoring_rocky-intertidal/CA_MPA_sites_20210907b.csv" 
rocky_sites <- read.csv(file.path(data_path, input_file))

#load surf zone site table (no site table reported on DataOne so pull from monitoring data)
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring"
input_file <- "Ecol_perform_metrics_means_working.xlsx" 
surf_sites <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")%>%
              filter(group=='surf-zone')


# ccfrp processing -------------------------------------------------------------------

#clean up
ccfrp_process <- CCFRP_sites %>%
  mutate(affiliated_mpa = paste(Area,"smr"), #all ccfrp MPAs are defacto SMRs per PI
         mpa_designation = ifelse(MPA_Status == "MPA","smr", MPA_Status))

ccfrp_process$mpa_designation <- tolower(ccfrp_process$mpa_designation)
ccfrp_process$affiliated_mpa <- tolower(ccfrp_process$affiliated_mpa)

ccfrp_data <- ccfrp_process %>%
  ungroup() %>%
  mutate(mpa_class="smr", 
         group="ccfrp")%>%
  dplyr::select(group, affiliated_mpa, mpa_class, mpa_designation, site=Grid_Cell_ID, lat=lat_center_point_dd, lon=lon_center_point_dd)%>%
  drop_na()


#recode MPA spelling to match other groups

ccfrp_data$affiliated_mpa <- recode_factor(ccfrp_data$affiliated_mpa, "southeast farallon islands smr" = "southeast farallon island smr") #clean up names 
ccfrp_data$affiliated_mpa <- recode_factor(ccfrp_data$affiliated_mpa, "swamis smr" = "swami's smca") #clean up names 



# deep reef processing ----------------------------------------------------

deep_reef_process <- deep_reef_sites %>%
  mutate(affiliated_mpa = paste(MPA_Group,Type))

deep_reef_process$affiliated_mpa <- tolower(deep_reef_process$affiliated_mpa) #make lower

#check for inconsistencies in mpa name spelling and correct designations

deep_reef_process$affiliated_mpa <- recode_factor(deep_reef_process$affiliated_mpa, "se farallon islands smca" = "southeast farallon island smca") #correct spelling to match affiliated
deep_reef_process$affiliated_mpa <- recode_factor(deep_reef_process$affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr") #correct spelling to match affiliated
deep_reef_process$affiliated_mpa <- recode_factor(deep_reef_process$affiliated_mpa, "farnsworth smca" = "farnsworth offshore smca") #correct spelling to match affiliated
deep_reef_process$affiliated_mpa <- recode_factor(deep_reef_process$affiliated_mpa, "point st. george smca" = "point st. george reef offshore smca") #correct spelling to match affiliated

#create new mpa_designation field to match other datasets
deep_reef_data <- deep_reef_process %>%
  mutate(group="deep_reef",
         site=NA,
         mpa_designation = ifelse(Designation == "Reference","ref", Type))%>%
  ungroup() %>%
  dplyr::select(group, affiliated_mpa, mpa_class=Type, mpa_designation, site, lat=Lat, lon=Long)

deep_reef_data$mpa_class <-tolower(deep_reef_data$mpa_class)
deep_reef_data$mpa_designation <-tolower(deep_reef_data$mpa_designation)



# kelp process ------------------------------------------------------------

kelp_process <- kelp_sites %>%
                distinct(site, site_designation, latitude, longitude, CA_MPA_Name_Short, site_status)%>%
                mutate(group="kelp",
                       mpa_designation = ifelse(site_status=="mpa",site_designation,site_status))%>%
                select(group, affiliated_mpa=CA_MPA_Name_Short, mpa_class=site_designation,
                       mpa_designation, site, lat=latitude, lon=longitude)
                
kelp_process$affiliated_mpa <- tolower(kelp_process$affiliated_mpa)
kelp_process$mpa_designation <- tolower(kelp_process$mpa_designation)
kelp_process$mpa_class <- tolower(kelp_process$mpa_class)

kelp_process$mpa_designation <- recode_factor(kelp_process$mpa_designation, "reference"="ref")

kelp_data <- kelp_process



# Rocky process -----------------------------------------------------------

rocky_process <- rocky_sites %>%
                 mutate(group="rocky",
                        mpa_class=mpa_designation)%>%
                 select(group, affiliated_mpa=mpa_name, mpa_class,
                        mpa_designation, site=marine_site_name, lat=latitude, lon=longitude)
                
rocky_process$mpa_designation <- recode_factor(rocky_process$mpa_designation, "NONE"="ref")

rocky_process$mpa_designation <- tolower(rocky_process$mpa_designation)

rocky_process$affiliated_mpa <- tolower(rocky_process$affiliated_mpa)

rocky_process$mpa_class <- tolower(rocky_process$mpa_class)

rocky_data <- rocky_process

# surf processing ---------------------------------------------------------


surf_process <- surf_sites %>%
                mutate(site=NA)%>%
                distinct(group, affiliated_mpa, mpa_class, mpa_designation, 
                         site, lat_wgs84, lon_wgs84)%>%
                select(group, affiliated_mpa, mpa_class, mpa_designation, 
                       site, lat=lat_wgs84, lon=lon_wgs84)

surf_data <- surf_process


# join --------------------------------------------------------------------


site_locations <- rbind(ccfrp_data, deep_reef_data,kelp_data, rocky_data, surf_data)



