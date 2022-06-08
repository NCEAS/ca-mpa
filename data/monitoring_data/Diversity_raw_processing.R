#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; June 7, 2022


#load required packages
require(dplyr)
require(stringr)


# Load and clean derived data ---------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring"
input_file <- "Ecol_perform_metrics_means_working.xlsx" 
ecol_metrics <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")


#join defacto smrs

data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")




#check for inconsistencies in mpa name spelling and correct designations
buchon_row <- c("Point Buchon","SMR","point buchon smr","No take.","deep_reef","SMR") 
defacto_smr <- rbind(defacto_smr,buchon_row) #add missing row

ecol_metrics$affiliated_mpa <- recode_factor(ecol_metrics$affiliated_mpa, "se farallon islands smca" = "southeast farallon island smca") #correct spelling to match affiliated
ecol_metrics$affiliated_mpa <- recode_factor(ecol_metrics$affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr") #correct spelling to match affiliated
ecol_metrics$affiliated_mpa <- recode_factor(ecol_metrics$affiliated_mpa, "southeast farallon islands smr" = "southeast farallon island smr") #
ecol_metrics$affiliated_mpa <- recode_factor(ecol_metrics$affiliated_mpa, "farnsworth smca" = "farnsworth offshore smca") #correct spelling to match affiliated
ecol_metrics$affiliated_mpa <- recode_factor(ecol_metrics$affiliated_mpa, "point st. george smca" = "point st. george reef offshore smca") #correct spelling to match affiliated
ecol_metrics$affiliated_mpa <- recode_factor(ecol_metrics$affiliated_mpa, "swamis smr" = "swami's smca") #correct spell


H_processing <- left_join(ecol_metrics, defacto_smr, by=c("join_ID"="group","affiliated_mpa"="affiliated_mpa")) #join defacto smrs




#Join 4 subregions 
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name, region4 = four_region_north_ci)

H_processing <- left_join(H_processing,four_region, by=c("affiliated_mpa"="name"))





#clean up

H_processing <- H_processing %>%
                dplyr::select(-c(dfw_class,mpa_class.x))








H_processing <- ecol_metrics %>%
                filter(indicator=='diversity')

