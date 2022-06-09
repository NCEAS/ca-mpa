#Processing monitoring data for mpa-year level analyses
#Joshua G Smith; June 7, 2022


#load required packages
require(dplyr)
require(stringr)


# Load and clean derived data ---------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring"
input_file <- "Ecol_perform_metrics_means_working.xlsx" 
ecol_metrics <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

ecol_metrics <- ecol_metrics %>%
            filter(indicator=='diversity')


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
                dplyr::select(join_ID, group, mlpa_region, region4, affiliated_mpa, mpa_class=mpa_class.y, mpa_designation, lat_wgs84, lon_wgs84, year, variable, indicator, mean, sd, n, mpa_age)
               
H_processing <- H_processing %>%
                ungroup()%>%
                mutate(mpa_defacto_designation = ifelse(c(mpa_designation == "smca" & mpa_class=="SMR"), "smr", mpa_designation))%>%
                dplyr::select(join_ID, group, mlpa_region, region4, affiliated_mpa, mpa_defacto_class = mpa_class, mpa_defacto_designation, lat_wgs84, lon_wgs84, year, variable, indicator, mean, sd, n, mpa_age)
              






# calculate diversity for surf zone fishes - method = beach seine ---------------------------------

#load surf zone seine data 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sandy-beach"
input_file <- "surf_zone_fish_seine_data.csv" 
surf_seine <- read.csv(file.path(data_path, input_file))

#select variables of interest
surf_seine <- surf_seine %>%
              dplyr::select(region,affiliated_mpa, mpa_status, mpa_type, year, haul_number, genus, species, targeted, count)%>%
              mutate(genus_species = paste(genus,species))%>%
              drop_na(genus)

#calculate sum of each species per haul

surf_seine_sum <- surf_seine %>%
                  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, haul_number, genus_species)%>%
                  summarise(total_count = sum(count))

#calculate community total per haul

haul_total <- surf_seine %>%
                 group_by(region, affiliated_mpa, mpa_status, mpa_type, year, haul_number)%>%
                 summarise(total_count = sum(count))

#join sum per species and haul total to calculate proportion

prop_species <- left_join( haul_total, surf_seine_sum, by=c("affiliated_mpa","mpa_status","year","haul_number"))

#Calculate H_pi for each species -- prep for shannon diversity
prop_species <- prop_species %>%
  mutate(H_pi = (total_count.y/total_count.x)*log10(total_count.y/total_count.x))%>%
dplyr::select(region=region.x, affiliated_mpa, mpa_status, mpa_type=mpa_type.x, year, haul_number, genus_species, species_count=total_count.y, haul_total=total_count.x, H_pi)



haul_diversity <- prop_species %>%
                   group_by(region, affiliated_mpa, mpa_status, mpa_type, year, haul_number)%>%
                   summarize(H = -1*sum(H_pi)) #calculates shannon diversity for each haul

#take mean diversity of all hauls to end up with MPA-year level means
seine_diversity <- haul_diversity %>%
                   group_by(region, affiliated_mpa, mpa_status, mpa_type, year)%>%
                   summarize(diversity = mean(H),
                             sd = sd(H),
                             n = n()) 

#clean up

seine_diversity <- seine_diversity %>%
                   ungroup()%>%
                   dplyr::select(-c(mpa_type))
seine_diversity$mpa_status <- recode_factor(seine_diversity$mpa_status, "Reference"='ref')
seine_diversity$mpa_status <- recode_factor(seine_diversity$mpa_status, "MPA"='smr')
seine_diversity$affiliated_mpa <- tolower(seine_diversity$affiliated_mpa)
seine_diversity$region <- tolower(seine_diversity$region)


#Join 4 subregions 
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name, region4 = four_region_north_ci)

seine_diversity <- left_join(seine_diversity,four_region, by=c("affiliated_mpa"="name"))


#clean up

seine_diversity <- seine_diversity %>%
                   mutate(mpa_defacto_class = "smr", join_ID = "surf", group="surf-seine", variable="all fish", indicator="diversity")%>%
                   dplyr::select(join_ID, group, mlpa_region = region, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation="mpa_status",
                          year, variable, indicator, mean="diversity",sd, n)


#join surf fish diversity with other groups

Fish_processing <- H_processing %>%
  filter(variable == "all fish")%>%
  dplyr::select(-c(lat_wgs84, lon_wgs84,mpa_age))

Fish_processing$mpa_defacto_class <- tolower(Fish_processing$mpa_defacto_class)


fish_diversity <- rbind(Fish_processing, seine_diversity)



