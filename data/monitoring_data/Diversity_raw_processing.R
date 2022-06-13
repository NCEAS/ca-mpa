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
  mutate(H_pi = (total_count.y/total_count.x)*log(total_count.y/total_count.x))%>%
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









# # calculate diversity for surf zone fishes - method = BRUV  -------------

#load surf zone seine data 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sandy-beach"
input_file <- "surf_zone_bruv_data.csv" 
surf_bruv <- read.csv(file.path(data_path, input_file))


#select variables of interest
surf_bruv <- surf_bruv %>%
  dplyr::select(region,affiliated_mpa, mpa_status, mpa_type, year, bruv, common_name, targeted, max_n)%>%
  drop_na(common_name)

#calculate sum of each species per bruv

surf_bruv_sum <- surf_bruv %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv, common_name)%>%
  summarise(total_count = sum(max_n))


#calculate community total per bruv

bruv_total <- surf_bruv %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv)%>%
  summarise(total_count = sum(max_n))

#join sum per species and haul total to calculate proportion

prop_species <- left_join(bruv_total, surf_bruv_sum, by=c("affiliated_mpa","mpa_status","year","bruv"))


#Calculate H_pi for each species -- prep for shannon diversity
prop_species <- prop_species %>%
  mutate(H_pi = (total_count.y/total_count.x)*log(total_count.y/total_count.x))%>%
  dplyr::select(region=region.x, affiliated_mpa, mpa_status, mpa_type=mpa_type.x, year, bruv, 
                common_name, species_count=total_count.y, haul_total=total_count.x, H_pi)


bruv_diversity <- prop_species %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv)%>%
  summarize(H = -1*sum(H_pi)) #calculates shannon diversity for each haul

#take mean diversity of all hauls to end up with MPA-year level means
bruv_diversity <- bruv_diversity %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year)%>%
  summarize(diversity = mean(H),
            sd = sd(H),
            n = n()) 

#clean up

bruv_diversity <- bruv_diversity %>%
                  mutate(mpa_designation = ifelse(mpa_status=="MPA",mpa_type,mpa_status),
                         mpa_class = word(affiliated_mpa, start = -1))%>%
                 ungroup()%>%
                 dplyr::select(-c(mpa_status, mpa_type))

bruv_diversity$mpa_designation <- recode_factor(bruv_diversity$mpa_designation, "Reference"='ref')
bruv_diversity$mpa_designation <- recode_factor(bruv_diversity$mpa_designation, "MPA Reference"='ref')
bruv_diversity$affiliated_mpa <- tolower(bruv_diversity$affiliated_mpa)
bruv_diversity$mpa_designation <- tolower(bruv_diversity$mpa_designation)
bruv_diversity$mpa_class <- tolower(bruv_diversity$mpa_class)
bruv_diversity$region <- tolower(bruv_diversity$region)


#Join 4 subregions 
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name, region4 = four_region_north_ci)

bruv_diversity <- left_join(bruv_diversity,four_region, by=c("affiliated_mpa"="name"))


#clean up

bruv_diversity <- bruv_diversity %>%
  mutate(join_ID = "surf", group="surf-bruv", variable="all fish", indicator="diversity")%>%
  dplyr::select(join_ID, group, mlpa_region = region, region4, affiliated_mpa, mpa_defacto_class=mpa_class, mpa_defacto_designation=mpa_designation,
                year, variable, indicator, mean="diversity",sd, n)%>%
 drop_na(region4)


#join surf fish diversity with other groups

fish_diversity <- rbind(fish_diversity, bruv_diversity)




# Export all fish diversity -----------------------------------------------

#path_aurora <- "/home/shares/ca-mpa/data/sync-data/processed_data" 
#write.csv(fish_diversity,file.path(path_aurora, "all_fish_diversity.csv"), row.names = FALSE)




# Diversity of targeted and nontargeted fish  -----------------------------


data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring"
input_file <- "Ecol_perform_metrics_means_working.xlsx" 
ecol_metrics <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")


#CCFRP 

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_ccfrp/CCFRP_derived_data_tables_DataONE"
input_file <- "CCFRP_derived_effort_table.csv" 
CCFRP_raw <- read.csv(file.path(data_path, input_file)) #raw CCFRP biological data

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables"
input_file <- "CCFRP_Taxonomy.xlsx"
ccfrp_taxon <- readxl::read_excel(file.path(data_path, input_file), sheet=2, skip = 0, na="NA")

ccfrp_taxon <- ccfrp_taxon %>%
               dplyr::select(Common_Name, Fished)



CCFRP_data_raw <- left_join(CCFRP_raw,ccfrp_taxon, by="Common_Name")

CCFRP_data <- CCFRP_data_raw %>%
              mutate(Fished = ifelse(Fished=="?",NA,Fished))%>%
              drop_na(Fished)#from rows that do not have species designated target status
              

CCFRP_data$Fished <- recode_factor(CCFRP_data$Fished,"Non-targeted"="nontargeted")
CCFRP_data$Fished <- tolower(CCFRP_data$Fished)


#clean raw CCFRP 
CCFRP_data <- CCFRP_data %>%
                  mutate(join_ID="CCFRP", group="CCFRP", #define missing fields for future merge 
                         mpa_class = word(CA_MPA_name_short, start = -1))%>%
                  mutate_at(vars(mpa_class), ~replace_na(., "SMR"))%>% #replaces NAs in mpa_class with "SMR" -- per project PI all affiliated MPAs are SMRs
                  mutate(affiliated_mpa = paste(Area,mpa_class),
                         mpa_defacto_designation = ifelse(MPA_Status == "MPA","SMR","REF")) #assign defactio designation


#Join 4 subregions 
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name, region4 = four_region_north_ci)

CCFRP_data$affiliated_mpa <- tolower(CCFRP_data$affiliated_mpa)
CCFRP_data$affiliated_mpa <- recode_factor(CCFRP_data$affiliated_mpa, "southeast farallon islands smr" = "southeast farallon island smr") #clean up names 
CCFRP_data$affiliated_mpa <- recode_factor(CCFRP_data$affiliated_mpa, "swamis smr" = "swami's smca") #clean up names 
CCFRP_data$affiliated_mpa <- recode_factor(CCFRP_data$affiliated_mpa, "swamis smca" = "swami's smca") 

CCFRP_data <- left_join(CCFRP_data,four_region, by=c("affiliated_mpa"="name"))



# #Calculate diversity of CCFRP fished species at the MPA-year level ------------


CCFRP_Fished <- CCFRP_data %>%  #clean up
  dplyr::select(join_ID, group, region4, affiliated_mpa,mpa_class, mpa_defacto_designation, Year, Grid_Cell_ID,
                Common_Name, Fished, CPUE_catch_per_angler_hour) %>%
  drop_na(region4) %>%
  filter(Fished=='targeted')


#calculate sum of each species per cell

CCFRP_targeted <- CCFRP_Fished %>%
  group_by(join_ID, group, region4, affiliated_mpa, mpa_class, mpa_defacto_designation, Year, Grid_Cell_ID, Common_Name)%>%
  summarise(total_count = sum(CPUE_catch_per_angler_hour))


#calculate community total per cell

cell_total <- CCFRP_Fished %>%
  group_by(join_ID, group, region4, affiliated_mpa, mpa_class, mpa_defacto_designation, Year, Grid_Cell_ID)%>%
  summarise(total_count = sum(CPUE_catch_per_angler_hour))

#join sum per species and total to calculate proportion

prop_species <- left_join(cell_total, CCFRP_targeted, by=c("affiliated_mpa","mpa_class","mpa_defacto_designation","Year","Grid_Cell_ID"))


#Calculate H_pi for each species -- prep for shannon diversity
prop_species <- prop_species %>%
  mutate(H_pi = (total_count.y/total_count.x)*log(total_count.y/total_count.x))%>%
  dplyr::select(join_ID=join_ID.x, group=group.x, region4=region4.x, affiliated_mpa, mpa_class, mpa_defacto_designation, Year, Grid_Cell_ID, 
                Common_Name, species_count=total_count.y, cell_total=total_count.x, H_pi)%>%
  drop_na(H_pi)


CCFRP_diversity <- prop_species %>%
  group_by(join_ID, group, region4, affiliated_mpa, mpa_class, mpa_defacto_designation, Year, Grid_Cell_ID)%>%
  summarize(H = -1*sum(H_pi)) #calculates shannon diversity for each haul

#take mean diversity of all cells to end up with MPA-year level means
CCFRP_targeted_diversity <- CCFRP_diversity %>%
  group_by(join_ID, group, region4, affiliated_mpa, mpa_class, mpa_defacto_designation, Year)%>%
  summarize(diversity = mean(H),
            sd = sd(H),
            n = n())%>%
  mutate(target_status = 'targeted',
         variable = 'targeted_fish',
         indicator = 'diversity') %>%
  dplyr::select(join_ID, group, region4, affiliated_mpa, mpa_defacto_class=mpa_class, mpa_defacto_designation, year=Year, mean=diversity, sd, n,
                target_status, variable, indicator)
  




# diversity of CCFRP nontargeted species at the MPA-year level ------------

#NOTE:: Diversity of 'nontargeted' CCFRP species was calculated, but ended
#with too many zeros to include in any future analyses. 

















# Calculate target diversity for surf zone fishes -------------

#load surf zone seine data 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sandy-beach"
input_file <- "surf_zone_fish_seine_data.csv" 
surf_seine <- read.csv(file.path(data_path, input_file))

#select variables of interest
surf_seine <- surf_seine %>%
  dplyr::select(region,affiliated_mpa, mpa_status, mpa_type, year, haul_number, genus, species, targeted, count)%>%
  mutate(genus_species = paste(genus,species))%>%
  drop_na(genus)%>%
  filter(targeted=='Targeted')

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
  mutate(H_pi = (total_count.y/total_count.x)*log(total_count.y/total_count.x))%>%
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

seine_targeted_diversity <- seine_diversity %>%
  mutate(mpa_defacto_class = "smr", join_ID = "surf", group="surf-seine", variable="targeted_fish", indicator="diversity", target_status="targeted")%>%
  dplyr::select(join_ID, group, mlpa_region = region, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation="mpa_status",
                year, variable, indicator, mean="diversity",sd, n, target_status)









# Calculate nontarget diversity for surf zone fishes -------------

#load surf zone seine data 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sandy-beach"
input_file <- "surf_zone_fish_seine_data.csv" 
surf_seine <- read.csv(file.path(data_path, input_file))

#select variables of interest
surf_seine <- surf_seine %>%
  dplyr::select(region,affiliated_mpa, mpa_status, mpa_type, year, haul_number, genus, species, targeted, count)%>%
  mutate(genus_species = paste(genus,species))%>%
  drop_na(genus)%>%
  filter(targeted=='Nontargeted')

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
  mutate(H_pi = (total_count.y/total_count.x)*log(total_count.y/total_count.x))%>%
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

seine_nontargeted_diversity <- seine_diversity %>%
  mutate(mpa_defacto_class = "smr", join_ID = "surf", group="surf-seine", variable="nontargeted_fish", indicator="diversity", target_status="nontargeted")%>%
  dplyr::select(join_ID, group, mlpa_region = region, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation="mpa_status",
                year, variable, indicator, mean="diversity",sd, n, target_status)















# # calculate diversity for targeted surf zone fishes - method = BRUV  -------------

#load surf zone seine data 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sandy-beach"
input_file <- "surf_zone_bruv_data.csv" 
surf_bruv <- read.csv(file.path(data_path, input_file))


#select variables of interest
surf_bruv <- surf_bruv %>%
  dplyr::select(region,affiliated_mpa, mpa_status, mpa_type, year, bruv, common_name, targeted, max_n)%>%
  drop_na(common_name) %>%
  filter(targeted=='Targeted')

#calculate sum of each species per bruv

surf_bruv_sum <- surf_bruv %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv, common_name)%>%
  summarise(total_count = sum(max_n))


#calculate community total per bruv

bruv_total <- surf_bruv %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv)%>%
  summarise(total_count = sum(max_n))

#join sum per species and haul total to calculate proportion

prop_species <- left_join(bruv_total, surf_bruv_sum, by=c("affiliated_mpa","mpa_status","year","bruv"))


#Calculate H_pi for each species -- prep for shannon diversity
prop_species <- prop_species %>%
  mutate(H_pi = (total_count.y/total_count.x)*log(total_count.y/total_count.x))%>%
  dplyr::select(region=region.x, affiliated_mpa, mpa_status, mpa_type=mpa_type.x, year, bruv, 
                common_name, species_count=total_count.y, haul_total=total_count.x, H_pi)


bruv_diversity <- prop_species %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv)%>%
  summarize(H = -1*sum(H_pi)) #calculates shannon diversity for each haul

#take mean diversity of all hauls to end up with MPA-year level means
bruv_diversity <- bruv_diversity %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year)%>%
  summarize(diversity = mean(H),
            sd = sd(H),
            n = n()) 

#clean up

bruv_diversity <- bruv_diversity %>%
  mutate(mpa_designation = ifelse(mpa_status=="MPA",mpa_type,mpa_status),
         mpa_class = word(affiliated_mpa, start = -1))%>%
  ungroup()%>%
  dplyr::select(-c(mpa_status, mpa_type))

bruv_diversity$mpa_designation <- recode_factor(bruv_diversity$mpa_designation, "Reference"='ref')
bruv_diversity$mpa_designation <- recode_factor(bruv_diversity$mpa_designation, "MPA Reference"='ref')
bruv_diversity$affiliated_mpa <- tolower(bruv_diversity$affiliated_mpa)
bruv_diversity$mpa_designation <- tolower(bruv_diversity$mpa_designation)
bruv_diversity$mpa_class <- tolower(bruv_diversity$mpa_class)
bruv_diversity$region <- tolower(bruv_diversity$region)


#Join 4 subregions 
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name, region4 = four_region_north_ci)

bruv_diversity <- left_join(bruv_diversity,four_region, by=c("affiliated_mpa"="name"))


#clean up

bruv_targeted_diversity <- bruv_diversity %>%
  mutate(join_ID = "surf", group="surf-bruv", variable="targeted_fish", indicator="diversity",target_status="targeted")%>%
  dplyr::select(join_ID, group, mlpa_region = region, region4, affiliated_mpa, mpa_defacto_class=mpa_class, mpa_defacto_designation=mpa_designation,
                year, variable, indicator, mean="diversity",sd, n, target_status)%>%
  drop_na(region4)













# # calculate diversity for nontargeted surf zone fishes - method = BRUV  -------------

#load surf zone seine data 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_sandy-beach"
input_file <- "surf_zone_bruv_data.csv" 
surf_bruv <- read.csv(file.path(data_path, input_file))


#select variables of interest
surf_bruv <- surf_bruv %>%
  dplyr::select(region,affiliated_mpa, mpa_status, mpa_type, year, bruv, common_name, targeted, max_n)%>%
  drop_na(common_name) %>%
  filter(targeted=='Nontargeted')

#calculate sum of each species per bruv

surf_bruv_sum <- surf_bruv %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv, common_name)%>%
  summarise(total_count = sum(max_n))


#calculate community total per bruv

bruv_total <- surf_bruv %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv)%>%
  summarise(total_count = sum(max_n))

#join sum per species and haul total to calculate proportion

prop_species <- left_join(bruv_total, surf_bruv_sum, by=c("affiliated_mpa","mpa_status","year","bruv"))


#Calculate H_pi for each species -- prep for shannon diversity
prop_species <- prop_species %>%
  mutate(H_pi = (total_count.y/total_count.x)*log(total_count.y/total_count.x))%>%
  dplyr::select(region=region.x, affiliated_mpa, mpa_status, mpa_type=mpa_type.x, year, bruv, 
                common_name, species_count=total_count.y, haul_total=total_count.x, H_pi)


bruv_diversity <- prop_species %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year, bruv)%>%
  summarize(H = -1*sum(H_pi)) #calculates shannon diversity for each haul

#take mean diversity of all hauls to end up with MPA-year level means
bruv_diversity <- bruv_diversity %>%
  group_by(region, affiliated_mpa, mpa_status, mpa_type, year)%>%
  summarize(diversity = mean(H),
            sd = sd(H),
            n = n()) 

#clean up

bruv_diversity <- bruv_diversity %>%
  mutate(mpa_designation = ifelse(mpa_status=="MPA",mpa_type,mpa_status),
         mpa_class = word(affiliated_mpa, start = -1))%>%
  ungroup()%>%
  dplyr::select(-c(mpa_status, mpa_type))

bruv_diversity$mpa_designation <- recode_factor(bruv_diversity$mpa_designation, "Reference"='ref')
bruv_diversity$mpa_designation <- recode_factor(bruv_diversity$mpa_designation, "MPA Reference"='ref')
bruv_diversity$affiliated_mpa <- tolower(bruv_diversity$affiliated_mpa)
bruv_diversity$mpa_designation <- tolower(bruv_diversity$mpa_designation)
bruv_diversity$mpa_class <- tolower(bruv_diversity$mpa_class)
bruv_diversity$region <- tolower(bruv_diversity$region)


#Join 4 subregions 
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name, region4 = four_region_north_ci)

bruv_diversity <- left_join(bruv_diversity,four_region, by=c("affiliated_mpa"="name"))


#clean up

bruv_nontargeted_diversity <- bruv_diversity %>%
  mutate(join_ID = "surf", group="surf-bruv", variable="nontargeted_fish", indicator="diversity",target_status="nontargeted")%>%
  dplyr::select(join_ID, group, mlpa_region = region, region4, affiliated_mpa, mpa_defacto_class=mpa_class, mpa_defacto_designation=mpa_designation,
                year, variable, indicator, mean="diversity",sd, n, target_status)%>%
  drop_na(region4)












# # Mid-Depth Rock / Deep Reef fish diversity processing-------------
#load species level mean fish biomass 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_deep-reef/ROV_Dataset"
input_file <- "MidDepth_ROV_Fish_Mean_Density.xlsx" 
density_raw <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")


#load taxonomy table
data_path <- "/home/shares/ca-mpa/data/sync-data/Taxonomy_traits/Taxonomy_deep-reef"
input_file <- "ROV_Taxonomic_Traits_NCEAS.xlsx" 
taxonomy <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")


#Filter targeted status and scientific name for join
targeted_taxonomy <- taxonomy %>%
  dplyr::select(Scientific_Name, Common_Name=Common_name, targeted)%>%
  mutate(targeted = str_remove_all(targeted, '-'),
         Scientific_Name = str_replace(Scientific_Name, " ", "_"))


#clean up

targeted_taxonomy <- targeted_taxonomy %>%
  mutate(targeted = ifelse(Common_Name == "Blue/Deacon Rockfish", "targeted", targeted),
         targeted = ifelse(Common_Name == "Black/Blue/Deacon Rockfish complex", "targeted", targeted),
         targeted = ifelse(Common_Name == "Black/Blue Rockfish complex", "targeted", targeted),#All species in this complex are harvested
         targeted = ifelse(Common_Name == "California Scorpionfish", "targeted", targeted)) #targeted species

targeted_taxonomy$targeted <- tolower(targeted_taxonomy$targeted) #make lower
targeted_taxonomy$Scientific_Name <- tolower(targeted_taxonomy$Scientific_Name) #make lower


#Join targeted status with density data using common name

deep_reef_density <- left_join(density_raw,targeted_taxonomy,by="Common_Name") 
deep_reef_density <- deep_reef_density %>%
  filter(!is.na(targeted))

#add column to create affiliated_mpa 

deep_reef_density <- deep_reef_density %>%
  mutate(affiliated_mpa = paste(MPA_Group,Type),
         group='deep_reef')
deep_reef_density$affiliated_mpa <- tolower(deep_reef_density$affiliated_mpa) #make lower

#prep for joining reclassified defacto smrs
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")

#check for inconsistencies in mpa name spelling and correct designations
buchon_row <- c("Point Buchon","SMR","point buchon smr","No take.","deep_reef","SMR") 
defacto_smr <- rbind(defacto_smr,buchon_row) #add missing row

deep_reef_density$affiliated_mpa <- recode_factor(deep_reef_density$affiliated_mpa, "se farallon islands smca" = "southeast farallon island smca") #correct spelling to match affiliated
deep_reef_density$affiliated_mpa <- recode_factor(deep_reef_density$affiliated_mpa, "se farallon islands smr" = "southeast farallon island smr") #correct spelling to match affiliated
deep_reef_density$affiliated_mpa <- recode_factor(deep_reef_density$affiliated_mpa, "farnsworth smca" = "farnsworth offshore smca") #correct spelling to match affiliated
deep_reef_density$affiliated_mpa <- recode_factor(deep_reef_density$affiliated_mpa, "point st. george smca" = "point st. george reef offshore smca") #correct spelling to match affiliated


deep_reef <- left_join(deep_reef_density, defacto_smr, by=c("group"="group","affiliated_mpa"="affiliated_mpa"))


#create new mpa_designation field to match other datasets
deep_reef <- deep_reef %>%
  mutate(mpa_designation = ifelse(Designation == "Reference","ref", Type))%>%
  ungroup() %>%
  dplyr::select(year=Year, group, region3=Region, affiliated_mpa, mpa_class, mpa_designation, targeted, Common_Name, Mean_Density)

deep_reef$mpa_designation <- toupper(deep_reef$mpa_designation)

#join new four subregions
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

four_region <- four_region %>%
  dplyr::select(name, region4 = four_region_north_ci)

deep_reef_den <- left_join(deep_reef,four_region, by=c("affiliated_mpa"="name"))

deep_reef_den <- deep_reef_den %>%
  dplyr::select(year, group, region3, region4, affiliated_mpa, mpa_class, mpa_designation, species=Common_Name, target_status=targeted, mean=Mean_Density)



# Mid-depth rock targeted diversity --------------------------------------



#select variables of interest
deep_reef_vars <- deep_reef_den %>%
  filter(target_status=='targeted')

#calculate community total per MPA

MPA_total <- deep_reef_vars %>%
  group_by(group, year, region3, region4, affiliated_mpa, mpa_class, mpa_designation)%>%
  summarise(total_count = sum(mean))

#join sum per species and haul total to calculate proportion

prop_species <- left_join(deep_reef_vars, MPA_total, by=c("group", "year", "region3", "region4", "affiliated_mpa", "mpa_class", "mpa_designation"))


#Calculate H_pi for each species -- prep for shannon diversity
prop_species <- prop_species %>%
  mutate(H_pi = (mean/total_count)*log(mean/total_count))%>%
  drop_na(H_pi)
  

deep_reef_target_diversity <- prop_species %>%
  group_by(group, year, region3, region4, affiliated_mpa, mpa_class, mpa_designation)%>%
  summarize(mean = -1*sum(H_pi))%>% #calculates shannon diversity for each MPA
  mutate(join_ID="deep_reef", target_status="targeted",variable="targeted_fish",indicator="diversity")%>%
  dplyr::select(join_ID, group, year, mlpa_region=region3, region4, affiliated_mpa, mpa_defacto_class=mpa_class, mpa_defacto_designation=mpa_designation,mean,
                target_status, variable, indicator)




# Mid-depth rock nontargeted diversity --------------------------------------



#select variables of interest
deep_reef_vars <- deep_reef_den %>%
  filter(target_status=='nontargeted')

#calculate community total per MPA

MPA_total <- deep_reef_vars %>%
  group_by(group, year, region3, region4, affiliated_mpa, mpa_class, mpa_designation)%>%
  summarise(total_count = sum(mean))

#join sum per species and haul total to calculate proportion

prop_species <- left_join(deep_reef_vars, MPA_total, by=c("group", "year", "region3", "region4", "affiliated_mpa", "mpa_class", "mpa_designation"))


#Calculate H_pi for each species -- prep for shannon diversity
prop_species <- prop_species %>%
  mutate(H_pi = (mean/total_count)*log(mean/total_count))%>%
  drop_na(H_pi)


deep_reef_nontarget_diversity <- prop_species %>%
  group_by(group, year, region3, region4, affiliated_mpa, mpa_class, mpa_designation)%>%
  summarize(mean = -1*sum(H_pi))%>% #calculates shannon diversity for each MPA
  mutate(join_ID="deep_reef", target_status="nontargeted",variable="nontargeted_fish",indicator="diversity")%>%
  dplyr::select(join_ID, group, year, mlpa_region=region3, region4, affiliated_mpa, mpa_defacto_class=mpa_class, mpa_defacto_designation=mpa_designation,mean,
                target_status, variable, indicator)





















#Join targeted and nontargeted datasets



diversity_combined <- rbind(CCFRP_targeted_diversity, seine_targeted_diversity, seine_nontargeted_diversity, bruv_targeted_diversity, 
                            bruv_nontargeted_diversity, deep_reef_target_diversity, deep_reef_nontarget_diversity)




