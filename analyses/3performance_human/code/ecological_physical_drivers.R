# Read data
################################################################################

# Clear workspace
rm(list = ls())

#load packages
require(dplyr)
require(stringr)
require(broom)
require(vcdExtra)
require(forcats)

# #load charismatic MPAs --------------------------------------------------

data_path <- "/home/joshsmith/CA_MPA_Project/ca-mpa/analyses/3performance_human/output" 
input_file <- "CA_MPA_charisma_key.csv" 
charisma_data <- read.csv(file.path(data_path, input_file))

#clean up
charisma_data$mpa <- tolower(charisma_data$mpa)
charisma_data$mpa_short <- tolower(charisma_data$mpa_short)
charisma_data$mpa <- recode_factor(charisma_data$mpa, 'año nuevo' = 'ano nuevo')
charisma_data$mpa <- recode_factor(charisma_data$mpa, 'año nuevo smr' = 'ano nuevo smr')

charisma_data$mpa <- gsub("\\s*\\([^\\)]+\\)","",as.character(charisma_data$mpa))


# # load MPA attribute data ----------------------------------------------------
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits/processed"
input_file <- "mpa_attributes_clean.csv" 
mpa_attrib <- read.csv(file.path(data_path, input_file))

#select vars
phys_drivers <- mpa_attrib %>%
                  dplyr::select(mpa=name, mpa_class, bioregion, four_region_north_ci, long, lat,
                         coastal_estuary, implementation_date, port_size,
                         distance_to_port, size_km2, shore_span_km, protection,
                         take, sandy_beach_km, rocky_inter_km, offshore_rock_km,
                         max_kelp_canopy_cdfw_km2, estuary_km2, mpa_within_or_part_of_state_park,
                         mpa_within_or_part_of_nat_marine_sanctuary, total_hard_substrate,
                         total_soft_substrate, depth_range)
          
#calculate mpa age
phys_drivers <- phys_drivers %>%
                mutate(implementation_year = format(as.Date(phys_drivers$implementation_date, format="%m/%d/%Y"),"%Y"),
                       mpa_age = 2022-as.numeric(implementation_year))


# Load infrastructure data ----------------------------------------------------


data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits/raw"
input_file <- "mpa_nearby_infrastructure.xlsx" 
mpa_inf <- read_excel(file.path(data_path, input_file), 1) %>%
            select(mpa, area_sqkm, num_camps, num_fish_point,
                   num_parking, num_picnic, park_names, park_types)

mpa_inf$mpa <- tolower(mpa_inf$mpa)



# Load biological data ----------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "all_fish_diversity.csv" 
fish_diversity <- read.csv(file.path(data_path, input_file))%>%
                  filter(group == 'kelp forest-fish')%>%
                  group_by(affiliated_mpa)%>%
                  dplyr::summarize(H_fish = mean(mean, na.rm=TRUE))


input_file <- "targeted_nontargeted_fish_biomass.csv"
fish_biomass <- read.csv(file.path(data_path, input_file))%>%
  filter(group == 'kelp')%>%
  group_by(affiliated_mpa, target_status)%>%
  dplyr::summarize(mean_fish_biomass = mean(sum_biomass, na.rm=TRUE))%>%
  pivot_wider(names_from='target_status',values_from="mean_fish_biomass")%>%
  dplyr::mutate(total_fish_biomass = rowSums(across(where(is.numeric))))


bio_drivers <- left_join(fish_diversity,fish_biomass, by='affiliated_mpa')%>%
               dplyr::select(mpa=affiliated_mpa, H_fish, total_fish_biomass)


#join data --------------------------------------------------------------


traits_join <- left_join(phys_drivers, mpa_inf, by="mpa")

drivers_join <- left_join(charisma_data, traits_join, by="mpa")

all_drivers <- left_join(drivers_join, bio_drivers, by="mpa")

all_drivers <- all_drivers %>%
  mutate_at('sandy_beach_km', ~replace_na(.,0)) %>%
  mutate_at(c('num_camps', 'num_fish_point','num_parking','num_picnic'), as.numeric)




# Build full logistic models ---------------------------------------------------


#iNat logistic model
logit_iNat_char <- all_drivers %>%
              filter(!(category=="Inaccessible"))%>%
              mutate(logit_y = ifelse(category=="Charismatic", "1", "0"))%>%
              filter(!(mpa=='robert w. crown smca')) #%>%
              #filter(!(is.na(num_parking) | is.na(num_picnic)))

logit_iNat_char$num_camps[is.na(logit_iNat_char$num_camps)] <- 0
logit_iNat_char$num_parking[is.na(logit_iNat_char$num_parking)] <- 0


iNat_full_model <-glm(as.numeric(logit_y) ~ 
                        #npeople_50km+
                        size_km2+
                        #shore_span_km+
                        #protection + 
                        #take + 
                        #mpa_age + 
                        mpa_within_or_part_of_state_park + 
                        #mpa_within_or_part_of_nat_marine_sanctuary+
                        #port_size + 
                        #distance_to_port+
                        sandy_beach_km + 
                        #rocky_inter_km +
                        estuary_km2 +
                        num_camps
                        #num_parking
                        #num_picnic
                        #depth_range 
                        #H_fish+#biological
                        #total_fish_biomass +
                        ,
                  data = logit_iNat_char, family = binomial(link="logit"), 
                  na.action=na.exclude)

sjPlot::tab_model(iNat_full_model, show.aic=T, show.r2=T, title="reduced logistic models",auto.label=T,
          #pred.labels = c("intercept","state parks (yes)","sandy beach","estuary","national marine sanctuary (yes)"),
          dv.labels = c("iNaturalist"))


summary(iNat_full_model)

#lowest score - AIC 78
logit.1 <-glm(as.numeric(logit_y) ~ mpa_within_or_part_of_state_park + sandy_beach_km + estuary_km2 +
               mpa_within_or_part_of_nat_marine_sanctuary, data = logit_data, family = binomial(link="logit"))

summary(logit.1)


tab_model(logit.1, show.aic=T, show.r2=T, title="reduced logistic models",auto.label=T,
          pred.labels = c("intercept","state parks (yes)","sandy beach","estuary","national marine sanctuary (yes)"),
          dv.labels = c("iNaturalist"))


tidy_lofit <- tidy(logit)








# REEF data ---------------------------------------------------------------


data_path <- "/home/shares/ca-mpa/data/sync-data/reef/processed"
input_file <- "REEF_sites_without_xy_data.csv" 
reef.data <- read.csv(file.path(data_path, input_file))



input_file<-"REEF_1994_2022_survey_metadata.Rds"
reef.meta <- readRDS(file.path(data_path, input_file)) %>%
  drop_na(mpa)


#clean and standardize mpa names
reef.meta$mpa <- gsub("\\s*\\([^\\)]+\\)","",as.character(reef.meta$mpa))
reef.meta$mpa <- tolower(reef.meta$mpa)



#check frequency of sampling by mpa
mpa_fq_all <- reef.meta %>%
  group_by(mpa)%>%
  dplyr::summarize(n = sum(n()))%>%
  mutate(type = "focal")


#Join REEF with phys drivers

reef_drivers <- left_join(all_drivers, mpa_fq_all, by='mpa')
#reef_drivers <- left_join(reef_phys_drivers, bio_drivers, by="mpa")


#clean up
reef_drivers <- reef_drivers %>%
                mutate(reef_n = n)%>%
                mutate_at('type', ~replace_na(.,'nonfocal'))%>%
                filter(type=='focal') #select ONLY REEF survey


logit_reef <- reef_drivers %>%
  mutate(logit_y = ifelse(charisma_yn=="Charismatic", "1", "0")) %>%
  mutate_at('reef_n', ~replace_na(.,0))

logit_reef$logit_y = as.numeric(logit_reef$logit_y)



#reef full model

reef_full_model <-glm(as.numeric(logit_y) ~ npeople_50km+
                        size_km2+
                        shore_span_km+
                        protection + 
                        take + 
                        mpa_age + 
                        mpa_within_or_part_of_state_park + 
                        mpa_within_or_part_of_nat_marine_sanctuary+
                        port_size + 
                        sandy_beach_km + 
                        rocky_inter_km +
                        estuary_km2 + 
                        depth_range +
                        max_kelp_canopy_cdfw_km2 +
                        H_fish+#biological
                        total_fish_biomass, 
                      data = logit_reef, family = binomial(link="logit"), na.action=na.exclude)

summary(reef_full_model)


#lowest score 
logit.2 <-glm(as.numeric(logit_y) ~ mpa_within_or_part_of_state_park + mpa_age + estuary_km2 + max_kelp_canopy_cdfw_km2+ 
               mpa_within_or_part_of_nat_marine_sanctuary+H_fish+total_fish_biomass, data = logit_reef, family = binomial(link="logit"),na.action=na.exclude)


summary(logit.2)


logit_data_focal <- logit_data %>%
                    filter(type == 'focal')
ggplot(logit_data_focal,
       aes(x = log(n+1),
           y= reorder(mpa, n),
           fill = charisma_yn
       )
) +
  xlab("number of REEF surveys")+
  ylab("")+
  geom_col()


tidy_lofit <- tidy(logit)




tab_model(logit.1, logit.2, show.aic=T, show.r2=T, title="reduced logistic models",auto.label=T,
          pred.labels = c("intercept","state parks (yes)","sandy beach","estuary","national marine sanctuary (yes)","population density","mpa age","max kelp canopy"),
          dv.labels = c("iNaturalist", "REEF"))

tab_model(iNat_full_model, reef_full_model, show.aic=T, show.r2=T, title="reduced logistic models",auto.label=T
          )













#create table of physical and biological drivers 

table_working <- as.data.frame(colnames(logit_data_focal)) %>%
                 mutate(driver = colnames(logit_data_focal))%>%
                 filter(!(driver=='mpa'|
                          driver=='mpa_short'|
                          driver=='four_region_north_ci'|
                          driver=='inat_observers_tot'|
                          driver=='re'|
                          driver=='re_perc'|
                          driver=='charisma_yn'|
                          driver=='mpa_class'|
                          driver=='long'|
                          driver=='lat'|
                          driver=='mpa'|
                          driver=='n'|
                          driver=='reef_n'|
                          driver=='offshore_rock_km'|
                          driver=='implementation_year'|
                          driver=='implementation date'|
                          driver=='type'|
                          driver=='logit_y'))
                  
                
table_working$driver <- recode_factor(table_working$driver, npeople_50km='population density')
table_working$driver <- recode_factor(table_working$driver, coastal_estuary='coastal or estuary')
table_working$driver <- recode_factor(table_working$driver, port_size='port size')
table_working$driver <- recode_factor(table_working$driver, distance_to_port='distance to port')
table_working$driver <- recode_factor(table_working$driver, size_km2='MPA size (area)')
table_working$driver <- recode_factor(table_working$driver, shore_span_km='shore span')
table_working$driver <- recode_factor(table_working$driver, protection='protection level')
table_working$driver <- recode_factor(table_working$driver, take='allowed take')
table_working$driver <- recode_factor(table_working$driver, sandy_beach_km='sandy beach extent')
table_working$driver <- recode_factor(table_working$driver, rocky_inter_km='rocky intertidal extent')
table_working$driver <- recode_factor(table_working$driver, max_kelp_canopy_cdfw_km2='max kelp canopy extent')
table_working$driver <- recode_factor(table_working$driver, estuary_km2='estuary extent')
table_working$driver <- recode_factor(table_working$driver, mpa_within_or_part_of_state_park='state park')
table_working$driver <- recode_factor(table_working$driver, mpa_within_or_part_of_nat_marine_sanctuary='national marine sanctuary')
table_working$driver <- recode_factor(table_working$driver, total_hard_substrate='total hard substrate')
table_working$driver <- recode_factor(table_working$driver, total_soft_substrate='total soft substrate')
table_working$driver <- recode_factor(table_working$driver, depth_range ='depth range')
table_working$driver <- recode_factor(table_working$driver, mpa_age='MPA age')


table_working <- table_working %>%
                 mutate(dummy = 1) %>%
                 pivot_wider(names_from = 'driver', values_from='dummy') %>%
                 dplyr::select('population density', 'bioregion','coastal or estuary', 'port size', 'distance to port',
                               'MPA size (area)', 'shore span','')
         





