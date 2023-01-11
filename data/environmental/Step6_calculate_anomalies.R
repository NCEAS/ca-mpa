
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)


#load envr data
envr_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/1988_2022_cuti_beuti_daily_by_monitoring_site.Rds")
SST_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/2002_2022_mursst_monthly_by_monitoring_site.Rds")
MOCI_dat <- read.csv("/home/shares/ca-mpa/data/sync-data/environmental/raw/MOCI.csv")
glorys_dat <- read.csv("/home/shares/ca-mpa/data/sync-data/environmental/raw/CA_MPA_glorys_bottomT.csv")



#make everything lower
envr_df <- as.data.frame(sapply(envr_dat, tolower))
glorys_df <- as.data.frame(sapply(glorys_dat, tolower))

################################################################################
#Process beuti and cuti

# #calculate baseline annual mean ---- average for each site --------------


#across 1988-2012
baseline_means <- envr_df %>%
  filter(year<=2012)%>%
  group_by(habitat, site, site_type, mpa) %>%
  dplyr::summarise(lt_cuti = mean(as.numeric(cuti)),
                   lt_beuti = mean(as.numeric(beuti)))


#join baseline back to original dataset
envr_build_step1 <- left_join(envr_df, baseline_means, 
                              by=c("habitat", "site", "site_type", "mpa"
                                   
                              ))



# #calculate monthly baseline ---- average for each month across 1 --------



monthly_base <- envr_df %>%
  filter(year<=2012)%>%
  group_by(habitat, site, site_type, mpa, month) %>% #leave out year because we want long term average for each month
  dplyr::summarise(lt_monthly_cuti = mean(as.numeric(cuti)),
                   lt_monthly_beuti = mean(as.numeric(beuti))) %>%
  arrange(as.numeric(month))# %>% 
#pivot_wider(names_from = month, values_from = c("base_cuti","base_beuti"))

#join monthly baseline base to dataset

envr_build_step2 <- left_join(envr_build_step1, monthly_base,
                              by=c("habitat", "site", "site_type", "mpa", 
                                   "month"))




# #calculate observed annual beuti and cuti -------------------------------


annual_observed <- envr_build_step2 %>%
  group_by(habitat, site, site_type, mpa, year) %>%
  dplyr::summarise(obs_annual_cuti = mean(as.numeric(cuti)),
                   obs_annual_beuti = mean(as.numeric(beuti))) 

envr_build_step3 <- left_join(envr_build_step2, annual_observed, 
                              by=c("habitat", "site", "site_type", "mpa", 
                                   "year"))



# #calculate observed monthly beuti and cuti------------------------------------



monthly_observed <- envr_build_step2 %>%
  group_by(habitat, site, site_type, mpa, year, month) %>%
  dplyr::summarise(obs_monthly_cuti = mean(as.numeric(cuti)),
                   obs_monthly_beuti = mean(as.numeric(beuti))) 

envr_build_step4 <- left_join(envr_build_step3, monthly_observed, 
                              by=c("habitat", "site", "site_type", "mpa", 
                                   "year","month"))




# #calculate anomalies ----------------------------------------------------


envr_build_step5 <- envr_build_step4 %>%
  mutate(monthly_anomaly_cuti = obs_monthly_cuti-lt_monthly_cuti,
         monthly_anomaly_beuti = obs_monthly_beuti-lt_monthly_beuti)


#path_aurora <- "/home/shares/ca-mpa/data/sync-data/environmental/processed"
#saveRDS(data, file.path(path_aurora, "1988_2022_cuti_beuti_monthly_anom.Rds"))





################################################################################
#Process SST

# SST anomalies -----------------------------------------------------------

#format date to columns

sst_anom_step1 <- SST_dat %>%
                  dplyr::mutate(year = lubridate::year(date), 
                                  month = lubridate::month(date), 
                                  day = lubridate::day(date))

#baseline SST 2002-2012
baseline_SST <- sst_anom_step1 %>%
  filter(year<=2012)%>%
  group_by(habitat, mpa, site, site_type, mpa_orig) %>%
  dplyr::summarise(sst_baseline = mean(sst_c))

#join baseline back to original dataset
sst_build_step1 <- left_join(sst_anom_step1, baseline_SST, 
                              by=c("habitat", "mpa", "site", "site_type", 
                                   "mpa_orig"
                              ))

#calculate SST monthly baseline
monthly_base_SST <- sst_anom_step1 %>%
  filter(year<=2012)%>%
  group_by(habitat, mpa, site, site_type, mpa_orig, month) %>% #leave out year because we want long term average for each month
  dplyr::summarise(sst_monthly_baseline = mean(sst_c)) %>%
  arrange(as.numeric(month))# %>% 
#pivot_wider(names_from = month, values_from = c("base_cuti","base_beuti"))

#join monthly baseline base to dataset
sst_build_step2 <- left_join(sst_build_step1, monthly_base_SST,
                              by=c("habitat", "mpa", "site", 
                                   "site_type", "mpa_orig", "month"))

#SST annual observed
SST_annual_observed <- sst_build_step2 %>%
  group_by(habitat, mpa, site, site_type, mpa_orig, year) %>%
  dplyr::summarise(obs_annual_SST = mean(sst_c)) 

sst_build_step3 <- left_join(sst_build_step2, SST_annual_observed, 
                              by=c("habitat", "mpa", "site", "site_type", 
                                   "mpa_orig", "year"))


#SST monthly observed

SST_monthly_observed <- sst_build_step2 %>%
  group_by(habitat, mpa, site, site_type, mpa_orig, year, month) %>%
  dplyr::summarise(obs_monthly_SST = mean(sst_c)) 

sst_build_step4 <- left_join(sst_build_step3, SST_monthly_observed, 
                              by=c("habitat", "mpa", "site", "site_type", 
                                   "mpa_orig", 
                                   "year", "month"))

#calculate anomalies
sst_build_step5 <- sst_build_step4 %>%
  mutate(monthly_anomaly_sst = obs_monthly_SST-sst_monthly_baseline)




################################################################################
#Process GLORYS

glorys_step1 <- glorys_df %>%
                mutate(date= as.character(date),
                       year = format(as.Date(date, format="%Y-%m-%d"),"%Y"),
                       month = format(as.Date(date, format="%Y-%m-%d"),"%m"),
                       day = format(as.Date(date, format="%Y-%m-%d"),"%d"),
                       year = as.numeric(year),
                       bottomT = as.numeric(bottomT))%>%
                data.frame()

#baseline bottomT 1993-2012
baseline_bottomT <- glorys_step1 %>%
  filter(year<=2012)%>%
  dplyr::group_by(mpa_name) %>%
  dplyr::summarize(bottomT_baseline = mean(bottomT, na.rm=TRUE))

#join baseline back to original dataset
glorys_step2 <- left_join(glorys_step1, baseline_bottomT, 
                             by=c("mpa_name"
                             ))

#calculate bottomT monthly baseline
monthly_base_bottomT <- glorys_step1 %>%
  filter(year<=2012)%>%
  group_by(mpa_name, month) %>% #leave out year because we want long term average for each month
  dplyr::summarise(bottomT_monthly_baseline = mean(bottomT, na.rm=TRUE)) %>%
  arrange(as.numeric(month))

#join montlhy baseline back to dataset
glorys_step3 <- left_join(glorys_step2, monthly_base_bottomT,
                             by=c("mpa_name", "month"))


#bottomT mean annual observed
bottomT_annual_observed <- glorys_step3 %>%
  group_by(mpa_name, year) %>%
  dplyr::summarise(obs_annual_bottomT = mean(bottomT, na.rm=TRUE)) 

#join annual bottomT with dataset
glorys_step4 <- left_join(glorys_step3, bottomT_annual_observed, 
                             by=c("mpa_name", "year"))



#bottomT monthly observed

bottomT_monthly_observed <- glorys_step4 %>%
  group_by(mpa_name, year, month) %>%
  dplyr::summarise(obs_monthly_bottomT = mean(bottomT, na.rm=TRUE)) 

glorys_step5 <- left_join(glorys_step4, bottomT_monthly_observed, 
                             by=c("mpa_name", 
                                  "year", "month"))

#calculate anomalies
glorys_step6 <- glorys_step5 %>%
  mutate(monthly_anomaly_bottomT = obs_monthly_bottomT-bottomT_monthly_baseline,
         mpa_name = factor(mpa_name))




# Join beuti, cuti, sst, bottomT -----------------------------------------------

#make everything lower
sst_df <- as.data.frame(sapply(sst_build_step5, tolower))
            

#sst_df_rocky <- as.data.frame(sapply(sst_build_step5, tolower))%>%         
#   filter(habitat=='rocky intertidal') #process rocky intertidal

#step 1: aggregate at MPA - site_type - year level
#drop day 
envr_drop <- envr_build_step5 %>%
  dplyr::select(!(c("day", "date", "lat_dd_bin", "cuti", "beuti")))%>%
  distinct(across(everything()))%>%
  mutate(year=as.factor(year),
         month=as.factor(month),
         habitat=as.factor(habitat),
         mpa=as.factor(mpa),
         site_type = as.factor(site_type))
  
sst_drop <- sst_df %>%
  select(!(c("day", "date")))%>%
  distinct(across(everything()))%>%
  mutate(year=as.factor(year),
         month=as.factor(month),
         habitat=as.factor(habitat),
         mpa=as.factor(mpa),
         site_type = as.factor(site_type))

glorys_drop <- glorys_step6 %>%
              select(!(c("day", "date","lat_dd","long_dd","lat_approx","long_approx","bottomT")))%>%
              distinct(across(everything()))%>%
              mutate(year=factor(year),
                month=factor(month),
                mpa=as.factor(mpa_name))%>%
              dplyr::select(!(mpa_name)) %>%
              #remove leading zeros
              mutate(month = str_remove(month, "^0+"))

#step 2 - join
envr_data_step2 <- left_join(sst_drop, envr_drop,
                            by=c("habitat","mpa","site_type","year","month"
                                                      )) 


envr_data_step3 <- left_join(envr_data_step2, glorys_drop, by=c("mpa","year","month"))


#step 3 - rename and clean
envr_data <- envr_data_step3 %>%
              mutate(mpa_class = str_extract(mpa, "smca|smr|special closure"),
                     mpa_designation = ifelse(site_type == "reference","ref",mpa_class),
                     mpa_name = gsub("\\s*\\([^\\)]+\\)","",mpa),
                     mpa_name = ifelse(is.na(mpa_name),site.x,mpa_name))%>% #replace rocky intertidal mpa 'name' with site name
              select(group=habitat,
                     mpa_name,
                     mpa_class,
                     mpa_designation,
                     year,
                     month,
                     sst_annual_baseline=sst_baseline, 
                     sst_monthly_baseline,
                     sst_annual_obs= obs_annual_SST,
                     sst_monthly_obs = obs_monthly_SST,
                     sst_monthly_anom = monthly_anomaly_sst,
                     cuti_annual_baseline = lt_cuti, 
                     cuti_monthly_baseline = lt_monthly_cuti,
                     cuti_annual_obs = obs_annual_cuti,
                     cuti_monthly_obs=obs_monthly_cuti,
                     cuti_monthly_anom = monthly_anomaly_cuti,
                     beuti_annual_baseline = lt_beuti,
                     beuti_monthly_baseline = lt_monthly_beuti,
                     beuti_annual_obs = obs_annual_beuti,
                     beuti_monthly_obs = obs_monthly_beuti,
                     beuti_monthly_anom = monthly_anomaly_beuti,
                     bottomT_annual_baseline = bottomT_baseline,
                     bottomT_monthly_baseline = bottomT_monthly_baseline,
                     bottomT_annual_obs = obs_annual_bottomT,
                     bottomT_monthly_obs = obs_monthly_bottomT,
                     bottomT_monthly_anom = monthly_anomaly_bottomT
                     )

#reclassify

envr_data <- envr_data %>%
            mutate(group= as.factor(group),
                   mpa_name = as.factor(mpa_name),
                   mpa_class = as.factor(mpa_class),
                   mpa_designation = as.factor(mpa_designation))%>%
             mutate_if(is.character,as.numeric)





# add regions and defacto SMRs--------------------------------------------------

envr_data_join <- envr_data %>%
                  filter(!(group=="rocky intertidal")) # drop intertidal because these envr vars are not applicable
                  
envr_rocky <- envr_data %>%
  filter(group=="rocky intertidal") #process rocky intertidal separate

#add 4 regions
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

regions <- four_region %>%
  dplyr::select(name, region3=bioregion, region4 = four_region_north_ci)

#check for errors and correct
mismatch_join_<- anti_join(envr_data_join, regions, by=c("mpa_name"="name"))

envr_data_join$mpa_name <- recode_factor(envr_data_join$mpa_name, "año nuevo smr"="ano nuevo smr")
envr_data_join$mpa_name <- recode_factor(envr_data_join$mpa_name, "bodega head smr/smca"="bodega head smr")
envr_data_join$mpa_name <- recode_factor(envr_data_join$mpa_name, "piedras blancas smr/smca"="piedras blancas smr")
envr_data_join$mpa_name <- recode_factor(envr_data_join$mpa_name, "point arena smr/smca"="point arena smr")

envr_data_join <- envr_data_join %>%
                        filter(!is.na(mpa_name))%>%
                        filter(!(mpa_name=='trinidad smr')) #PI indicated this
                                                            #is not an SMR

envr_join_step2 <- left_join(envr_data_join, regions, by=c("mpa_name"="name"))
                    

#add defacto SMRs
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")%>%
  filter(group=="kelp") %>%
  dplyr::select(mpa_name=affiliated_mpa, mpa_defacto_class=mpa_class)

envr_join_step3 <- left_join(envr_join_step2, defacto_smr, by="mpa_name") 

#envr_rocky_join <- left_join(envr_rocky, defacto_smr, by="mpa_name") #for rocky intertidal


#clean up
envr_join_step4 <- envr_join_step3%>%
            mutate(mpa_defacto_designation = 
                     ifelse(mpa_designation=="ref","ref",
                            mpa_defacto_class)) %>%
            select(group, mpa_name, mpa_class, mpa_designation,
                   mpa_defacto_class, mpa_defacto_designation,
                   region3, region4, year, month, everything())
                    
envr_join_step4$mpa_defacto_class <- tolower(envr_join_step4$mpa_defacto_class)
envr_join_step4$mpa_defacto_designation <- tolower(envr_join_step4$mpa_defacto_designation)

envr_join_step4$group <- recode_factor(envr_join_step4$group, "deep reef"="deep_reef")
envr_join_step4$group <- recode_factor(envr_join_step4$group, "rocky intertidal"="rocky")
envr_join_step4$group <- recode_factor(envr_join_step4$group, "rocky reef"="ccfrp")
envr_join_step4$group <- recode_factor(envr_join_step4$group, "surf zone"="surf_zone")

envr_join_step5 <- envr_join_step4 %>%
                    mutate(year = as.numeric(as.character(year)),
                           sst_monthly_anom = as.numeric(as.character(
                             sst_monthly_anom
                           )),
                           qter = ifelse(month == "1"|month=="2"|month=="3","1",
                                         ifelse(month=="4"|month=="5"|month=="6","2",
                                                ifelse(month=="7"|month=="8"|month=="9","3","4"))))




# add MOCI ----------------------------------------------------------------

MOCI_step1 <- MOCI_dat %>%
              dplyr::rename(year = Year,
                     north = North.California..38.42N.,
                     central = Central.California..34.5.38N.,
                     south = Southern.California..32.34.5N.)%>%
              select(!(time))%>%
              pivot_longer(cols = c("north","central","south"),
                         values_to = "quarterly_MOCI",
                         names_to = "region3") %>%
              mutate(qter = ifelse(Season == "JFM","1",
                                   ifelse(Season == "AMJ","2",
                                          ifelse(Season == "JAS","3","4"))))

MOCI_annual <- MOCI_step1 %>%
                group_by(year, region3)%>%
                summarize(annual_MOCI=mean(quarterly_MOCI))

MOCI_step2 <- left_join(MOCI_step1, MOCI_annual, by=c("year","region3")) %>%
               select(year, qter, region3, annual_MOCI, quarterly_MOCI)



envr_vars_all <- left_join(envr_join_step5, MOCI_step2, by=c("year","region3", "qter"))%>%
                  select(group, mpa_name, mpa_class, mpa_designation,
                         mpa_defacto_class, mpa_defacto_designation, region3,
                         region4, year, qter, month, everything())

#path_aurora <- "/home/shares/ca-mpa/data/sync-data/environmental/processed"
#saveRDS(envr_vars_all, file.path(path_aurora, "envr_anomalies_at_mpas.Rds"))










# Process rocky intertidal ------------------------------------------------



#step 1: aggregate at site - site_type - year level
#drop day 
envr_drop <- envr_build_step5 %>% filter(habitat=='rocky intertidal')%>%
  dplyr::select(!(c("day", "date", "lat_dd_bin", "cuti", "beuti")))%>%
  distinct(across(everything()))%>%
  mutate(year=as.factor(year),
         month=as.factor(month),
         habitat=as.factor(habitat),
         site=as.factor(site),
         site_type = as.factor(site_type),
         )%>%
  group_by(habitat, site, site_type, mpa, year)%>%
  dplyr::summarise(across(everything(), mean, na.rm = TRUE))

sst_drop <- sst_df %>% filter(habitat=='rocky intertidal')%>%
  select(!(c("day", "date")))%>%
  distinct(across(everything()))%>%
  mutate(year=as.factor(year),
         month=as.factor(month),
         habitat=as.factor(habitat),
         site=as.factor(site),
         site_type = as.factor(site_type))%>%
  group_by(habitat, site, site_type, year)%>%
  dplyr::summarize(sst_c = mean(as.numeric(sst_c)),
                   sst_baseline = mean(as.numeric(sst_baseline)),
                   sst_monthly_baseline = mean(as.numeric(sst_monthly_baseline)),
                   obs_annual_SST = mean(as.numeric(obs_annual_SST)),
                   obs_monthly_SST = mean(as.numeric(obs_monthly_SST)),
                   monthly_anomaly_sst = mean(as.numeric(monthly_anomaly_sst)),
                   )

#step 2 - join
envr_data_step2 <- left_join(sst_drop, envr_drop,
                             by=c("habitat","site","site_type","year"
                                  
                             ))
#step 3 - rename and clean
envr_data <- envr_data_step2 %>%
  #mutate(mpa_class = str_extract(mpa, "smca|smr|special closure"),
  #       mpa_designation = ifelse(site_type == "reference","ref",mpa_class))%>%
  select(group=habitat,
         site,
         year,
         sst_annual_baseline=sst_baseline, 
         sst_monthly_baseline,
         sst_annual_obs= obs_annual_SST,
         sst_monthly_obs = obs_monthly_SST,
         sst_monthly_anom = monthly_anomaly_sst,
         cuti_annual_baseline = lt_cuti, 
         cuti_monthly_baseline = lt_monthly_cuti,
         cuti_annual_obs = obs_annual_cuti,
         cuti_monthly_obs=obs_monthly_cuti,
         cuti_monthly_anom = monthly_anomaly_cuti,
         beuti_annual_baseline = lt_beuti,
         beuti_monthly_baseline = lt_monthly_beuti,
         beuti_annual_obs = obs_annual_beuti,
         beuti_monthly_obs = obs_monthly_beuti,
         beuti_monthly_anom = monthly_anomaly_beuti
  )


envr_data <- envr_data %>%
  mutate(group= as.factor(group),
         site = as.factor(site))%>%
  mutate_if(is.character,as.numeric)


#add regions 
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_rocky-intertidal/Community analysis"
input_file <- "intertidal_site_counts.csv"

rocky_sites <- read.csv(file.path(data_path, input_file)) %>%
  janitor::clean_names() %>% select(site, region3, region4)

rocky_sites$site <- tolower(rocky_sites$site)

rocky_envr_data <- left_join(envr_data, rocky_sites, by="site")%>%
                    mutate(year=as.factor(year))


MOCI_annual <- MOCI_step1 %>%
  group_by(year, region3)%>%
  summarize(annual_MOCI=mean(quarterly_MOCI))

MOCI_step2 <- left_join(MOCI_step1, MOCI_annual, by=c("year", "region3")) %>%
  select(year, region3, annual_MOCI) %>%
  mutate(year= as.factor(year))


rocky_envr_vars_all <- left_join(rocky_envr_data, MOCI_step2, by=c("year","region3"))
  

rocky_envr_vars_cen <- left_join(rocky_envr_data, MOCI_step2, by=c("year","region3"))%>%
                        filter(region4=='central')%>%
                        group_by(group, site, year)%>%
                        dplyr::summarise(across(everything(), list(mean)))
                        
names(rocky_envr_vars_cen) <-  gsub("_1", "", names(rocky_envr_vars_cen))

rocky_envr_vars_cen <- rocky_envr_vars_cen %>% select(!(c(region3, region4)))


#path_aurora <- "/home/shares/ca-mpa/data/sync-data/environmental/processed"
#saveRDS(rocky_envr_vars_cen, file.path(path_aurora, "envr_anomalies_at_intertidal_mpas.Rds"))

                       
                        



