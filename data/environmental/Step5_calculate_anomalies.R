
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)


#load envr data
envr_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/1988_2022_cuti_beuti_daily_by_monitoring_site.Rds")
SST_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/2002_2022_mursst_monthly_by_monitoring_site.Rds")




#make everything lower
envr_df <- as.data.frame(sapply(envr_dat, tolower))



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







# Join beuti, cuti, and sst -----------------------------------------------

#make everything lower
sst_df <- as.data.frame(sapply(sst_build_step5, tolower))

#step 1: aggregate at MPA - site_type - year level
#drop day 
envr_drop <- envr_build_step5 %>%
  select(!(c("day", "date", "lat_dd_bin", "cuti", "beuti","site")))%>%
  distinct(across(everything()))%>%
  mutate(year=as.factor(year),
         month=as.factor(month),
         habitat=as.factor(habitat),
         mpa=as.factor(mpa),
         site_type = as.factor(site_type))
  
sst_drop <- sst_df %>%
  select(!(c("day", "date","site")))%>%
  distinct(across(everything()))%>%
  mutate(year=as.factor(year),
         month=as.factor(month),
         habitat=as.factor(habitat),
         mpa=as.factor(mpa),
         site_type = as.factor(site_type))

#step 2 - join
envr_data_step2 <- left_join(sst_drop, envr_drop,
                            by=c("habitat","mpa","site_type","year","month"
      
                                                      ))
#step 3 - rename and clean
envr_data <- envr_data_step2 %>%
              mutate(mpa_class = str_extract(mpa, "smca|smr|special closure"),
                     mpa_designation = ifelse(site_type == "reference","ref",mpa_class),
                     mpa_name = gsub("\\s*\\([^\\)]+\\)","",mpa))%>%
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
                     beuti_monthly_anom = monthly_anomaly_beuti
                     )

#reclassify

envr_data <- envr_data %>%
            mutate(group= as.factor(group),
                   mpa_name = as.factor(mpa_name),
                   mpa_class = as.factor(mpa_class),
                   mpa_designation = as.factor(mpa_designation))%>%
             mutate_if(is.character,as.numeric)



                
#path_aurora <- "/home/shares/ca-mpa/data/sync-data/environmental/processed"
#saveRDS(envr_data, file.path(path_aurora, "envr_anomalies_at_mpas.Rds"))




