
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)


#load envr data
envr_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/1988_2022_cuti_beuti_daily_by_monitoring_site.Rds")




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


