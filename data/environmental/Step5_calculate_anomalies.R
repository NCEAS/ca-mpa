
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)


#load envr data
envr_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/1988_2022_cuti_beuti_daily_by_monitoring_site.Rds")
SST_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/2002_2022_mursst_monthly_by_monitoring_site.Rds")
MOCI_dat <- read.csv("/home/shares/ca-mpa/data/sync-data/environmental/raw/MOCI.csv")



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





# add regions and defacto SMRs--------------------------------------------------

envr_data_join <- envr_data %>%
                  filter(!(group=="rocky intertidal")) # drop intertidal because these envr vars are not applicable
                  

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
                           )))


# add MOCI ----------------------------------------------------------------

MOCI_step1 <- MOCI_dat %>%
              rename(year = Year,
                     north = North.California..38.42N.,
                     central = Central.California..34.5.38N.,
                     south = Southern.California..32.34.5N.)%>%
              select(!(time))%>%
              pivot_longer(cols = c("north","central","south"),
                         values_to = "quarterly_MOCI",
                         names_to = "region3")

MOCI_annual <- MOCI_step1 %>%
                group_by(year)%>%
                summarize(annual_MOCI=mean(quarterly_MOCI))

MOCI_step2 <- left_join(MOCI_step1, MOCI_annual, by="year") %>%
               select(year, region3, annual_MOCI)


envr_vars_all <- left_join(envr_join_step5, MOCI_step2, by=c("year","region3"))

#path_aurora <- "/home/shares/ca-mpa/data/sync-data/environmental/processed"
#saveRDS(envr_vars_all, file.path(path_aurora, "envr_anomalies_at_mpas.Rds"))





# Quick plot to see how things look ---------------------------------------



envr_plot <- envr_vars_all %>%
              filter(region3=='central')%>%
              group_by(year, month)%>%
               dplyr::summarise(beuti_anom = mean(beuti_monthly_anom),
                   cuti_anom = mean(cuti_monthly_anom),
                   sst_anom = mean(as.numeric(as.character(sst_monthly_anom)),na.rm=T),
                   MOCI = mean(as.numeric(annual_MOCI), na.rm=T),
                   ) %>%
                pivot_longer(names_to = "index", cols=c("beuti_anom","cuti_anom","sst_anom","MOCI"))


envr_plot$index <- factor(envr_plot$index, levels = c("beuti_anom", "cuti_anom", "sst_anom", "MOCI"))


envr_plot%>%
  filter(year>=2000)%>%
  ggplot(aes(x=as.numeric(year),y=value,color=index, fill=index, group=index))+
  geom_point()+
  stat_summary(fun=mean, geom="line", aes(color = index), size=2)+
  geom_rect(data = data.frame(year = 2015), aes(xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf), alpha = 0.2, fill="red", inherit.aes = FALSE)+
  #annotate("rect", xmin = 2014.5, xmax = 2016.5, ymin = -12, ymax = 12,
  #      alpha = .2, fill="pink")+
  facet_wrap(~index, ncol=2, nrow=2, scale="free_y")+
  xlab("year")+
  #scale_y_continuous(
  #  "beuti_anom", 
  #  sec.axis = sec_axis(trans=~ . * .0001, name = "cuti_anom")
  #)+
  theme_minimal(base_size = 22,)+
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5))+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  labs(title="Central CA oceanographic anomalies")









