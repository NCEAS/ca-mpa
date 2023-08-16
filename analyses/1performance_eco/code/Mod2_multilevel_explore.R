#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, metafor)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) 

################################################################################
#prep data

biomass_mod <- biomass_mod %>% mutate(target_status = ifelse(habitat == "Rocky reef","Targeted",target_status),
                                      affiliated_mpa = str_to_title(affiliated_mpa) %>% 
                                        str_replace(" Smr$", " SMR") %>% 
                                        str_replace(" Smca$", " SMCA"))

################################################################################
##calcualte effect size for each year and MPA

#this is the calculation of the variance without the use of escalc
dat <- biomass_mod %>%
  mutate(yi = logRR,
         vi = ((sd_smr^2) / (n_rep_smr*((biomass_smr + scalar_smr)^2))) +
           ((sd_ref^2) / (n_rep_ref*((biomass_ref + scalar_ref)^2))))

#calcualte effect size for each year and MPA
#dat <- escalc(measure="ROM", m1i=biomass_smr + scalar_smr, m2i=biomass_ref + scalar_ref, sd1i=sd_smr, 
#             sd2i=sd_ref, n1i=n_rep_smr, n2i=n_rep_ref, data=biomass_mod)


forest_dat <- dat %>% filter(age_at_survey > 0) %>% 
  #drop missing variance
  filter(!(is.na(vi) | vi == 0))


################################################################################
##build multilevel meta regression


dat <- escalc(measure="ROM", m1i=biomass_smr + scalar_smr, m2i=biomass_ref + scalar_ref, sd1i=sd_smr, 
                       sd2i=sd_ref, n1i=n_rep_smr, n2i=n_rep_ref, data=biomass_mod, slab = paste("Habitat",habitat,"MPA",affiliated_mpa))

dat <- dat %>% filter(!(is.na(vi) | vi ==0)) %>% group_by(habitat, affiliated_mpa) %>% summarize(vi = mean(vi))

### assume that the effect sizes within habitats are correlated with rho=0.6
V <- vcalc(vi=vi, cluster=habitat, obs=affiliated_mpa, data=dat, rho=0.6)

?metafor::vcalc




