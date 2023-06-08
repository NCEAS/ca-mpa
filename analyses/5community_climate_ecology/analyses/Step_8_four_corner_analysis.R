#Joshua G. Smith
#November 14, 2022

rm(list=ls())

require(tidyverse)
require(mvabund)

#set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")


#read data
load(file.path(datadir,"all_groups_mpa_level_means_long.rda"))

#tidy up
CCFRP_join1[CCFRP_join1 == ""] <- NA
kelp_fish_join1[kelp_fish_join1 == ""] <- NA
kelp_combined_join1[kelp_combined_join1 == ""] <- NA
deep_reef_join1[deep_reef_join1 == ""]<- NA
kelp_swath_join1[kelp_swath_join1==""]<-NA
kelp_upc_join1[kelp_upc_join1==""]<-NA
rocky_join1[rocky_join1==""]<-NA

#Four corners trait-based modeling using conceptualization reported in Brown
# et al. 2014. Trait based modeling requires abundance matrix (L) in wide
#format, environmental matrix (R) in wide format (with rows that correspond to
#each site), and trait matrix (Q) in long format, where nrows(Q) = ncol(L).


#for reference see https://heather-grab.github.io/Entom-4940/rql.html
#and also https://rpubs.com/dwarton/68823


################################################################################
#Step 1 - build L, R, and Q


#====================CCFRP Build

CCFRP_join1[CCFRP_join1 == ""] <- NA

CCFRP_join2 <- CCFRP_join1 %>%
              filter(year>=2007)%>%
              dplyr::select(year, group, region3, region4, MHW, affiliated_mpa,
                            mpa_class, mpa_designation, sst_monthly_anom, sst_monthly_obs, 
                            bottomT_monthly_anom, bottomT_monthly_obs, beuti_monthly_anom, beuti_monthly_obs, annual_MOCI,
                            species, counts, thermal_affinity, targeted)%>%
              drop_na()

CCFRP_L <- CCFRP_join2 %>% 
           #species included in analysis must have known therm affinity
           drop_na(thermal_affinity)%>%
           select(!(c(thermal_affinity, targeted)))%>%
           pivot_wider(names_from="species", values_from = "counts")%>%
           select(16:ncol(.))%>%
           as.data.frame()


CCFRP_R <- CCFRP_join2 %>% 
           #species included in analysis must have known therm affinity
           drop_na(thermal_affinity)%>%
           select(!(c(thermal_affinity, targeted)))%>%
           pivot_wider(names_from="species", values_from = "counts")%>%
           #select envr vars, note mvabund does not take function formula
          dplyr::select(sst_monthly_anom,
                        sst_monthly_obs,
                  bottomT_monthly_anom,
                  bottomT_monthly_obs,
                  beuti_monthly_anom,
                  beuti_monthly_obs,
                  annual_MOCI)%>%
            as.data.frame()

CCFRP_Q <- data.frame(CCFRP_join2) %>% 
            #species included in analysis must have known therm affinity
            drop_na(thermal_affinity) %>%
            dplyr::select(species, thermal_affinity)%>%
            distinct(species,.keep_all = TRUE)%>%
            #set species to row name
            column_to_rownames(var="species")%>%
            mutate(thermal_affinity = factor(thermal_affinity))%>%
            as.data.frame()

#check lengths
nrow(CCFRP_L)
nrow(CCFRP_R)
ncol(CCFRP_L)
nrow(CCFRP_Q)




#====================kelp fish Build

#remove missing values
kelp_fish_join2 <- kelp_fish_join1 %>%
                dplyr::select(year, group, region3, region4, MHW, affiliated_mpa,
                mpa_defacto_class, mpa_defacto_designation, sst_monthly_anom, sst_monthly_obs,
                bottomT_monthly_anom, bottomT_monthly_obs, beuti_monthly_anom,beuti_monthly_obs, annual_MOCI,
                species, counts, thermal_affinity, targeted)%>%
                   filter(year >= 2007)%>%
                drop_na()

kelp_fish_L <- kelp_fish_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  select(16:ncol(.))%>%
  as.data.frame()



kelp_fish_R <- kelp_fish_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  #select envr vars, note mvabund does not take function formula
  dplyr::select(sst_monthly_anom,
                sst_monthly_obs,
                bottomT_monthly_anom,
                bottomT_monthly_obs,
                beuti_monthly_anom,
                beuti_monthly_obs,
                annual_MOCI)%>%
  as.data.frame()


kelp_fish_Q <- kelp_fish_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity) %>%
  select(species, thermal_affinity)%>%
  distinct(species,.keep_all = TRUE)%>%
  #set species to row name
  column_to_rownames(var="species")%>%
  mutate(thermal_affinity = factor(thermal_affinity))%>%
  as.data.frame()

nrow(kelp_fish_L)
nrow(kelp_fish_R)
ncol(kelp_fish_L)
nrow(kelp_fish_Q)

#====================deep reef build

deep_reef_join2 <- deep_reef_join1 %>%
  dplyr::select(year, group, region3, region4, MHW, affiliated_mpa,
                mpa_defacto_class, mpa_defacto_designation, sst_monthly_anom, sst_monthly_obs, 
                bottomT_monthly_anom, bottomT_monthly_obs, beuti_monthly_anom, beuti_monthly_obs, annual_MOCI,
                species, counts, thermal_affinity, targeted)%>%
  filter(year >= 2007)%>%
  drop_na()

deep_reef_L <- deep_reef_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  select(16:ncol(.))%>%
  as.data.frame()

deep_reef_R <- deep_reef_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  #select envr vars, note mvabund does not take function formula
  dplyr::select(sst_monthly_anom,
                sst_monthly_obs,
                bottomT_monthly_anom,
                bottomT_monthly_obs,
                beuti_monthly_anom,
                beuti_monthly_obs,
                annual_MOCI)%>%
  as.data.frame()



deep_reef_Q <- deep_reef_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity) %>%
  select(species, thermal_affinity)%>%
  distinct(species,.keep_all = TRUE)%>%
  #set species to row name
  column_to_rownames(var="species")%>%
  mutate(thermal_affinity = factor(thermal_affinity))%>%
  as.data.frame()

#check lengths
nrow(deep_reef_L)
nrow(deep_reef_R)
ncol(deep_reef_L)
nrow(deep_reef_Q)


#====================kelp swath build

kelp_swath_join2 <- kelp_swath_join1 %>%
  dplyr::select(year, group, region3, region4, MHW, affiliated_mpa,
                mpa_defacto_class, mpa_defacto_designation, sst_monthly_anom, sst_monthly_obs,
                bottomT_monthly_anom, bottomT_monthly_obs, beuti_monthly_anom, beuti_monthly_obs, annual_MOCI,
                species, counts, thermal_affinity, targeted)%>%
  filter(year>=2007)%>%
  drop_na()%>%
  mutate(thermal_affinity = factor(
    thermal_affinity, levels=c(
      'cosmopolitan', 'tropical','subtropical','warm temperate','cold temperate'
    )
  ))

kelp_swath_L <- kelp_swath_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  select(16:ncol(.))%>%
  replace(is.na(.),0)%>%
  as.data.frame()

kelp_swath_R <- kelp_swath_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  #select envr vars, note mvabund does not take function formula
  dplyr::select(sst_monthly_anom,
                sst_monthly_obs,
                bottomT_monthly_anom,
                bottomT_monthly_obs,
                beuti_monthly_anom,
                beuti_monthly_obs,
                annual_MOCI)%>%
  as.data.frame()



kelp_swath_Q <- kelp_swath_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity) %>%
  select(species, thermal_affinity)%>%
  distinct(species,.keep_all = TRUE)%>%
  #set species to row name
  column_to_rownames(var="species")%>%
  mutate(thermal_affinity = factor(thermal_affinity))%>%
  as.data.frame()

#check lengths
nrow(kelp_swath_L)
nrow(kelp_swath_R)
ncol(kelp_swath_L)
nrow(kelp_swath_Q)


#====================rocky intertidal build

rocky_join2 <- rocky_join1 %>%
  dplyr::select(year, group, region3, region4, affiliated_mpa,
                 mpa_designation, sst_monthly_anom, sst_monthly_obs,
               beuti_monthly_anom, beuti_monthly_obs, annual_MOCI,
                species, counts, thermal_affinity)%>%
  filter(year>=2007)%>%
  drop_na()

rocky_L <- rocky_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  select(12:ncol(.))%>%
  replace(is.na(.),0)%>%
  as.data.frame()

rocky_R <- rocky_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  #select envr vars, note mvabund does not take function formula
  dplyr::select(sst_monthly_anom,
                sst_monthly_obs,
               # bottomT_monthly_anom,
                beuti_monthly_anom,
               beuti_monthly_obs,
                annual_MOCI)%>%
  as.data.frame()


rocky_Q <- rocky_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity) %>%
  select(species, thermal_affinity)%>%
  distinct(species,.keep_all = TRUE)%>%
  #set species to row name
  column_to_rownames(var="species")%>%
  mutate(thermal_affinity = factor(thermal_affinity))%>%
  as.data.frame()
 
#check lengths
nrow(rocky_L)
nrow(rocky_R)
ncol(rocky_L)
nrow(rocky_Q)


#====================kelp upc

kelp_upc_join2 <- kelp_upc_join1 %>%
  dplyr::select(year, group, region3, region4, MHW, affiliated_mpa,
                mpa_defacto_class, mpa_defacto_designation, sst_monthly_anom, sst_monthly_obs, 
                bottomT_monthly_anom, bottomT_monthly_obs, beuti_monthly_anom, beuti_monthly_obs, annual_MOCI,
                species, counts, thermal_affinity, targeted)%>%
  drop_na()%>%
  mutate(thermal_affinity = factor(
    thermal_affinity, levels=c(
      'cosmopolitan', 'tropical','subtropical','warm temperate','cold temperate'
    )
  ))

kelp_upc_L <- kelp_upc_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  select(16:ncol(.))%>%
  replace(is.na(.),0)%>%
  as.data.frame()

kelp_upc_R <- kelp_upc_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity)%>%
  select(!(c(thermal_affinity, targeted)))%>%
  pivot_wider(names_from="species", values_from = "counts")%>%
  #select envr vars, note mvabund does not take function formula
  dplyr::select(sst_monthly_anom,
                sst_monthly_obs,
                bottomT_monthly_anom,
                bottomT_monthly_obs,
                beuti_monthly_anom,
                beuti_monthly_obs,
                annual_MOCI)%>%
  as.data.frame()



kelp_upc_Q <- kelp_upc_join2 %>% 
  #species included in analysis must have known therm affinity
  drop_na(thermal_affinity) %>%
  select(species, thermal_affinity)%>%
  distinct(species,.keep_all = TRUE)%>%
  #set species to row name
  column_to_rownames(var="species")%>%
  mutate(thermal_affinity = factor(thermal_affinity))%>%
  as.data.frame()


#check lengths
nrow(kelp_upc_L)
nrow(kelp_upc_R)
ncol(kelp_upc_L)
nrow(kelp_upc_Q)


################################################################################
# trait based model

#============================CCFRP==============================================
CCFRP_anom <- CCFRP_R %>% dplyr::select(sst_monthly_anom, bottomT_monthly_anom, beuti_monthly_anom, annual_MOCI)
CCFRP_abs <- CCFRP_R %>% dplyr::select(sst_monthly_obs, bottomT_monthly_obs, beuti_monthly_obs, annual_MOCI)


#convert to integer
CCFRP_L_100 <- ceiling(CCFRP_L*100)

ft_ccfrp_anom<- traitglm(CCFRP_L_100, CCFRP_anom, CCFRP_Q,
              method="glm1path", family = "negative.binomial")

ft_ccfrp_abs<- traitglm(CCFRP_L_100, CCFRP_abs, CCFRP_Q,
                    method="glm1path", family = "negative.binomial")

#test plot
ft_ccfrp_anom$fourth

#check residuals
plot(ft_ccfrp_anom)

library(lattice)

a        = max( abs(ft_ccfrp_anom$fourth.corner) )
colort   = colorRampPalette(c("blue","white","red")) 
plot.4th.anom = levelplot(t(as.matrix(ft_ccfrp_anom$fourth.corner)), xlab="Environmental Variables",
                     ylab="Species traits", col.regions=colort(100), at=seq(-a, a, length=100),
                     scales = list( x= list(rot = 45)))
print(plot.4th.anom)


#============================Kelp fish==========================================

kelp_fish_anom <- kelp_fish_R %>% dplyr::select(sst_monthly_anom, bottomT_monthly_anom, beuti_monthly_anom, annual_MOCI)
kelp_fish_abs <- kelp_fish_R %>% dplyr::select(sst_monthly_obs, bottomT_monthly_obs, beuti_monthly_obs, annual_MOCI)

ft_kelp_fish_anom<- traitglm(kelp_fish_L, kelp_fish_anom, kelp_fish_Q,
                    method="glm1path", family = "negative.binomial")

ft_kelp_fish_abs<- traitglm(kelp_fish_L, kelp_fish_abs, kelp_fish_Q,
                             method="glm1path", family = "negative.binomial")

#============================Deep reef==========================================

deep_reef_anom <- deep_reef_R %>% dplyr::select(sst_monthly_anom, bottomT_monthly_anom, beuti_monthly_anom, annual_MOCI)
deep_reef_abs <- deep_reef_R %>% dplyr::select(sst_monthly_obs, bottomT_monthly_obs, beuti_monthly_obs, annual_MOCI)

deep_reef_L <- ceiling(deep_reef_L)

ft_deep_reef_anom <- traitglm(deep_reef_L, deep_reef_anom, deep_reef_Q,
                        method="glm1path", family = "negative.binomial")

ft_deep_reef_abs <- traitglm(deep_reef_L, deep_reef_abs, deep_reef_Q,
                              method="glm1path", family = "negative.binomial")

#============================kelp swath=========================================

kelp_swath_anom <- kelp_swath_R %>% dplyr::select(sst_monthly_anom, bottomT_monthly_anom, beuti_monthly_anom, annual_MOCI)
kelp_swath_abs <- kelp_swath_R %>% dplyr::select(sst_monthly_obs, bottomT_monthly_obs, beuti_monthly_obs, annual_MOCI)

ft_kelp_swath_anom <- traitglm(kelp_swath_L, kelp_swath_anom, kelp_swath_Q,
                         method="glm1path", family = "negative.binomial")

ft_kelp_swath_abs <- traitglm(kelp_swath_L, kelp_swath_abs, kelp_swath_Q,
                               method="glm1path", family = "negative.binomial")

#============================rocky intertidal===================================

rocky_anom <- rocky_R %>% dplyr::select(sst_monthly_anom, beuti_monthly_anom, annual_MOCI)
rocky_abs <- rocky_R %>% dplyr::select(sst_monthly_obs,  beuti_monthly_obs, annual_MOCI)

rocky_L_100 <- ceiling(rocky_L*100)

rocky_corner_anom <- traitglm(rocky_L_100, rocky_anom, rocky_Q,
                          method="glm1path", family = "negative.binomial")
rocky_corner_abs <- traitglm(rocky_L_100, rocky_abs, rocky_Q,
                         method="glm1path", family = "negative.binomial")

#============================kelp upc===========================================

kelp_upc_anom <- kelp_upc_R %>% dplyr::select(sst_monthly_anom, bottomT_monthly_anom, beuti_monthly_anom, annual_MOCI)
kelp_upc_abs <- kelp_upc_R %>% dplyr::select(sst_monthly_obs, bottomT_monthly_obs, beuti_monthly_obs, annual_MOCI)

kelp_upc_L_round <- ceiling(kelp_upc_L)

ft_kelp_upc_anom <- traitglm(kelp_upc_L_round, kelp_upc_anom, kelp_upc_Q,
                         method="glm1path", family = "negative.binomial")
ft_kelp_upc_abs <- traitglm(kelp_upc_L_round, kelp_upc_abs, kelp_upc_Q,
                             method="glm1path", family = "negative.binomial")



################################################################################
#Tidy coef for anom

coef_ccfrp <- ft_ccfrp_anom$fourth %>%
              as.data.frame()%>%
              rownames_to_column(var="Thermal affinity")%>%
              rename(SST = "sst_monthly_anom",
                     BT = "bottomT_monthly_anom",
                     BEUTI = "beuti_monthly_anom",
                     MOCI = "annual_MOCI")%>%
              mutate(`Thermal affinity` = recode(`Thermal affinity`,
                          "thermal_affinitycold temperate" = "Cold temperate",
                          "thermal_affinitysubtropical" = "Subtropical",
                          "thermal_affinitywarm temperate"="Warm temperate"))%>%
              mutate(Group = "Rocky reef fish")

coef_kelp_fish <- ft_kelp_fish_anom$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_anom",
         BT = "bottomT_monthly_anom",
         BEUTI = "beuti_monthly_anom",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Kelp forest fish")


coef_deep_reef <- ft_deep_reef_anom$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_anom",
         BT = "bottomT_monthly_anom",
         BEUTI = "beuti_monthly_anom",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate"))%>%
  mutate(Group = "Deep reef fish")

coef_kelp_swath <- ft_kelp_swath_anom$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_anom",
         BT = "bottomT_monthly_anom",
         BEUTI = "beuti_monthly_anom",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Kelp forest inverts and algae (swath)")


coef_kelp_upc <- ft_kelp_upc_anom$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_anom",
         BT = "bottomT_monthly_anom",
         BEUTI = "beuti_monthly_anom",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "therma_affinitytropical" = "Tropical",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Kelp forest inverts and algae (upc)")

coef_rocky <- rocky_corner_anom$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  mutate(BT = NA)%>%
  rename(SST = "sst_monthly_anom",
         BEUTI = "beuti_monthly_anom",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Rocky intertidal")





coef_out <- rbind(coef_ccfrp, coef_kelp_fish, coef_deep_reef, coef_kelp_swath,
                  coef_rocky, coef_kelp_upc)%>%
            pivot_longer(cols=c("SST","BT","BEUTI","MOCI"),
                                names_to = "Environmental Variables",
                                values_to = "Beta")

View(coef_out)


################################################################################
#Save model output

#save(coef_out, file="/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/four_corner_output_anom_run2.rda")



################################################################################
#Tidy coef for abs

coef_ccfrp <- ft_ccfrp_abs$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_obs",
         BT = "bottomT_monthly_obs",
         BEUTI = "beuti_monthly_obs",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate"))%>%
  mutate(Group = "Rocky reef fish")

coef_kelp_fish <- ft_kelp_fish_abs$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_obs",
         BT = "bottomT_monthly_obs",
         BEUTI = "beuti_monthly_obs",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Kelp forest fish")


coef_deep_reef <- ft_deep_reef_abs$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_obs",
         BT = "bottomT_monthly_obs",
         BEUTI = "beuti_monthly_obs",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate"))%>%
  mutate(Group = "Deep reef fish")

coef_kelp_swath <- ft_kelp_swath_abs$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_obs",
         BT = "bottomT_monthly_obs",
         BEUTI = "beuti_monthly_obs",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Kelp forest inverts and algae (swath)")


coef_kelp_upc <- ft_kelp_upc_abs$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  rename(SST = "sst_monthly_obs",
         BT = "bottomT_monthly_obs",
         BEUTI = "beuti_monthly_obs",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "therma_affinitytropical" = "Tropical",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Kelp forest inverts and algae (upc)")

coef_rocky <- rocky_corner_abs$fourth %>%
  as.data.frame()%>%
  rownames_to_column(var="Thermal affinity")%>%
  mutate(BT = NA)%>%
  rename(SST = "sst_monthly_obs",
         BEUTI = "beuti_monthly_obs",
         MOCI = "annual_MOCI")%>%
  mutate(`Thermal affinity` = recode(`Thermal affinity`,
                                     "thermal_affinitycold temperate" = "Cold temperate",
                                     "thermal_affinitysubtropical" = "Subtropical",
                                     "thermal_affinitywarm temperate"="Warm temperate",
                                     "thermal_affinitycosmopolitan" = "Cosmopolitan"))%>%
  mutate(Group = "Rocky intertidal")





coef_out <- rbind(coef_ccfrp, coef_kelp_fish, coef_deep_reef, coef_kelp_swath,
                  coef_rocky, coef_kelp_upc)%>%
  pivot_longer(cols=c("SST","BT","BEUTI","MOCI"),
               names_to = "Environmental Variables",
               values_to = "Beta")

View(coef_out)


################################################################################
#Save model output

save(coef_out, file="/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/four_corner_output_abs.rda")





################################################################################
#Plot 


theme1 <- theme(axis.text=element_text(size=6),
                axis.text.y=element_text(size=6),
                axis.text.x = element_text(angle = 45, vjust = 1, hjust=1),
                legend.text=element_text(size=6),
                legend.title=element_text(size=7),
                strip.text=element_text(size=7),
                title = element_text(face = "bold", color = "black", size=6),
                axis.title = element_text(face = "bold", color = "black",
                                          size=6),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.key.size = unit(0.5, "cm"),
                legend.background = element_rect(fill=alpha('blue', 0)),
                #margins
                plot.margin=unit(c(0,0,0,0),"cm")
                )


p1 <- coef_out %>%
  filter(Group == "Rocky reef fish")%>%
  ggplot(aes(x=`Environmental Variables`, y=`Thermal affinity`, fill=Beta)) +
  # Raster
  geom_tile(lwd=0.1, show.legend=FALSE, color="black") +
  #scale_fill_gradientn(name="Coefficient", colours=rev(brewer.pal(11,rev("RdYlBu"))),#breaks=seq(-10,15,by=5)
                   #    )+
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), mid = "white",
                        high = muted("darkred"), space = "Lab" )+
  # Legend
  #scale_color_manual(name="MPA type", values=c("navy", "darkred")) +
  #guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1 +
  coord_equal()+
  # Labels
  labs(x="", y="",
       title = "Rocky reef fish")+
  theme(plot.title = element_text(vjust = -3))
p1


p2 <- coef_out %>%
  filter(Group == "Kelp forest fish")%>%
  ggplot(aes(x=`Environmental Variables`, y=`Thermal affinity`, fill=Beta)) +
  # Raster
  geom_tile(lwd=0.1, show.legend=FALSE, color="black") +
  #scale_fill_gradientn(name="Coefficient", colours=rev(brewer.pal(11,rev("RdYlBu"))),#breaks=seq(-10,15,by=5)
  #    )+
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), mid = "white",
                       high = muted("darkred"), space = "Lab" )+
  # Legend
  #scale_color_manual(name="MPA type", values=c("navy", "darkred")) +
  #guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1 +
  coord_equal()+
  # Labels
  labs(x="", y="",
       title = "Kelp forest fish")+
  theme(plot.title = element_text(vjust = -3))
p2

p3 <- coef_out %>%
  filter(Group == "Deep reef fish")%>%
  ggplot(aes(x=`Environmental Variables`, y=`Thermal affinity`, fill=Beta)) +
  # Raster
  geom_tile(lwd=0.1, show.legend=FALSE, color="black") +
  #scale_fill_gradientn(name="Coefficient", colours=rev(brewer.pal(11,rev("RdYlBu"))),#breaks=seq(-10,15,by=5)
  #    )+
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), mid = "white",
                       high = muted("darkred"), space = "Lab" )+
  # Legend
  #scale_color_manual(name="MPA type", values=c("navy", "darkred")) +
  #guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  # Theme
  theme_bw() + theme1 +
  coord_equal()+
  # Labels
  labs(x="", y="",
       title = "Deep reef fish")+
  theme(plot.title = element_text(vjust = -3))
p3


p4 <- coef_out %>%
  filter(Group == "Kelp forest inverts and algae")%>%
  ggplot(aes(x=`Environmental Variables`, y=`Thermal affinity`, fill=Beta)) +
  # Raster
  geom_tile(lwd=0.1, show.legend=TRUE, color="black") +
  #legend
  scale_fill_gradient2(midpoint = 0, low = muted("blue"), mid = "white",
                       high = muted("darkred"), space = "Lab" )+
  # Theme
  theme_bw() + theme1 +
  coord_equal()+
  # Labels
  labs(x="", y="",
       title = "Kelp forest inverts\nand algae")
  
p4



p <- ggarrange(p1,p2,p3,p4, common.legend = TRUE, ncol=1,
          legend="right",
          align="h")
p_merge <- annotate_figure(p, 
         bottom = text_grob("Environmental variables", 
                  size = 10, hjust=0.45),
         left = text_grob("Thermal affinity",  rot = 90,
                 vjust=1, size=10))

ggsave(p_merge, filename=file.path(figdir, "Four_corners_plot.png"), 
       width=3, height=8, units="in", dpi=600, bg="white")







  