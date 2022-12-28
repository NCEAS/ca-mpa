#Joshua G. Smith
#December 13, 2022

rm(list=ls())

#require packages
require(dplyr)
require(here)
require(MASS)
require(ggeffects)

#set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/statewide_data"
figdir <-  here::here("analyses", "5community_climate_ecology", "figures")

mpa_trait <- read.csv("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_clean.csv")

#load model output
mod_out_raw <- read.csv(file.path(datadir,"mpa_betadisp_mod.csv"))


###############################################################################
#Join model output with MPA traits

#Step 1 - select traits of interest

trait_drivers <- mpa_trait %>%
  dplyr::select(name, long, lat, implementation_date, size_km2, shore_span_km,
                max_depth_m, protection, depth_range, four_region)


mpa_dat <- left_join(mod_out_raw, trait_drivers, by=c("MPA"="name"))%>%
  ## set target as 'no change' == stable
  mutate(stable = ifelse(p.value < 0.05,"no","yes"))%>%
  ##define resist / resilience
  mutate(period=paste(period_1, period_2, sep="-"),
         process=recode_factor(period,
                               "before-during"="Resistance",
                               "before-after"="Resilience"))%>%
  #remove ref sites, since we don't know anything about their traits
  filter(!(MPA_type == "ref"))%>%
  dplyr::select(!(MPA_type))%>%
  #calculate relative age
  mutate(imp_year = format(as.Date(implementation_date, format="%m/%d/%Y"),"%Y"),
         rel_age = 2022 - as.numeric(imp_year))


###############################################################################
#build model for kelp forest fishes

kf_fish_resist <- mpa_dat %>%
  dplyr::select(c(process, habitat, MPA, size_km2, shore_span_km,
                  max_depth_m, depth_range, rel_age, lat,
                  four_region, distance))%>%
  filter(process=="Resistance",
         habitat == 'Kelp forest fishes')%>%
  dplyr::select(!(process))

kf_fish_resil <- mpa_dat %>%
  drop_na()%>%
  dplyr::select(c(process, habitat, MPA,  size_km2, shore_span_km,
                  max_depth_m, depth_range, rel_age, lat, four_region, distance))%>%
  filter(process=="Resilience",
         habitat == "Kelp forest fishes")%>%
  dplyr::select(!(process))

#resistance 
kf_fish_resist_mod <-glm(distance ~ size_km2 + shore_span_km +
                                    depth_range + lat, data = kf_fish_resist, 
                                  family="gaussian", 
                                  na.action = na.exclude)
summary(kf_fish_resist_mod)

#resilience
kf_fish_resil_mod <- glm(distance ~ size_km2 + shore_span_km +
                                    depth_range + lat, data = kf_fish_resil, 
                                  family="gaussian", 
                                  na.action = na.exclude)
summary(kf_fish_resist_mod)



###############################################################################
#build model for kelp forest inverts and algae

kf_invalg_resist <- mpa_dat %>%
  dplyr::select(c(process, habitat, MPA, 
                  size_km2, shore_span_km,
                  max_depth_m, depth_range, rel_age, lat,
                  four_region, distance))%>%
  filter(process=="Resistance",
         habitat == 'Kelp forest inverts and algae')%>%
  dplyr::select(!(process))

kf_invalg_resil <- mpa_dat %>%
  drop_na()%>%
  dplyr::select(c(process, habitat, MPA,  
                  size_km2, 
                  shore_span_km,
                  max_depth_m, depth_range, rel_age, lat, four_region, distance))%>%
  filter(process=="Resilience",
         habitat == "Kelp forest inverts and algae")%>%
  dplyr::select(!(process))

#resistance 
kf_invalg_resist_mod <-glm(distance ~ size_km2 + shore_span_km +
                           depth_range + lat, data = kf_invalg_resist, 
                         family="gaussian", 
                         na.action = na.exclude)
summary(kf_invalg_resist_mod)

#resilience
kf_invalg_resil_mod <- glm(distance ~ 
                             size_km2 * 
                             shore_span_km +
                           depth_range + 
                             lat, data = kf_invalg_resil, 
                         family=quasipoisson(), 
                         na.action = na.exclude)
summary(kf_invalg_resil_mod)

boot::glm.diag.plots(kf_invalg_resil_mod)

###############################################################################
#build model for intertidal

rocky_resist <- mpa_dat %>%
  dplyr::select(c(process, habitat, MPA, size_km2, shore_span_km,
                  max_depth_m, depth_range, rel_age, lat,
                  four_region, distance))%>%
  filter(process=="Resistance",
         habitat == 'Rocky intertidal')%>%
  dplyr::select(!(process))

rocky_resil <- mpa_dat %>%
  drop_na()%>%
  dplyr::select(c(process, habitat, MPA,  size_km2, shore_span_km,
                  max_depth_m, depth_range, rel_age, lat, four_region, distance))%>%
  filter(process=="Resilience",
         habitat == "Rocky intertidal")%>%
  dplyr::select(!(process))

#resistance 
rocky_resist_mod <-glm(distance ~ size_km2 + shore_span_km +
                             depth_range + lat, data = rocky_resist, 
                           family="gaussian", 
                           na.action = na.exclude)
summary(rocky_resist_mod)

#resilience
rocky_resil_mod <- glm(distance ~ size_km2 + shore_span_km +
                             depth_range + lat, data = rocky_resil, 
                           family="gaussian", 
                           na.action = na.exclude)
summary(rocky_resil_mod)


###############################################################################
#create table of output
kf_fish_tab <- sjPlot::tab_model(kf_fish_resist_mod,
                                   kf_fish_resil_mod,
                                   show.aic=F, show.r2=T, 
                                   title="Kelp forest fishes",auto.label=T,
               
                                   #pred.labels = c("intercept","state parks (yes)","sandy beach","estuary","national marine sanctuary (yes)"),
                                   dv.labels = c("Resistance", "Resilience"))

kf_invalg_tab <- sjPlot::tab_model(kf_invalg_resist_mod,
                                 kf_invalg_resil_mod,
                                 show.aic=F, show.r2=T, 
                                 title="Kelp forest inverts and algae",auto.label=T,
                                 
                                 #pred.labels = c("intercept","state parks (yes)","sandy beach","estuary","national marine sanctuary (yes)"),
                                 dv.labels = c("Resistance", "Resilience"))


rocky_tab <- sjPlot::tab_model(rocky_resist_mod,
                                  rocky_resil_mod,
                                   show.aic=F, show.r2=T, 
                                   title="Rocky intertidal",auto.label=T,
                                   
                                   #pred.labels = c("intercept","state parks (yes)","sandy beach","estuary","national marine sanctuary (yes)"),
                                   dv.labels = c("Resistance", "Resilience"))



