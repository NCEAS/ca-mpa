#Joshua G. Smith
#December 19, 2022

rm(list=ls())

#required packages
require(dplyr)
require(mvabund)
require(here)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/statewide_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")
tabledir <- here::here("analyses", "5community_climate_ecology", "tables")
modout <- here::here("analyses", "5community_climate_ecology", "output")


#load habitat data
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")


#load ecological data
comm_data <- load(file.path(data_path, "comm_data_statewide.rda"))
group_vars <- load(file.path(data_path, "group_vars_statewide.rda"))

################################################################################
#merge habitat data and join with ecological data

#step 1 - merge habitat gen and diversity
mpa_traits1 <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name")
mpa_traits2 <- left_join(mpa_traits1, mpa_attributes_hab_div, by="name")

#step 2 - merge habitat and proportion rock
mpa_traits3 <- left_join(mpa_traits2, prop_rock, by="name")

#step 3 - merge habitat and fishing effort
mpa_traits4 <- left_join(mpa_traits3, fishing_effort, by="name")

#step 4 - clean up

mpa_traits <- mpa_traits4 %>%
              #select variables of interest
              dplyr::select(affiliated_mpa, implementation_date, size=size_km2.x,
                            habitat_richness, habitat_diversity=habitat_diversity_sw, 
                            prop_rock, fishing_pressure = annual_avg_lb_sqkm_20002006
                            )%>%
              mutate(affiliated_mpa = recode(affiliated_mpa,
                                 "año nuevo smr" = "ano nuevo smr"))

################################################################################
#join mpa traits 

#Step 1 -- pair traits with comm data
kelp_swath_join <- cbind(kelp_swath_group_vars, kelp_swath_ord_data)
kelp_swath_traits <- left_join(kelp_swath_join, mpa_traits, by="affiliated_mpa")
kelp_swath_traits1 <- kelp_swath_traits%>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")),
         im_year = format(as.Date(implementation_date, format="%Y/%m/%d"),"%Y"),
         age_at_survey = as.numeric(as.character(year))-as.numeric(im_year)) %>%
  dplyr::select(1:9, 73:ncol(.), 10:72)%>%
  select(!(c(implementation_date, im_year)))


kelp_upc_join <- cbind(kelp_upc_group_vars, kelp_upc_ord_data)
kelp_upc_traits <- left_join(kelp_upc_join, mpa_traits, by="affiliated_mpa")
kelp_upc_traits1 <- kelp_upc_traits %>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")),
         im_year = format(as.Date(implementation_date, format="%Y/%m/%d"),"%Y"),
         age_at_survey = as.numeric(as.character(year))-as.numeric(im_year)) %>%
  dplyr::select(1:9, 66:ncol(.), 10:65)%>%
  select(!(c(implementation_date, im_year)))


kelp_fish_join <- cbind(kelp_fish_group_vars, kelp_fish_ord_data)
kelp_fish_traits <- left_join(kelp_fish_join, mpa_traits, by="affiliated_mpa")
kelp_fish_traits1 <- kelp_fish_traits%>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")),
         im_year = format(as.Date(implementation_date, format="%Y/%m/%d"),"%Y"),
         age_at_survey = as.numeric(as.character(year))-as.numeric(im_year)) %>%
  dplyr::select(1:9, 120:ncol(.), 10:119)%>%
  select(!(c(implementation_date, im_year)))


rocky_join <- cbind(rocky_group_vars, rocky_ord_data)
rocky_traits <- left_join(rocky_join, mpa_traits, by="affiliated_mpa")
rocky_traits1 <- rocky_traits%>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")),
         im_year = format(as.Date(implementation_date, format="%Y/%m/%d"),"%Y"),
         age_at_survey = as.numeric(as.character(year))-as.numeric(im_year)) %>%
  dplyr::select(1:9, 55:ncol(.), 10:54)%>%
  select(!(c(implementation_date, im_year)))

################################################################################
#Step 1 --- average by MHW period

kelp_swath_traits2 <- kelp_swath_traits1 %>%
  filter(year>=2007) %>%
  group_by(desig_state, group, region3, region4, affiliated_mpa, mpa_defacto_class,
           mpa_defacto_designation, MHW)%>%
  dplyr::summarise(across(2:70, .fns = mean ))

kelp_upc_traits2 <- kelp_upc_traits1 %>%
  filter(year>=2007) %>%
  group_by(desig_state, group, region3, region4, affiliated_mpa, mpa_defacto_class,
           mpa_defacto_designation, MHW)%>%
  dplyr::summarise(across(2:63, .fns = mean))

kelp_fish_traits2 <- kelp_fish_traits1%>%
  filter(year>=2007) %>%
  group_by(desig_state, group, region3, region4, affiliated_mpa, mpa_defacto_class,
           mpa_defacto_designation, MHW)%>%
  dplyr::summarise(across(2:117, .fns = mean))


rocky_traits2 <- rocky_traits1%>%
  filter(year>=2007) %>%
  group_by(desig_state, group, region3, region4, affiliated_mpa, 
           mpa_designation, MHW)%>%
  dplyr::summarise(across(4:53, .fns = mean))

################################################################################
#Step 2 --- balance samples

#samples must be balanced for repeated measures, so drop sites that were not
#included in all years during the study period 2007-2020

#note -- can't use standardized data, so must use raw counts for swath and upc
kelp_swath_dat <- kelp_swath_traits2%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         MHW = factor(MHW, levels=c('before','during','after')))%>%
  filter(mpa_defacto_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(MHW) %in% MHW))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`))) %>%
  #add this line to test model, but should drop NAs or replace with true 0
  replace(is.na(.), 0) %>%
  #test setting all response vars to integer
  mutate_at(15:ncol(.),ceiling)

kelp_upc_dat <- kelp_upc_traits2%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         MHW = factor(MHW, levels=c('before','during','after')))%>%
  filter(mpa_defacto_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(MHW) %in% MHW))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`))) 

kelp_fish_dat <- kelp_fish_traits2%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         MHW = factor(MHW, levels=c('before','during','after')))%>%
  filter(mpa_defacto_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(MHW) %in% MHW))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`))) 


rocky_dat <- rocky_traits2%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_designation)),
         MHW = factor(MHW, levels=c('before','during','after')))%>%
  filter(mpa_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(MHW) %in% MHW))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`))) 




################################################################################
#Format as mvabund objects

#format as mvabund objects
kelp_swath_mv <- mvabund(kelp_swath_dat[,15:77])
kelp_upc_mv <- mvabund(kelp_upc_dat[,15:70])
kelp_fish_mv <- mvabund(kelp_fish_dat[,15:124])
rocky_mv <- mvabund(rocky_dat[,13:57])

#check mean to variance relationships
mvabund::meanvar.plot(kelp_swath_mv)
mvabund::meanvar.plot(kelp_upc_mv)
mvabund::meanvar.plot(kelp_fish_mv)
mvabund::meanvar.plot(rocky_mv)

#as expected, species with high means have high variances, so GLMs are 
#are good choice here. 

################################################################################
# Step 2 --fit glms to test for drivers

#=====================Kelp swath===================
kelp_swath_compF_glm <- manyglm(kelp_swath_mv ~ 
                            MHW*habitat_diversity +
                            MHW*size+
                            MHW*prop_rock+
                            MHW*age_at_survey+
                            MHW*fishing_pressure,
                          show.coef=F,
                          data=kelp_swath_dat,
                          composition = F)

plot(kelp_swath_compF_glm) #check fit
coefplot(kelp_swath_compF_glm, y.label = TRUE, #which.Xcoef = NULL,
         #which.Ys = NULL, 
         incl.intercept = FALSE, cex.ylab = 0.5, mfrow = NULL) #plot coefficients

# Do MPA traits influence composition across heatwave periods?
# use a randomization test of equality of all species
# note: this is slow
kelp_swath_compF_aov <- anova.manyglm(kelp_swath_compF_glm, 
                         block = kelp_swath_dat$MHW)


# which species changed?  Extract univariate p-values and adjust for multiple testing
kelp_swath_compF_puni <- anova.manyglm(kelp_swath_compF_glm, p.uni="adjusted", 
                                 pairwise.comp = ~kelp_swath_dat$MHW,
                                 block = kelp_swath_dat$MHW,
                                 show.time="all")

####take care of this in post-mod processing
# which species have adjusted p < 0.05?
#kelp_swath_out <- as.data.frame(kelp_swath_pairwise_MHW[["uni.p"]])

#kelp_swath_sig <- kelp_swath_out %>%
#  tibble::rownames_to_column()%>%
#  pivot_longer(cols=2:ncol(.), names_to="species")%>%
#  filter(rowname == "kelp_swath_traits$MHW:kelp_swath_traits$habitat_diversity_sw")%>%
#  drop_na()%>%
#  filter(value <= 0.05) %>%
#  mutate(group="kelp_swath")



#Composition = TRUE
kelp_swath_compT_glm <- manyglm(kelp_swath_mv ~ 
                            MHW*habitat_diversity +
                            MHW*size+
                            MHW*prop_rock+
                            MHW*age_at_survey+
                            MHW*fishing_pressure,
                          show.coef=F,
                          data=kelp_swath_dat,
                          composition = T)

######WARNING: SLOW
kelp_swath_compT_aov <- anova.manyglm(kelp_swath_compT_glm)

save(#kelp_swath_compF_aov, kelp_swath_compF_puni,
     kelp_swath_compT_aov,
     file = file.path(modout, "kelp_swath_compT_mvabund.RData"))


#=====================Kelp fish===================
kelp_fish_compF_glm <- manyglm(kelp_fish_mv ~ 
                           MHW*habitat_diversity +
                           MHW*size+
                           MHW*prop_rock+
                           MHW*age_at_survey+
                           MHW*fishing_pressure,
                         show.coef=F,
                         data=kelp_fish_dat,
                         composition = F)


plot(kelp_fish_compF_glm) #check fit
coefplot(kelp_fish_compF_glm, y.label = TRUE, #which.Xcoef = NULL,
         #which.Ys = NULL, 
         incl.intercept = FALSE, cex.ylab = 0.5, mfrow = NULL) #plot coefficients

# use a randomization test of equality of all species
#  note: this is slow
kelp_fish_compF_aov <- anova.manyglm(kelp_fish_compF_glm, 
                                      block = kelp_fish_dat$MHW)

# which species changed?  Extract univariate p-values and adjust for multiple testing
kelp_fish_compF_puni <- anova.manyglm(kelp_fish_compF_glm, p.uni="adjusted", 
                                       pairwise.comp = ~kelp_fish_dat$MHW,
                                       block = kelp_fish_dat$MHW)


#Composition = TRUE
kelp_fish_compT_glm <- manyglm(kelp_fish_mv ~ 
                            MHW*habitat_diversity +
                            MHW*size+
                            MHW*prop_rock+
                            MHW*age_at_survey+
                            MHW*fishing_pressure,
                          show.coef=F,
                          data=kelp_fish_dat,
                          composition = T)

######WARNING: SLOW
kelp_fish_compT_aov <- anova.manyglm(kelp_fish_compT_glm)


save(kelp_fish_compF_aov, kelp_fish_compF_puni,
     kelp_fish_compT_aov,
     file = file.path(modout, "kelp_fish_mvabund.RData"))



#=====================Kelp upc===================

kelp_upc_compF_glm <- manyglm(kelp_upc_mv ~ 
                           MHW*habitat_diversity +
                           MHW*size+
                           MHW*prop_rock+
                           MHW*age_at_survey+
                           MHW*fishing_pressure,
                         show.coef=F,
                         data=kelp_upc_dat,
                         composition = F)


plot(kelp_upc_compF_glm) #check fit
coefplot(kelp_upc_compF_glm, y.label = TRUE, #which.Xcoef = NULL,
         #which.Ys = NULL, 
         incl.intercept = FALSE, cex.ylab = 0.5, mfrow = NULL) #plot coefficients

# use a randomization test of equality of all species
#  note: this is slow
kelp_upc_compF_aov <- anova.manyglm(kelp_upc_compF_glm, 
                                     block = kelp_upc_dat$MHW)

# which species changed?  Extract univariate p-values and adjust for multiple testing
kelp_upc_compF_puni <- anova.manyglm(kelp_upc_compF_glm, p.uni="adjusted", 
                                      pairwise.comp = ~kelp_upc_dat$MHW,
                                      block = kelp_upc_dat$MHW)

#Composition = TRUE
kelp_upc_compT_glm <- manyglm(kelp_upc_mv ~ 
                            MHW*habitat_diversity +
                            MHW*size+
                            MHW*prop_rock+
                            MHW*age_at_survey+
                            MHW*fishing_pressure,
                          show.coef=F,
                          data=kelp_upc_dat,
                          composition = T)

######WARNING: SLOW
kelp_upc_compT_aov <- anova.manyglm(kelp_upc_compT_glm)


save(kelp_upc_compF_aov, kelp_upc_compF_puni,
     kelp_upc_compT_aov,
     file = file.path(modout, "kelp_upc_mvabund.RData"))




#=====================rocky intertidal===================


rocky_compF_glm <- manyglm(rocky_mv ~ 
                          MHW*habitat_diversity +
                          MHW*size+
                          MHW*prop_rock+
                          MHW*age_at_survey+
                          MHW*fishing_pressure,
                        show.coef=F,
                        data=rocky_dat,
                        composition = F)


plot(rocky_compF_glm) #check fit
coefplot(rocky_compF_glm, y.label = TRUE, #which.Xcoef = NULL,
         #which.Ys = NULL, 
         incl.intercept = FALSE, cex.ylab = 0.5, mfrow = NULL) #plot coefficients

# use a randomization test of equality of all species
#  note: this is slow
rocky_compF_aov <- anova.manyglm(rocky_compF_glm, 
                                    block = rocky_dat$MHW)

# which species changed?  Extract univariate p-values and adjust for multiple testing
rocky_compF_puni <- anova.manyglm(rocky_compF_glm, p.uni="adjusted", 
                                     pairwise.comp = ~rocky_dat$MHW,
                                     block = rocky_dat$MHW)

#Composition = TRUE
rocky_compT_glm <- manyglm(rocky_mv ~ 
                            MHW*habitat_diversity +
                            MHW*size+
                            MHW*prop_rock+
                            MHW*age_at_survey+
                            MHW*fishing_pressure,
                          show.coef=F,
                          data=rocky_dat,
                          composition = T)

######WARNING: SLOW
rocky_compT_aov <- anova.manyglm(rocky_compT_glm)

save(rocky_compF_aov, rocky_compF_puni,
     rocky_compT_aov,
     file = file.path(modout, "rocky_mvabund.RData"))




