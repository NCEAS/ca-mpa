#Joshua G. Smith
#Cori Lopazanski
#December 15, 2022

rm(list=ls())

#required packages
require(dplyr)
require(mvabund)
require(here)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")
mpa_traits <- read.csv("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_clean.csv")

comm_data <- load(file.path(data_path, "comm_data.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))



################################################################################
#mvabund explore  --- community responses by heatwave period 
#CENTRAL COAST ONLY
#testing anova.manyglm using block = year 

#samples must be balanced for repeated measures, so drop sites that were not
#included in all years during the study period 2007-2020
CCFRP_join <- cbind(CCFRP_group_vars, CCFRP_ord_data)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_designation)),
         year = factor(year))%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  dplyr::select(!(siteID))

#note -- can't use standardized data, so must use raw counts for swath and upc
kelp_swath_join <- cbind(kelp_swath_group_vars, kelp_swath_ord_data)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         year = factor(year))%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  dplyr::select(!(siteID))

kelp_upc_join <- cbind(kelp_upc_group_vars, kelp_upc_ord_data)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         year = factor(year))%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  dplyr::select(!(siteID))

kelp_fish_join <- cbind(kelp_fish_group_vars, kelp_fish_ord_data)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         year = factor(year))%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  dplyr::select(!(siteID))

####probmlem with deep reef ... not a single site was surveyed in all years###
deep_reef_join <- cbind(deep_reef_group_vars, deep_reef_ord_data)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         year = factor(year))%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))

rocky_join <- cbind(rocky_group_vars, rocky_ord_data)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_designation)),
         year = factor(year))%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  dplyr::select(!(siteID))


#format as mvabund objects
CCFRP_spp <- mvabund(CCFRP_join[,10:46])
kelp_swath_spp <- mvabund(kelp_swath_join[,10:72])
kelp_upc_spp <- mvabund(kelp_upc_join[,10:51])
kelp_fish_spp <- mvabund(kelp_fish_join[,10:76])
#deep_reef_spp <- mvabund(deep_reef_join[,11:66])
rocky_spp <- mvabund(rocky_join[,10:51])

#check mean to variance relationships
mvabund::meanvar.plot(CCFRP_spp)
mvabund::meanvar.plot(kelp_swath_spp)
mvabund::meanvar.plot(kelp_upc_spp)
mvabund::meanvar.plot(kelp_fish_spp)
#mvabund::meanvar.plot(deep_reef_spp)
mvabund::meanvar.plot(rocky_spp)

#as expected, species with high means have high variances, so GLMs are 
#are good choice here. 

################################################################################
#fit glms to test for interaction between MPA type and heatwave period 

#CCFRP
CCFRP_glm <- manyglm(CCFRP_spp ~ 
                       CCFRP_join$MHW*CCFRP_join$mpa_designation, 
                     family = "poisson",
                     block = CCFRP_join$year)
plot(CCFRP_glm) #check fit

#Kelp swath
kelp_swath_glm <- manyglm(kelp_swath_spp ~ 
                            kelp_swath_join$MHW*kelp_swath_join$mpa_defacto_designation)
plot(kelp_swath_glm) #check fit

#Kelp upc
kelp_upc_glm <- manyglm(kelp_upc_spp ~ kelp_upc_join$MHW)
plot(kelp_upc_glm) #check fit

#Kelp fish
kelp_fish_glm <- manyglm(kelp_fish_spp ~ kelp_fish_join$MHW)
plot(kelp_fish_glm) #check fit

#Deep reef
#deep_reef_glm <- manyglm(deep_reef_spp ~ deep_reef_join$MHW)
#dummary(deep_reef_glm) #check fit

#Rocky intertidal
rocky_glm <- manyglm(rocky_spp ~ rocky_join$MHW)
plot(rocky_glm) #check fit


################################################################################
#examine pairwise response
#NOTE:: slow computation time for this chunk. anova.manyglm() cannot run
#parallel processing. 

#CCFRP
summary(CCFRP_glm)
CCFRP_pairwise_MHW <- anova.manyglm(CCFRP_glm, p.uni="adjusted", 
                   #pairwise.comp = CCFRP_join$MHW,
                   block = CCFRP_join$year)
CCFRP_out <- as.data.frame(CCFRP_aov[["uni.p"]])

#Kelp swath
summary(kelp_swath_glm)
kelp_swath_pairwise_MHW <- anova(kelp_swath_glm, p.uni="adjusted", 
                        #pairwise.comp = kelp_swath_join$MHW,
                        block = kelp_swath_join$year)
kelp_swath_out <- as.data.frame(kelp_swath_pairwise_MHW[["uni.p"]])

#kelp upc
summary(kelp_upc_glm)
kelp_upc_pairwise_MHW <- anova(kelp_upc_glm, p.uni = "adjusted",
                      #pairwise.comp = kelp_upc_join$MHW,
                      block = kelp_upc_join$year)
kelp_upc_out <- as.data.frame(kelp_upc_pairwise_MHW[["uni.p"]])

#kelp fish
summary(kelp_fish_glm)
kelp_fish_pairwise_MHW <- anova(kelp_fish_glm, p.uni="adjusted",
                       #pairwise.comp = kelp_fish_join$MHW,
                       block = kelp_fish_join$year)
kelp_fish_out <- as.data.frame(kelp_fish_pairwise_MHW[["uni.p"]])

#deep reef
#deep_reef_aov <- anova(deep_reef_glm, p.uni="adjusted")
#deep_reef_out <- as.data.frame(deep_reef_aov[["uni.p"]])

#Rocky intertidal
summary(rocky_glm)
rocky_pairwise_MHW <- anova(rocky_glm, p.uni='adjusted',
                   #pairwise.comp = rocky_join$MHW,
                   block = rocky_join$year)
rocky_out <- as.data.frame(rocky_pairwise_MHW[["uni.p"]])


#examine output
CCFRP_sig <- CCFRP_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group="CCFRP")

kelp_swath_sig <- kelp_swath_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group="kelp_swath")

kelp_upc_sig <- kelp_upc_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group="kelp_upc")

kelp_fish_sig <- kelp_fish_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group=="kelp_fish")

#deep_reef_sig <- deep_reef_out %>%
#  pivot_longer(cols=1:ncol(.), names_to="species")%>%
#  drop_na()%>%
#  filter(value <= 0.05) %>%
#  mutate(group=="deep_reef")

rocky_sig <- rocky_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group=="rocky")

aov_out <- rbind(CCFRP_sig, kelp_swath_sig, kelp_upc_sig, kelp_fish_sig,
                # deep_reef_sig, 
                rocky_sig)


View(aov_out)

