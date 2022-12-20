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

mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")

mpa_traits1 <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name")
mpa_traits <- left_join(mpa_traits1, mpa_attributes_hab_div, by="name")

comm_data <- load(file.path(data_path, "comm_data_statewide.rda"))
group_vars <- load(file.path(data_path, "group_vars_statewide.rda"))


################################################################################
#Step 1 --- balance samples

#samples must be balanced for repeated measures, so drop sites that were not
#included in all years during the study period 2007-2020

#note -- can't use standardized data, so must use raw counts for swath and upc
kelp_swath_join <- cbind(kelp_swath_group_vars, kelp_swath_ord_data)%>%
  #choose year that maximizes n MPAs 
  filter(year>=2010)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         year = factor(year))%>%
  filter(mpa_defacto_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`))) 

kelp_upc_join <- cbind(kelp_upc_group_vars, kelp_upc_ord_data)%>%
  #choose year that maximizes n MPAs 
  filter(year>=2010)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         year = factor(year))%>%
  filter(mpa_defacto_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`)))

kelp_fish_join <- cbind(kelp_fish_group_vars, kelp_fish_ord_data)%>%
  filter(year>=2010)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_defacto_designation)),
         year = factor(year))%>%
  filter(mpa_defacto_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`)))


rocky_join <- cbind(rocky_group_vars, rocky_ord_data)%>%
  mutate(siteID = factor(paste(affiliated_mpa, mpa_designation)),
         year = factor(year))%>%
  filter(mpa_designation=="smr")%>%
  group_by((siteID))%>%
  filter(all(levels(year) %in% year))%>%
  ungroup()%>%
  dplyr::select(!(c(siteID, `(siteID)`)))

################################################################################
#join mpa traits 

#Step 1 -- select traits
select_traits <- mpa_traits %>%
  dplyr::select(affiliated_mpa, size = size_km2.x,
                level_of_protection, historical_protection_overlap,
                21:34, habitat_richness, habitat_diversity_sw)%>%
  mutate(affiliated_mpa = recode(affiliated_mpa,
                                 "año nuevo smr" = "ano nuevo smr"))

#Step 2 -- pair traits with comm data
kelp_swath_traits <- left_join(kelp_swath_join, select_traits, by="affiliated_mpa")%>%
  dplyr::select(1:9, 73:ncol(.), 10:72)%>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")))

kelp_upc_traits <- left_join(kelp_upc_join, select_traits, by="affiliated_mpa")%>%
  dplyr::select(1:9, 66:ncol(.), 10:65)%>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")))

kelp_fish_traits <- left_join(kelp_fish_join, select_traits, by="affiliated_mpa")%>%
  dplyr::select(1:9, 120:ncol(.), 10:119)%>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")))
  
rocky_traits <- left_join(rocky_join, select_traits, by="affiliated_mpa")%>%
  dplyr::select(1:9, 55:ncol(.), 10:54)%>%
  #set reference level
  mutate(MHW = factor(MHW, levels = c("before","during","after")))


################################################################################
#Format as mvabund objects

#format as mvabund objects
kelp_swath_mv <- mvabund(kelp_swath_traits[,29:91])
kelp_upc_mv <- mvabund(kelp_upc_traits[,29:84])
kelp_fish_mv <- mvabund(kelp_fish_traits[,29:138])
rocky_mv <- mvabund(rocky_traits[,29:73])

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
kelp_swath_glm <- manyglm(kelp_swath_mv ~ 
                            #kelp_swath_traits$MHW*kelp_swath_traits$habitat_diversity_sw+
                            #kelp_swath_traits$MHW*kelp_swath_traits$size,
                            kelp_swath_traits$MHW*kelp_swath_traits$size*kelp_swath_traits$habitat_diversity_sw,
                          show.coef=F,
                          composition = F)
plot(kelp_swath_glm) #check fit
coefplot(kelp_swath_glm, y.label = TRUE, #which.Xcoef = NULL,
         #which.Ys = NULL, 
         incl.intercept = FALSE, cex.ylab = 0.5, mfrow = NULL) #plot coefficients

#is the interaction between habitat and MHW significant?
glm_sum <- summary(kelp_swath_glm) #NOTE -- HEAVY COMP TIME

#what species were impacted?
kelp_swath_pairwise_MHW <- anova(kelp_swath_glm, p.uni="adjusted", 
                                 pairwise.comp = ~kelp_swath_traits$MHW,
                                 block = kelp_swath_traits$year)

kelp_swath_out <- as.data.frame(kelp_swath_pairwise_MHW[["uni.p"]])

kelp_swath_sig <- kelp_swath_out %>%
  tibble::rownames_to_column()%>%
  pivot_longer(cols=2:ncol(.), names_to="species")%>%
  filter(rowname == "kelp_swath_traits$MHW:kelp_swath_traits$habitat_diversity_sw")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group="kelp_swath")





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




