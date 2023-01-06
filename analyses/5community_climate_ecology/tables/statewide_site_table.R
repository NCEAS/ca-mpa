#Joshua G. Smith
#January 5. 2023

rm(list=ls())

require(dplyr)
require(here)

#set directories

datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")
tabdir <- here::here("analyses", "5community_climate_ecology", "tables")

#load data
kelp_data <- read.csv(file.path(datadir, "kelp_swath_mpa_year_statewide.csv"))
intertidal <- read.csv(file.path(datadir, "rocky_statewide.csv"))
CCFRP <- read.csv(file.path(datadir, "CCFRP_statewide.csv"))
deep_reef <- read.csv(file.path(datadir, "deep_reef_statewide.csv"))
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")%>%
                      dplyr::select(affiliated_mpa, latitude_centroid, longitude_centroid)

################################################################################
#Process and select variables of interest

kelp_data1 <- kelp_data %>%
              filter(year >= 2007,
                     mpa_defacto_class == "smr")%>%
              distinct(year, region3, region4, affiliated_mpa, mpa_designation = mpa_defacto_designation)%>%
              mutate(dummy_var = 1,
                     habitat = "Kelp forest")

intertidal1 <- intertidal %>%
  filter(year >= 2007)%>%
  distinct(year, region3, region4, affiliated_mpa, mpa_designation)%>%
  mutate(dummy_var = 1,
         habitat = "Rocky intertidal")%>%
  #drop unpaired reference sites
  filter(!(affiliated_mpa == "none" |
             affiliated_mpa == "anacapa island special closure"))

CCFRP1 <- CCFRP %>%
  filter(year >= 2007,
         mpa_class == "smr")%>%
  distinct(year, region3, region4, affiliated_mpa, mpa_designation)%>%
  mutate(dummy_var = 1,
         habitat = "Rocky reef fishes")%>%
  filter(!(affiliated_mpa == "trinidad smr"))

deep_reef1 <- deep_reef %>%
  filter(year >= 2007,
         mpa_defacto_class == "smr")%>%
  distinct(year, region3, region4, affiliated_mpa, mpa_designation= mpa_defacto_designation)%>%
  mutate(dummy_var = 1,
         habitat = "Deep reef fishes")

site_dat <- rbind(kelp_data1, intertidal1, CCFRP1, deep_reef1)



no_groups_yr <- site_dat %>%
                group_by(year, region4, affiliated_mpa, mpa_designation)%>%
                summarize(total = sum(dummy_var))%>%
                filter(!(mpa_designation == "smca"))%>%
                mutate(affiliated_mpa = ifelse(affiliated_mpa =="ano nuevo smr",
                                               "año nuevo smr",affiliated_mpa))

no_groups_yr1 <- left_join(no_groups_yr, mpa_attributes_gen, by="affiliated_mpa")

################################################################################
#generate plot

no_groups_yr1 %>%
  filter(mpa_designation=="smr")%>%
  mutate(region4 = factor(region4, levels=c("north","central","north islands","south")))%>%
ggplot(aes(x = year, y = reorder(affiliated_mpa, latitude_centroid), fill=total)) +
       geom_tile(color = "white",
                 lwd=0.5,
                 linetype=1)+
       facet_wrap(~region4, scales="free")+
  scale_fill_gradient2(low = "#075AFF",
                       mid = "#FFFFCC",
                       high = "#FF0000") +
  theme_classic()

no_groups_yr1$latitude_centroid














