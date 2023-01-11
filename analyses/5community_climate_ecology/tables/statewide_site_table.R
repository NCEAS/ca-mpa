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
                      dplyr::select(affiliated_mpa, latitude_centroid, longitude_centroid, mpa_class)

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
                group_by(year, region3, affiliated_mpa, mpa_designation)%>%
                summarize(total = sum(dummy_var))%>%
                filter(!(mpa_designation == "smca"))%>%
                mutate(affiliated_mpa = ifelse(affiliated_mpa =="ano nuevo smr",
                                               "año nuevo smr",affiliated_mpa))

no_groups_yr1 <- left_join(no_groups_yr, mpa_attributes_gen, by="affiliated_mpa")

################################################################################
#generate plot
# Theme
theme1 <- theme(axis.text=element_text(size=5),
                axis.text.y=element_text(size=4.5),
                axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=0),
                axis.title=element_blank(),
                #Legend
                legend.text=element_text(size=5),
                legend.title=element_text(size=6),
                legend.key.size = unit(0.4, 'cm'), #change legend key size
                legend.key.height = unit(0.4, 'cm'), #change legend key height
                legend.key.width = unit(0.4, 'cm'), #change legend key width
                legend.spacing.x = unit(0.2, 'cm'),
                #strip.text=element_text(size=7),
                #plot title
                plot.title=element_text(size=10, face = "bold"),
                # Gridlines
                panel.grid.major = element_blank(), 
                panel.grid.minor = element_blank(),
                panel.background = element_blank(), 
                axis.line = element_line(colour = "black"),
                # Legend
                legend.background = element_rect(fill=alpha('blue', 0)))


#filter data
survey_effort <- no_groups_yr1 %>%
  filter(mpa_designation=="smr")%>%
  mutate(region3 = factor(region3, levels=c("north","central","south")))

north <- survey_effort %>%
          filter(region3 == "north")%>%
          ggplot(aes(x = year, y = reorder(affiliated_mpa, latitude_centroid), fill=total)) +
          geom_tile(color = "white",
                 lwd=0.5,
                 linetype=1)+
       scale_fill_gradient2(name="No. monitoring \ngroups",
                            breaks=seq(0:4),
                            labels=c(seq(0:4)),
                          low="white", high="navy", na.value="grey50")+
       scale_x_continuous(breaks = seq(2007,2020,1))+
       geom_vline(xintercept = 2013.5, linetype = "dashed")+
       geom_vline(xintercept = 2016.5, linetype = "dashed")+
       
       ggtitle("North")+
       theme_bw() + theme1 +
       theme(legend.position = "none")+
  coord_fixed()

north

central <- survey_effort %>%
  filter(region3 == "central")%>%
  ggplot(aes(x = year, y = reorder(affiliated_mpa, latitude_centroid), fill=total)) +
  geom_tile(color = "white",
            lwd=0.5,
            linetype=1)+
  scale_fill_gradient2(name="No. monitoring \ngroups",
                       breaks=seq(0:4),
                       labels=c(seq(0:4)),
                       low="white", high="navy", na.value="grey50")+
  scale_x_continuous(breaks = seq(2007,2020,1))+
  geom_vline(xintercept = 2013.5, linetype = "dashed")+
  geom_vline(xintercept = 2016.5, linetype = "dashed")+
  
  ggtitle("Central")+
  theme_bw() + theme1 +
  theme(legend.position = "none")+
  coord_fixed()

central


islands <- survey_effort %>%
  filter(region3 == "north islands")%>%
  ggplot(aes(x = year, y = reorder(affiliated_mpa, latitude_centroid), fill=total)) +
  geom_tile(color = "white",
            lwd=0.5,
            linetype=1)+
  scale_fill_gradient2(name="No. monitoring \ngroups",
                       breaks=seq(0:4),
                       labels=c(seq(0:4)),
                       low="white", high="navy", na.value="grey50")+
  scale_x_continuous(breaks = seq(2007,2020,1))+
  geom_vline(xintercept = 2013.5, linetype = "dashed")+
  geom_vline(xintercept = 2016.5, linetype = "dashed")+
  
  ggtitle("North Channel Islands")+
  theme_bw() + theme1 +
  theme(legend.position = "none")+
  coord_fixed()

islands


south <- survey_effort %>%
  filter(region3 == "south")%>%
  ggplot(aes(x = year, y = reorder(affiliated_mpa, latitude_centroid), fill=total)) +
  geom_tile(color = "white",
            lwd=0.5,
            linetype=1)+
  scale_fill_gradient2(name="No. monitoring \ngroups",
                       breaks=seq(0:4),
                       labels=c(seq(0:4)),
                       low="white", high="navy", na.value="grey50",
                       limits = range(1,4))+
  scale_x_continuous(breaks = seq(2007,2020,1))+
  geom_vline(xintercept = 2013.5, linetype = "dashed")+
  geom_vline(xintercept = 2016.5, linetype = "dashed")+
  
  ggtitle("South")+
  theme_bw() + theme1 +
  theme(plot.margin = margin(t=4,1,1,1,"lines"),
        legend.position = c(0.2,1.2),
        legend.direction = "horizontal")+
  coord_fixed()

south

## Let ggarrange line up the panels
g <- ggpubr::ggarrange(egg::ggarrange(north, central, ncol=1), 
               south, ncol=2)


# Export figure
ggsave(g, filename=file.path(figdir, "FigSx_site_table.png"), 
       width=6, height=7, units="in", dpi=600, bg="white")

















