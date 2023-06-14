#Joshua G. Smith
#June 9, 2023

rm(list=ls())

#require packages
require(tidyverse)

#set directories
figdir <-  here::here("analyses", "5community_climate_ecology", "figures")


#load MPA traits
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")
mpa_attributes_cdfw <- read.csv("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_clean.csv")

#step 1 - merge habitat gen and diversity
mpa_traits1 <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name")
mpa_traits2 <- left_join(mpa_traits1, mpa_attributes_hab_div, by="name")

#step 2 - merge habitat and proportion rock
mpa_traits3 <- left_join(mpa_traits2, prop_rock, by="name")

#step 3 - merge habitat and fishing effort
mpa_traits4 <- left_join(mpa_traits3, fishing_effort, by="name")

#step 4 - join with CDFW attributes
mpa_traits_cdfw <- mpa_attributes_cdfw %>%
  dplyr::select(affiliated_mpa = name, min_depth_m,
                max_depth_m) %>%
  mutate(depth_range_m = max_depth_m - min_depth_m)


mpa_traits5 <- left_join(mpa_traits4, mpa_traits_cdfw, by="affiliated_mpa")

#step 4 - clean up

mpa_traits <- mpa_traits5 %>%
  #select variables of interest
  dplyr::select(region = bioregion.x, affiliated_mpa, designation, implementation_date, size=size_km2.x,
                habitat_richness, habitat_diversity=habitat_diversity_sw, 
                prop_rock, fishing_pressure = annual_avg_lb_sqkm_20002006,
                max_depth_m)%>%
  mutate(affiliated_mpa = recode(affiliated_mpa,
                                 "año nuevo smr" = "ano nuevo smr"))%>%
  #assign region based on Figure 5
  mutate(network = case_when(
    affiliated_mpa %in% c("vandenberg smr", 
                          "point sur smr", 
                          "point lobos smr",
                          "point buchon smr",
                          "natural bridges smr",
                          "lovers point - julia platt smr",
                          "big creek smr") ~ "Central coast",
    TRUE ~ "Network")
    ) %>%
  #filter to MPAs in Fig 5
  filter(network == "Central coast" |
           affiliated_mpa == "stewarts point smr" |
           affiliated_mpa == "bodega head smr" |
           affiliated_mpa == "south point smr" |
           affiliated_mpa == "south la jolla smr" |
           affiliated_mpa == "scorpion smr" |
           affiliated_mpa =="santa barbara island smr" |
           affiliated_mpa == "point vicente smca" |
           affiliated_mpa == "point dume smr" |
           affiliated_mpa == "point dume smca" |
           affiliated_mpa =="point conception smr" |
           affiliated_mpa =="matlahuayl smr" |
           affiliated_mpa == "long point smr" |
           affiliated_mpa == "laguna beach smr" |
           affiliated_mpa == "harris point smr" |
           affiliated_mpa =="gull island smr" |
           affiliated_mpa == "farnsworth onshore smca" |
           affiliated_mpa == "campus point smca" |
           affiliated_mpa == "cabrillo smr" |
           affiliated_mpa == "blue cavern onshore smca" |
           affiliated_mpa == "anacapa island smr" |
           affiliated_mpa == "abalone cove smca"
           ) %>%
  mutate(prop_rock = prop_rock*100)%>%
  pivot_longer(cols = c("fishing_pressure",
                        "habitat_diversity",
                        "habitat_richness",
                        "prop_rock",
                        "size",
                        "max_depth_m",
                        #"depth_range_m",
                        ), names_to = "feature", values_to = "value") %>%
  mutate(
    feature = case_when(
      #feature == "fishing_pressure" ~ "Historic annual landings \n(pounds per km² 2000-2006)",
      feature == "fishing_pressure" ~ "Fishing pressure",
      #feature == "habitat_diversity" ~ "Habitat diversity \n(Shannon-Wiener)",
      feature == "habitat_diversity" ~ "Habitat diversity",
      #feature == "habitat_richness" ~ "Habitat richness \n(no. habitats)",
      feature == "habitat_richness" ~ "Habitat richness",
      feature == "prop_rock" ~ "Percent rock",
      feature == "max_depth_m" ~ "Maximum depth",
      #feature == "depth_range_m" ~ "Depth range (m)",
      feature == "size" ~ "MPA size",
      # Add more renaming conditions as needed
      TRUE ~ feature  # Keep the original feature name if no renaming condition matches
    )
  )



################################################################################
# Theme
my_theme <-  theme(axis.text=element_text(size=6,color = "black"),
                   #axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8,color = "black"),
                   plot.tag=element_text(size=8, face = "bold", color = "black"),
                   plot.title =element_text(size=7, face="bold",color = "black"),
                   # Gridlines 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.height = unit(1, "lines"), 
                   legend.text = element_text(size = 6,color = "black"),
                   legend.title = element_text(size = 7,color = "black"),
                   #legend.spacing.y = unit(0.75, "cm"),
                   #facets
                   strip.background = element_blank(),
                   strip.text = element_text(size = 6 ,face="bold", hjust=0,color = "black"),
)


g1 <- ggplot(data = mpa_traits %>% filter(feature == "Habitat diversity")
            , aes(x=network, y = value, fill = feature)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.1, height = 0.3, alpha = 0.2, size=1) +
  facet_wrap(~feature, scales = "free")+
  scale_fill_manual(values = "#1B9E77")+
  # ggsignif::geom_signif(comparisons = list(c("resistant", "transitioned")),
  #                      map_signif_level = TRUE,
  #                     tip_length = c(0.01, 0.01),
  #                    textsize=3)+
  #ylim(0,20)+
  xlab("") +
  ylab("Habitat diversity \n(Shannon-Wiener)")+
  theme_classic()+
  my_theme+
  theme(legend.position = "none")
g1

g2 <- ggplot(data = mpa_traits %>% filter(feature == "Habitat richness")
             , aes(x=network, y = value, fill = feature)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.1, height = 0.3, alpha = 0.2, size=1) +
  facet_wrap(~feature, scales = "free")+
  scale_fill_manual(values = "#D95F02")+
  # ggsignif::geom_signif(comparisons = list(c("resistant", "transitioned")),
  #                      map_signif_level = TRUE,
  #                     tip_length = c(0.01, 0.01),
  #                    textsize=3)+
  #ylim(0,20)+
  xlab("") +
  ylab("Habitat richness \n(no. habitats)")+
  theme_classic()+
  my_theme+
  theme(legend.position = "none")
g2

g3 <- ggplot(data = mpa_traits %>% filter(feature == "Fishing pressure")
             , aes(x=network, y = value, fill = feature)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.1, height = 0.3, alpha = 0.2, size=1) +
  facet_wrap(~feature, scales = "free")+
  scale_fill_manual(values = "#7570B3")+
  # ggsignif::geom_signif(comparisons = list(c("resistant", "transitioned")),
  #                      map_signif_level = TRUE,
  #                     tip_length = c(0.01, 0.01),
  #                    textsize=3)+
  #ylim(0,20)+
  xlab("") +
  ylab("Historic annual landings \n(pounds per km² 2000-2006)")+
  theme_classic()+
  my_theme+
  theme(legend.position = "none")
g3

g4 <- ggplot(data = mpa_traits %>% filter(feature == "Maximum depth")
             , aes(x=network, y = value, fill = feature)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.1, height = 0.3, alpha = 0.2, size=1) +
  facet_wrap(~feature, scales = "free")+
  scale_fill_manual(values = "#E7298A")+
  # ggsignif::geom_signif(comparisons = list(c("resistant", "transitioned")),
  #                      map_signif_level = TRUE,
  #                     tip_length = c(0.01, 0.01),
  #                    textsize=3)+
  #ylim(0,20)+
  xlab("") +
  ylab("Maximum depth (m)")+
  theme_classic()+
  my_theme+
  theme(legend.position = "none")
g4

g5 <- ggplot(data = mpa_traits %>% filter(feature == "MPA size")
             , aes(x=network, y = value, fill = feature)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.1, height = 0.3, alpha = 0.2, size=1) +
  facet_wrap(~feature, scales = "free")+
  scale_fill_manual(values = "#66A61E")+
  # ggsignif::geom_signif(comparisons = list(c("resistant", "transitioned")),
  #                      map_signif_level = TRUE,
  #                     tip_length = c(0.01, 0.01),
  #                    textsize=3)+
  #ylim(0,20)+
  xlab("") +
  ylab("MPA size (km²)")+
  theme_classic()+
  my_theme+
  theme(legend.position = "none")
g5


g6 <- ggplot(data = mpa_traits %>% filter(feature == "Percent rock")
             , aes(x=network, y = value, fill = feature)) +
  geom_boxplot(color = "black") +
  geom_jitter(width = 0.1, height = 0.3, alpha = 0.2, size=1) +
  facet_wrap(~feature, scales = "free")+
  scale_fill_manual(values = "#E6AB02")+
  # ggsignif::geom_signif(comparisons = list(c("resistant", "transitioned")),
  #                      map_signif_level = TRUE,
  #                     tip_length = c(0.01, 0.01),
  #                    textsize=3)+
  #ylim(0,20)+
  xlab("") +
  ylab("Percent rock")+
  theme_classic()+
  my_theme+
  theme(legend.position = "none")
g6

g_merge <- ggpubr::ggarrange(g1, g2, g3, g4, g5, g6, ncol=3, nrow=2) 
g <- annotate_figure(g_merge, bottom = textGrob("Region", vjust=-1.5, hjust=0, gp = gpar(cex = 0.7)))


ggsave(g, filename=file.path(figdir, "FigS2_MPA_features.png"), 
      width=5.5, height=5, units="in", dpi=600, bg="white")


