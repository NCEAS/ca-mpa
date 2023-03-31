#author: "Joshua G. Smith"
#date: '2023-03-14'

rm(list=ls())

#required packages
require(vegan)
require(dplyr)
require(tidyr)
require(gridExtra)
require(usedist)
require(ggplot2)
require(metafor)
require(reshape2)
require(ggfittext)
require(pairwiseAdonis)
require(here)
require(forcats)


# #load data --------------------------------------------------------------

#data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
tab_out <- here::here("analyses","5community_climate_ecology","tables")

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))





# Explore distance between ref and smr by year ----------------------------
#Question: did communities inside and outside of MPAs before more distant after 
#the MHW/=?

#create new grouping vars
CCFRP_group_vars2 <- CCFRP_group_vars %>%
  mutate(desig_state_year = paste(mpa_designation,
                                  year))
kelp_fish_group_vars2 <- kelp_fish_group_vars %>%
  mutate(desig_state_year = paste(mpa_defacto_designation,
                                  year))
kelp_invalg_group_vars2 <- kelp_invalg_group_vars %>%
  mutate(desig_state_year = paste(mpa_defacto_designation,
                                  year))
deep_reef_group_vars2 <- deep_reef_group_vars %>%
  mutate(desig_state_year = paste(mpa_defacto_designation,
                                  year))
rocky_group_vars2 <- rocky_group_vars %>%
  mutate(desig_state_year = paste(mpa_designation,
                                  year))


#modify helper function
cenfun3 <- function(group, x) {
  
  group$desig_state_year <- as.factor(group$desig_state_year)
  levels(group$desig_state_year)
  n <- nlevels(group$desig_state_year)
  start <- levels(group$desig_state_year)[1:(n - 1)]
  end <- levels(group$desig_state_year)[2:n]
  map2_dfr(start, end, ~ {
    idx1 <- which(group$desig_state_year == .x)
    idx2 <- which(group$desig_state_year == .y)
    tibble(
      centroid_1 = .x,
      centroid_2 = .y,
      distance = dist_between_centroids(x, idx1, idx2)
    )
  })
} #start and end are grouping vars, x is distmat

CCFRP_mpa <- cenfun3(CCFRP_group_vars2, CCFRP_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="CCFRP")

kelp_invalg_mpa <- cenfun3(kelp_invalg_group_vars2, kelp_invalg_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="kelp inverts and algae")

kelp_fish_mpa <- cenfun3(kelp_fish_group_vars2, kelp_fish_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="kelp fish")

deep_reef_mpa <- cenfun3(deep_reef_group_vars2, deep_reef_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="deep reef")

rocky_mpa <- cenfun3(rocky_group_vars2, rocky_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="rocky intertidal")

all_mpa <- rbind(CCFRP_mpa, kelp_invalg_mpa, 
                 kelp_fish_mpa, deep_reef_mpa, rocky_mpa)

ref_smr_distance <- all_mpa %>%
  #filter(centroid_2>=2010)%>%
  ggplot(aes(x=as.numeric(year_1), y=distance, color=group))+
  geom_point(alpha=0.4, aes(shape=group), size=3)+
  geom_line(alpha=0.4)+
  stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = 0, ymax = 0.6,
           alpha = .15, fill='red')+
  xlab("year")+
  ylab("distance")+
  ggtitle("SMR inside vs outside distance (Euclidean)")+
  theme_minimal()+theme(aspect.ratio = 1/1.5)

#ggsave(here("analyses", "5community_climate_ecology", "figures", "ref_smr_distance_all_years.png"), ref_smr_distance, height=4, width = 8, units = "in", 
#   dpi = 600, bg="white")


cen_distances <- all_mpa %>% 
  mutate(MHW = ifelse(year_2 < 2014, "before",
                      ifelse(year_2 > 2016, "after",
                             "during")))
cen_distances$MHW <- factor(cen_distances$MHW, levels = c("before", "during","after"))
cen_distances$MPA_type <- recode_factor(cen_distances$MPA_type, "ref"='REF', 'smr'="SMR")

cen_distances2 <- cen_distances %>% 
  #filter(year_2>=2010)%>%
  group_by(group, MHW, MPA_type) %>% 
  dplyr::summarise(
    n = n(),
    m = mean(distance),
    stdv = sd(distance),
    se=stdv/sqrt(n),
    ci=se * qt((1-0.05)/2 + .5, n-1)
  )



my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=10),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=9, face="bold"),
                   # Gridlines
                   #panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.text=element_text(size=8),
                   legend.title=element_text(size=10),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   #facets
                   strip.text = element_text(size=6),
                   #margins
                   #plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)






g1 <- cen_distances2 %>%
  mutate(group = recode(group, "CCFRP"= "Rocky reef",
                        "deep reef" = "Deep reef",
                        "kelp fish" = "Kelp forest fish",
                        "kelp inverts and algae" = "Kelp inverts and algae",
                        "rocky intertidal" = "Rocky intertidal"),
         MHW = recode(MHW, "before" = "Before",
                      "during" = "During",
                      "after" = "After"),
         MPA_type = recode(MPA_type, "REF" = "Reference",
                           "SMR" = "MPA"))%>%
  mutate(group = factor(group, levels = c("Rocky intertidal",
                                          "Kelp inverts and algae",
                                          "Kelp forest fish",
                                          "Rocky reef",
                                          "Deep reef")))%>%
  ggplot(aes(x=factor(MHW), y=m, fill=MPA_type
  ))+
  geom_col(width=0.5, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=m-1.96*se, ymax=m+1.96*se),
                # Width of the error bars
                position=position_dodge(width=0.5),width = 0.3, size=.2)+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.43))+
  facet_grid(~group) +
  xlab("Heatwave period")+
  ylab("Distance (Bray-Curtis)")+
  theme_bw()+
  scale_fill_manual(name = "MPA type",
                    values=c('#13A0DD','#EB6977')
  )+
  theme_classic()+
  my_theme

g1
#scale_fill_manual(values=c('#44b89d','#f56969','#4c78b5'))

ggsave(here::here("analyses", "5community_climate_ecology", "figures", "FigureS6.png"), g1, height=6, width = 9, units = "in", 
   dpi = 600, bg="white")



a1 <- aov(cen_distances$distance ~ cen_distances$MHW + cen_distances$group + cen_distances$MPA_type)
posthoc <- TukeyHSD(x=a1, conf.level=0.95)
