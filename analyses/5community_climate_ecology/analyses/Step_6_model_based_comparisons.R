#Joshua G. Smith
#November 9, 2022

rm(list=ls())

#required packages
require(dplyr)
require(vegan)
require(mvabund)

#set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")
outdir <- 
  
#read data
load(file.path(datadir,"all_groups_mpa_level_means_long.rda"))

#tidy up
CCFRP_join1[CCFRP_join1 == ""] <- NA
kelp_fish_join1[kelp_fish_join1 == ""] <- NA
kelp_combined_join1[kelp_combined_join1 == ""] <- NA
deep_reef_join1[deep_reef_join1 == ""]<- NA

################################################################################
#calculate %abund by affinity for each year


#step 1, calculate grand mean for each species, for each year, and thermal affin


CCFRP_affin_mean_total <- CCFRP_join1 %>% 
               filter(year >= 2010)%>%
               filter(!(is.na(thermal_affinity)))%>%
               group_by(year, species, thermal_affinity)%>%
               dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
               group_by(year, thermal_affinity)%>%
               dplyr::summarize(group_total = sum(mean))

kelp_fish_affin_mean_total <- kelp_fish_join1 %>% 
  filter(year >= 2010)%>%
  filter(!(is.na(thermal_affinity)))%>%
  group_by(year, species, thermal_affinity)%>%
  dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
  group_by(year, thermal_affinity)%>%
  dplyr::summarize(group_total = sum(mean))

kelp_combined_affin_mean_total <- kelp_combined_join1 %>% 
  filter(year >= 2010)%>%
  filter(!(is.na(thermal_affinity)))%>%
  group_by(year, species, thermal_affinity)%>%
  dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
  group_by(year, thermal_affinity)%>%
  dplyr::summarize(group_total = sum(mean))

deep_reef_mean_total <- deep_reef_join1 %>% 
  filter(year >= 2010)%>%
  filter(!(is.na(thermal_affinity)))%>%
  group_by(year, species, thermal_affinity)%>%
  dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
  group_by(year, thermal_affinity)%>%
  dplyr::summarize(group_total = sum(mean))

################################################################################
#plot

my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=8, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   legend.background = element_rect(fill=alpha('blue', 0)))


color_set <- c("cold temperate" = "#80B1D3","cosmopolitan" = "#BEBADA",
               "subtropical" ="#FDB462", 
               "tropical" = "#FB8072",
               "warm temperate"= "#8DD3C7")

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


g1 <- ggplot(CCFRP_affin_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="Year", 
       y="Perc. total abundance", tag="F", title="CCFRP") +
  guides(fill=guide_legend(title="Thermal affinity"))+
  scale_x_discrete(breaks=every_nth(n = 2))


g2 <- ggplot(kelp_fish_affin_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="Year", 
       y="Perc. total abundance", tag="F", title="Kelp forest fish")+
  guides(fill=guide_legend(title="Thermal affinity"))+
  scale_x_discrete(breaks=every_nth(n = 2))

g3 <- ggplot(kelp_combined_affin_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="Year", 
       y="Perc. total abundance", tag="F", title="Kelp forest inverts and algae")+
  guides(fill=guide_legend(title="Thermal affinity"))+
  scale_x_discrete(breaks=every_nth(n = 2))

g4 <- ggplot(deep_reef_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="Year", 
       y="Perc. total abundance", tag="F", title="Deep reef")+
  guides(fill=guide_legend(title="Thermal affinity"))




# Merge plots
g <- ggpubr::ggarrange(g1, g2, g3, g4, nrow=2,ncol=2,common.legend=TRUE,
                       legend="right")
g

# Export
ggsave(g, filename=file.path(figdir, "spp_affinities_perc.png"), 
      width=6.5, height=4.5, units="in", dpi=600, bg="white")





################################################################################
#prep data for mixed model

CCFRP_mod <- CCFRP_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_designation)),
                                    year = as.numeric(year))%>%
                            filter(!(is.na(thermal_affinity)))
kelp_fish_mod <- kelp_fish_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_defacto_designation)),
                                            year = as.numeric(year))%>%
                            filter(!(is.na(thermal_affinity)))
kelp_invalg_mod <- kelp_combined_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_defacto_designation)),
                                            year = as.numeric(year))%>%
                            filter(!(is.na(thermal_affinity)))
deep_reef_mod <- deep_reef_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_defacto_designation)),
                                            year = as.numeric(year))%>%
                            filter(!(is.na(thermal_affinity)))


################################################################################
#build mixed models

#build linear mixed model
library("lme4")
library("lmerTest")

########### CCFRP

#build model
ccfrp_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), data = CCFRP_mod)
ccfrp_sig<- anova(mpa_mixed)

#examine least square means
library(emmeans)
ccfrp_ls <- emmeans(ccfrp_mixed, list(pairwise ~ MHW*thermal_affinity), adjust = "tukey")


########### kelp fish

#build model
kelp_fish_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), data = kelp_fish_mod)
kelp_fish_sig<- anova(kelp_fish_mixed)

#examine least square means
library(emmeans)
kelp_fish_ls <- emmeans(kelp_fish_mixed, list(pairwise ~ MHW*thermal_affinity), adjust = "tukey")


########### kelp inverts and algae

#build model
kelp_invalg_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), data = kelp_invalg_mod)
kelp_invalg_sig<- anova(kelp_invalg_mixed)

#examine least square means
library(emmeans)
kelp_invalg_ls <- emmeans(kelp_invalg_mixed, list(pairwise ~ MHW*thermal_affinity), adjust = "tukey")



########### deep reef

#build model
deep_reef_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), data = deep_reef_mod)
deep_reef_sig<- anova(deep_reef_mixed)

#examine least square means
library(emmeans)
deep_reef_ls <- emmeans(deep_reef_mixed, list(pairwise ~ MHW*thermal_affinity), adjust = "tukey")



################################################################################
#collect ls means



testfun <- summary(deep_reef_ls)

test2 <- as.matrix(testfun[["pairwise differences of MHW, thermal_affinity"]])







