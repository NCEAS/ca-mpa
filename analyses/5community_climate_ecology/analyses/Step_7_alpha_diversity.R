

rm(list=ls())


require(vegan)
require(tidyverse)
require(patchwork)
require(ggpubr)

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")

comm_data <- load(file.path(data_path, "comm_data.rda"))
nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
env_fit_scores <- load(file.path(data_path, "env_fit_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))


################################################################################
#calculate alpha diversity

#CCFRP

ccfrp_richness <- data.frame(S.obs = apply(CCFRP_ord_data[,1:37]>0, 1, sum))
ccfrp_evenness <- diversity(CCFRP_ord_data)/log(specnumber(CCFRP_ord_data))
ccfrp_shannon <- diversity(CCFRP_ord_data, index="shannon")
ccfrp_abund <- rowSums(CCFRP_ord_data[,1:37])

ccfrp_alphadiv <- cbind(CCFRP_group_vars, ccfrp_richness, ccfrp_shannon, ccfrp_evenness, ccfrp_abund)%>%
  mutate(MHW = str_to_sentence(MHW),
         MHW = factor(MHW, levels=c("Before","During","After")))


#kelp fish
kelp_fish_richness <- estimateR(kelp_fish_ord_data)
kelp_fish_evenness <- diversity(kelp_fish_ord_data)/log(specnumber(kelp_fish_ord_data))
kelp_fish_shannon <- diversity(kelp_fish_ord_data, index="shannon")
kelp_fish_abund <- rowSums(kelp_fish_ord_data[,1:67]) 

kelp_fish_alphadiv <- cbind(kelp_fish_group_vars, t(kelp_fish_richness), kelp_fish_shannon,kelp_fish_abund, kelp_fish_evenness)%>%
  mutate(MHW = str_to_sentence(MHW),
         MHW = factor(MHW, levels=c("Before","During","After"))) %>%
  #drop outliers 
  filter(!(kelp_fish_abund > 10000))

#deep reef
deep_reef_richness <- data.frame(S.obs = apply(deep_reef_ord_data[,1:56]>0, 1, sum))
deep_reef_evenness <- diversity(deep_reef_ord_data)/log(specnumber(deep_reef_ord_data))
deep_reef_shannon <- diversity(deep_reef_ord_data, index="shannon")
deep_reef_abund <- rowSums(deep_reef_ord_data[,1:56])

deep_reef_alphadiv <- cbind(deep_reef_group_vars, deep_reef_richness, 
                            deep_reef_shannon,deep_reef_abund, deep_reef_evenness)%>%
  mutate(MHW = str_to_sentence(MHW),
         MHW = factor(MHW, levels=c("Before","During","After")))

#kelp swath
kelp_swath_richness <- estimateR(kelp_swath_ord_data)
kelp_swath_evenness <- diversity(kelp_swath_ord_data)/log(specnumber(kelp_swath_ord_data))
kelp_swath_shannon <- diversity(kelp_swath_ord_data, index="shannon")
kelp_swath_abund <- rowSums(kelp_swath_ord_data[,1:63])

kelp_swath_alphadiv <- cbind(kelp_swath_group_vars, t(kelp_swath_richness), 
                            kelp_swath_shannon, kelp_swath_abund, kelp_swath_evenness)%>%
  mutate(MHW = str_to_sentence(MHW),
         MHW = factor(MHW, levels=c("Before","During","After")))




#rocky
rocky_richness <- data.frame(S.obs = apply(rocky_ord_data[,1:42]>0, 1, sum))
rocky_evenness <- diversity(rocky_ord_data)/log(specnumber(rocky_ord_data))
rocky_shannon <- diversity(rocky_ord_data, index="shannon")
rocky_abund <- rowSums(rocky_ord_data[,1:42])

rocky_alphadiv <- cbind(rocky_group_vars, rocky_richness, 
                             rocky_shannon, rocky_abund, rocky_evenness)%>%
  mutate(MHW = str_to_sentence(MHW),
         MHW = factor(MHW, levels=c("Before","During","After")))



################################################################################
#plot


my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=10),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=7, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   #facets
                   strip.text = element_text(size=6),
                   #margins
                   plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)


color_set <- c("MPA" = "#EB6977","Reference" = "#13A0DD")

#richness plots
ccfrp_alphadiv$mpa_designation <- recode_factor(ccfrp_alphadiv$mpa_designation, "smr"="MPA")
ccfrp_alphadiv$mpa_designation <- recode_factor(ccfrp_alphadiv$mpa_designation, "ref"="Reference")
r1 <-ggplot(ccfrp_alphadiv, aes(x = mpa_designation, y=S.obs))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation))+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Shallow reef",
       tag = "Richness")+
  scale_fill_manual(values = color_set)+
  guides(fill=guide_legend(title="MPA type"))

kelp_fish_alphadiv$mpa_defacto_designation <- recode_factor(kelp_fish_alphadiv$mpa_defacto_designation, "smr"="MPA")
kelp_fish_alphadiv$mpa_defacto_designation <- recode_factor(kelp_fish_alphadiv$mpa_defacto_designation, "ref"="Reference")
r2 <- ggplot(kelp_fish_alphadiv, aes(x = mpa_defacto_designation, y=S.obs))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Kelp forest fishes")+
  scale_fill_manual(values = color_set)

deep_reef_alphadiv$mpa_defacto_designation <- recode_factor(deep_reef_alphadiv$mpa_defacto_designation, "smr"="MPA")
deep_reef_alphadiv$mpa_defacto_designation <- recode_factor(deep_reef_alphadiv$mpa_defacto_designation, "ref"="Reference")
r3 <- ggplot(deep_reef_alphadiv, aes(x = mpa_defacto_designation, y=S.obs))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Deep reef")+
  scale_fill_manual(values = color_set)

kelp_swath_alphadiv$mpa_defacto_designation <- recode_factor(kelp_swath_alphadiv$mpa_defacto_designation, "smr"="MPA")
kelp_swath_alphadiv$mpa_defacto_designation <- recode_factor(kelp_swath_alphadiv$mpa_defacto_designation, "ref"="Reference")
r4 <- ggplot(kelp_swath_alphadiv, aes(x = mpa_defacto_designation, y=S.obs))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="", 
       title="Kelp forest inverts and algae")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)


rocky_alphadiv$mpa_designation <- recode_factor(rocky_alphadiv$mpa_designation, "smr"="MPA")
rocky_alphadiv$mpa_designation <- recode_factor(rocky_alphadiv$mpa_designation, "ref"="Reference")
r5 <- ggplot(rocky_alphadiv, aes(x = mpa_designation, y=S.obs),show.legend = FALSE)+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="Richness", 
       title="Rocky intertidal",
       tag = "Richness")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)



r_plot <- ggarrange(r5, r1, r4, r2, r3, nrow=1, common.legend = TRUE)
r_plot <- annotate_figure(r_plot, left = text_grob("Richness", rot = 90, face = "bold"))
                                
                

#abundance plots
a1 <-ggplot(ccfrp_alphadiv, aes(x = mpa_designation, y=ccfrp_abund))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Shallow reef",
       tag = "Richness")+
  scale_fill_manual(values = color_set)

a2 <- ggplot(kelp_fish_alphadiv, aes(x = mpa_defacto_designation, y=kelp_fish_abund))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Kelp forest fishes")+
  scale_fill_manual(values = color_set)

a3 <- ggplot(deep_reef_alphadiv, aes(x = mpa_defacto_designation, y=deep_reef_abund))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Deep reef")+
  scale_fill_manual(values = color_set)

a4 <- ggplot(kelp_swath_alphadiv, aes(x = mpa_defacto_designation, y=kelp_swath_abund))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="", 
       title="Kelp forest inverts and algae")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)

a5 <- ggplot(rocky_alphadiv, aes(x = mpa_designation, y=rocky_abund))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="", 
       title="Rocky intertidal")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)


a_plot <- ggarrange(a5, a1, a4, a2, a3, nrow=1, common.legend = TRUE)
a_plot <- annotate_figure(a_plot, left = text_grob("Abundance", rot = 90, face = "bold"))


#evenness 
#abundance plots
e1 <-ggplot(ccfrp_alphadiv, aes(x = mpa_designation, y=ccfrp_evenness))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Shallow reef")+
  scale_fill_manual(values = color_set)

e2 <- ggplot(kelp_fish_alphadiv, aes(x = mpa_defacto_designation, y=kelp_fish_evenness))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Kelp forest fishes")+
  scale_fill_manual(values = color_set)

e3 <- ggplot(deep_reef_alphadiv, aes(x = mpa_defacto_designation, y=deep_reef_abund))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Deep reef")+
  scale_fill_manual(values = color_set)

e4 <- ggplot(kelp_swath_alphadiv, aes(x = mpa_defacto_designation, y=kelp_swath_evenness))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="", 
       title="Kelp forest inverts and algae")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)

e5 <- ggplot(rocky_alphadiv, aes(x = mpa_designation, y=rocky_evenness))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="Evenness", 
       title="Rocky intertidal")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)



e_plot <- ggarrange(e5, e1, e4, e2, e3, nrow=1, common.legend = TRUE)
e_plot <- annotate_figure(e_plot, left = text_grob("Evenness", rot = 90, face = "bold"))

#Diversity
s1 <-ggplot(ccfrp_alphadiv, aes(x = mpa_designation, y=ccfrp_shannon))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Shallow reef")+
  scale_fill_manual(values = color_set)

s2 <- ggplot(kelp_fish_alphadiv, aes(x = mpa_defacto_designation, y=kelp_fish_shannon))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="Marine heatwave period", 
       y="", 
       title="Kelp forest fishes")+
  scale_fill_manual(values = color_set)

s3 <- ggplot(deep_reef_alphadiv, aes(x = mpa_defacto_designation, y=deep_reef_shannon))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", 
       title="Deep reef")+
  scale_fill_manual(values = color_set)

s4 <- ggplot(kelp_swath_alphadiv, aes(x = mpa_defacto_designation, y=kelp_swath_shannon))+
  geom_boxplot(aes(x=MHW, fill=mpa_defacto_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="", 
       title="Kelp inverts and algae")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)

s5 <- ggplot(rocky_alphadiv, aes(x = mpa_designation, y=rocky_shannon))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  labs(x="", 
       y="Shannon diversity", 
       title="Rocky intertidal")+
  theme_bw()+my_theme+
  scale_fill_manual(values = color_set)



#s_plot <- ggarrange(s1, s2, s3, s4, s5, nrow=1, common.legend = TRUE)
#s_plot <- annotate_figure(s_plot, left = text_grob("Shannon", rot = 90, face = "bold"))

#boxplots <- ggarrange(r_plot, a_plot, e_plot, s_plot, nrow=4)


boxplots<- 
  ggarrange(r5, r4, r2, r1, r3,
            #a1,a2,a3,a4,a5, 
            e5,e4,e2,e1,e3,
            s5,s4,s2,s1,s3, nrow=3, ncol=5, common.legend=TRUE)

#ggsave(boxplots, filename=file.path(figdir, "FigS8_diversity.png"), 
 #      width=10, height=8.5, units="in", dpi=600, bg="white")





aov_test <- aov(kelp_fish_shannon ~ mpa_defacto_designation*MHW, data=kelp_fish_alphadiv)
TukeyHSD(aov_test)










################################################################################
#check discrepancies -- delete later 

ccfrp_diversity_check <- CCFRP_CPUE 

ccfrp_diversity_check1 <- ccfrp_diversity_check %>%
                          mutate(MHW = ifelse(year < 2014, "before",
                                              ifelse(year > 2015, "after","during")))%>%
                          select(MHW, everything()) 


ccfrp_div <- diversity(ccfrp_diversity_check1[,8:101], index="shannon")

ccfrp_div2 <- cbind(ccfrp_diversity_check1[,1:7], ccfrp_div)%>%
  mutate(MHW = factor(MHW, levels=c("before","during","after")))%>%
  mutate(mpa_designation = factor(mpa_designation, levels = c("smr","ref")))


ccfrp_div3 <- ccfrp_div2 %>%
              group_by(MHW, mpa_designation)%>%
              dplyr::summarize(mean = mean(ccfrp_div),
                               SE = sd(ccfrp_div) / sqrt(length(ccfrp_div)))

raw_plot <-ggplot(ccfrp_div3, aes(x = MHW, y=mean, fill=mpa_designation))+
  geom_bar(position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar( aes(ymin=mean-1.96*SE, ymax=mean+1.96*SE), width=0.4, colour="orange", alpha=0.9, size=1.3,
                 position=position_dodge(width=0.9))+
  #geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+
  labs(x="", 
       y="Shannon", 
       title="Rock reef fish")








ccfrp_diversity_check <- CCFRP_CPUE 

ccfrp_diversity_check1 <- ccfrp_diversity_check %>%
  mutate(MHW = ifelse(year < 2014, "before",
                      ifelse(year > 2015, "after","during")))%>%
  select(MHW, everything()) %>%
  group_by(MHW, group, year, affiliated_mpa, mpa_designation)%>%
  dplyr::summarise(across(8:96, list(mean)))


ccfrp_div <- diversity(ccfrp_diversity_check1[,7:94], index="shannon")

ccfrp_div2 <- cbind(ccfrp_diversity_check1[,1:5], ccfrp_div)%>%
  mutate(MHW = factor(MHW, levels=c("before","during","after")))%>%
  mutate(mpa_designation = factor(mpa_designation, levels = c("smr","ref")),
         ccfrp_div = `...6`)


ccfrp_div3 <- ccfrp_div2 %>%
  group_by(MHW, mpa_designation)%>%
  dplyr::summarize(mean = mean(ccfrp_div),
                   SE = sd(ccfrp_div) / sqrt(length(ccfrp_div)))



raw_plot <-ggplot(ccfrp_div3, aes(x = MHW, y=mean, fill=mpa_designation))+
  geom_bar(position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar( aes(ymin=mean-1.96*SE, ymax=mean+1.96*SE), width=0.4, colour="orange", alpha=0.9, size=1.3,
                 position=position_dodge(width=0.9))+
  #geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+
  labs(x="", 
       y="Shannon", 
       title="Rock reef fish")





drop_total_plot <-ggplot(drop_total, aes(x = MHW, y=mean, fill=mpa_designation))+
  geom_bar(position=position_dodge(width=0.9), stat="identity") +
  geom_errorbar( aes(ymin=mean-1.96*SE, ymax=mean+1.96*SE), width=0.4, colour="orange", alpha=0.9, size=1.3,
                 position=position_dodge(width=0.9))+
  #geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+
  labs(x="", 
       y="Shannon", 
       title="Rock reef fish")

