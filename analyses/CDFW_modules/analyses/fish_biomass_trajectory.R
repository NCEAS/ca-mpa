rm(list = ls())


require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)
require(ggpmisc)


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_biomass_logRRs.csv" 

means.data <- read.csv(file.path(data_path, input_file))


means.data$group <- recode_factor(means.data$group, "deep_reef"="Deep reef")
means.data$group <- recode_factor(means.data$group, "kelp"="Kelp forest")
means.data$group <- recode_factor(means.data$group, "ccfrp"="Rocky reef")

means.data$region4 <- recode_factor(means.data$region4, "north"="North")
means.data$region4 <- recode_factor(means.data$region4, "central"="Central")
means.data$region4 <- recode_factor(means.data$region4, "north islands"="North islands")
means.data$region4 <- recode_factor(means.data$region4, "south"="South")

# Biomass trajectory ------------------------------------------------------


means_plot <- means.data %>%
  mutate(group = factor(group, levels=c("Kelp forest",'Deep reef',"Rocky reef")))%>%
  dplyr::filter(!(group=='Rocky reef'&target_status=='nontargeted'))

means_plot$region4<-factor(means_plot$region4,levels=c("North","Central","North islands","South"))

biomass_plot <- ggplot(means_plot %>%
                         dplyr::filter(group == 'Rocky reef'|group == 'Kelp forest'| group == 'Deep reef',
                                       mpa_class == 'smr')
                       , aes(x=year, y=logRR, color=target_status))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) +
  #geom_ribbon(aes(ymax=eCI_95, ymin=eCI_5), group=1, alpha=0.2, fill = "#BC1605") +
  #geom_point(shape=19, size=3) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 1,
    shape = 19,
    aes(color=target_status))+
  geom_smooth(method='lm', level=0.95, formula=y~x, linetype='solid', size=0.5
  ) +
  stat_poly_eq(formula = y ~ x, 
             aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
             parse = TRUE,
           label.x.npc = "right",
            vstep = 0.1,
           size=3) + # sets vertical spacing
  scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Biomass[MPA]/Biomass[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "Fished status") +
  theme_classic(base_size = 8)+ #+ theme(aspect.ratio = 1/1.5)
  coord_cartesian(
    ylim = c(-1,2.2)
  )





biomass_targeted <- ggplot(means_plot %>%
                       dplyr::filter(group == 'Kelp forest'|group == 'Rocky reef'|group == 'Deep reef',
                                       mpa_class == 'smr',
                                     target_status=='targeted')
                       , aes(x=year, y=logRR))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) +
  #geom_ribbon(aes(ymax=eCI_95, ymin=eCI_5), group=1, alpha=0.2, fill = "#BC1605") +
  #geom_point(shape=19, size=3) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 1,
    shape = 19
    )+
  geom_smooth(method='lm', level=0.95, formula=y~x, color="black",linetype='solid', size=0.5
  ) +
  stat_poly_eq(formula = y ~ x, 
              aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
              parse = TRUE,
           label.x.npc = "right",
            vstep = 0.1,
           size=3) + # sets vertical spacing
  #scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  #scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Biomass[MPA]/Biomass[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "Fished status") +
  theme_classic(base_size = 8)+
  coord_cartesian(
    ylim = c(-1,1.8)
  )





biomass_nontargeted <- ggplot(means_plot %>%
                             dplyr::filter(group == 'Kelp forest'|group == 'Rocky reef'|group == 'Deep reef',
                                           mpa_class == 'smr',
                                           target_status=='nontargeted')
                           , aes(x=year, y=logRR))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) +
  #geom_ribbon(aes(ymax=eCI_95, ymin=eCI_5), group=1, alpha=0.2, fill = "#BC1605") +
  #geom_point(shape=19, size=3) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 1,
    shape = 19
  )+
  geom_smooth(method='lm', level=0.95, formula=y~x, color="black",linetype='solid', size=0.5
  ) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.1,
               size=3) + # sets vertical spacing
  #scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  #scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Biomass[MPA]/Biomass[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "Fished status") +
  theme_classic(base_size = 8) +
  coord_cartesian(
    ylim = c(-0.5,1)
  )






#ggsave(here::here("analyses", "CDFW_modules", "figures","module_1_eco_perform", "biomass_trajectory.png"), biomass_plot, height=8, width = 8, units = "in", 
#  dpi = 600, bg="white")

#ggsave(here::here("analyses", "CDFW_modules", "figures","module_1_eco_perform", "biomass_targeted.png"), biomass_targeted, height=8, width = 8, units = "in", 
# dpi = 600, bg="white")

#ggsave(here::here("analyses", "CDFW_modules", "figures","module_1_eco_perform", "biomass_nontargeted.png"), biomass_nontargeted, height=8, width = 8, units = "in", 
   #   dpi = 600, bg="white")




# Total fish biomass ------------------------------------------------------

total_biom <- means_plot %>%
              group_by(year, group, region3, region4, affiliated_mpa,
                       mpa_class)%>%
              dplyr::summarise(sum_smr = sum(smr),
                        sum_ref = sum(ref))%>%
              mutate(logRR = log(sum_smr/sum_ref))


biomass_overall <- ggplot(total_biom %>%
                                dplyr::filter(group == 'Kelp forest'|group == 'Rocky reef'|group == 'Deep reef',
                                              mpa_class == 'smr')
                              , aes(x=year, y=logRR))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) +
  #geom_ribbon(aes(ymax=eCI_95, ymin=eCI_5), group=1, alpha=0.2, fill = "#BC1605") +
  #geom_point(shape=19, size=3) +
  stat_summary(
    geom = "point",
    fun = "mean",
    size = 1,
    shape = 19
  )+
  geom_smooth(method='lm', level=0.95, formula=y~x, color="black",linetype='solid', size=0.5
  ) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.1,
               size=3) + # sets vertical spacing
  #scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  #scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Biomass[MPA]/Biomass[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "fished status") +
  theme_classic(base_size = 8) +
  coord_cartesian(
    ylim = c(-1,1.8)
  )



ggsave(here::here("analyses", "CDFW_modules", "figures","module_1_eco_perform", "biomass_overall.png"), biomass_overall, height=8, width = 8, units = "in", 
       dpi = 600, bg="white")





