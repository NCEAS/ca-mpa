rm(list = ls())


require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)
require(ggpmisc)


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_biomass_logRRs.csv" 

means.data <- read.csv(file.path(data_path, input_file))




##========2016-2020 -- BIOMASS=======


region.yr<- means.data%>%
  filter(mpa_class=='smr',
         year=='2016'| year=='2017'| year=='2018'|year=='2019'|year=='2020'
  )


region.yr.means<- region.yr%>%
  group_by(group,region4, target_status)%>%
  summarize(mu_logRR = mean(logRR),
            sd = sd(logRR),
            n=n())


mu_site <- region.yr.means %>%
  mutate(group = reorder_within(group, mu_logRR, region4))

mu_site$group <- reorder(mu_site$group, mu_site$mu_logRR)

mu_site$region4 <- factor(mu_site$region4, levels = c("north","central","north islands","south"))
A <- mu_site %>%
  mutate(group = reorder_within(group, mu_logRR, region4))%>%
  ggplot(
    aes(x=group,
        y=mu_logRR,
        #fct_reorder(group,RR),
        #y=reorder(RR, RR, function(x)-length(x)),
        fill=target_status)) +
  geom_bar(stat="identity", 
           position = "dodge") + 
  geom_hline(yintercept = 0, 
             linetype = "dashed") +
  #geom_errorbar(aes(ymin = RR-HedgeG,ymax = RR+HedgeG), position = position_dodge(0.9), width = 0.2)+
  coord_flip() +
  xlab("") + 
  ylab("") +
  theme_bw() +
  facet_wrap(~region4, scales="free_y",ncol=1)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_reordered() +
  #scale_y_continuous(expand = c(0,0)) +
  labs(y = "Log response ratio",
       x = NULL,
       title = "Targeted fish biomass 2016-2020",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="right", plot.margin = margin(1,2,1,1, "cm"))
  
  
  

# Biomass trajectory ------------------------------------------------------

biomass_traject <- means.data %>%
                  dplyr::group_by(year, group, region4, mpa_class, target_status)%>%
                  dplyr::summarise(mean_RR = mean(logRR),
                            ssize = n())%>%
                  dplyr::filter(group == 'ccfrp'|group == 'kelp'|group == 'deep_reef',
                         mpa_class == 'smr')%>%
                  dplyr::filter(!(group=='ccfrp'&target_status=='targeted'))
                  
biomass_traject$region4<-factor(biomass_traject$region4,levels=c("north","central","north islands","south"))

means_plot <- means.data %>%
  dplyr::filter(!(group=='ccfrp'&target_status=='nontargeted'))

biomass_plot <- ggplot(means_plot %>%
         dplyr::filter(group == 'ccfrp'|group == 'kelp'|group == 'deep_reef',
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
  #stat_poly_eq(formula = y ~ x, 
  #            aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
  #            parse = TRUE,
  #           label.x.npc = "right",
  #           vstep = 0.1) + # sets vertical spacing
  scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Biomass[MPA]/Biomass[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free", nrow=4)+
  scale_x_continuous()+
  labs(colour = "fished status") +
  theme_classic(base_size = 8) #+ theme(aspect.ratio = 1/1.5)



#ggsave(here::here("analyses", "CDFW_modules", "figures","module_1_eco_perform", "biomass_trajectory.png"), biomass_plot, height=6, width = 8, units = "in", 
#  dpi = 600, bg="white")




