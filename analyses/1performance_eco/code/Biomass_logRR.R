rm(list = ls())


require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_biomass_logRRs.csv" 

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
        fill=target_status) +
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







