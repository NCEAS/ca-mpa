rm(list = ls())


require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_biomass.csv" 
targeted_data <- read.csv(file.path(data_path, input_file))

targeted_data$mpa_class <- tolower(targeted_data$mpa_class)
targeted_data$mpa_designation <- tolower(targeted_data$mpa_designation)

total_biomass <- targeted_data %>%
  group_by(year, group, region4, affiliated_mpa, mpa_class, mpa_designation)%>%
  summarize(total_biomass = sum(sum_biomass))



##========meta analysis for 2016-2019 -- All fish BIOMASS =======


region.yr.means<- total_biomass%>%
  filter(mpa_class =="smr",
         year=='2016' | year=='2017'|year=='2018' | year=='2019' | year=='2020'
  )

region.yr.means$mpa_designation <- recode_factor(region.yr.means$mpa_designation, "smca"="smr") #set defacto SMR

region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_designation)%>%
  dplyr::summarize(yr.mean = mean(total_biomass,na.rm=TRUE), 
                   sd=sd(total_biomass), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n))
  

#means by MPA

mu_site <- region.yr.means %>% 
  mutate(mean_diff = yr.mean_smr-yr.mean_ref,
         se        = sqrt((sd_smr^2/n_ref)+(sd_ref^2/n_ref)),
         HedgeG    = sqrt((sd_smr^2/(n_smr*yr.mean_smr))+(sd_ref^2/(n_ref*yr.mean_ref))),
         t_score   = mean_diff/se,
         df        = n_smr + n_ref - 2,
         p_value   = pt(t_score, df, lower.tail = FALSE),
         sig       = if_else(p_value < 0.05, 1, 0),
         cohens_d  = abs(mean_diff)/(sqrt(((sd_smr^2*(n_smr-1))+(sd_ref^2*(n_ref-1)))/df)),
         RR        = log(yr.mean_smr/yr.mean_ref),
         n_total   = n_smr + n_ref
         #lower.CI  = log((RR - (qt(0.975, df)*n_total/sqrt(n_total))+1)+1),
         #upper.CI  = log((RR + (qt(0.975, df)*n_total/sqrt(n_total)))+1)
  )

mu_site <- mu_site %>%
  mutate(group = reorder_within(group, RR, region4))

mu_site$group <- reorder(mu_site$group, mu_site$RR)

mu_site$region4 <- factor(mu_site$region4, levels = c("north","central","north islands","south"))

mu_site <- mu_site %>%
  drop_na(region4)

A <- mu_site %>%
  mutate(group = reorder_within(group, RR, region4))%>%
  ggplot(
    aes(x=group,
        y=RR,
        #fct_reorder(group,RR),
        #y=reorder(RR, RR, function(x)-length(x)),
        fill=region4)) +
  geom_bar(stat="identity", 
           position = "dodge") + 
  geom_hline(yintercept = 0, 
             linetype = "dashed") +
  geom_errorbar(aes(ymin = RR-HedgeG,ymax = RR+HedgeG), position = position_dodge(0.9), width = 0.2)+
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
       title = "All fish biomass for 2016-20",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))







##========meta analysis for 2016-2019 -- targeted fish BIOMASS =======


region.yr.means<- targeted_data%>%
  filter(mpa_class =="smr",
         year=='2016' | year=='2017'|year=='2018' | year=='2019' | year=='2020',
         target_status == 'targeted'
  )

region.yr.means$mpa_designation <- recode_factor(region.yr.means$mpa_designation, "smca"="smr") #set defacto SMR

region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_designation)%>%
  dplyr::summarize(yr.mean = mean(sum_biomass,na.rm=TRUE), 
                   sd=sd(sum_biomass), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n))


#means by MPA

mu_site <- region.yr.means %>% 
  mutate(mean_diff = yr.mean_smr-yr.mean_ref,
         se        = sqrt((sd_smr^2/n_ref)+(sd_ref^2/n_ref)),
         HedgeG    = sqrt((sd_smr^2/(n_smr*yr.mean_smr))+(sd_ref^2/(n_ref*yr.mean_ref))),
         t_score   = mean_diff/se,
         df        = n_smr + n_ref - 2,
         p_value   = pt(t_score, df, lower.tail = FALSE),
         sig       = if_else(p_value < 0.05, 1, 0),
         cohens_d  = abs(mean_diff)/(sqrt(((sd_smr^2*(n_smr-1))+(sd_ref^2*(n_ref-1)))/df)),
         RR        = log(yr.mean_smr/yr.mean_ref),
         n_total   = n_smr + n_ref
         #lower.CI  = log((RR - (qt(0.975, df)*n_total/sqrt(n_total))+1)+1),
         #upper.CI  = log((RR + (qt(0.975, df)*n_total/sqrt(n_total)))+1)
  )

mu_site <- mu_site %>%
  mutate(group = reorder_within(group, RR, region4))

mu_site$group <- reorder(mu_site$group, mu_site$RR)

mu_site$region4 <- factor(mu_site$region4, levels = c("north","central","north islands","south"))

mu_site <- mu_site %>%
  drop_na(region4)

B <- mu_site %>%
  mutate(group = reorder_within(group, RR, region4))%>%
  ggplot(
    aes(x=group,
        y=RR,
        #fct_reorder(group,RR),
        #y=reorder(RR, RR, function(x)-length(x)),
        fill=region4)) +
  geom_bar(stat="identity", 
           position = "dodge") + 
  geom_hline(yintercept = 0, 
             linetype = "dashed") +
  geom_errorbar(aes(ymin = RR-HedgeG,ymax = RR+HedgeG), position = position_dodge(0.9), width = 0.2)+
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
       title = "Targeted fish biomass for 2016-20",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))












##========meta analysis for 2016-2019 -- nontargeted fish biomass =======


region.yr.means<- targeted_data%>%
  filter(mpa_class =="smr",
         year=='2016' | year=='2017'|year=='2018' | year=='2019' | year=='2020',
         target_status == 'nontargeted'
  )

region.yr.means$mpa_designation <- recode_factor(region.yr.means$mpa_designation, "smca"="smr") #set defacto SMR

region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_designation)%>%
  dplyr::summarize(yr.mean = mean(sum_biomass,na.rm=TRUE), 
                   sd=sd(sum_biomass), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n))

#means by MPA

mu_site <- region.yr.means %>% 
  mutate(mean_diff = yr.mean_smr-yr.mean_ref,
         se        = sqrt((sd_smr^2/n_ref)+(sd_ref^2/n_ref)),
         HedgeG    = sqrt((sd_smr^2/(n_smr*yr.mean_smr))+(sd_ref^2/(n_ref*yr.mean_ref))),
         t_score   = mean_diff/se,
         df        = n_smr + n_ref - 2,
         p_value   = pt(t_score, df, lower.tail = FALSE),
         sig       = if_else(p_value < 0.05, 1, 0),
         cohens_d  = abs(mean_diff)/(sqrt(((sd_smr^2*(n_smr-1))+(sd_ref^2*(n_ref-1)))/df)),
         RR        = log(yr.mean_smr/yr.mean_ref),
         n_total   = n_smr + n_ref
         #lower.CI  = log((RR - (qt(0.975, df)*n_total/sqrt(n_total))+1)+1),
         #upper.CI  = log((RR + (qt(0.975, df)*n_total/sqrt(n_total)))+1)
  )

mu_site <- mu_site %>%
  mutate(group = reorder_within(group, RR, region4))

mu_site$group <- reorder(mu_site$group, mu_site$RR)

mu_site$region4 <- factor(mu_site$region4, levels = c("north","central","north islands","south"))

mu_site <- mu_site %>%
  drop_na(region4)

C <- mu_site %>%
  mutate(group = reorder_within(group, RR, region4))%>%
  ggplot(
    aes(x=group,
        y=RR,
        #fct_reorder(group,RR),
        #y=reorder(RR, RR, function(x)-length(x)),
        fill=region4)) +
  geom_bar(stat="identity", 
           position = "dodge") + 
  geom_hline(yintercept = 0, 
             linetype = "dashed") +
  geom_errorbar(aes(ymin = RR-HedgeG,ymax = RR+HedgeG), position = position_dodge(0.9), width = 0.2)+
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
       title = "Nontargeted fish biomass for 2016-20",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))





ggarrange(A,B,C, nrow=1)

