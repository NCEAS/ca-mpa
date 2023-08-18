rm(list = ls())


require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_diversity.csv" 
targeted_data <- read.csv(file.path(data_path, input_file))

input_file <- "all_fish_diversity.csv" 
all_data <- read.csv(file.path(data_path, input_file))


##========meta analysis for 2016-2019 -- All fish DIVERSITY =======


region.yr.means<- all_data%>%
  filter(mpa_defacto_class =="smr",
         year=='2016' | year=='2017'|year=='2018' | year=='2019' | year=='2020'
  )%>%
  group_by(group,region4, mpa_defacto_designation)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )%>%
  drop_na(yr.mean_ref)

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
       title = "All fish diversity for 2016-20",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))







##========meta analysis for 2016-2019 -- targeted fish DIVERSITY =======


region.yr.means<- targeted_data%>%
  filter(mpa_defacto_class =="smr",
         year=='2016' | year=='2017'|year=='2018' | year=='2019' | year=='2020',
         target_status == 'targeted'
  )%>%
  group_by(group,region4, mpa_defacto_designation)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )%>%
  drop_na(yr.mean_ref)

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
       title = "Targeted fish diversity for 2016-20",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))












##========meta analysis for 2016-2019 -- nontargeted fish DIVERSITY =======


region.yr.means<- targeted_data%>%
  filter(mpa_defacto_class =="smr",
         year=='2016' | year=='2017'|year=='2018' | year=='2019' | year=='2020',
         target_status == 'nontargeted'
  )%>%
  group_by(group,region4, mpa_defacto_designation)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )%>%
  drop_na(yr.mean_ref)

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
       title = "Nontargeted fish diversity for 2016-20",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))





ggarrange(A,B,C, nrow=1)

