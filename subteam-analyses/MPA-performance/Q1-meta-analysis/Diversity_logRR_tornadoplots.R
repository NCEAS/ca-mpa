rm(list = ls())


require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)


data_path <- "/home/shares/ca-mpa/data/sync-data"
input_file <- "Ecol_perform_metrics_means_4regions.csv" 

means.data <- read.csv(file.path(data_path, input_file))
means.data$mpa_class <- recode_factor(means.data$mpa_class, none="ref")

##========meta analysis for 2016-2019 -- DIVERSITY =======


region.yr.means<- means.data%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_class == "ref"| mpa_class =="smr" | mpa_class == "none"
         ,year=='2016' | year=='2017'|year=='2018' | year=='2019'
  )%>%
  group_by(group,mlpa_region, mpa_designation,variable,indicator)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n)
  )









#======Log Ratios across all years======
#This code takes the mean across all years and calculates sd across all years (each MPA/yr is a single rep)

region.yr.means<- means.data%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_class == "ref"|mpa_class =="smr")%>%
  group_by(group,mlpa_region, mpa_designation,variable,indicator)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n)
  )


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
  mutate(group = reorder_within(group, RR, mlpa_region))

mu_site$group <- reorder(mu_site$group, mu_site$RR)

mu_site$mlpa_region <- factor(mu_site$mlpa_region, levels = c("north","central","island","south"))
A <- mu_site %>%
  mutate(group = reorder_within(group, RR, mlpa_region))%>%
  ggplot(
    aes(x=group,
        y=RR,
        #fct_reorder(group,RR),
        #y=reorder(RR, RR, function(x)-length(x)),
        fill=mlpa_region)) +
  geom_bar(stat="identity", 
           position = "dodge") + 
  geom_hline(yintercept = 0, 
             linetype = "dashed") +
  geom_errorbar(aes(ymin = RR-HedgeG,ymax = RR+HedgeG), position = position_dodge(0.9), width = 0.2)+
  coord_flip() +
  xlab("") + 
  ylab("") +
  theme_bw() +
  facet_wrap(~mlpa_region, scales="free_y",ncol=1)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_reordered() +
  #scale_y_continuous(expand = c(0,0)) +
  labs(y = "Log response ratio",
       x = NULL,
       title = "Diversity (Shannon-Wiener) for all years",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))



#======Log Ratios for 2016-2019======
#This code takes the mean across all years and calculates sd across all years (each MPA/yr is a single rep)

region.yr.means<- means.data%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_designation == "ref"|mpa_designation =="smr",
         year=="2016" | year=="2017" | year=="2018" | year=="2019")%>%
  group_by(group,mlpa_region, mpa_designation,variable,indicator)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n)
  )


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
  mutate(group = reorder_within(group, RR, mlpa_region))

mu_site$group <- reorder(mu_site$group, mu_site$RR)

mu_site$mlpa_region <- factor(mu_site$mlpa_region, levels = c("north","central","island","south"))
B <- mu_site %>%
  mutate(group = reorder_within(group, RR, mlpa_region))%>%
  ggplot(
    aes(x=group,
        y=RR,
        #fct_reorder(group,RR),
        #y=reorder(RR, RR, function(x)-length(x)),
        fill=mlpa_region)) +
  geom_bar(stat="identity", 
           position = "dodge") + 
  geom_hline(yintercept = 0, 
             linetype = "dashed") +
  geom_errorbar(aes(ymin = RR-HedgeG,ymax = RR+HedgeG), position = position_dodge(0.9), width = 0.2)+
  coord_flip() +
  xlab("") + 
  ylab("") +
  theme_bw() +
  facet_wrap(~mlpa_region, scales="free_y",ncol=1)+
  theme(axis.text.x=element_text(angle=60, hjust=1))+
  scale_x_reordered() +
  #scale_y_continuous(expand = c(0,0)) +
  labs(y = "Log response ratio",
       x = NULL,
       title = "Diversity (Shannon-Wiener) for 2016-2019",
       subtitle = "Log response ratio by monitoring group") +
  theme(legend.position="none", plot.margin = margin(1,2,1,1, "cm"))

ggarrange(A,B)

