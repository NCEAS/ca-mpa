#---
#author: "Joshua G. Smith"
#date: '2022-08-10'
#---
  
  rm(list = ls())


#Requird libraries
require(dplyr)
require(Rmisc)
require(ggplot2)
require(ggpmisc)
require(ggtext)
require(facetscales)

#load data

data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_diversity.csv" 

means <- read.csv(file.path(data_path, input_file))
                        

means_wide <- means %>% 
  filter(mpa_defacto_class=='smr')%>%
  pivot_wider(id_cols = c(affiliated_mpa, group, region4, variable, indicator, year),
              names_from = mpa_defacto_designation,
              values_from = c(mean, sd, n)) 


# Drop data when MPA/reference pairs are not both surveyed in a given year
drop_data <- means_wide %>% drop_na(mean_smr)
drop_data.2 <- drop_data %>% drop_na(mean_ref)

drop_data.2$mean_smr <- as.numeric(drop_data.2$mean_smr)
drop_data.2$mean_ref <- as.numeric(drop_data.2$mean_ref)

drop_data.2$logRR <- log10(drop_data.2$mean_smr/drop_data.2$mean_ref)


# Merge the data frame with the original data to keep only the existing MPA/reference pairs and onvert back to long
means.new <- left_join(drop_data.2, means) 

means.final<-select(means.new, -c(mean_smr,mean_ref,sd_smr,sd_ref,n_smr,n_ref))


# Re-order the region levels 
means.final$region4 <- factor(means.final$region4, levels = c("north", "central","north islands","south"))




#clean up
means_final <- means.final %>%
                filter(!(logRR=='-Inf'|
                           logRR=='Inf'|
                           logRR=='NaN')) 
              
means_final$group <- recode_factor(means_final$group, "deep_reef"="deep reef")
means_final$group <- recode_factor(means_final$group, "kelp-fish"="kelp forest")




#plot
diversity_plot_combined <- ggplot(means_final %>%
                         filter(group == 'ccfrp'|group == 'kelp forest'|group == 'deep reef',
                                mpa_defacto_designation == 'smr'
                                ) 
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
  geom_smooth(method='lm', alpha=0.2, level=0.95, formula=y~x, linetype='solid', size=0.5
  ) +
stat_poly_eq(formula = y ~ x, 
             aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
           parse = TRUE,
         label.x.npc = "right",
          vstep = 0.1,
         size=3) + # sets vertical spacing
  scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Diversity[MPA]/Diversity[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "fished status") +
  theme_classic(base_size = 8)+ #+ theme(aspect.ratio = 1/1.5)
  coord_cartesian(
    ylim = c(-1,1)
  )



diversity_targeted <- ggplot(means_final %>%
                               filter(group == 'ccfrp'|group == 'kelp forest'|group == 'deep reef',
                                      mpa_defacto_designation == 'smr',
                                      target_status=="targeted"
                               )
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
  geom_smooth(method='lm', alpha=0.2, level=0.95, formula=y~x, color="black", linetype='solid', size=0.5
  ) +
  stat_poly_eq(formula = y ~ x, 
              aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
             parse = TRUE,
             label.x.npc = "right",
             vstep = 0.1,
             size=3) + # sets vertical spacing
  #scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Diversity[MPA]/Diversity[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "fished status") +
  theme_classic(base_size = 8)+ #+ theme(aspect.ratio = 1/1.5)
  coord_cartesian(
    ylim = c(-0.75,1)
  )





diversity_nontargeted <- ggplot(means_final %>%
                               filter(group == 'ccfrp'|group == 'kelp forest'|group == 'deep reef',
                                      mpa_defacto_designation == 'smr',
                                      target_status=="nontargeted"
                               )
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
  geom_smooth(method='lm',alpha=0.2, level=0.95, formula=y~x, color="black", linetype='solid', size=0.5
  ) +
  stat_poly_eq(formula = y ~ x, 
             aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
             parse = TRUE,
           label.x.npc = "right",
          vstep = 0.1,
          size=3) + # sets vertical spacing
  #scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Diversity[MPA]/Diversity[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "fished status") +
  theme_classic(base_size = 8)+
  coord_cartesian(
    ylim = c(-0.75,0.5)
  )




#setwd("/home/joshsmith/CA_MPA_Project/ca-mpa/")

#ggsave(here::here("analyses","CDFW_modules", "figures","module_1_eco_perform",
#                  "diversity_trajectory_targeted_nontargeted.png"), diversity_plot_combined, height=8, 
#       width = 8, units = "in", 
#   dpi = 600, bg="white")


#ggsave(here::here("analyses","CDFW_modules", "figures","module_1_eco_perform",
#                  "diversity_trajectory_targeted.png"), diversity_targeted, height=8, 
#       width = 8, units = "in", 
#       dpi = 600, bg="white")


#ggsave(here::here("analyses","CDFW_modules", "figures","module_1_eco_perform",
#                  "diversity_trajectory_nontargeted.png"), diversity_nontargeted, height=8, 
#       width = 8, units = "in", 
#       dpi = 600, bg="white")



# all fish diversity ------------------------------------------------------



data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "all_fish_diversity.csv" 

means <- read.csv(file.path(data_path, input_file))


means_wide <- means %>% 
  filter(mpa_defacto_class=='smr')%>%
  pivot_wider(id_cols = c(affiliated_mpa, group, region4, variable, indicator, year),
              names_from = mpa_defacto_designation,
              values_from = c(mean, sd, n)) 



# Drop data when MPA/reference pairs are not both surveyed in a given year
drop_data <- means_wide %>% drop_na(mean_smr)
drop_data.2 <- drop_data %>% drop_na(mean_ref)

drop_data.2$mean_smr <- as.numeric(drop_data.2$mean_smr)
drop_data.2$mean_ref <- as.numeric(drop_data.2$mean_ref)

drop_data.2$logRR <- log10(drop_data.2$mean_smr/drop_data.2$mean_ref)


# Merge the data frame with the original data to keep only the existing MPA/reference pairs and onvert back to long
means.new <- left_join(drop_data.2, means) 

means.final<-select(means.new, -c(mean_smr,mean_ref,sd_smr,sd_ref,n_smr,n_ref))


# Re-order the region levels 
means.final$region4 <- factor(means.final$region4, levels = c("north", "central","north islands","south"))




#clean up
means_final <- means.final %>%
  filter(!(logRR=='-Inf'|
             logRR=='Inf'|
             logRR=='NaN')) 

means_final$group <- recode_factor(means_final$group, "deep-reef"="deep reef")
means_final$group <- recode_factor(means_final$group, "kelp forest-fish"="kelp forest")





diversity_all <- ggplot(means_final %>%
                                  filter(group == 'ccfrp'|group == 'kelp forest'|group == 'deep reef',
                                         mpa_defacto_designation == 'smr'
                                         
                                  )
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
  geom_smooth(method='lm', alpha=0.2, level=0.95, formula=y~x, color="black", linetype='solid', size=0.5
  ) +
  stat_poly_eq(formula = y ~ x, 
              aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
             parse = TRUE,
            label.x.npc = "right",
             vstep = 0.1,
            size=3) + # sets vertical spacing
  #scale_color_manual(values=c("#6187eb", "#ec7560"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(y=expression(bold(Log(Diversity[MPA]/Diversity[REF]))),
       x = expression(bold(year))) +
  labs(colour = 'Difference', linetype = 'Slope') +
  facet_wrap(region4~group, scales="free_x", nrow=4)+
  scale_x_continuous()+
  labs(colour = "fished status") +
  theme_classic(base_size = 8)+
  coord_cartesian(
    ylim = c(-0.3,0.4)
  )



#ggsave(here::here("analyses","CDFW_modules", "figures","module_1_eco_perform",
#                 "diversity_trajectory_all.png"), diversity_all, height=6, 
#       width = 8, units = "in", 
#     dpi = 600, bg="white")








