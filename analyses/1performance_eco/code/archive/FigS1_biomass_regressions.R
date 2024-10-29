#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, metafor)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) %>% 
  mutate(habitat = ifelse(habitat == "Rocky reef","Shallow reef",habitat))

################################################################################
#prep data

biomass_build1 <- biomass_mod %>% mutate(target_status = ifelse(habitat == "Shallow reef","Targeted",target_status),
                                              affiliated_mpa = str_to_title(affiliated_mpa) %>% 
                                                str_replace(" Smr$", " SMR") %>% 
                                                str_replace(" Smca$", " SMCA")) %>%
  #calcualte mean for each MPA across years where age > 0
  filter(age_at_survey > 0 )


################################################################################
#plot

my_theme <-  theme(axis.text=element_text(size=6, color = "black"),
                   axis.text.y = element_text(color = "black"),
                   axis.title=element_text(size=8, color = "black"),
                   plot.tag=element_text(size= 8, color = "black"), #element_text(size=8),
                   plot.title =element_text(size=7, face="bold", color = "black"),
                   # Gridlines 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.height = unit(1, "lines"), 
                   legend.text = element_text(size = 6, color = "black"),
                   legend.title = element_text(size = 7, color = "black"),
                   #legend.spacing.y = unit(0.75, "cm"),
                   #facets
                   strip.background = element_blank(),
                   strip.text = element_text(size = 6 , hjust = 0, face="plain", color = "black")
)

# set order
habitat_order <- c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef")
state_region_order <- c("North Coast", "North Central Coast", "Central Coast", "South Coast")

biomass_build1$habitat <- factor(biomass_build1$habitat, levels = habitat_order)
biomass_build1$state_region <- factor(biomass_build1$state_region, levels = state_region_order)



my_colors <- scale_color_brewer(palette = "Dark2")
my_fill_colors <- scale_fill_brewer(palette = "Dark2") 


north <- ggplot(biomass_build1 %>% filter(state_region == "North Coast"), aes(x = age_at_survey, y = logRR, color = target_status)) +
  geom_point(alpha = 0.2) +  
  geom_smooth(aes(fill = target_status), method = "lm", se = TRUE) +  
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(#..rr.label.., 
                          ..p.value.label.., sep = "*`,`~")), 
                        parse = TRUE,
                        label.x.npc = "right",
                        vstep = 0.05,
                        size=2) + # sets vertical spacing
  facet_wrap(~habitat, ncol = 4, nrow=1) +
  labs(title = "North Coast",
    x = "", y = "",
    color = "Target status",
    fill = "Target status") +
  theme_bw() +
  my_theme +
  my_colors +
  my_fill_colors
north


north_central <- ggplot(biomass_build1 %>% filter(state_region == "North Central Coast"), aes(x = age_at_survey, y = logRR, color = target_status)) +
  geom_point(alpha = 0.2) +  
  geom_smooth(aes(fill = target_status), method = "lm", se = TRUE) +  
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(#..rr.label.., 
                          ..p.value.label.., sep = "*`,`~")), 
                        parse = TRUE,
                        label.x.npc = "right",
                        vstep = 0.05,
                        size=2) + # sets vertical spacing
  facet_wrap(~habitat, ncol = 4, nrow=1) +
  labs(title = "North Central Coast",
       x = "", y = "",
       color = "Target status",
       fill = "Target status") +
  theme_bw() +
  my_theme +
  my_colors +
  my_fill_colors
north_central

central <- ggplot(biomass_build1 %>% filter(state_region == "Central Coast"), aes(x = age_at_survey, y = logRR, color = target_status)) +
  geom_point(alpha = 0.2) +  
  geom_smooth(aes(fill = target_status), method = "lm", se = TRUE) +  
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(#..rr.label.., 
                          ..p.value.label.., sep = "*`,`~")), 
                        parse = TRUE,
                        label.x.npc = "right",
                        vstep = 0.05,
                        size=2) + # sets vertical spacing
  facet_wrap(~habitat, ncol = 4, nrow=1) +
  labs(title = "Central Coast",
       x = "", y = "",
       color = "Target status",
       fill = "Target status") +
  theme_bw() +
  my_theme +
  my_colors +
  my_fill_colors
central

south <- ggplot(biomass_build1 %>% filter(state_region == "South Coast"), aes(x = age_at_survey, y = logRR, color = target_status)) +
  geom_point(alpha = 0.2) +  
  geom_smooth(aes(fill = target_status), method = "lm", se = TRUE) +  
  ggpmisc::stat_poly_eq(formula = y ~ x, 
                        aes(label = paste(#..rr.label.., 
                          ..p.value.label.., sep = "*`,`~")), 
                        parse = TRUE,
                        label.x.npc = "right",
                        vstep = 0.05,
                        size=2) + # sets vertical spacing
  facet_wrap(~habitat, ncol = 4, nrow=1) +
  labs(title = "South Coast",
       x = "", y = "",
       color = "Target status",
       fill = "Target status") +
  theme_bw() +
  my_theme +
  my_colors +
  my_fill_colors
south


g <- ggpubr::ggarrange(north, north_central, central, south, ncol=1, common.legend = TRUE,
                       legend = "right")

g_annotate <- ggpubr::annotate_figure(g,
                                      bottom = ggpubr::text_grob("MPA age (years)", 
                                                                 hjust = 4, x = 0.97, size = 10,
                                                                 vjust=-1),
                                      left = ggpubr::text_grob("Log response ratio", rot = 90, size = 10, vjust=1))
g_annotate




ggsave(g_annotate, filename=file.path(fig_dir, "Fig4_biomass_trajectories.png"), bg = "white",
       width=8, height=9, units="in", dpi=600) 











