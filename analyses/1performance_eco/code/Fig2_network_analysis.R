#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, metafor)

#set directories
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) 


################################################################################
#find the latest year for each mpa

filtered_data <- biomass_mod %>%
  #filter(target_status == "Targeted")%>%
  group_by(habitat, mpa, target_status) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  # left_join(n_habitats, by = c("affiliated_mpa","target_status"))%>%
  mutate(#state_region = factor(str_replace(state_region, "Coast$", "\nCoast"), levels = c("North \nCoast",
          #                                                                                "North Central \nCoast", 
           #                                                                               "Central \nCoast", 
            #                                                                              "South \nCoast")),
         target_status = factor(target_status, levels = c("Targeted","Nontargeted")))

unique(filtered_data$mpa)

################################################################################
# Calculate the pooled effects for each habitat, region, and target_status
habitat_region <- filtered_data %>%
  group_by(habitat, state_region, mpa_defacto_class, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() 


#save results to .rdata to generate summary table
#saveRDS(habitat_region, file = file.path(dat_path, "habitat_region_meta_results2.Rds"))

################################################################################
# Calculate the pooled effects for each habitat and target status
habitat <- filtered_data %>%
  group_by(habitat, mpa_defacto_class, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() %>%
  mutate(target_status = factor(target_status, levels = c("Targeted","Nontargeted")),
         #assign dummy state region for plotting
         state_region = "Pooled" 
         )

#save results to .rdata to generate summary table
#saveRDS(habitat, file = file.path(dat_path, "habitat_target_meta_results2.Rds"))

################################################################################
# Calculate the pooled effects for each region across habitat
region <- filtered_data %>%
  group_by(state_region, mpa_defacto_class, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() %>%
  mutate(target_status = factor(target_status, levels = c("Targeted","Nontargeted")),
         #create dummy habitat
         habitat = "Region"
  )

#save results to .rdata to generate summary table
#saveRDS(region, file = file.path(dat_path, "region_meta_results2.Rds"))

################################################################################
# Calculate the pooled effect for entire state
state <- filtered_data %>%
  group_by(mpa_defacto_class, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() %>%
  mutate(target_status = factor(target_status, levels = c("Targeted","Nontargeted")),
         #create dummy state region
         state_region = "Network level",
         #create dummy habitat
         habitat = "Network"
  )

 #save results to .rdata to generate summary table
#saveRDS(state, file = file.path(dat_path, "state_meta_results2.Rds"))

################################################################################
#join everything

meta_results <- rbind(habitat_region, habitat, region, state) %>%
  mutate(
    significance = ifelse(ci.lb > 0 | ci.ub < 0,"*",NA),
    n_mpas = ifelse(habitat == "Region" | habitat == "Network",NA,n_mpas),
    target_status = factor(str_replace(target_status, "Nontargeted", "Non-targeted"),
                           levels = c("Targeted", "Non-targeted"))) %>%
  #remove 'Coast'
  mutate(state_region = str_trim(str_replace_all(state_region, "Coast", "")))
  

####note that n_MPAS is NOT the number of distinct mpas. It is the number of 
#unique habitat-mpa combinations ('study' rows) included 


################################################################################
#plot

meta_results$mpa_defacto_class <- factor(meta_results$mpa_defacto_class,
                                         levels = c("smr", "smca"),
                                         labels = c("No-Take", "Partial-Take"))
meta_results$habitat <- factor(meta_results$habitat, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef", "Region", "Network"))
meta_results$state_region <- factor(meta_results$state_region, levels = c("Network level","Pooled","South", "Central", "North Central","North"))
meta_results$target_status <- factor(meta_results$target_status, levels = c("Targeted","Non-targeted"))  # Reversed order



# labels
#state_labels <- c(expression(italic("Pooled")), "South Coast", "Central Coast", "North Central Coast", "North Coast")


habitat <- meta_results %>% filter(!(habitat %in% c("Region","Network"))) %>%
  mutate(mpa_defacto_class = mpa_defacto_class %>%
           str_replace_all("-", " ") %>%  
           str_to_lower() %>%          
           str_to_sentence())               
                    
  
region <- meta_results %>% filter(habitat %in% c('Region','Network')) %>% filter(!(state_region == "Network level")) %>%
  mutate(mpa_defacto_class = mpa_defacto_class %>%
           str_replace_all("-", " ") %>%  
           str_to_lower() %>%          
           str_to_sentence())   
  
network <- meta_results %>% filter(habitat %in% c('Region','Network')) %>% filter(state_region == "Network level") %>%
  mutate(mpa_defacto_class = mpa_defacto_class %>%
           str_replace_all("-", " ") %>%  
           str_to_lower() %>%          
           str_to_sentence())   


# Theme
my_theme <-  theme(axis.text=element_text(size=9, color = "black"),
                   axis.text.y = element_text(size = 9, color = "black"),
                   axis.title=element_text(size=11, color = "black"),
                   plot.tag=element_text(size= 10, color = "black"), #element_text(size=8),
                   plot.title =element_text(size=9, face="bold", color = "black"),
                   # Gridlines 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.height = unit(1, "lines"), 
                   legend.text = element_text(size = 8, color = "black"),
                   legend.title = element_text(size = 9, color = "black"),
                   #legend.spacing.y = unit(0.75, "cm"),
                   #facets
                   strip.background = element_blank(),
                   strip.text = element_text(size = 7 , face="bold", color = "black")
)


breaks <- c(10, 20, 30)
labels <- c("1-10", "11-20", "21-35")

g1 <- ggplot(habitat %>% 
               #truncate error bar to ease visualization
               mutate(ci.ub = ifelse(ci.ub > 3, 3, ci.ub)),
             aes(x = estimate, y = state_region, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  #add points for mpas with target_status as shape and color
  geom_point(aes(size = n_mpas, shape = target_status),  # Shape for target_status, size for n_mpas
             position = position_dodge(width = 0.7)
  ) +
  #add points for pooled effects
  geom_point(data = subset(habitat, habitat == 'Pooled effect size'), 
             shape = 18, 
             size = 2,
             position = position_dodge(width = 0.7),
             show.legend = FALSE
  ) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  geom_hline(yintercept = which(levels(meta_results$state_region) == "Pooled") - 0.5, 
             linetype = "solid", color = "black", size = 0.2) +  
  geom_text(aes(label = significance, 
                x = ifelse(estimate > 0, ci.ub + 0.2, ci.lb - 0.2)), 
            position = position_dodge(width = 0.7),
            vjust=0.7,
            size = 4, show.legend = FALSE) +
  facet_grid(habitat~mpa_defacto_class, scales = "fixed") +  
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("Targeted" = "#007F00", "Non-targeted" = "#663399"),
                     name = "Target status") +  
  scale_shape_manual(values = c("Non-targeted" = 0, "Targeted" = 15), 
                     name = "Target status") +  # Open square for Non-targeted, solid square for Targeted
  scale_size_continuous(name = "No. MPAs", breaks = breaks, labels = labels,
                        range = c(1, 3), guide = guide_legend(override.aes = list(shape = 15))) +  # Square shape in size legend
  scale_x_continuous(limits= c(-3,3)) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(x= "Effect size \n(log response ratio)",
       title = "Ecosystem performance",
       tag = "c") +
  theme_bw() + my_theme + theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))
#g1




g2 <- ggplot(region %>% 
               #truncate error bar to ease visualization
               mutate(ci.ub = ifelse(ci.ub > 3, 3, ci.ub)),
             aes(x = estimate, y = state_region, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  #add points for mpas --- dummy legend
  geom_point(aes(size = n_mpas), 
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 15, 
             position = position_dodge(width = 0.7)
  ) +
  #add points for pooled effects
  geom_point(data = subset(region, habitat == 'Region'), 
             shape = ifelse(region$target_status == "Targeted", 18, 23), 
             #shape = 18, 
             size = ifelse(region$target_status == "Targeted", 3, 2), 
             position = position_dodge(width = 0.7),
             show.legend=FALSE
  ) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  #geom_hline(yintercept = which(levels(meta_results$state_region) == "South Coast") - 0.5, 
   #          linetype = "solid", color = "black", size = 0.2) +  
  # geom_text(aes(label = significance), vjust = -0.2, size = 4, show.legend = FALSE) + 
  geom_text(aes(label = significance, 
                x = ifelse(estimate > 0, ci.ub + 0.2, ci.lb - 0.2)), 
            position = position_dodge(width = 0.7),
            vjust=0.7,
            size = 4, show.legend = FALSE)+
  facet_grid(habitat~mpa_defacto_class, scales = "fixed") +  
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#007F00","#663399"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", range = c(1, 3)) +
  scale_x_continuous(limits= c(-3,3))+
  #guides(color = guide_legend(override.aes = list(shape = c(15, 15))),  # Set the shape to 15 (square) for color legend
  #      size = guide_legend(override.aes = list(shape = c(15, 15)))) +  # Set the shape to 15 (square) for size legend
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Regional performance",
       tag = "b")+
  theme_bw() + my_theme + theme(plot.margin = ggplot2::margin(-0.2,0,0,0,"cm"))

#g2


g3 <- ggplot(network %>% 
               #truncate error bar to ease visualization
               mutate(ci.ub = ifelse(ci.ub > 3, 3, ci.ub)),
             aes(x = estimate, y = state_region, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  #add points for mpas
  geom_point(aes(size = n_mpas), 
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 15, 
             position = position_dodge(width = 0.7)
  ) +
  #add points for pooled effects
  geom_point(data = subset(network, habitat == 'Network'), 
             shape = ifelse(network$target_status == "Targeted", 18, 23), 
             #shape = 18, 
             size = ifelse(network$target_status == "Targeted", 3, 2), 
             position = position_dodge(width = 0.7),
             show.legend=FALSE
  ) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  # geom_text(aes(label = significance), vjust = -0.2, size = 4, show.legend = FALSE) + 
  geom_text(aes(label = significance, 
                x = ifelse(estimate > 0, ci.ub + 0.2, ci.lb - 0.2)), 
            position = position_dodge(width = 0.7),
            vjust=0.7,
            size = 4, show.legend = FALSE)+
  facet_grid(habitat~mpa_defacto_class, scales = "fixed") +  
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#007F00","#663399"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", range = c(1, 3)) +
  scale_x_continuous(limits= c(-3,3))+
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA))+
  labs(title = "Network performance",
       tag = "a")+
  theme_bw() + my_theme + theme(plot.margin = ggplot2::margin(0,0,0,0,"cm"))

#g3


#plot
g <- ggpubr::ggarrange(g3, g2, g1, heights=c(0.15, 0.2,0.75), ncol=1, 
                  legend = "none", align = "v") 

#legend
legend_only <- cowplot::get_legend(g1) 

g_final <- ggpubr::ggarrange(g, legend_only, widths = c(0.8,0.2), ncol=2)
g_final

#ggsave(g_final, filename=file.path(fig_dir, "Fig2_network_forestplot.png"), bg = "white",
 #      width=6.5, height=10, units="in", dpi=600) 








###############################################################################
#Ecosystem pooled results only

# Theme
my_theme <-  theme(axis.text=element_text(size=10, color = "black"),
                   axis.text.y = element_text(size = 10, color = "black"),
                   axis.title=element_text(size=12, color = "black"),
                   plot.tag=element_text(size= 10, color = "black"), #element_text(size=8),
                   plot.title =element_text(size=10, face="bold", color = "black"),
                   # Gridlines 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.height = unit(1, "lines"), 
                   legend.text = element_text(size = 10, color = "black"),
                   legend.title = element_text(size = 10, color = "black"),
                   #legend.spacing.y = unit(0.75, "cm"),
                   #facets
                   strip.background = element_blank(),
                   strip.text = element_text(size = 9 , face="bold", color = "black")
)


# Define breaks and labels for the scale
breaks <- c(10, 20, 30)
labels <- c("1-10", "11-20", "21-35")

g1 <- ggplot(habitat %>% 
               filter(mpa_defacto_class == "No-Take",
                      state_region == "Pooled")%>%
               mutate(habitat = factor(habitat, levels = c("Deep reef","Shallow reef",
                                                           "Kelp forest","Surf zone")))%>%
               #truncate error bar to ease visualization
               mutate(ci.ub = ifelse(ci.ub > 3, 3, ci.ub)),
             aes(x = estimate, y = habitat, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  #add points for mpas
  geom_point(aes(size = n_mpas), 
             shape = 15, 
             position = position_dodge(width = 0.7)
  ) +
  #add points for pooled effects
  geom_point(data = subset(habitat, habitat == 'Pooled effect size'), 
             shape = 18, 
             size = 2,
             position = position_dodge(width = 0.7),
             show.legend = FALSE
  ) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  geom_text(aes(label = significance, 
                x = ifelse(estimate > 0, ci.ub + 0.2, ci.lb - 0.2)), 
            position = position_dodge(width = 0.7),
            vjust=0.7,
            size = 4, show.legend = FALSE)+
  #facet_grid(habitat~mpa_defacto_class, scales = "fixed") +  
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#007F00","#663399"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", breaks = breaks, labels = labels,
                        range = c(1, 3)) +
  scale_x_continuous(limits= c(-1,1.5))+
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(x= "",
       title = "Ecosystem performance",
       tag = "C")+
  theme_bw() + my_theme + theme(plot.margin = ggplot2::margin(0, 0, 0, 0, "cm"))

g1



g2 <- ggplot(region %>% 
               filter(mpa_defacto_class == "No-Take")%>%
               #truncate error bar to ease visualization
               mutate(ci.ub = ifelse(ci.ub > 3, 3, ci.ub)),
             aes(x = estimate, y = state_region, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  #add points for mpas
  geom_point(aes(size = n_mpas), 
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 15, 
             position = position_dodge(width = 0.7)
  ) +
  #add points for pooled effects
  geom_point(
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 18, 
             size = 4,
             position = position_dodge(width = 0.7),
             show.legend=FALSE
  ) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  #geom_hline(yintercept = which(levels(meta_results$state_region) == "South Coast") - 0.5, 
  #          linetype = "solid", color = "black", size = 0.2) +  
  # geom_text(aes(label = significance), vjust = -0.2, size = 4, show.legend = FALSE) + 
  geom_text(aes(label = significance, 
                x = ifelse(estimate > 0, ci.ub + 0.2, ci.lb - 0.2)), 
            position = position_dodge(width = 0.7),
            vjust=0.7,
            size = 4, show.legend = FALSE)+
  xlab("") +
  ylab("") +
  scale_color_manual(values = c("#007F00","#663399"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", range = c(1, 3)) +
  scale_x_continuous(limits= c(-1,1.5))+
  #guides(color = guide_legend(override.aes = list(shape = c(15, 15))),  # Set the shape to 15 (square) for color legend
  #      size = guide_legend(override.aes = list(shape = c(15, 15)))) +  # Set the shape to 15 (square) for size legend
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA)) +
  labs(title = "Regional performance",
       tag = "B")+
  theme_bw() + my_theme + theme(plot.margin = ggplot2::margin(-0.2,0,0,0,"cm"))

g2


g3 <- ggplot(network %>% 
               filter(mpa_defacto_class == "No-Take")%>%
               #truncate error bar to ease visualization
               mutate(ci.ub = ifelse(ci.ub > 3, 3, ci.ub)),
             aes(x = estimate, y = state_region, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  #add points for mpas
  geom_point(aes(size = n_mpas), 
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 15, 
             position = position_dodge(width = 0.7)
  ) +
  #add points for pooled effects
  geom_point(
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 18, 
             size = 4,
             position = position_dodge(width = 0.7),
             show.legend=FALSE
  ) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  # geom_text(aes(label = significance), vjust = -0.2, size = 4, show.legend = FALSE) + 
  geom_text(aes(label = significance, 
                x = ifelse(estimate > 0, ci.ub + 0.2, ci.lb - 0.2)), 
            position = position_dodge(width = 0.7),
            vjust=0.7,
            size = 4, show.legend = FALSE)+
  xlab("Effect size \n(log response ratio)") +
  ylab("") +
  scale_color_manual(values = c("#007F00","#663399"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", range = c(1, 3)) +
  scale_x_continuous(limits= c(-1,1.5))+
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA))+
  labs(title = "Network performance",
       tag = "A")+
  theme_bw() + my_theme + theme(plot.margin = ggplot2::margin(0,0,0,0,"cm"))

g3


#plot
g <- ggpubr::ggarrange(g1, g2, g3, heights=c(0.4, 0.35,0.25), ncol=1, 
                       legend = "none", align = "v") 

#legend
legend_only <- cowplot::get_legend(g1) 

g_final <- ggpubr::ggarrange(g, legend_only, widths = c(0.8,0.2), ncol=2)


#ggsave(g_final, filename=file.path(fig_dir, "archive/Fig2_network_forestplot_no_take.png"), bg = "white",
 #     width=6.5, height=8, units="in", dpi=800) 


