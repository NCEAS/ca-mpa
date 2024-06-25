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
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators_new.Rds")) 


################################################################################
#find the latest year for each mpa

filtered_data <- biomass_mod %>%
  #filter(target_status == "Targeted")%>%
  group_by(habitat, affiliated_mpa, target_status) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  na.omit() %>%
  # left_join(n_habitats, by = c("affiliated_mpa","target_status"))%>%
  mutate(#state_region = factor(str_replace(state_region, "Coast$", "\nCoast"), levels = c("North \nCoast",
          #                                                                                "North Central \nCoast", 
           #                                                                               "Central \nCoast", 
            #                                                                              "South \nCoast")),
         target_status = factor(target_status, levels = c("Targeted","Nontargeted")))

################################################################################
# Calculate the pooled effects for each habitat, region, and target_status
habitat_region <- filtered_data %>%
  group_by(habitat, state_region, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() 

#save results to .rdata to generate summary table
saveRDS(habitat_region, file = file.path(dat_path, "habitat_region_meta_results.Rds"))

################################################################################
# Verify that the group_by and do() call worked using a test
rma_test_dat <- filtered_data %>% dplyr::filter(habitat == "Kelp forest" &
                                                  state_region == "North Coast" &
                                                  target_status == "Targeted")

rma_test_results <- coef(summary(rma(yi, vi, data = rma_test_dat)))
                   
pooled_results_filtered <- pooled_results %>% dplyr::filter(habitat == "Kelp forest" &
                                                              state_region == "North Coast" &
                                                              target_status == "Targeted")      

################################################################################
# Calculate the pooled effects for each habitat and target status
habitat <- filtered_data %>%
  group_by(habitat, target_status) %>%
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
saveRDS(habitat, file = file.path(dat_path, "habitat_target_meta_results.Rds"))

################################################################################
# Calculate the pooled effects for each region across habitat
region <- filtered_data %>%
  group_by(state_region, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() %>%
  mutate(target_status = factor(target_status, levels = c("Targeted","Nontargeted")),
         #create dummy habitat
         habitat = "Pooled effect size"
  )

#save results to .rdata to generate summary table
saveRDS(region, file = file.path(dat_path, "region_meta_results.Rds"))

################################################################################
# Calculate the pooled effect for entire state
state <- filtered_data %>%
  group_by(target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() %>%
  mutate(target_status = factor(target_status, levels = c("Targeted","Nontargeted")),
         #create dummy state region
         state_region = "Pooled",
         #create dummy habitat
         habitat = "Pooled effect size"
  )

#save results to .rdata to generate summary table
saveRDS(state, file = file.path(dat_path, "state_meta_results.Rds"))

################################################################################
#join everything

meta_results <- rbind(habitat_region, habitat, region, state) %>%
  mutate(
    significance = ifelse(ci.lb > 0 | ci.ub < 0,"*",NA),
    n_mpas = ifelse(habitat == "Pooled effect size",NA,n_mpas),
    target_status = factor(str_replace(target_status, "Nontargeted", "Non-targeted"),
                           levels = c("Targeted", "Non-targeted")))
  

####note that n_MPAS is NOT the number of distinct mpas. It is the number of 
#unique habitat-mpa combinations ('study' rows) included 


################################################################################
#plot

meta_results$habitat <- factor(meta_results$habitat, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef", "Pooled effect size"))
meta_results$state_region <- factor(meta_results$state_region, levels = c("Pooled","South Coast", "Central Coast", "North Central Coast","North Coast"))
meta_results$target_status <- factor(meta_results$target_status, levels = c("Targeted","Non-targeted"))  # Reversed order

# labels
#state_labels <- c(expression(italic("Pooled")), "South Coast", "Central Coast", "North Central Coast", "North Coast")


# Theme
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
                   strip.text = element_text(size = 6 , face="bold", color = "black")
)


g <- ggplot(meta_results, aes(x = estimate, y = state_region, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  # Fill the last facet. Call this first to place it behind the points. 
  geom_rect(data = subset(meta_results, habitat == 'Pooled effect size'), 
            fill = "grey80", xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.05, show.legend = FALSE) +
  #add points for mpas
  geom_point(aes(size = n_mpas), 
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 15, 
             position = position_dodge(width = 0.7)
  ) +
  #add points for pooled effects
  geom_point(data = subset(meta_results, habitat == 'Pooled effect size'), 
             # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
             shape = 18, 
             size = 2,
             position = position_dodge(width = 0.7),
             show.legend = FALSE
  ) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  geom_hline(yintercept = which(levels(meta_results$state_region) == "South Coast") - 0.5, 
             linetype = "solid", color = "black", size = 0.2) +  
  geom_text(aes(label = significance), vjust = -0.2, size = 4, show.legend = FALSE) + 
  facet_grid(habitat ~ ., scales = "fixed") +  
  xlab("Effect size \n(log response ratio)") +
  ylab("") +
  scale_color_manual(values = c("indianred","navyblue"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", range = c(1, 3)) +
  scale_x_continuous(limits= c(-2,2.3))+
  #guides(color = guide_legend(override.aes = list(shape = c(15, 15))),  # Set the shape to 15 (square) for color legend
  #      size = guide_legend(override.aes = list(shape = c(15, 15)))) +  # Set the shape to 15 (square) for size legend
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA)) +
  theme_bw() + my_theme 

g

ggsave(g, filename=file.path(fig_dir, "Fig3_habitat_meta_forestplot7.png"), bg = "white",
       width=6, height=7, units="in", dpi=600) 


