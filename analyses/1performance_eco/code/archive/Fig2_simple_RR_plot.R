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
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) 
      
################################################################################
#prep data

biomass_mod <- biomass_mod %>% mutate(target_status = ifelse(habitat == "Rocky reef","Targeted",target_status),
                                      affiliated_mpa = str_to_title(affiliated_mpa) %>% 
                                        str_replace(" Smr$", " SMR") %>% 
                                        str_replace(" Smca$", " SMCA"))

################################################################################
#create forest plot for pooled effect size by MPA, where effect size 
#is calculated using age > 0 and weighted by no. of habitats included in that 
#MPA

#calcualte effect size for each year and MPA
dat <- escalc(measure="ROM", m1i=biomass_smr + scalar_smr, m2i=biomass_ref + scalar_ref, sd1i=sd_smr, 
              sd2i=sd_ref, n1i=n_rep_smr, n2i=n_rep_ref, data=biomass_mod)


forest_dat <- dat %>% filter(age_at_survey > 0) %>% 
  #drop missing variance
  filter(!(is.na(vi) | vi == 0))


################################################################################
#plot mean effect size for each MPA

# Theme
my_theme <-  theme(axis.text=element_text(size=8, color = "black"),
                   axis.text.y = element_text(color = "black"),
                   axis.title=element_text(size=10, color = "black"),
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
                   strip.text = element_text(size = 10 , face="bold", color = "black"),
)

mean_es <- forest_dat %>% group_by(affiliated_mpa, state_region, target_status) %>%
  summarize(mean_logRR = mean(yi, na.rm=TRUE),
            se_logRR = sd(yi, na.rm=TRUE) / sqrt(n()))

# Calculate the number of unique habitats in each affiliated_mpa in the original data frame
habitat_counts <- forest_dat %>%
  group_by(affiliated_mpa) %>%
  summarize(num_habitats = n_distinct(habitat))

# Join the habitat_counts with mean_es
mean_es_with_habitat_count <- mean_es %>%
  left_join(habitat_counts, by = "affiliated_mpa") %>%
  #fix levels
  mutate(state_region = factor(str_replace(state_region, "Coast$", "\nCoast"), levels = c("North \nCoast",
                                                        "North Central \nCoast", 
                                                        "Central \nCoast", 
                                                        "South \nCoast")))

#set the levels of target_status 
mean_es_with_habitat_count$target_status_f <- factor(mean_es_with_habitat_count$target_status, levels = c("Targeted", "Nontargeted"))


# Calculate the total count of MPAs where logRR > 0 and logRR < 0
total_counts <- mean_es_with_habitat_count %>%
  group_by(target_status_f, state_region) %>%
  summarize(total_gt_0 = sum(mean_logRR > 0, na.rm = TRUE),
            total_lt_0 = sum(mean_logRR < 0, na.rm = TRUE))


# Order affiliated_mpa 
mean_es_with_habitat_count <- mean_es_with_habitat_count %>%
  group_by(state_region, target_status) %>%
  arrange(state_region, target_status == "Targeted", desc(-mean_logRR)) %>%
  mutate(affiliated_mpa = factor(affiliated_mpa, levels = unique(affiliated_mpa)))

g <- ggplot(mean_es_with_habitat_count,
            aes(x = mean_logRR, y = affiliated_mpa)) +
  geom_errorbarh(aes(xmin = mean_logRR - se_logRR, xmax = mean_logRR + se_logRR, color = mean_logRR), height = 0) +
  geom_point(aes(size = num_habitats, fill = mean_logRR, color = mean_logRR)) +  
  facet_grid(state_region~target_status_f, scales = "free_y", space = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +  
  # Add text indicating the total count of MPAs where logRR > 0 and logRR < 0
  geom_text(data = total_counts,
            aes(x = Inf, y = -Inf, label = paste("n =", total_gt_0)),
            color = "indianred", hjust = 1.1, vjust = -0.2, size = 3, show.legend = FALSE) +
  geom_text(data = total_counts,
            aes(x = -Inf, y = -Inf, label = paste("n =", total_lt_0)),
            color = "navyblue", hjust = -0.1, vjust = -0.2, size = 3, show.legend = FALSE) +
  
  xlab("Mean effect size (log response ratio)") +
  ylab("") +
  scale_size_continuous(name = "No. habitats") +  
  scale_color_gradientn(colors = c("navyblue","grey80", "indianred"),
                        values = scales::rescale(c(-1,-0.2, 0, 0.1,1.3)),
                        name = "Effect size") +  
  scale_fill_gradientn(colors = c("navyblue","grey80", "indianred"),
                       values = scales::rescale(c(-1,-0.2, 0, 0.1, 1.3)),
                       name = "Effect size")  +
  guides(fill = guide_colorbar(ticks.colour = "black",
                               frame.colour = "black")) +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines")) +
  theme_bw() + my_theme

g



ggsave(g, filename=file.path(fig_dir, "Fig2_mpa_effect_size4.png"), bg = "white",
       width=7.5, height=9, units="in", dpi=600) 





