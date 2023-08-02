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
                   strip.text = element_text(size = 6 , face="bold", color = "black"),
)

mean_es <- forest_dat %>% group_by(affiliated_mpa, region4, target_status) %>%
  summarize(mean_logRR = mean(yi, na.rm=TRUE),
            se_logRR = sd(yi, na.rm=TRUE) / sqrt(n()))

# Calculate the number of unique habitats in each affiliated_mpa in the original data frame
habitat_counts <- forest_dat %>%
  group_by(affiliated_mpa) %>%
  summarize(num_habitats = n_distinct(habitat))

# Join the habitat_counts with mean_es
mean_es_with_habitat_count <- mean_es %>%
  left_join(habitat_counts, by = "affiliated_mpa")

# Create the ggplot with facet_grid and equal y-axis spacing
ggplot(mean_es_with_habitat_count %>%
         mutate(region4 = factor(region4, levels = c("North", "Central", "N. Channel Islands", "South"))),
       aes(x = mean_logRR, y = affiliated_mpa)) +
  geom_point(aes(size = num_habitats)) +
  geom_errorbarh(aes(xmin = mean_logRR - se_logRR, xmax = mean_logRR + se_logRR)) +
  facet_grid(cols = vars(target_status), rows = vars(region4), scales = "free_y", space = "free") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +  # Add vertical line at x = 0
  xlab("Mean Effect Size (mean_logRR)") +
  ylab("Affiliated MPA") +
  scale_size_continuous(name = "Number of Habitats") +  # Rename the legend
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines")) +
  theme_bw() + my_theme





