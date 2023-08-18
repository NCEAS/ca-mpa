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


biomass_mod <- biomass_mod %>% mutate(target_status = ifelse(habitat == "Shallow reef reef","Targeted",target_status),
                                      affiliated_mpa = str_to_title(affiliated_mpa) %>% 
                                        str_replace(" Smr$", " SMR") %>% 
                                        str_replace(" Smca$", " SMCA"))

#calcualte effect size for each year and MPA
dat <- escalc(measure="ROM", m1i=biomass_smr + scalar_smr, m2i=biomass_ref + scalar_ref, sd1i=sd_smr, 
              sd2i=sd_ref, n1i=n_rep_smr, n2i=n_rep_ref, data=biomass_mod)


forest_dat <- dat %>% filter(age_at_survey > 0) %>% 
  #drop missing variance
  filter(!(is.na(vi) | vi == 0))%>%
  mutate(target_status = ifelse(habitat == "Shallow reef","Targeted",target_status))


################################################################################
#determine weighted means for each region by habitat

# Calculate the weighted effect size (weighted_yi)
forest_weighted <- forest_dat %>%
  mutate(weighted_yi = yi * (1/vi)) #weight each effect size by the inverse of its variance

# Calculate the sum of weighted variance for each habitat, region, and target status 
pooled_mean <- forest_weighted %>%
  group_by(habitat, state_region, target_status) %>%
  summarize(sum_weight = sum(1 / vi),
            sum_weighted_yi = sum(weighted_yi))

# Calculate the pooled mean and standard error
pooled_mean_data <- pooled_mean %>%
  mutate(pooled_mean_yi = sum_weighted_yi / sum_weight,
         pooled_se_yi = 1 / sqrt(sum_weight))


# Calculate the 95% confidence intervals
pooled_mean_ci <- pooled_mean_data %>%
  mutate(lower_ci_yi = pooled_mean_yi - 1.96 * pooled_se_yi,
         upper_ci_yi = pooled_mean_yi + 1.96 * pooled_se_yi)

# Calculate the number of unique MPAs in each habitat and region
mpa_counts <- forest_dat %>%
  group_by(habitat, state_region) %>%
  summarize(num_mpas = n_distinct(affiliated_mpa))

# Join the habitat_counts with pooled_mean
mean_es_with_mpa_count <- pooled_mean_ci %>%
  left_join(mpa_counts, by = c("habitat","state_region")) %>%
  #fix levels
  mutate(state_region = factor(str_replace(state_region, "Coast$", "\nCoast"), levels = c("North \nCoast",
                                                                                          "North Central \nCoast", 
                                                                                          "Central \nCoast", 
                                                                                          "South \nCoast")),
         #add meta level
         meta_level = "Region",
         es_stat = "Region"
         )


################################################################################
#determine weighted means for each habitat pooled across region

# Calculate the sum of weighted variance for each habitat, region, and target status 
hab_region <- forest_weighted %>%
  group_by(habitat, target_status) %>%
  summarize(sum_weight = sum(1 / vi),
            sum_weighted_yi = sum(weighted_yi))

# Calculate the pooled mean and standard error
hab_region_pool <- hab_region %>%
  mutate(pooled_mean_yi = sum_weighted_yi / sum_weight,
         pooled_se_yi = 1 / sqrt(sum_weight))


# Calculate the 95% confidence intervals
hab_region_pooled_mean_ci <- hab_region_pool %>%
  mutate(lower_ci_yi = pooled_mean_yi - 1.96 * pooled_se_yi,
         upper_ci_yi = pooled_mean_yi + 1.96 * pooled_se_yi)

# Calculate the number of unique MPAs for each habitat 
hab_mpa_counts <- forest_dat %>%
  group_by(habitat) %>%
  summarize(num_mpas = n_distinct(affiliated_mpa))

# Join the habitat_counts with pooled_mean
hab_region_mpa_count <- hab_region_pooled_mean_ci %>%
  left_join(hab_mpa_counts, by = c("habitat")) %>%
  #fix levels
  mutate(
         meta_level = "Habitat",
         es_stat = "Pooled"
  )


################################################################################
#determine weighted mean for each region pooled acrosss habitats

# Calculate the sum of weighted variance for each habitat, region, and target status 
region <- forest_weighted %>%
  group_by(state_region, target_status) %>%
  summarize(sum_weight = sum(1 / vi),
            sum_weighted_yi = sum(weighted_yi))

# Calculate the pooled mean and standard error
region_pool <- region %>%
  mutate(pooled_mean_yi = sum_weighted_yi / sum_weight,
         pooled_se_yi = 1 / sqrt(sum_weight))


# Calculate the 95% confidence intervals
region_pooled_mean_ci <- region_pool %>%
  mutate(lower_ci_yi = pooled_mean_yi - 1.96 * pooled_se_yi,
         upper_ci_yi = pooled_mean_yi + 1.96 * pooled_se_yi)

# Calculate the number of unique MPAs for each region
region_mpa_counts <- forest_dat %>%
  group_by(state_region) %>%
  summarize(num_mpas = n_distinct(affiliated_mpa))

# Join the counts with pooled_mean
region_mpa_count <- region_pooled_mean_ci %>%
  left_join(region_mpa_counts, by = c("state_region")) %>%
  #fix levels
  mutate(
    meta_level = "state_region",
    es_stat = "Region",
    habitat="Pooled effect size"
  )


################################################################################
#determine weighted mean pooled across entire state

# Calculate the sum of weighted variance for each habitat, region, and target status 
state <- forest_weighted %>%
  group_by(target_status)%>%
  summarize(sum_weight = sum(1 / vi),
            sum_weighted_yi = sum(weighted_yi))

# Calculate the pooled mean and standard error
state_pool <- state %>%
  mutate(pooled_mean_yi = sum_weighted_yi / sum_weight,
         pooled_se_yi = 1 / sqrt(sum_weight))


# Calculate the 95% confidence intervals
state_pooled_mean_ci <- state_pool %>%
  mutate(lower_ci_yi = pooled_mean_yi - 1.96 * pooled_se_yi,
         upper_ci_yi = pooled_mean_yi + 1.96 * pooled_se_yi)

# Calculate the number of unique MPAs for each region
state_mpa_counts <- forest_dat %>%
  group_by(target_status)%>%
  summarize(num_mpas = n_distinct(affiliated_mpa))

# Join the counts with pooled_mean
state_mpa_count <- state_pooled_mean_ci %>%
  left_join(state_mpa_counts, by = ("target_status")) %>%
  #fix levels
  mutate(
    meta_level = "state",
    es_stat = "Pooled",
    habitat="Pooled effect size"
  )

################################################################################
#merge everything

merge_build1 <- rbind(mean_es_with_mpa_count, hab_region_mpa_count, 
                     region_mpa_count, state_mpa_count)

combined_dat <- merge_build1 %>% mutate(state_region = ifelse(is.na(state_region),"Pooled",state_region),
                                        state_region = ifelse(state_region == "North Coast","North \nCoast",
                                                              ifelse(state_region == "North Central Coast", "North Central \nCoast",
                                                                     ifelse(state_region == "Central Coast","Central \nCoast",
                                                                            ifelse(state_region == "South Coast","South \nCoast",state_region)))),
                                        significance = ifelse(lower_ci_yi > 0 | upper_ci_yi < 0,"*",NA))

################################################################################
#plot

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


combined_dat$habitat <- factor(combined_dat$habitat, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef", "Pooled effect size"))
combined_dat$state_region <- factor(combined_dat$state_region, levels = c("Pooled","South \nCoast", "Central \nCoast", "North Central \nCoast","North \nCoast"))
combined_dat$target_status <- factor(combined_dat$target_status, levels = c("Nontargeted", "Targeted"))  # Reversed order

# labels
state_labels <- c(expression(italic("Pooled")), "South Coast", "Central Coast", "North Central Coast", "North Coast")


g <- ggplot(combined_dat, aes(x = pooled_mean_yi, y = state_region, color = target_status)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  # Fill the last facet. Call this first to place it behind the points. 
  geom_rect(data = subset(combined_dat, habitat == 'Pooled effect size'), 
            fill = "grey80", xmin = -Inf, xmax = Inf,
            ymin = -Inf, ymax = Inf, alpha = 0.05, show.legend = FALSE) +
  geom_point(aes(size = num_mpas), 
            # shape = ifelse(combined_dat$habitat == "Pooled effect size", 18, 15), 
            shape = 15, 
             position = position_dodge(width = 0.7)
  ) +
  geom_errorbarh(aes(xmin = lower_ci_yi, xmax = upper_ci_yi), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.3) +
  geom_hline(yintercept = which(levels(combined_dat$state_region) == "South \nCoast") - 0.5, 
             linetype = "solid", color = "black", size = 0.2) +  
  geom_text(aes(label = significance), vjust = -0.2, size = 4, show.legend = FALSE) + 
  facet_grid(habitat ~ ., scales = "fixed") +  
  xlab("Effect size (log response ratio)") +
  ylab("") +
  scale_color_manual(values = c("navyblue", "indianred"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", range = c(1, 3)) +
  #guides(color = guide_legend(override.aes = list(shape = c(15, 15))),  # Set the shape to 15 (square) for color legend
   #      size = guide_legend(override.aes = list(shape = c(15, 15)))) +  # Set the shape to 15 (square) for size legend
  scale_y_discrete(labels = state_labels) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines"),
        panel.background = element_rect(fill = "white", color = NA)) +
  theme_bw() + my_theme 

g

ggsave(g, filename=file.path(fig_dir, "Fig3_habitat_meta_forestplot4.png"), bg = "white",
       width=6, height=7, units="in", dpi=600) 

