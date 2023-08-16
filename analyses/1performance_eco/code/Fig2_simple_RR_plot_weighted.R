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
##calcualte effect size for each year and MPA

#this is the calculation of the variance without the use of escalc
dat <- biomass_mod %>%
          mutate(yi = logRR,
                 vi = ((sd_smr^2) / (n_rep_smr*((biomass_smr + scalar_smr)^2))) +
                   ((sd_ref^2) / (n_rep_ref*((biomass_ref + scalar_ref)^2))))

#calcualte effect size for each year and MPA
#dat <- escalc(measure="ROM", m1i=biomass_smr + scalar_smr, m2i=biomass_ref + scalar_ref, sd1i=sd_smr, 
 #             sd2i=sd_ref, n1i=n_rep_smr, n2i=n_rep_ref, data=biomass_mod)


forest_dat <- dat %>% filter(age_at_survey > 0) %>% 
  #drop missing variance
  filter(!(is.na(vi) | vi == 0))


# Estimate the between-study variance (va) using DerSimonian-Laird estimator --- this is overall and not mpa specific
#Q <- sum((forest_dat$yi - mean(forest_dat$yi))^2 / (forest_dat$vi + forest_dat$vi))
#k <- nrow(forest_dat)
#tau_squared <- max(0, (Q - (k - 1)) / sum(1 / (forest_dat$vi + forest_dat$vi)))

# Calculate the Q statistic for each affiliated_mpa
Q <- forest_dat %>%
  group_by(affiliated_mpa) %>%
  summarize(Q = sum((yi - mean(yi))^2 / vi))

# Estimate the between-study variance (va) for each affiliated_mpa
Q <- Q %>%
  group_by(affiliated_mpa) %>%
  mutate(va = max(0, (Q - (n() - 1)) / n()))

# Merge the va values back into the original data
forest_dat <- left_join(forest_dat, Q, by = "affiliated_mpa")

################################################################################
#determine weighted means

# Calculate the weighted effect size (weighted_yi)
#forest_weighted <- forest_dat %>%
 # mutate(weighted_yi = yi * (1/ vi + va)) #weight each effect size by the inverse of its variance and the between study variance

forest_weighted <- forest_dat %>%
  mutate(weighted_yi = yi * (1/ vi)) 

# Calculate the sum of weighted variance for each target status and MPA
pooled_mean <- forest_weighted %>%
  group_by(state_region, affiliated_mpa, target_status) %>%
  summarize(sum_weight = sum(1 / vi),
            sum_weighted_yi = sum(weighted_yi),
            #sum_weighted_yi = sum(yi) # this is uneweighted
            )

# Calculate the pooled mean and standard error
pooled_mean_data <- pooled_mean %>%
  mutate(pooled_mean_yi = sum_weighted_yi / sum_weight,
         pooled_se_yi = 1 / sqrt(sum_weight))


# Calculate the 95% confidence intervals
pooled_mean_ci <- pooled_mean_data %>%
  mutate(lower_ci_yi = pooled_mean_yi - 1.96 * pooled_se_yi,
         upper_ci_yi = pooled_mean_yi + 1.96 * pooled_se_yi)


# Calculate the number of unique habitats in each affiliated_mpa in the original data frame
habitat_counts <- forest_dat %>%
  group_by(affiliated_mpa) %>%
  summarize(num_habitats = n_distinct(habitat))

# Join the habitat_counts with pooled_mean
mean_es_with_habitat_count <- pooled_mean_ci %>%
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
  summarize(total_gt_0 = sum(pooled_mean_yi > 0, na.rm = TRUE),
            total_lt_0 = sum(pooled_mean_yi < 0, na.rm = TRUE))

# Order affiliated_mpa 
mean_es_with_habitat_count <- mean_es_with_habitat_count %>%
  group_by(state_region, target_status) %>%
  mutate(target_status = factor(target_status, levels = c("Targeted","Nontargeted")))%>%
  arrange(state_region, target_status == "Targeted", desc(-pooled_mean_yi)) %>%
  mutate(affiliated_mpa = factor(affiliated_mpa, levels = unique(affiliated_mpa)))

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



# Assuming you have a dataframe named 'mean_es_with_habitat_count' with columns: pooled_mean_yi, lower_ci_yi, upper_ci_yi, affiliated_mpa, num_habitats, state_region, target_status_f

# Create the plot
g <- ggplot(mean_es_with_habitat_count,
            aes(x = pooled_mean_yi, y = affiliated_mpa)) +
  geom_errorbarh(aes(xmin = lower_ci_yi, xmax = upper_ci_yi, color = pooled_mean_yi), height = 0) +  # Adjusted xmin and xmax
  geom_point(aes(size = num_habitats, fill = pooled_mean_yi, color = pooled_mean_yi)) +
  facet_grid(state_region ~ target_status_f, scales = "free_y", space = "free") +
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
  scale_color_gradientn(colors = c("navyblue", "grey80", "indianred"),
                        values = scales::rescale(c(-1, -0.2, 0, 0.1, 1.3)),
                        name = "Effect size") +
  scale_fill_gradientn(colors = c("navyblue", "grey80", "indianred"),
                       values = scales::rescale(c(-1, -0.2, 0, 0.1, 1.3)),
                       name = "Effect size") +
  guides(fill = guide_colorbar(ticks.colour = "black", frame.colour = "black")) +
  theme_minimal() +
  theme(panel.spacing = unit(1, "lines")) +
  theme_bw() +
  my_theme

g

#ggsave(g, filename=file.path(fig_dir, "Fig2_mpa_effect_size5.png"), bg = "white",
 #      width=7.5, height=9, units="in", dpi=600) 

