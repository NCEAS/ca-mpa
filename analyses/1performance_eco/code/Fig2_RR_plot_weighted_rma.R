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
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators_new2.Rds")) 

unique(biomass_mod$mpa)

################################################################################

#find the latest year for each mpa

filtered_data <- biomass_mod %>%
  #filter(target_status == "Targeted")%>%
  group_by(habitat, mpa, target_status) %>%
  #determine the most recent year of sampling for each ecosystem
  filter(year == max(year))%>%
  ungroup() 
  #na.omit()

unique(filtered_data$mpa)


################################################################################
#use rma to calculate among study variance and find the overall (pooled) ES
#for all MPAs

# find n habitats for each mpa
n_habitats <- filtered_data %>%
              group_by(mpa, target_status)%>%
              summarize(n_habitat = n_distinct(habitat))

# Calculate the pooled effects for each affiliated_mpa

pooled_results <- filtered_data %>%
  group_by(state_region, mpa, target_status) %>%
  do(meta_result = rma(yi, vi, data = .)) %>%
  mutate(coef(summary(meta_result))) %>%
  data.frame() %>%
  left_join(n_habitats, by = c("mpa", "target_status")) %>%
  mutate(state_region = case_when(
    str_detect(state_region, "Coast$") ~ str_replace(state_region, "Coast$", ""),
    state_region == "North Central" ~ "N. Central",
    TRUE ~ state_region
  ),
  target_status = factor(str_replace(target_status, "Nontargeted", "Non-targeted"),
                         levels = c("Targeted", "Non-targeted"))) %>%
  mutate(state_region = trimws(state_region),
         state_region = factor(state_region, levels = c("North",'North Central',"Central","South")))%>%
  arrange(state_region, target_status == "Non-targeted", desc(-estimate)) %>%
  mutate(mpa = factor(mpa, levels = unique(mpa)))


#save results to .rdata to generate summary table
#saveRDS(pooled_results, file = file.path(dat_path, "mpa_level_meta_results.Rds"))

##warning is OK -- tau^2 can't be estimated for MPAs with only one habitat. 

# Calculate the total count of MPAs where logRR > 0 and logRR < 0
total_counts <- pooled_results %>%
  group_by(target_status, state_region) %>%
  summarize(total_gt_0 = sum(estimate > 0, na.rm = TRUE),
            total_lt_0 = sum(estimate < 0, na.rm = TRUE))

################################################################################
#plot

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


# all MPAs
g <- ggplot(pooled_results, aes(x = estimate, y = mpa)) +
  geom_point(aes(fill = estimate, color = estimate, size = n_habitat)) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub, color= estimate), height = 0) +
  facet_grid(state_region ~ target_status, space = "free", scales = "free_y")+
  # Add text indicating the total count of MPAs where logRR > 0 and logRR < 0
  geom_text(data = total_counts,
            aes(x = Inf, y = -Inf, label = paste("n =", total_gt_0)),
            color = "indianred", hjust = 1.1, vjust = -0.2, size = 3, show.legend = FALSE) +
  geom_text(data = total_counts,
            aes(x = -Inf, y = -Inf, label = paste("n =", total_lt_0)),
            color = "navyblue", hjust = -0.1, vjust = -0.2, size = 3, show.legend = FALSE) +
  #add inside/ outside text
  # Add text indicating the total count of MPAs where logRR > 0 and logRR < 0
  geom_text(
            aes(x = Inf, y = -Inf, label = "Inside"),
            color = "indianred", hjust = 1.05, vjust = -1.5, size = 3, show.legend = FALSE) +
  geom_text(
    aes(x = Inf, y = -Inf, label = "Outside"),
    color = "navyblue", hjust = 4.25, vjust = -1.5, size = 3, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +
  scale_color_gradientn(colors = c("navyblue", "grey80", "indianred"),
                        values = scales::rescale(c(-1.4, -0.2, 0, 2.4)),
                        name = "Effect size") +
  scale_fill_gradientn(colors = c("navyblue", "grey80", "indianred"),
                       values = scales::rescale(c(-1.4, -0.2, 0, 2.4)),
                       name = "Effect size") +
  scale_size_continuous(name = "No. ecosystems") +
  scale_x_continuous(limits = c(-5,6.8))+
  xlab("Effect size \n(log response ratio)") +
  ylab("") +
  theme_bw() + my_theme 
g

ggsave(g, filename=file.path(fig_dir, "Fig2_mpa_effect_size10.png"), bg = "white",
      width=8, height=10, units="in", dpi=600) 






################################################################################
#use rma to calculate among study variance and find the overall (pooled) ES
#for defacto SMRs only

# find n habitats for each mpa
n_habitats <- filtered_data %>%
  filter(mpa_defacto_class == "smr")%>%
  group_by(mpa, target_status)%>%
  summarize(n_habitat = n_distinct(habitat))

# Calculate the pooled effects for each affiliated_mpa

pooled_results <- filtered_data %>%
  filter(mpa_defacto_class == "smr")%>%
  group_by(state_region, mpa, target_status) %>%
  do(meta_result = rma(yi, vi, data = .)) %>%
  mutate(coef(summary(meta_result))) %>%
  data.frame() %>%
  left_join(n_habitats, by = c("mpa", "target_status")) %>%
  mutate(state_region = case_when(
    str_detect(state_region, "Coast$") ~ str_replace(state_region, "Coast$", ""),
    state_region == "North Central" ~ "N. Central",
    TRUE ~ state_region
  ),
  target_status = factor(str_replace(target_status, "Nontargeted", "Non-targeted"),
                         levels = c("Targeted", "Non-targeted"))) %>%
  mutate(state_region = trimws(state_region),
         state_region = factor(state_region, levels = c("North",'North Central',"Central","South")))%>%
  arrange(state_region, target_status == "Non-targeted", desc(-estimate)) %>%
  mutate(mpa = factor(mpa, levels = unique(mpa)))


#save results to .rdata to generate summary table
#saveRDS(pooled_results, file = file.path(dat_path, "mpa_level_meta_results.Rds"))

##warning is OK -- tau^2 can't be estimated for MPAs with only one habitat. 

# Calculate the total count of MPAs where logRR > 0 and logRR < 0
total_counts <- pooled_results %>%
  group_by(target_status, state_region) %>%
  summarize(total_gt_0 = sum(estimate > 0, na.rm = TRUE),
            total_lt_0 = sum(estimate < 0, na.rm = TRUE))

################################################################################
#plot

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


# all MPAs
g <- ggplot(pooled_results, aes(x = estimate, y = mpa)) +
  geom_point(aes(fill = estimate, color = estimate, size = n_habitat)) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub, color= estimate), height = 0) +
  facet_grid(state_region ~ target_status, space = "free", scales = "free_y")+
  # Add text indicating the total count of MPAs where logRR > 0 and logRR < 0
  geom_text(data = total_counts,
            aes(x = Inf, y = -Inf, label = paste("n =", total_gt_0)),
            color = "indianred", hjust = 1.1, vjust = -0.2, size = 3, show.legend = FALSE) +
  geom_text(data = total_counts,
            aes(x = -Inf, y = -Inf, label = paste("n =", total_lt_0)),
            color = "navyblue", hjust = -0.1, vjust = -0.2, size = 3, show.legend = FALSE) +
  #add inside/ outside text
  # Add text indicating the total count of MPAs where logRR > 0 and logRR < 0
  geom_text(
    aes(x = Inf, y = -Inf, label = "Inside"),
    color = "indianred", hjust = 1.05, vjust = -1.5, size = 3, show.legend = FALSE) +
  geom_text(
    aes(x = Inf, y = -Inf, label = "Outside"),
    color = "navyblue", hjust = 4.25, vjust = -1.5, size = 3, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +
  scale_color_gradientn(colors = c("navyblue", "grey80", "indianred"),
                        values = scales::rescale(c(-1.4, -0.2, 0, 2)),
                        name = "Effect size") +
  scale_fill_gradientn(colors = c("navyblue", "grey80", "indianred"),
                       values = scales::rescale(c(-1.4, -0.2, 0, 2)),
                       name = "Effect size") +
  scale_size_continuous(name = "No. ecosystems") +
 # scale_x_continuous(limits = c(-5,6.8))+
  xlab("Effect size \n(log response ratio)") +
  ylab("") +
  theme_bw() + my_theme 
g





