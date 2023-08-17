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

#find the latest year for each mpa

filtered_data <- forest_dat %>%
  #filter(target_status == "Targeted")%>%
  group_by(habitat, affiliated_mpa, target_status) %>%
  filter(year == max(year))%>%
  ungroup() %>%
  na.omit()

################################################################################
##calcualte effect size for each year and MPA

# find n habitats for each mpa
n_habitats <- filtered_data %>%
              group_by(affiliated_mpa, target_status)%>%
              summarize(n_habitat = n_distinct(habitat))

# Calculate the pooled effects for each affiliated_mpa
pooled_results <- filtered_data %>%
  group_by(state_region, affiliated_mpa, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result))) %>%
  data.frame() %>%
  left_join(n_habitats, by = c("affiliated_mpa","target_status"))%>%
  mutate(state_region = factor(str_replace(state_region, "Coast$", "\nCoast"), levels = c("North \nCoast",
                                                                                                  "North Central \nCoast", 
                                                                                                  "Central \nCoast", 
                                                                                                  "South \nCoast")),
          target_status = factor(target_status, levels = c("Targeted","Nontargeted")))%>%
 arrange(state_region, target_status == "Nontargeted", desc(-estimate)) %>%
  mutate(affiliated_mpa = factor(affiliated_mpa, levels = unique(affiliated_mpa)))


# Calculate the total count of MPAs where logRR > 0 and logRR < 0
total_counts <- pooled_results %>%
  group_by(target_status, state_region) %>%
  summarize(total_gt_0 = sum(estimate > 0, na.rm = TRUE),
            total_lt_0 = sum(estimate < 0, na.rm = TRUE))



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


# Create ggplot
g <- ggplot(pooled_results, aes(x = estimate, y = affiliated_mpa)) +
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
    color = "navyblue", hjust = 4.1, vjust = -1.5, size = 3, show.legend = FALSE) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +
  scale_color_gradientn(colors = c("navyblue", "grey80", "indianred"),
                        values = scales::rescale(c(-2.4, 0, 0.1, 1)),
                        name = "Effect size") +
  scale_fill_gradientn(colors = c("navyblue", "grey80", "indianred"),
                       values = scales::rescale(c(-2.4, 0, 0.1, 1)),
                       name = "Effect size") +
  scale_size_continuous(name = "No. habitats") +
  xlab("Effect size (log ratio)") +
  ylab("") +
  theme_bw() + my_theme 
g

ggsave(g, filename=file.path(fig_dir, "Fig2_mpa_effect_size6.png"), bg = "white",
      width=7.5, height=9, units="in", dpi=600) 

















# Calculate the pooled effects for each affiliated_mpa
pooled_results <- filtered_data %>%
  group_by(habitat, state_region, target_status) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result))) %>%
  data.frame() %>%
 # left_join(n_habitats, by = c("affiliated_mpa","target_status"))%>%
  mutate(state_region = factor(str_replace(state_region, "Coast$", "\nCoast"), levels = c("North \nCoast",
                                                                                          "North Central \nCoast", 
                                                                                          "Central \nCoast", 
                                                                                          "South \nCoast")),
         target_status = factor(target_status, levels = c("Targeted","Nontargeted")))%>%
  arrange(state_region, target_status == "Nontargeted", desc(-estimate))




ggplot(pooled_results, aes(x = estimate, y = state_region)) +
  geom_point(aes(fill = estimate, color = estimate)) +
  geom_errorbarh(aes(xmin = ci.lb, xmax = ci.ub, color= estimate), height = 0) +
  facet_wrap( ~ habitat, ncol = 1)+
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey20") +
  #scale_size_continuous(name = "No. habitats") + #make this MPA no.
  xlab("Effect size (log ratio)") +
  ylab("") +
  theme_bw() + my_theme 
