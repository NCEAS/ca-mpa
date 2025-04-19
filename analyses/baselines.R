# Step 5. Fit Targeted Fish Model
# Cori Lopazanski
# Feb 2025

library(tidyverse)
library(lmerTest)
library(performance)
library(patchwork)


# Set up --------------------------------------------------------------------------
rm(list = ls())
gc()


my_theme <- theme_minimal(base_family = "Arial") + 
  theme(
  plot.title = element_text(size = 10, face = "bold"),
  plot.subtitle = element_text(size = 8),
  axis.title = element_text(size = 10),
  axis.text.x = element_text(size = 10),
  axis.text.y = element_text(size = 10),
  legend.title = element_text(size = 10),
  legend.text = element_text(size = 10),
  plot.caption = element_text(size = 8),
  strip.text = element_text(size = 8, face = "bold"),
  panel.background = element_rect(fill = "white", color = NA),  
  plot.background = element_rect(fill = "white", color = NA)
)

source("analyses/7habitat/code/Step4a_prep_focal_data.R")
source("analyses/7habitat/code/Step0_helper_functions.R")


# Read Data --------------------------------------------------------------------
ltm.dir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/update_2024"

pred_kelp <- readRDS(file.path("analyses/7habitat/intermediate_data/kelp_predictors.Rds")) %>% filter(pred_group %in% c("all", "combined"))

# Use the complete dataset 
kelp_complete <- readRDS(file.path(ltm.dir, "kelp_biomass_complete.Rds")) %>% 
  mutate(site_type = factor(site_type, levels = c("Reference", "MPA"))) %>% 
  mutate(kg_per_100m2 = kg_per_m2*100) 


# Prep Data --------------------------------------------------------------------

kelp_sites <- kelp_complete %>% 
  # Identify distinct site-year combinations
  distinct(year, site, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, cluster_area_km2) %>%
  mutate(before = if_else(year < implementation_year, 1, 0),
         after = if_else(year >= implementation_year, 1, 0),
         baseline = if_else(between(year, implementation_year - 1, implementation_year + 1), 1, 0)) %>% 
  # Count number of years each site was visited before and after the MPA was implemented
  group_by(site, bioregion, region4, affiliated_mpa,  mpa_defacto_class, implementation_year, implementation_year_adj, size_km2, cluster_area_km2, site_type) %>% 
  summarize(n_before = sum(before),
            n_after = sum(after),
            n_total = n_before + n_after,
            n_baseline = sum(baseline),
            years = paste(unique(year), collapse = ", ")) %>% 
  # Drop sites with no inside/outside information
  filter(!is.na(site_type)) %>%  
  # Keep sites that have been visited at least 5 times after implementation
  filter(n_after >= 5) %>%  
 # filter(n_baseline > 0) %>% 
  # Keep MPAs that still have at least one MPA site and one Reference site
  group_by(affiliated_mpa) %>% 
  filter(n_distinct(site_type) == 2) %>% 
  # Keep MPAs with at least one site with baseline information
  filter(any(site_type == "MPA" & n_baseline > 0) &
           any(site_type == "Reference" & n_baseline > 0)) %>% ungroup()

length(unique(kelp_sites$site))
length(unique(kelp_sites$affiliated_mpa))
length(unique(kelp_sites$affiliated_mpa[kelp_sites$mpa_defacto_class == "smr"]))


biomass_variable <- "kg_per_100m2"
focal_group <- "targeted"

kelp_subset <- kelp_complete %>% 
  # Drop observations for dropped sites 
  filter(site %in% kelp_sites$site) %>% 
  group_by(year, site, site_type, bioregion, region4, affiliated_mpa, mpa_defacto_class, age_at_survey, target_status) %>%
  summarize(biomass = sum(!!sym(biomass_variable), na.rm = T), .groups = 'drop') %>%
  filter(target_status == str_to_sentence(focal_group)) %>% 
  mutate(year = as.factor(year),
         bioregion = as.factor(bioregion),
         region4 = as.factor(region4),
         affiliated_mpa = as.factor(affiliated_mpa)) 

# Add a small constant, defined as 10x smaller than the minimum value 
const <- if_else(min(kelp_subset$biomass) > 0, 0, min(kelp_subset$biomass[kelp_subset$biomass > 0], na.rm = TRUE)/10)
kelp_subset <- kelp_subset %>% mutate(log_c_biomass = log(biomass + const))

# Prep data
# kelp_sp <- prep_focal_data(
#   type = "target_status",
#   focal_group = "targeted",
#   drop_outliers = "no",
#   biomass_variable = "kg_per_100m2",
#   data = data_kelp,
#   regions = c("North", "Central", "N. Channel Islands", "South")
#   )

# kelp2 <- data2
# data_sp is the aggregated and scaled version
# data2 is the aggregated but NOT scaled version

# Calculate baseline biomass ----------

kelp_baseline <- kelp_subset %>%
  # Focus on surveys within 1 year of implementation (-1 to 1 age)
  filter(between(age_at_survey, -1, 1)) %>% 
  # Calculate baseline as the average across all sites in given MPA/Ref area
  group_by(affiliated_mpa, mpa_defacto_class, site_type) %>% 
  summarize(bb      = mean(biomass, na.rm = TRUE),
            bb_se   =  sd(biomass, na.rm = T)/sqrt(n()), 
            n_sites = length(unique(site)),
            n_years = length(unique(year)),
            n = n(),
            min_age = round(min(age_at_survey), 0), .groups = 'drop') %>% 
  group_by(affiliated_mpa) %>% 
  mutate(mpa_bb       = bb[site_type == "MPA"],
         mpa_ref_cat  = factor(if_else(bb[site_type == "MPA"] > bb[site_type == "Reference"], "MPA Higher", "Ref Higher")),
         mpa_ref_diff = bb[site_type == "MPA"] - bb[site_type == "Reference"]) %>% 
  ungroup() %>% 
  mutate(affiliated_mpa_label = str_to_title(affiliated_mpa) %>% 
           str_replace_all("Smr", "SMR") %>% 
           str_replace_all("Smca", "SMCA")) %>% 
  mutate(affiliated_mpa_label = fct_reorder2(affiliated_mpa_label, mpa_ref_diff, -mpa_bb)) 

median(kelp_baseline$bb)
mean(kelp_baseline$bb)
median(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"])
mean(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"])
length(unique(kelp_baseline$affiliated_mpa[kelp_baseline$mpa_defacto_class != "smr"])) # 7 additional SMCAs


# Join baseline info to data ---------------------------------------------------

kelp_subset2 <- kelp_subset %>%
  left_join(kelp_baseline, by = c("affiliated_mpa", "mpa_defacto_class", "site_type")) %>% 
  # Focus on after-only data
  filter(age_at_survey >= 0) %>% 
  # Focus on SMRs
  filter(mpa_defacto_class == "smr")

median(kelp_subset2$bb)
mean(kelp_subset2$bb)


fig2 <- ggplot(data = kelp_baseline %>% filter(mpa_defacto_class == "smr") %>% 
                 mutate(point_shape = if_else(n < 3, "open", "closed")),
       aes(x = bb, y = affiliated_mpa_label, color = site_type, group = affiliated_mpa_label)) +
  geom_vline(aes(xintercept = median(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"])), linetype = "55", alpha = 0.4) +
  geom_linerange(aes(xmin = bb - bb_se,
                     xmax = bb + bb_se),
                 linewidth = 1.5, alpha = 0.3) +
  geom_point(aes(shape = point_shape), size = 2.5) +
  scale_color_manual(values = c("Reference" = "#7e67f8", "MPA" =  "#e5188b")) +
  scale_shape_manual(values = c("open" = 1, "closed" = 16)) +
  labs(x = "Baseline biomass (kg per 100m²)", y = NULL, color = NULL) +
  theme_minimal() +
  my_theme + 
  theme(legend.position = "top") 

fig2
ggsave("baseline-fig2.png", plot = fig2, width = 7, height = 5, units = "in", dpi = 600)

# Categorize raw data for visuals  ---------------------------------------------------

## 1. High/Low Options --------

kelp_subset3 <- kelp_subset2 %>% 
  # Create categories for visualizing
  mutate(area_median = if_else(bb > median(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"]), "High", "Low"),
         area_mean   = if_else(bb > mean(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"]), "High", "Low"),
         site_median = if_else(bb > median(kelp_subset2 %>% distinct(site, bb) %>% pull(bb)), "High", "Low"),
         site_mean   = if_else(bb > mean(kelp_subset2 %>% distinct(site, bb) %>% pull(bb)), "High", "Low"),
         full_median = if_else(bb > median(kelp_subset2$bb), "High", "Low"),
         full_mean   = if_else(bb > mean(kelp_subset2$bb), "High", "Low")) %>% 
  mutate(across(c(area_median, area_mean, site_median, site_mean, full_median, full_mean), ~factor(., levels = c("Low", "High"))))

breakpoints <- tibble(
  category = c("area_median", "area_mean",  "site_median", "site_mean", "full_median", "full_mean"),
  breakpoint = c(
    median(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"]),
    mean(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"]),
    median(kelp_subset2 %>% distinct(site, bb) %>% pull(bb)),
    mean(kelp_subset2 %>% distinct(site, bb) %>% pull(bb)),
    median(kelp_subset2$bb),
    mean(kelp_subset2$bb)))

kelp_subset3 %>% 
  group_by(site_type, site_median) %>% 
  summarize(n_sites = length(unique(site)),
            n_mpas = length(unique(affiliated_mpa))) %>% 
  group_by(site_type) %>% 
  mutate(pct_sites = n_sites/sum(n_sites),
         pct_mpas = n_mpas/sum(n_mpas)) %>% ungroup() 


# Explore these values - not huge difference, but "full" especially makes less conceptual sense (since overweights by visitation)
# Will likely go with "base" - which is based on the actual
ggplot(kelp_subset3 %>% 
         pivot_longer(cols = c(area_median, area_mean,  site_median, site_mean, full_median, full_mean),
                      names_to = "category",
                      values_to = "biomass_group") %>%
         mutate(site_type = factor(site_type, levels = c("MPA", "Reference")),
                biomass_group = factor(biomass_group, levels = c("Low", "High")),
                category = factor(category, levels = c("area_median", "area_mean",  "site_median", "site_mean", "full_median", "full_mean"))), 
       aes(x = age_at_survey, 
           y = biomass, 
           color = biomass_group, 
           fill = biomass_group, 
           linetype = biomass_group), alpha = 0.8) +
  geom_hline(data = breakpoints, aes(yintercept = breakpoint), linetype = "dotted", color = "gray30") +
  geom_smooth(method = 'lm') +
  labs(x = "Years since implementation",
       y = expression("Biomass (kg per 100m"^2*")"), color = NULL, fill = NULL, linetype = NULL) +
  scale_color_manual(values = c("Low" = "#4292c6", "High" = "#084594")) +
  scale_fill_manual(values = c("Low" = "#9ecae1", "High" = "#084594")) +
  scale_linetype_manual(values = c("Low" = "22", "High" = "solid")) +
  scale_x_continuous(limits = c(0, 20), expand = c(0, 0))+
  theme_minimal() + 
  facet_grid(site_type ~ category) +
  my_theme +
  theme(panel.spacing = unit(0.9, "lines"))


fig3 <- ggplot(kelp_subset3 %>% mutate(site_type = factor(site_type, levels = c("MPA", "Reference"))),
       aes(x = age_at_survey, y = biomass, color = area_median, fill = area_median, linetype = area_median), alpha = 0.8) +
  geom_smooth(method = 'lm') +
  labs(x = "Years since implementation",
       y = expression("Biomass (kg per 100m"^2*")"), 
       color = expression("Baseline \ncategory"), fill = expression("Baseline \ncategory"), linetype = expression("Baseline \ncategory")) +
  scale_color_manual(values = c("Low" = "#4292c6", "High" = "#084594")) +
  scale_fill_manual(values = c("Low" = "#9ecae1", "High" = "#084594")) +
  scale_linetype_manual(values = c("Low" = "22", "High" = "solid")) +
  scale_x_continuous(limits = c(0, 20), expand = c(0, 0.3))+
  theme_minimal() + 
  facet_wrap(~site_type) +
  my_theme +
  theme(panel.spacing = unit(0.9, "lines"))
                 
fig3
ggsave("baseline-fig3.png", plot = fig3, width = 6, height = 4, units = "in", dpi = 600)


## 2. Quantile Options --------

# kelp_subset4 <- kelp_subset2 %>% 
#   mutate(area_quant3 = cut(bb, breaks = quantile(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"], probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), labels = c("Low", "Mid", "High"), include.lowest = TRUE),
#          area_quant5 = cut(bb, breaks = quantile(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"], probs = c(0, 1/5, 2/5, 3/5, 4/5, 1), na.rm = TRUE), labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High"), include.lowest = TRUE),
#          site_quant3 = cut(bb, breaks = quantile(kelp_subset2 %>% distinct(site, bb) %>% pull(bb), probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), labels = c("Low", "Mid", "High"), include.lowest = TRUE),
#          site_quant5 = cut(bb, breaks = quantile(kelp_subset2 %>% distinct(site, bb) %>% pull(bb), probs = c(0, 1/5, 2/5, 3/5, 4/5, 1), na.rm = TRUE), labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High"), include.lowest = TRUE),
#          full_quant3 = cut(bb, breaks = quantile(bb, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), labels = c("Low", "Mid", "High"), include.lowest = TRUE),
#          full_quant5 = cut(bb, breaks = quantile(bb, probs = c(0, 1/5, 2/5, 3/5, 4/5, 1), na.rm = TRUE), labels = c("Low", "Low-Mid", "Mid", "Mid-High", "High"), include.lowest = TRUE),
#   )
# 
# 
# ggplot(kelp_subset4 %>% 
#        pivot_longer(cols = c(area_quant3, area_quant5, site_quant3, site_quant5, full_quant3, full_quant5),
#                     names_to = "category",
#                     values_to = "biomass_group"),
#        aes(x = age_at_survey, y = biomass, fill = biomass_group, color = biomass_group)) +
#   geom_smooth(method = "lm") +
#   facet_wrap(site_type ~ category)


# ggplot(data = kelp_subset4, 
#                aes(x = age_at_survey, 
#                    y = log_c_biomass, color = site_type, fill = site_type)) +
#  # geom_jitter(alpha = 0.2, size = 1) +
#   geom_smooth(method = 'lm') +
#   labs(x = "Years since implementation",
#        y = "Biomass (kg per 100m2)", color = NULL, fill = NULL) +
#   scale_color_manual(values = c("#7e67f8", "#e5188b")) +
#   scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
#   theme_minimal() + 
#   facet_wrap(~site_quant5, nrow = 1) +
#   my_theme
# 
# fig3
# # ggsave("baseline-fig3.png", plot = fig3, width = 6, height = 4, units = "in", dpi = 600)

# 
# ggplot(data = kelp_subset4, 
#        aes(x = age_at_survey, 
#            y = log_c_biomass, color = site_type, fill = site_type)) +
#   geom_jitter(alpha = 0.2, size = 1) +
#   geom_smooth(method = 'lm') +
#   labs(x = "Years since implementation",
#        y = "Biomass (kg per 100m2)", color = NULL, fill = NULL) +
#   scale_color_manual(values = c("#7e67f8", "#e5188b")) +
#   scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
#   theme_minimal() + 
#   facet_wrap(~affiliated_mpa) +
#   my_theme


# Model  --------------------------------------------------------------------------

base_model <- lmer(log_c_biomass ~ site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year), 
                   data = kelp_subset2)

summary(base_model)  


# Log-transform baseline biomass using same constant
kelp_subset2$log_c_bb <- log(kelp_subset2$bb + const)

# Center baseline biomass
kelp_subset2$log_c_bb_scaled <- scale(kelp_subset2$log_c_bb)

base_biomass <- lmer(log_c_biomass ~ log_c_bb * site_type * age_at_survey  + 
                       (1|region4/affiliated_mpa/site) + (1|year), 
                     data = kelp_subset2)

summary(base_biomass)

base_biomass2 <- lmer(log_c_biomass ~ log_c_bb_scaled * site_type * age_at_survey  + 
                       (1|region4/affiliated_mpa/site) + (1|year), 
                     data = kelp_subset2)

summary(base_biomass2)

plot(allEffects(base_biomass), x.var = "age_at_survey", multiline = T, confint = list(style = "auto"), rows = 1)
effects_list <- allEffects(base_biomass, partial.residuals = T, 
                           xlevels = list(log_c_bb = round(log(quantile(kelp_baseline$bb[kelp_baseline$mpa_defacto_class=="smr"],
                                                                        probs = c(0, 1/4, 2/4, 3/4, 9/10)) + const), digits = 1), 
                                          age_at_survey = 50)) 
                           
plot(effects_list, multiline = T,
     x.var = "age_at_survey",
     confint = list(style = "auto"), 
     lattice = list(strip = list(cex = 0.8)),
     main = NULL, layout = c(NA, 1))


effects_list_df <- data.frame(effects_list$`log_c_bb:site_type:age_at_survey`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         bb = round(exp(log_c_bb) - const, 3))

bb_vals <- unique(effects_list_df$bb)

fig4 <- ggplot(data = effects_list_df %>% 
         filter(bb > bb_vals[1]) %>% 
         mutate(bb = case_when(bb == bb_vals[2] ~ paste0("25% (Baseline = ", round(bb_vals[2], 1), ")"),
                               bb == bb_vals[3] ~ paste0("50% (Baseline = ", round(bb_vals[3], 1), ")"),
                               bb == bb_vals[4] ~ paste0("75% (Baseline = ", round(bb_vals[4], 1), ")"),
                               bb == bb_vals[5] ~ paste0("90% (Baseline = ", round(bb_vals[5], 1), ")"))),
       aes(x = age_at_survey,
           y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = "identity") +
  geom_smooth(aes(color = site_type), method = "loess") +
  facet_wrap(~bb, nrow = 1) +
  labs(x = "Years since implementation",
       y = expression("Biomass (kg per 100m"^2*")"), color = NULL, fill = NULL) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  theme_minimal() +
  theme(legend.position = "top")+ 
  my_theme

fig4

ggsave("baseline-fig4.png", plot = fig4, width = 8, height = 4, units = "in", dpi = 600)


# Make the plot for the SI that shows more levels
effects_list_full <- allEffects(base_biomass, partial.residuals = T, 
                                xlevels = list(log_c_bb = round(log(quantile(kelp_baseline$bb[kelp_baseline$mpa_defacto_class=="smr"],
                                                                             probs = seq(0, 1, by = 0.05)) + const), digits = 3), 
                                               age_at_survey = 50)) 

effects_list_full_df <- data.frame(effects_list_full$`log_c_bb:site_type:age_at_survey`) %>% 
  mutate(fit = exp(fit) - const,
         lower = exp(lower) - const,
         upper = exp(upper) - const,
         bb = round(exp(log_c_bb) - const, 3)) %>% 
  mutate(line_type = if_else(site_type == "Reference" & bb > max(kelp_baseline$bb[kelp_baseline$site_type == "Reference" & kelp_baseline$mpa_defacto_class == "smr"]), TRUE, FALSE))
  
bb_vals_full <- sort(unique(effects_list_full_df$bb))

mpa_counts <- kelp_baseline %>%
  filter(mpa_defacto_class == "smr" & site_type == "MPA") %>% 
  mutate(bb_bin = cut(bb, breaks = bb_vals_full, include.lowest = TRUE, right = FALSE, labels = bb_vals_full[-1])) %>%
  add_count(bb_bin, name = "mpa_count") %>%
  dplyr::select(bb_bin, mpa_count) %>% 
  mutate(bb = as.numeric(as.character(bb_bin))) %>% distinct()

ref_counts <- kelp_baseline %>%
  filter(mpa_defacto_class == "smr" & site_type == "Reference") %>% 
  mutate(bb_bin = cut(bb, breaks = bb_vals_full, include.lowest = TRUE, right = FALSE, labels = bb_vals_full[-1])) %>%
  add_count(bb_bin, name = "ref_count") %>% 
  dplyr::select(bb_bin, ref_count) %>% 
  mutate(bb = as.numeric(as.character(bb_bin))) %>% distinct()


effects_list_full_df2 <- effects_list_full_df %>%
  left_join(mpa_counts, by = "bb") %>% 
  left_join(ref_counts, by = "bb") %>% 
  mutate(mpa_count = if_else(is.na(mpa_count), 0, mpa_count),
         ref_count = if_else(is.na(ref_count), 0, ref_count)) %>% 
  mutate(mpa_count = paste0("MPA = ", mpa_count),
         ref_count = paste0("REF = ", ref_count)) %>% 
  mutate(n_label = paste0(mpa_count, ", ", ref_count))


fig4_si <- ggplot(data = effects_list_full_df2 %>% 
                    mutate(bb = fct_reorder(paste0(seq(0, 100, by = 5)[match(bb, bb_vals_full)], "% (Baseline = ", round(bb, 1), ")"), log_c_bb)) %>% 
                    filter(log_c_bb > min(log_c_bb)),
               aes(x = age_at_survey,
                   y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = "identity") +
  geom_smooth(aes(color = site_type, linetype = line_type), method = "loess") +
  geom_text(aes(x = -Inf, y = Inf, label = n_label, hjust = -0.1, vjust = 1.4), size = 2.5, color = "grey40") +
  facet_wrap(~bb, nrow = 5) +
  labs(x = "Years since implementation",
       y = expression("Biomass (kg per 100m"^2*")"), 
       color = NULL, fill = NULL, linetype = NULL) +
  guides(linetype = "none") +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  coord_cartesian(ylim = c(0, 15)) +
  theme_minimal() +
  theme(legend.position = "top")+ 
  my_theme

fig4_si

ggsave("baseline-si-fig4.png", plot = fig4_si, width = 8, height = 8, units = "in", dpi = 600)




coef_table <- tidy(base_model, conf.int = TRUE, effect = "fixed") %>%
  mutate(term = str_replace(term, "typeMPA", "type"),
         importance = 1) %>% 
  janitor::clean_names() %>% 
  clean_terms() %>%
  add_significance() %>%
  mutate(key = "Base Model")


coef_table2 <- tidy(base_biomass, conf.int = TRUE, effect = "fixed") %>%
  mutate(term = str_replace(term, "typeMPA", "type"),
         importance = 1) %>% 
  janitor::clean_names() %>% 
  clean_terms() %>%
  add_significance() %>%
  mutate(key = "Top Model")


all <- bind_rows(coef_table, coef_table2) %>% 
  mutate(term_revised = if_else(is.na(term_revised), term, term_revised)) %>% 
  mutate(term_revised = str_replace_all(term_revised, "log_c_bb", "Baseline")) %>% 
  dplyr::select(term_revised, estimate, std_error, statistic, df, p_value, significance, key) 


gt_table <- all %>% 
  filter(term_revised != "(Intercept)") %>% 
  mutate(p_value = case_when(p_value < 0.001 ~ "< 0.001", T~as.character(round(p_value, 3)))) %>%
  mutate(significance = if_else(significance == "NS", NA_character_, significance)) %>%
  mutate(term_revised = str_replace_all(term_revised, "site_type", "Protected Status") %>% 
           str_replace_all("age_at_survey", "MPA Age") %>% 
           str_replace_all(":", " x ")) %>% 
  gt() %>%
  cols_label(term_revised = "Term",
             estimate = "Estimate",
             std_error = "Std. Error",
             statistic = "Statistic",
             df = "df",
             p_value = "p-value",
             significance = "") %>%
  fmt_number(columns = c(estimate, std_error, statistic),  decimals = 3) %>%
  fmt_number(columns = c(df),  decimals = 0) %>%
  sub_missing(columns = everything(), missing_text = "") %>%
  tab_options(heading.align = "left") %>%
  cols_align(align = "center", columns = everything()) %>% 
  tab_row_group(label = "Model without baseline", rows = key == "Base Model") %>%
  tab_row_group(label = "Baseline-informed model", rows = key == "Top Model") %>% 
  cols_hide(key) %>% 
  tab_source_note(source_note = paste0("Random effects: region/MPA/site, year")) %>% 
  tab_style(style = cell_text(font = "Arial", size = px(12)), 
            locations = cells_source_notes()) %>%
  tab_style(style = cell_text(font = "Arial", size = px(12)), 
            locations = cells_body(columns = everything())) %>%
  tab_style(style = cell_text(font = "Arial", size = px(12)), 
            locations = cells_row_groups()) %>%
  tab_style(style = cell_text(weight = "bold"),
            locations = cells_body(columns = c(p_value, significance), rows = significance %in% c("***", "**", "*") )) %>%
  tab_style(style = cell_text(font = "Arial", size = px(12), weight = "bold"), 
            locations = cells_column_labels(columns = everything())) %>%
  tab_style(style = cell_text(font = "Arial", size = px(13), weight = "bold"),
            locations = cells_title(groups = "title")) %>% 
  tab_options(table.width = pct(50)) %>% 
  tab_options(data_row.padding = px(6),
              row_group.padding = px(6))

gt_table

gtsave(gt_table, "baselines-table1.png", vwidth = 1200, vheight = 1000)




# For the simpler model
emm_simple <- emmeans(base_model, ~ site_type | age_at_survey, at = list(age_at_survey = 10))
contrast(emm_simple, method = "revpairwise")

# For the full model — need to fix bb at a value or average over it
emm_full <- emmeans(base_biomass, ~ site_type | age_at_survey,
                    at = list(age_at_survey = 10, log_c_bb = mean(kelp_subset2$log_c_bb)))
contrast(emm_full, method = "revpairwise")


# Constants
ages <- seq(0, 15, by = 1)

# Data grid for the simpler model (no baseline needed)
new_simple <- expand.grid(
  site_type = c("MPA", "Reference"),
  age_at_survey = ages
)

# Data grid for the full model (include baseline biomass)
new_full <- expand.grid(
  site_type = c("MPA", "Reference"),
  age_at_survey = ages,
  bb = bb_vals
) %>%
  mutate(log_c_bb = log(bb + const))  # match transformed scale in model

# Simple model
new_simple$pred <- predict(base_model, newdata = new_simple, re.form = NA)
new_simple$biomass <- exp(new_simple$pred) - const

# Full model
new_full$pred <- predict(base_biomass, newdata = new_full, re.form = NA)
new_full$biomass <- exp(new_full$pred) - const

# Simple model difference
diff_simple <- new_simple %>% 
  dplyr::select(age_at_survey, site_type, biomass) %>% 
  pivot_wider(names_from = site_type, values_from = biomass) %>%
  mutate(delta = MPA - Reference,
         model = "Without baseline",
         bb = NA)  # No baseline stratification

# Complex model difference
diff_full <- new_full %>%
  select(age_at_survey, site_type, bb, biomass) %>%
  pivot_wider(names_from = site_type, values_from = biomass) %>%
  mutate(delta = MPA - Reference,
         model = "With baseline")

bind_rows(diff_simple, diff_full) %>%
  filter(is.na(bb) | bb > 1) %>% 
  mutate(bb = factor(bb, levels = rev(sort(unique(bb))))) %>%
  ggplot(aes(x = age_at_survey, y = delta, color = factor(bb), linetype = model)) +
  geom_line(size = 1) +
  labs(x = "Years since implementation",
       y = expression("MPA effect (kg per 100m2)"),
       color = "Baseline biomass",
       linetype = "Model") +
  theme_minimal() +
  scale_color_manual(values = c("#084594", "#2171b5", "#4292c6", "#6baed6", "#9ecae1"), na.value = "black") +
  scale_linetype_manual(values = c("With baseline" = "solid", "Without baseline" = "22")) +
  my_theme

r2_nakagawa(base_model)

base_biomass <- lmer(log_c_biomass ~ log_c_bb * site_type * age_at_survey  + 
                     + (1|affiliated_mpa/site) + (1|year), 
                     data = kelp_subset2)
summary(base_biomass)

r2_nakagawa(base_biomass)


library(broom.mixed)

# Get fitted values and residuals from the full model
model_aug <- augment(base_biomass) %>%
  rename(predicted = .fitted,
         residual = .resid,
         observed = log_c_biomass) %>% 
  mutate(baseline_biomass = exp(log_c_bb) - const,
         affiliated_label = fct_reorder(str_to_title(affiliated_mpa) %>% 
                                          str_replace_all("Smr", "SMR") %>% 
                                          str_replace_all("Smca", "SMCA"), baseline_biomass))

si2 <- ggplot(data = model_aug, aes(x = age_at_survey, color = site_type, fill = site_type)) +
  geom_point(aes(y = observed), alpha = 0.2) +
  geom_smooth(aes(y = observed, linetype = "Observed"), method = "lm", se = F) +
  geom_smooth(aes(y = predicted, linetype = "Predicted"), method = "lm", se = F) +
  facet_wrap(~affiliated_label, scales = "free_y", ncol = 4) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  scale_fill_manual(values = c("#7e67f8", "#e5188b")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "22")) +
  labs(x = "Years since implementation",
       y = "Log Biomass", color = NULL, fill = NULL, linetype = NULL) +
  guides(linetype = guide_legend(override.aes = list(color = "grey30"))) +
  my_theme +
  theme(legend.position = "top",
        strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 8))

si2 
ggsave("baseline-si-fig2.png", plot = si2, width = 7.5, height = 7.5, units = "in", dpi = 600)

mpa_trajectory <- model_aug %>%
  group_by(affiliated_mpa, affiliated_label, site_type, age_at_survey) %>%
  summarise(mean_observed = mean(observed),
            mean_predicted = mean(predicted),
            mean_residual = mean(residual),
            baseline_biomass = mean(baseline_biomass),.groups = "drop") %>%
  mutate(deviation = mean_observed - mean_predicted)




mpa_diff <- mpa_trajectory %>%
  dplyr::select(affiliated_mpa, site_type, age_at_survey, mean_observed, mean_predicted) %>%
  pivot_wider(
    names_from = site_type,
    values_from = c(mean_observed, mean_predicted),
    names_glue = "{.value}_{site_type}"
  ) %>%
  mutate(
    obs_diff = mean_observed_MPA - mean_observed_Reference,
    pred_diff = mean_predicted_MPA - mean_predicted_Reference,
    deviation = obs_diff - pred_diff
  )

# Then summarize
mpa_diff_summary <- mpa_diff %>%
  group_by(affiliated_mpa) %>%
  summarise(
    avg_deviation = mean(deviation, na.rm = T),
    .groups = "drop"
  )

ggplot(mpa_diff_summary, aes(x = reorder(affiliated_mpa, avg_deviation), y = avg_deviation)) +
  geom_col(fill = "darkgreen") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(y = "Observed – Predicted Difference in MPA Effect", x = "MPA") +
  theme_minimal()



summary_deviation <- mpa_trajectory %>%
  filter(site_type == "MPA", age_at_survey >= 5) %>%
  group_by(affiliated_mpa) %>%
  summarise(
    avg_deviation = mean(deviation, na.rm = T),
    .groups = "drop"
  )

ggplot(summary_deviation, aes(x = reorder(affiliated_mpa, avg_deviation), y = avg_deviation)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  coord_flip() +
  labs(y = "Average Deviation (Observed - Predicted)", x = "MPA") +
  theme_minimal()

# Get MPA-level baseline biomass
mpa_baseline <- model_aug %>%
  filter(site_type == "MPA") %>%
  group_by(affiliated_mpa) %>%
  summarise(
    baseline_biomass = mean(baseline_biomass),
    .groups = "drop"
  )

# Join with deviation summary
mpa_diff_summary <- mpa_diff_summary %>%
  left_join(mpa_baseline, by = "affiliated_mpa")


library(ggrepel)

ggplot(mpa_diff_summary, aes(x = baseline_biomass, y = avg_deviation*-1)) +
  geom_point() +
  geom_text_repel(aes(label = affiliated_mpa)) +
  # geom_text_repel(data = ~filter(.x, label_flag), aes(label = affiliated_mpa)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  labs(
    x = "Baseline Biomass (kg per 100m²)",
    y = "Predicted - Observed Difference in MPA Effect") +
  theme_minimal()


threshold <- sd(mpa_diff_summary$avg_deviation, na.rm = TRUE) * 1.5

mpa_diff_summary <- mpa_diff_summary %>%
  mutate(label_flag = abs(avg_deviation - mean(avg_deviation, na.rm = TRUE)) > threshold)

# Calculate full model predictions and deviations
data_with_resid <- kelp_subset2 %>%
  mutate(predicted_full = predict(base_biomass),                   # full model prediction (incl. random effects)
         deviation = log_c_biomass - predicted_full,               # observed - predicted
         affiliated_label = fct_reorder(str_to_title(affiliated_mpa) %>%
                                          str_replace_all("Smr", "SMR") %>%
                                          str_replace_all("Smca", "SMCA"), predicted_full)
  )

# Plot deviations over time, faceted by MPA
ggplot(data_with_resid, aes(x = age_at_survey, y = deviation, color = site_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.3) +
  geom_smooth(aes(group = site), method = "lm", se = FALSE, linewidth = 0.5) +
  facet_wrap(~ affiliated_label, scales = "free_y") +
  scale_color_manual(values = c("MPA" = "#7e67f8", "Reference" = "#e5188b")) +
  labs(x = "Years since implementation", y = "Observed – Predicted", color = "Site Type") +
  theme_minimal()



plot(base_biomass, partial.residuals = T)






# Focus on MPA sites with low baseline biomass
low_bb_mpa_threshold <- quantile(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr" & kelp_baseline$site_type == "MPA"], probs = 0.25)
low_bb_threshold <- quantile(model_aug$baseline_biomass[model_aug$site_type == "MPA"], probs = 0.25)

low_bb_mpa <- model_aug %>%
  filter(site_type == "MPA", baseline_biomass <= low_bb_mpa_threshold)

# Plot observed vs. predicted biomass over time
ggplot(low_bb_mpa, aes(x = age_at_survey)) +
  geom_point(aes(y = observed), alpha = 0.5, color = "black") +
  geom_line(aes(y = predicted), color = "blue", alpha = 0.7) +
  facet_wrap(~site, scales = "free_y") +
  labs(
    x = "Years Since Protection",
    y = "Log Biomass (observed vs. predicted)",
    title = "Observed vs. Predicted Biomass for Low-Baseline MPA Sites"
  ) +
  theme_minimal()

mpa_level_df <- model_aug %>%
  group_by(affiliated_mpa, site_type, age_at_survey) %>%
  summarise(
    observed_mean = mean(observed, na.rm = TRUE),
    predicted_mean = mean(predicted, na.rm = TRUE),
    residual_mean = mean(residual, na.rm = TRUE),
    baseline_biomass = mean(baseline_biomass, na.rm = TRUE)
  ) %>%
  ungroup()


low_bb_mpas <- mpa_level_df %>%
  filter(site_type == "MPA") %>%
  group_by(affiliated_mpa) %>%
  summarise(start_bb = first(baseline_biomass)) %>%
  filter(start_bb <= quantile(start_bb, 0.25)) %>%
  pull(affiliated_mpa)

ggplot(mpa_level_df %>% 
         filter(affiliated_mpa %in% low_bb_mpas), aes(x = age_at_survey)) +
  geom_smooth(aes(y = observed_mean), method = "loess", color = "black", se = FALSE, span = 0.75) +
  geom_smooth(aes(y = predicted_mean), method = "loess", color = "blue", se = FALSE, span = 0.75) +
  facet_wrap(~affiliated_mpa, scales = "free_y") +
  labs(
    x = "Years Since Protection",
    y = "Smoothed Log Biomass",
    title = "Smoothed Observed vs. Predicted Biomass (MPA Level)"
  ) +
  theme_minimal()


ggplot(model_aug, aes(x = age_at_survey, y = residual, color = site_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~affiliated_mpa) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  labs(
    x = "Years Since Protection",
    y = "Partial Residual (log biomass)",
    title = "Partial Residuals by Site Type for Each MPA",
    color = "Site Type"
  ) +
  theme_minimal()


# Order affiliated MPAs by mean baseline biomass
mpa_order <- model_aug %>%
  group_by(affiliated_mpa) %>%
  summarise(mean_baseline = mean(baseline_biomass, na.rm = TRUE)) %>%
  arrange(mean_baseline) %>%
  pull(affiliated_mpa)

# Set factor levels for ordered facets
model_aug <- model_aug %>%
  mutate(affiliated_mpa = factor(affiliated_mpa, levels = mpa_order))

# Plot residuals by MPA, ordered by baseline biomass
ggplot(model_aug, aes(x = age_at_survey, y = residual, color = site_type)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey60") +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  facet_wrap(~affiliated_mpa) +
  scale_color_manual(values = c("#7e67f8", "#e5188b")) +
  labs(
    x = "Years Since Protection",
    y = "Partial Residual (log biomass)",
    title = "Partial Residuals by Site Type, Faceted by MPA (Ordered by Baseline Biomass)",
    color = "Site Type"
  ) +
  theme_minimal()


# Filter for MPAs with low baseline biomass and plot predicted trajectories by site type
model_aug %>%
  #group_by(affiliated_mpa) %>%
  # mutate(mean_baseline = mean(baseline_biomass[site_type == "MPA"], na.rm = TRUE)) %>%
  #  ungroup() %>%
  # filter(mean_baseline <= quantile(mean_baseline, 0.25, na.rm = TRUE)) %>%
  filter(log_c_bb <= log(low_bb_mpa_threshold + const)) %>% 
  ggplot(aes(x = age_at_survey, color = site_type)) +
  geom_point(aes(y = observed), alpha = 0.2) +
  geom_smooth(aes(y = observed, linetype = "Observed"), method = "lm", se = FALSE, size = 1) +
  geom_smooth(aes(y = predicted, linetype = "Predicted"), method = "lm", se = FALSE, size = 1) +
  facet_wrap(~affiliated_mpa, scales = "free_y") +
  scale_color_manual(values =c("#7e67f8", "#e5188b")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "22")) +
  labs(
    x = "Duration of protection (years)",
    y = "Log Biomass",
    color = "Site Type",
    linetype = "Trajectory",
  ) +
  theme_minimal() +my_theme



kelp_effort <- kelp_orig %>% 
  # Identify distinct transects - 35251 (after the NA dropped above)
  distinct(year, month, day, site, zone, transect, 
           bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  # Identify distinct "sampling events" (e.g. visits to a site)
  group_by(year, month, day, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation) %>% 
  summarize(n_rep = n(), .groups = 'drop') %>%  # total number of transects for that event
  # Join implementation dates
  left_join(mpas %>% dplyr::select(affiliated_mpa, implementation_date, implementation_year, size_km2, cluster_area_km2)) %>% 
  mutate(survey_date = lubridate::make_date(year, month, day),
         implementation_date = as.Date(implementation_date),
         site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA"),
         age_at_survey_days = as.numeric(survey_date - implementation_date),   # Calculate actual age at survey
         age_at_survey = year - implementation_year, # Calculate age at survey by year
         before = if_else(age_at_survey_days < 0, n_rep, 0),
         after = if_else(age_at_survey_days >= 0, n_rep, 0)) %>% 
  # Drop sites with no affiliation
  filter(!is.na(site_type)) %>% 
  # Keep sites visited at least 5 distinct years after MPA implementation
  group_by(site, site_type, affiliated_mpa) %>% 
  filter(n_distinct(year[after > 0]) >= 5) %>% ungroup() %>% 
  # Keep MPAs that have at least one inside and one outside site
  group_by(affiliated_mpa) %>% 
  filter(n_distinct(site_type) == 2) %>% 
  # Keep MPAs with in/out data within 2 years of implementation
  filter(any(site_type == "MPA" & sum(n_rep[between(age_at_survey_days, -731, 731)]) > 0) &
           any(site_type == "Reference" & sum(n_rep[between(age_at_survey_days, -731, 731)]) > 0)) %>% ungroup()

length(unique(kelp_effort$affiliated_mpa)) # 32 total within 2 years 
length(unique(kelp_effort$affiliated_mpa[kelp_effort$mpa_defacto_class == "smr"])) # 25 SMRs within 2 years


kelp <- kelp_orig %>% 
  group_by(year, month, day, site, bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, target_status) %>% 
  # Total biomass of targeted species per site per **day**
  summarise(weight_kg = sum(weight_kg),
            count = sum(count), .groups = 'drop') %>%
  filter(!target_status == "NO_ORG") %>%    # drop these bc aren't true zeroes
  select(-target_status)

# kelp_missing <- kelp %>%  # 7 sites with no info about inside/outside
#   filter(is.na(site_type)) %>% 
#   group_by(site, affiliated_mpa, mpa_defacto_designation, bioregion, site_type) %>% 
#   summarize(n_years = length(unique(year)),
#             n_fish = sum(total_count))

kelp_complete <- kelp_effort2 %>% 
  # Add counts and weights (those that were not seen will be NA)
  left_join(kelp) %>% 
  # Change NAs to zeroes (those species were not observed in that site-year)
  mutate_at(vars(weight_kg), ~ replace(., is.na(.), 0)) %>% 
  mutate_at(vars(count), ~ replace(., is.na(.), 0)) %>% 
  mutate(site_type = if_else(mpa_defacto_designation == "ref", "Reference", "MPA")) %>% 
  mutate(kg_per_m2 = weight_kg/(n_rep*60), # 30x2x2m but JC says typical density is per m2 (60)
         count_per_m2 = count/(n_rep*60),
         age_at_survey = year - implementation_year) %>% 
  dplyr::select(year, site, site_type, 
                bioregion, region4, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, implementation_year, size_km2, cluster_area_km2,
                age_at_survey, age_at_survey_days, n_rep, weight_kg, count, kg_per_m2, count_per_m2)



# Filter for the surveys within 365 days of implementation
kelp_baseline <- kelp_effort2 %>% 
  filter(between(age_at_survey_days, -365, 365)) %>% 
  group_by(affiliated_mpa, site, mpa_defacto_designation) %>% 
  summarize(n_rep = sum(n_rep))






