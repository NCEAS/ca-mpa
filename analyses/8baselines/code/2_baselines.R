# Baselines Analyses
# Cori Lopazanski
# Feb 2025

library(tidyverse)
library(lmerTest)
library(performance)
library(patchwork)
library(broom.mixed)
library(emmeans)
library(broom)
library(gt)
library(effects)

# Setup ------------------------------------------------------------------------

rm(list = ls())
gc()

my_theme <- theme_minimal(base_family = "Arial") + 
  theme(plot.title = element_text(size = 10, face = "bold"),
        plot.subtitle = element_text(size = 8),
        axis.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10),
        legend.title = element_text(size = 10),
        legend.text = element_text(size = 10),
        legend.margin = margin(t = 0, unit='cm'),
        plot.caption = element_text(size = 8),
        strip.text = element_text(size = 8, face = "bold"),
        panel.background = element_rect(fill = "white", color = NA),  
        plot.background = element_rect(fill = "white", color = NA))

#source("analyses/7habitat/code/Step4a_prep_focal_data.R")
source("analyses/7habitat/code/Step0_helper_functions.R")

out.dir <-  "~/ca-mpa/analyses/8baselines/output"
fig.dir <- "~/ca-mpa/analyses/8baselines/figures"

kelp_complete <- readRDS(file.path(out.dir, "kelp_complete.Rds"))


# Build data -------------------------------------------------------------------

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

# Add a small constant, defined as minimum observed nonzero value
const <- if_else(min(kelp_subset$biomass) > 0, 0, min(kelp_subset$biomass[kelp_subset$biomass > 0], na.rm = TRUE))
kelp_subset <- kelp_subset %>% mutate(log_c_biomass = log(biomass + const))

length(unique(kelp_subset$site))
length(unique(kelp_subset$affiliated_mpa))
length(unique(kelp_subset$affiliated_mpa[kelp_subset$mpa_defacto_class == "smr"]))

# Calculate baseline biomass ----------

kelp_baseline <- kelp_subset %>%
  # Focus on surveys within 1 year of implementation (-1 to 1 age)
  filter(between(age_at_survey, -1, 1)) %>% 
  # Calculate baseline as the average across all sites in given MPA/Ref area
  group_by(affiliated_mpa, region4, mpa_defacto_class, site_type) %>% 
  summarize(bb      = mean(biomass, na.rm = TRUE),
            bb_se   =  sd(biomass, na.rm = T)/sqrt(n()), 
            n_sites = length(unique(site)),
            n_years = length(unique(year)),
            n = n(),
            min_age = round(min(age_at_survey), 0), .groups = 'drop') %>% 
  group_by(affiliated_mpa, region4) %>% 
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
sd(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"])

length(unique(kelp_baseline$affiliated_mpa[kelp_baseline$mpa_defacto_class != "smr"])) # 7 additional SMCAs

# Get some basic stats for results section -------------------------------------

# Subset
kelp_baseline_smr <- subset(kelp_baseline, mpa_defacto_class == "smr")

# Check normality
shapiro.test(kelp_baseline_smr$bb[kelp_baseline_smr$site_type == "MPA"])
shapiro.test(kelp_baseline_smr$bb[kelp_baseline_smr$site_type == "Reference"])

# Check variance
var.test(bb ~ site_type, data = kelp_baseline_smr)

# Run t-test 
t.test(bb ~ site_type, data = kelp_baseline_smr)

# Get mean and sd
mean(kelp_baseline_smr$bb[kelp_baseline_smr$site_type == "MPA"])
sd(kelp_baseline_smr$bb[kelp_baseline_smr$site_type == "MPA"])

mean(kelp_baseline_smr$bb[kelp_baseline_smr$site_type == "Reference"])
sd(kelp_baseline_smr$bb[kelp_baseline_smr$site_type == "Reference"])

# Check the correlation:
agg <- kelp_baseline_smr %>%
  group_by(affiliated_mpa, site_type) %>%
  summarize(mean_bb = mean(bb, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = site_type, values_from = mean_bb)

cor.test(agg$MPA, agg$Reference, method = "pearson")


# Join baseline info to data ---------------------------------------------------

kelp_subset2 <- kelp_subset %>%
  left_join(kelp_baseline, by = c("affiliated_mpa", "region4", "mpa_defacto_class", "site_type")) %>% 
  # Focus on after-only data
  filter(age_at_survey >= 0) %>% 
  # Focus on SMRs
  filter(mpa_defacto_class == "smr")


## REMINDER: FIGURE 1 IS CONCEPTUAL. START WITH FIGURE 2.

## Figure 2 ----

fig2 <- ggplot(data = kelp_baseline %>% filter(mpa_defacto_class == "smr") %>% 
                 mutate(point_shape = if_else(n < 3, "open", "closed")),
       aes(x = bb, y = affiliated_mpa_label, color = site_type, group = affiliated_mpa_label)) +
  geom_vline(aes(xintercept = median(kelp_baseline$bb[kelp_baseline$mpa_defacto_class == "smr"])), linetype = "55", alpha = 0.4) +
  geom_linerange(aes(xmin = bb - bb_se,
                     xmax = bb + bb_se),
                 linewidth = 1.5, alpha = 0.3) +
  geom_point(size = 2.5) + # aes(shape = point_shape), 
  scale_color_manual(values = c("Reference" = "#6d55aa", "MPA" =  "#c42119")) + # c("Reference" = "#7e67f8", "MPA" =  "#e5188b")
 # scale_shape_manual(values = c("open" = 1, "closed" = 16)) + # c(, "#c42119")
  labs(x = "Baseline biomass (kg per 100m²)", y = NULL, color = NULL) +
  theme_minimal() +
  my_theme + 
  theme(legend.position = "top") 

fig2
ggsave(file.path(fig.dir, "baselines-fig2.png"), plot = fig2, width = 7, height = 5, units = "in", dpi = 600)


# Models  --------------------------------------------------------------------------

# Log-transform baseline biomass using same constant
kelp_subset2$log_c_bb <- log(kelp_subset2$bb + const)

# Fit uniform-effect model
base_model <- lmer(log_c_biomass ~ site_type * age_at_survey + (1|region4/affiliated_mpa/site) + (1|year), 
                   data = kelp_subset2)

# Fit baseline-informed model using log-transformed biomass
base_biomass <- lmer(log_c_biomass ~ log_c_bb * site_type * age_at_survey  + 
                       (1|region4/affiliated_mpa/site) + (1|year), 
                     data = kelp_subset2)

# Singular Fit: MPA-level variance is zero. This does not bias fixed effects 
# coefficients, by retaining we can calculate MPA-level expectations (later)

# base_biomass <- lmer(log_c_biomass ~ log_c_bb * site_type * age_at_survey  + 
#                        (1|region4/site) + (1|year), 
#                      data = kelp_subset2)


## Table 1 (Model Results) -----

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

gtsave(gt_table, file.path(fig.dir, "baselines-table1.png"), vwidth = 1200, vheight = 1000)

## R2 ----
r2_nakagawa(base_model)
r2_nakagawa(base_biomass)

# Need to refit without the MPA RE to get the R2 values:
# base_biomass_r2 <- lmer(log_c_biomass ~ log_c_bb * site_type * age_at_survey  + 
#                           (1|region4/site) + (1|year), 
#                         data = kelp_subset2)
#r2_nakagawa(base_biomass_r2)


## Figure 3 ----

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


fig3 <- ggplot(data = effects_list_df %>% 
                 filter(bb > bb_vals[1]) %>% 
                 mutate(bb_linetype = case_when(bb < 3.3 & age_at_survey > 16 ~ "22", 
                                                T~"solid")) %>% 
                 mutate(bb = case_when(bb == bb_vals[2] ~ paste0("25% (Baseline = ", round(bb_vals[2], 1), ")"),
                                       bb == bb_vals[3] ~ paste0("50% (Baseline = ", round(bb_vals[3], 1), ")"),
                                       bb == bb_vals[4] ~ paste0("75% (Baseline = ", round(bb_vals[4], 1), ")"),
                                       bb == bb_vals[5] ~ paste0("90% (Baseline = ", round(bb_vals[5], 1), ")"))),
               aes(x = age_at_survey,
                   y = fit, fill = site_type)) +
  geom_ribbon(aes(ymin = lower, ymax = upper, fill = site_type),
              alpha = 0.2, stat = "identity") +
  geom_smooth(aes(color = site_type, linetype = bb_linetype), method = "loess") +
  facet_wrap(~bb, nrow = 1) +
  labs(x = "Years since implementation",
       y = expression("Biomass (kg per 100m"^2*")"), color = NULL, fill = NULL, linetype = NULL) +
  scale_linetype_manual(values = c("solid" = "solid", "22" = "22")) +
  guides(linetype = "none") +
  scale_color_manual(values = c("#6d55aa","#c42119")) + #  c("#7e67f8", "#e5188b")
  scale_fill_manual(values =  c("#6d55aa","#c42119")) +
  scale_x_continuous(limits = c(0, 15), expand = c(0,0)) + 
  scale_y_continuous(limits = c(0, 12), expand = c(0,0)) +
  my_theme +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.spacing = unit(1.1, "lines"),
        legend.key.size = unit(1, "lines"))

fig3

ggsave(file.path(fig.dir, "baselines-fig3.png"), plot = fig3, width = 7, height = 3, units = "in", dpi = 600)

## Figure S1 (Partial Effects Extended) ------

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
 # geom_text(aes(x = -Inf, y = Inf, label = n_label, hjust = -0.1, vjust = 1.4), size = 2.5, color = "grey40") +
  facet_wrap(~bb, nrow = 5) +
  labs(x = "Years since implementation",
       y = expression("Biomass (kg per 100m"^2*")"), 
       color = NULL, fill = NULL, linetype = NULL) +
  guides(linetype = "none") +
  scale_color_manual(values = c("#6d55aa","#c42119")) + # c("#7e67f8", "#e5188b") 
  scale_fill_manual(values = c("#6d55aa","#c42119")) +
  coord_cartesian(ylim = c(0, 15), expand = F) +
  scale_x_continuous(limits = c(0, 20), expand = c(0,0))+
  my_theme +
  theme(legend.position = "top",
        panel.spacing = unit(0.75, "lines"),
        legend.key.size = unit(1, "lines"))
 

fig4_si

ggsave(file.path(fig.dir, "baselines-si-fig1.png"), plot = fig4_si, width = 6.5, height = 6.5, units = "in", dpi = 600)


# Extension Analyses ------------


## Estimate the MPA effect across baseline biomass values ----

### Figure 4 ----


# Calculate slope difference (MPA - Reference) at each baseline biomass level
bb_vals_grid <- seq(0.01, 2, by = 0.2)
bb_vals_grid <- c(bb_vals_grid, unique(kelp_subset2$bb[kelp_subset2$site_type == "MPA"]))
log_bb_vals_grid <- log(bb_vals_grid + const)

slope_diffs <- map2_dfr(log_bb_vals_grid, bb_vals_grid, ~{
  emtrends(base_biomass,
           specs = "site_type",
           var = "age_at_survey",
           at = list(log_c_bb = .x)) %>%
    contrast(method = "revpairwise") %>%
    as.data.frame() %>%
    mutate(bb = .y,
           model = "With baseline")
})

slope_diffs <- slope_diffs %>% 
  mutate(mpa_bb = if_else(bb %in% kelp_subset2$bb[kelp_subset2$site_type == "MPA"], 1, 0)) %>% 
  mutate(est_pct = (exp(estimate) - 1) * 100,
         lower_pct = (exp(estimate - 1.96 * SE) - 1) * 100,
         upper_pct = (exp(estimate + 1.96 * SE) - 1) * 100)

# Calculate slope difference from simple model (no baseline)
base_diff <- emtrends(base_model,
                      specs = "site_type",
                      var = "age_at_survey") %>%
  contrast(method = "revpairwise") %>%
  as.data.frame() %>%
  mutate(bb = NA,
         model = "Without baseline",
         est_pct = (exp(estimate) - 1) * 100,
         lower_pct = (exp(estimate - 1.96 * SE) - 1) * 100,
         upper_pct = (exp(estimate + 1.96 * SE) - 1) * 100)

# Plot contrasts with ribbon and transformed values to show percent change
fig4 <- ggplot(slope_diffs, aes(x = bb, y = est_pct)) +
  # Uniform-effect model (95% CI)
  geom_rect(data = base_diff, inherit.aes = FALSE,
            aes(xmin = 0, xmax = max(bb_vals_full),
                ymin = lower_pct, ymax = upper_pct,
                fill = model, linetype = model)) +
  geom_hline(data = base_diff, aes(yintercept = est_pct, linetype = model, color = model), size = 0.8) +
  # Reference line for "No difference" with label + arrow
  geom_hline(yintercept = 0, linetype = "solid", color = "black", size = 0.5) +
  annotate("text", x = 1.5, y = -2, label = "No difference", hjust = 0, size = 3.5) +
  annotate("curve",
           x = 1.45, y = -2,       # starting point (tip of "No")
           xend = 1, yend = -0.05,    # endpoint (reference line)
           curvature = -0.3,         # positive = curve right, negative = left
           arrow = arrow(length = unit(0.15, "cm"), type = "closed"),
           color = "black",
           linewidth = 0.4) +
  # Baseline-informed model (95% CI)
  geom_ribbon(aes(ymin = lower_pct, ymax = upper_pct, fill = model), alpha = 0.2) +
  geom_line(aes(color = model, linetype = model), linewidth = 0.8) +
  geom_point(data = slope_diffs %>% filter(mpa_bb == 1), aes(color = model, fill = model)) +
  labs(
    x = "Baseline biomass (kg per 100 m²)",
    y = "MPA effect (% change in biomass per year)",
    color = NULL, fill = NULL, linetype = NULL
  ) +
  scale_color_manual(values = c("With baseline" = "#084594", "Without baseline" = "black")) +
  scale_fill_manual(values = c("With baseline" = "#084594", "Without baseline" = "grey80")) +
  scale_linetype_manual(values = c("With baseline" = "solid", "Without baseline" = "22")) +
  coord_cartesian(xlim = c(0, 14.45), ylim = c(-5, 30), expand = F) +
  scale_x_continuous(breaks = seq(0, 15, by = 2)) +
  my_theme +
  theme(legend.position = "inside",
        legend.position.inside = c(0.75, 0.8),
        panel.grid.minor = element_blank())

fig4
ggsave(file.path(fig.dir, "baselines-fig4.png"), plot = fig4, width = 4.5, height = 3.5, units = "in", dpi = 600)


## Compare magnitude of MPA-Ref difference through time -----

### Figure 5 ----

# Define baseline biomass values
bb_vals <- unique(effects_list_df$bb)

# Constants
ages <- seq(0, 15, by = 1)

# Data grid for the simpler model
new_simple <- expand.grid(site_type = c("MPA", "Reference"),
                          age_at_survey = ages)

# Data grid for the full model (include baseline biomass)
new_full <- expand.grid(site_type = c("MPA", "Reference"),
                        age_at_survey = ages,
                        bb = bb_vals) %>%
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

fig5 <- bind_rows(diff_simple, diff_full) %>%
  mutate(bb = round(bb, 1)) %>% 
  filter(is.na(bb) | bb > 1) %>% 
  mutate(bb = factor(bb, levels = sort(unique(bb))),
         bb = if_else(is.na(bb), "Without baseline", bb)) %>%
  ggplot(aes(x = age_at_survey, y = delta, color = bb, linetype = bb)) +
  geom_line(size = 1.5) +
  theme_minimal() +
  scale_color_manual(name = "Baseline biomass\n(kg per 100m²)", values = c("#c6dbef", "#6baed6", "#3182bd", "#08519c", "black")) +
  scale_linetype_manual(name = "Baseline biomass\n(kg per 100m²)", values = c("3.2" = "solid", "5.4" = "solid","7.3" = "solid", "8.1" = "solid",  "Without baseline" = "22")) +
  scale_y_continuous(limits = c(0,4.5), expand = c(0,0)) +
  scale_x_continuous(limits = c(0,15), expand = c(0,0)) +
  labs(x = "Years since implementation",
       y = expression("Biomass difference within MPAs (kg per 100m"^2*")"),
       color = expression("Baseline biomass\n(kg per 100m"^2*")")) +
  my_theme + theme(panel.grid.minor = element_blank())

fig5
ggsave(file.path(fig.dir, "baselines-fig5.png"), plot = fig5, width = 5.5, height = 4.5, units = "in", dpi = 600)


## Evaluate MPA deviations ----

### Figure 6 ----

# Get fitted values and residuals from the full model
model_aug <- augment(base_biomass) %>%
  rename(predicted = .fitted,
         residual = .resid,
         observed = log_c_biomass) %>% 
  left_join(kelp_baseline %>% distinct(affiliated_mpa, mpa_bb)) %>% 
  mutate(baseline_biomass = exp(log_c_bb) - const,
         affiliated_label = fct_reorder(str_to_title(affiliated_mpa) %>% 
                                          str_replace_all("Smr", "SMR") %>% 
                                          str_replace_all("Smca", "SMCA"), mpa_bb))

fig6 <- ggplot(data = model_aug, aes(x = age_at_survey, color = site_type, fill = site_type)) +
  geom_point(aes(y = observed), alpha = 0.2) +
  geom_smooth(aes(y = observed, linetype = "Observed"), method = "lm", se = F) +
  geom_smooth(aes(y = predicted, linetype = "Predicted"), method = "lm", se = F) +
  facet_wrap(~affiliated_label, scales = "free_y", ncol = 4) +
  scale_color_manual(values = c("#6d55aa","#c42119")) + # c("#7e67f8", "#e5188b")
  scale_fill_manual(values =  c("#6d55aa","#c42119")) +
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

fig6
ggsave(file.path(fig.dir,"baselines-fig6.png"), plot = fig6, width = 7.5, height = 7.5, units = "in", dpi = 600)


sl <- ggplot(data = model_aug %>% 
               filter(affiliated_label == "South La Jolla SMR"), 
             aes(x = age_at_survey, color = site_type, fill = site_type)) +
  geom_point(aes(y = observed), alpha = 0.3) +
  geom_smooth(aes(y = observed, linetype = "Observed"), method = "lm", se = F) +
  geom_smooth(aes(y = predicted, linetype = "Predicted"), method = "lm", se = F) +
  facet_wrap(~affiliated_label, scales = "free_y", ncol = 4) +
  scale_color_manual(values = c("#6d55aa", "#c42119")) +
  scale_fill_manual(values = c("#6d55aa", "#c42119")) + # c("#7e67f8", "#e5188b")
  scale_linetype_manual(values = c("Observed" = "solid", "Predicted" = "22")) +
  coord_cartesian(xlim = c(0, 11), ylim = c(-2, 2.1)) +
  scale_x_continuous(expand = c(0.01, 0.01))+
  scale_y_continuous(expand = c(0,0))+
  labs(x = "MPA Age (years)",
       y = "Log Biomass", color = NULL, fill = NULL, linetype = NULL) +
  guides(linetype = guide_legend(override.aes = list(color = "grey30"))) +
  my_theme +
  theme(legend.position = "top",
        strip.text = element_text(size = 8),
        axis.text.x = element_text(size = 7),
        axis.text.y = element_text(size = 7),
        legend.text = element_text(size = 8))
sl

ggsave(file.path(fig.dir,"baselines-pres-sl.png"), plot = sl, width = 2.5, height = 2.5, units = "in", dpi = 600)

### Figure 7 -----

# Fit OLS models for each MPA and its reference sites
ols_slopes <- model_aug %>%
  filter(!(affiliated_mpa == "natural bridges smr" & site_type == "MPA" & age_at_survey == 0)) %>% 
  group_by(affiliated_mpa, site_type) %>%
  do(tidy(lm(observed ~ age_at_survey, data = .))) %>%
  filter(term == "age_at_survey") %>%
  select(affiliated_mpa, site_type, estimate) %>%
  pivot_wider(names_from = site_type, values_from = estimate, names_prefix = "slope_") %>%
  mutate(observed_mpa_effect = slope_MPA - slope_Reference)

# Extract expected slopes for each MPA-reference pair at their own baseline biomass
# Get MPA and Ref baseline values separately
baseline_pairs <- kelp_subset2 %>%
  distinct(affiliated_mpa, site_type, log_c_bb) 

expected_effects <- pmap_dfr(
  baseline_pairs,
  function(affiliated_mpa, site_type, log_c_bb) {
    emtrends(base_biomass,
             specs = "site_type",
             var = "age_at_survey",
             at = list(log_c_bb = log_c_bb)) %>%
      as.data.frame() %>%
      filter(site_type == !!site_type) %>%
      mutate(affiliated_mpa = affiliated_mpa,
             site_type_baseline = log_c_bb)
  }
)

expected_effects2 <- expected_effects %>%
  dplyr::select(affiliated_mpa, site_type, trend = age_at_survey.trend, SE, site_type_baseline) %>%
  pivot_wider(names_from = site_type, values_from = c(trend, SE, site_type_baseline)) %>%
  mutate(expected_mpa_effect = trend_MPA - trend_Reference,
    se_expected_mpa_effect = sqrt(SE_MPA^2 + SE_Reference^2)) %>% 
  mutate(log_bb_difference = site_type_baseline_MPA - site_type_baseline_Reference)


expected_effects3 <- expected_effects2 %>% 
  dplyr::select(affiliated_mpa, expected_mpa_effect)


# Join with OLS results and compute deviation
mpa_deviation <- left_join(ols_slopes, expected_effects3, by = "affiliated_mpa") %>%
  mutate(
    deviation = observed_mpa_effect - expected_mpa_effect,
    deviation_pct = (exp(deviation) - 1) * 100
  ) %>% 
  left_join(kelp_baseline %>% distinct(affiliated_mpa, mpa_bb))


# Flag outliers
mpa_deviation <- mpa_deviation %>%
  mutate(deviation_pct = (exp(deviation) - 1) * 100) %>% 
  mutate(flag = factor(case_when(between(deviation_pct, -10, 10) ~ "As expected",
                          deviation_pct <= -10 ~ "Lower than expected",
                          deviation_pct >= 10 ~ "Higher than expected"),
                       levels = c("Higher than expected", "As expected","Lower than expected"))) %>% 
  mutate(affiliated_mpa = str_to_title(affiliated_mpa) %>%
           str_replace_all("Smr", "SMR") %>%
           str_replace_all("Smca", "SMCA"))

mpa_deviation_shading <- data.frame(xmin = c(0, 0, 0),
                                    xmax = c(15, 15, 15),
                                    ymin = c(-10, 10, -22),
                                    ymax = c(10, 22, -10),
                                    fill_color = c("gray90","#c42119", "#6d55aa")) # "#e5188b", "#7e67f8"

# Plot observed vs. expected MPA effects
fig7 <- ggplot() +
  geom_rect(data = mpa_deviation_shading, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                fill = fill_color), alpha = 0.1, show.legend = F) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(data = mpa_deviation,
             aes(x = mpa_bb, y = deviation_pct, color = flag), size = 2.5, show.legend = F) +
  geom_text_repel(data = mpa_deviation %>% mutate(label = if_else(!between(deviation_pct, -5, 5), affiliated_mpa, "")),
                  aes(x = mpa_bb, y = deviation_pct, label = label, color = flag),
                  hjust = -0.1, vjust = 0.5, size = 2.5, show.legend = F) +
  scale_fill_identity() +
  scale_color_manual(values = c("Higher than expected" = "#c42119",
                                "Lower than expected" =  "#6d55aa",
                                "As expected" = "gray50")) +
  scale_x_continuous(limits = c(0, 15), expand = c(0,0)) +
  scale_y_continuous(limits = c(-22,22), expand = c(0,0)) +
  labs(x = "Baseline biomass (kg per 100 m²)",
       y = "Deviation from expected MPA effect\n(% biomass change per year)",
       color = NULL) +
  my_theme + 
  theme(panel.grid.minor = element_blank())# +
 # annotate("text", x = 14.5, y = 21, label = "Stronger MPA effect than predicted", color = "#c42119", hjust = 1, size = 3, fontface = "italic") +
 # annotate("text", x = 14.5, y = -21, label = "Weaker MPA effect than predicted", color =  "#6d55aa", hjust = 1, size = 3, fontface = "italic")

fig7
ggsave(file.path(fig.dir, "baselines-fig7.png"), plot = fig7, width = 5, height = 4, units = "in", dpi = 600)

fig7_si <- ggplot() +
  geom_rect(data = mpa_deviation_shading, 
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, 
                fill = fill_color), alpha = 0.1, show.legend = F) + 
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray50") +
  geom_point(data = mpa_deviation,
             aes(x = mpa_bb, y = deviation_pct, color = flag), size = 2.5, show.legend = F) +
  geom_text_repel(data = mpa_deviation,
                  aes(x = mpa_bb, y = deviation_pct, label = affiliated_mpa, color = flag),
                  hjust = -0.1, vjust = 0.5, size = 2.5, show.legend = F) +
  scale_fill_identity() +
  scale_color_manual(values = c("Higher than expected" = "#c42119",
                                "Lower than expected" =   "#6d55aa",
                                "As expected" = "gray30")) +
  scale_x_continuous(limits = c(0, 15), expand = c(0,0)) +
  scale_y_continuous(limits = c(-22, 22), expand = c(0,0)) +
  labs(x = "Baseline biomass (kg per 100 m²)",
       y = "Deviation from expected MPA effect\n(% biomass change per year)",
       color = NULL) +
  my_theme + 
  theme(panel.grid.minor = element_blank()) +
  annotate("text", x = 14.5, y = 21, label = "Stronger MPA effect than predicted", color ="#c42119", hjust = 1, size = 3, fontface = "italic") +
  annotate("text", x = 14.5, y = -21, label = "Weaker MPA effect than predicted", color =  "#6d55aa", hjust = 1, size = 3, fontface = "italic")

fig7_si
ggsave(file.path(fig.dir, "baselines-si-fig2.png"), plot = fig7_si, width = 6.5, height = 6.5, units = "in", dpi = 600)


