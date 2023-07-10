#Joshua G. Smith
#December 13, 2022

rm(list=ls())

#require packages
require(dplyr)
require(here)
require(MASS)
require(ggeffects)

#set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/statewide_data"
figdir <-  here::here("analyses", "5community_climate_ecology", "figures")


#load MPA traits
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")

#load model output
mod_out_raw <- read.csv(file.path(datadir,"mpa_betadisp_mod_run2.csv"))

#connectivity
conn_path <- "/home/shares/ca-mpa/data/sync-data/connectivity"
input_file <- "Settlement_connectivity_by_habitat.csv"  
settle_dat <- read.csv(file.path(conn_path, input_file))

#biomass
biom_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/biomass_processed"
file <- "biomass_lnRR_MPA_means.csv"
biomass <- read.csv(file.path(biom_path, file))
targeted_biomass <- read.csv(file.path(biom_path, "targeted_biomass_lnRR_MPA_means.csv"))

###############################################################################
#process data

#-------------------------------------------------------------------------------
#all fish biomass
#calculate biomass slope pre-MHW for KF fish

kf_biomass <- biomass %>% filter(habitat == "Kelp forest") %>%
  filter(year >= 2007 & year <= 2013)

#quick check of slopes
ggplot(kf_biomass, aes(x=year, y=log_RR))+
  geom_point()+
  ggpmisc::stat_poly_line(formula = y ~ x)+
  facet_wrap(~affiliated_mpa)

#calculate slopes and save to df
slopes <- nlme::lmList(log_RR ~ year | affiliated_mpa, data=kf_biomass)
coef <- data.frame(affiliated_mpa=rownames(coef(slopes)),coef(slopes),check.names=FALSE) %>%
  rename(all_biomass_slope = year)
rownames(coef) <- NULL ## now redundant

#-------------------------------------------------------------------------------
#targeted fish biomass

kf_biomass_targeted <- targeted_biomass %>% filter(habitat == "Kelp forest") %>%
  filter(year >= 2007 & year <= 2013)

#quick check of slopes
ggplot(kf_biomass_targeted, aes(x=year, y=log_RR))+
  geom_point()+
  ggpmisc::stat_poly_line(formula = y ~ x)+
  facet_wrap(~affiliated_mpa)

#calculate slopes and save to df
slopes_targeted <- nlme::lmList(log_RR ~ year | as.factor(affiliated_mpa), data=kf_biomass_targeted, na.action = na.exclude)

coef_targeted <- data.frame(affiliated_mpa=rownames(coef(slopes_targeted)),coef(slopes_targeted),check.names=FALSE) %>%
  rename(targeted_biomass_slope = year)
rownames(coef) <- NULL ## now redundant

#merge

biomass_RR <- left_join(coef, coef_targeted, by="affiliated_mpa") %>%
  dplyr::select(affiliated_mpa, all_biomass_slope, targeted_biomass_slope) 


###############################################################################
###############################################################################
#MODEL 1 ---- DISTANCE-BASED RESPONSE RATIOS 


resist_RR_dat <- mod_out_raw %>% filter(habitat =="Kelp forest fishes" |
                                          habitat=="Kelp forest inverts and algae",
                                        period_2 =="during") %>%
  dplyr::select(habitat, MPA, MPA_type, distance) %>%
  pivot_wider(names_from = MPA_type, values_from = distance) %>%
  drop_na() %>%
  mutate(logRR = log(smr/ref),
         prop_shift = (ref-smr)/ref,
         period = "resistance") 


recover_RR_dat <- mod_out_raw %>% filter(habitat =="Kelp forest fishes" |
                                           habitat=="Kelp forest inverts and algae",
                                         period_2 =="after") %>%
  dplyr::select(habitat, MPA, MPA_type, distance) %>%
  pivot_wider(names_from = MPA_type, values_from = distance) %>%
  drop_na() %>%
  mutate(logRR = log(smr/ref),
         prop_shift = (ref-smr)/ref, 
         period = "recovery") 


RR_dat1 <- rbind(resist_RR_dat, recover_RR_dat)

rocky_join <- mod_out_raw %>% filter(habitat=="Rocky intertidal") %>%
  filter(MPA_type == "smr")%>%
  mutate(ref = NA,
         logRR=NA,
         prop_shift = NA)%>%
  dplyr::select(habitat, MPA, ref, smr = "distance", logRR,
                prop_shift, period = "period_2") %>%
  mutate(period = recode(period, during = "resistance",
                         after = "recovery"))
RR_dat  = rbind(RR_dat1, rocky_join)

#Join model output with MPA traits

#step 1 - merge habitat gen and diversity
mpa_traits1 <- left_join(mpa_attributes_gen, mpa_attributes_hab, by="name")
mpa_traits2 <- left_join(mpa_traits1, mpa_attributes_hab_div, by="name")

#step 2 - merge habitat and proportion rock
mpa_traits3 <- left_join(mpa_traits2, prop_rock, by="name")

#step 3 - merge habitat and fishing effort
mpa_traits4 <- left_join(mpa_traits3, fishing_effort, by="name")

#step 4 - clean up

mpa_traits <- mpa_traits4 %>%
  #select variables of interest
  dplyr::select(region = bioregion.x, affiliated_mpa, implementation_date, size=size_km2.x,
                habitat_richness, habitat_diversity=habitat_diversity_sw, total_rock =total_rock_km2,
                prop_rock, fishing_pressure = annual_avg_lb_sqkm_20002006
  )%>%
  mutate(affiliated_mpa = recode(affiliated_mpa,
                                 "año nuevo smr" = "ano nuevo smr"))


#step 5 - join traits and mod

RR_dat_join <- left_join(RR_dat, mpa_traits, by=c("MPA"="affiliated_mpa")) %>%
  mutate(implementation_year = as.numeric(format(implementation_date,'%Y')),
         habitat_short = ifelse(habitat =="Kelp forest fishes"|
                                  habitat =="Kelp forest inverts and algae","Kelp forest",habitat))


#step 6 - join settlement dat

settle_dat1 <- settle_dat %>% mutate(MPA = tolower(MPA)) %>%
  dplyr::rename("Rocky intertidal" = "Rocky_Intertidal",
                "Kelp forest" = `Shallow.Rocky.Reef..Kelp.and.Rock.`,
                "Shallow rocky reef" = `Rock.30_100m`,
                "Deep reef" = `Rock.100_200m`) %>%
  pivot_longer(cols = c("Rocky intertidal","Kelp forest","Shallow rocky reef","Deep reef"),
               values_to = "settlement", names_to = "habitat_short") %>%
  mutate(MPA = recode(MPA, "campus point smca (no-take)" = "campus point smca",
                      "point vicente smca (no-take)" = "point vicente smca",
                      "blue cavern onshore smca (no-take)" = "blue cavern onshore smca"))

RR_dat_full <- left_join(RR_dat_join, settle_dat1, by=c("MPA","habitat_short"))


##############################################################################
#MODEL 3 - --- LOGISTIC REGRESSION WITH PERMANOVA OUTPUT

resist_logit_dat <- mod_out_raw %>% filter(
  period_2 =="during",
  MPA_type == "smr") %>%
  mutate(stability = ifelse(p.value > 0.05, "yes","no"))%>%
  dplyr::select(habitat, MPA, stability) %>%
  #pivot_wider(names_from = MPA_type, values_from = stability) %>%
  drop_na() %>%
  mutate(
    period = "resistance") 


recover_logit_dat <- mod_out_raw %>% filter(
  period_2 =="after",
  MPA_type == "smr") %>%
  mutate(stability = ifelse(p.value > 0.05, "yes","no"))%>%
  dplyr::select(habitat, MPA, stability) %>%
  #pivot_wider(names_from = MPA_type, values_from = stability) %>%
  drop_na() %>%
  mutate(
    period = "recovery") 


RR_dat1 <- rbind(resist_logit_dat, recover_logit_dat)


#Join model output with MPA traits

RR_logit_join <- left_join(RR_dat1, mpa_traits, by=c("MPA"="affiliated_mpa")) %>%
  mutate(implementation_year = as.numeric(format(implementation_date,'%Y')),
         habitat_short = ifelse(habitat =="Kelp forest fishes"|
                                  habitat =="Kelp forest inverts and algae","Kelp forest",habitat))

#join settlement dat

RR_dat_full <- left_join(RR_logit_join, settle_dat1, by=c("MPA","habitat_short"))

#join biomass slopes for KF fishes

RR_dat_full1 <- left_join(RR_dat_full, biomass_RR, by=c("MPA"="affiliated_mpa")) %>%
  mutate(stability = ifelse(stability == "no",0,1),
         relative_age = 2020-implementation_year)

################################################################################
#perform logistic regression for Kelp forest inverts and algae

kf_invalg_dat <- RR_dat_full1 %>% filter(habitat == "Kelp forest inverts and algae")

library(ggplot2)

# Fit logistic regression models for each period
resist_model <- glm(stability ~ size + habitat_diversity + prop_rock + fishing_pressure + settlement + relative_age,
                    data = kf_invalg_dat,
                    family = "binomial",
                    na.action = na.exclude,
                    subset = period == "resistance")

recovery_model <- glm(stability ~ size + habitat_diversity + prop_rock + fishing_pressure + settlement + relative_age,
                      data = kf_invalg_dat,
                      family = "binomial",
                      na.action = na.exclude,
                      subset = period == "recovery")

# Subset the data for each period
resistance_data <- subset(kf_invalg_dat, period == "resistance")
recovery_data <- subset(kf_invalg_dat, period == "recovery")

# Add predicted probabilities to the data
resistance_data$predicted_stability <- predict(resist_model, newdata = resistance_data, type = "response")
recovery_data$predicted_stability <- predict(recovery_model, newdata = recovery_data, type = "response")

# Remove duplicate columns
resistance_data <- resistance_data[!duplicated(names(resistance_data))]
recovery_data <- recovery_data[!duplicated(names(recovery_data))]

# Combine the data frames back together
kf_invalg_dat <- rbind(resistance_data, recovery_data) 

# Pivot the predictor variables into a single column
kf_invalg_dat_pivot <- kf_invalg_dat %>%
  pivot_longer(cols = c(size, habitat_diversity, prop_rock, fishing_pressure, settlement, relative_age),
               names_to = "predictor",
               values_to = "value")

library(RColorBrewer)

# Define color palette
colors <- brewer.pal(8, "Dark2")[c(2, 3)]

base_theme <-  theme(axis.text=element_text(size=8),
                     #axis.text.y = element_text(angle = 90, hjust = 0.5),
                     axis.title=element_text(size=9),
                     plot.tag=element_blank(), #element_text(size=8),
                     plot.title =element_text(size=9, face="bold"),
                     # Gridlines
                     panel.grid.major = element_line(colour = "transparent"), 
                     panel.grid.minor = element_line(colour = "transparent"), 
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     axis.ticks = element_line(colour = "black"),
                     # Legend
                     legend.key = element_blank(),
                     legend.text=element_text(size=8),
                     legend.title=element_text(size=10),
                     # legend.background = element_rect(fill=alpha('blue', 0)),
                     #facets
                     strip.text = element_text(size=7, hjust=0),
                     strip.background = element_blank()
                     #margins
                     #plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)

# Plot partial regression lines with points and smoothed curves
# Define custom labels for predictor variables
label_names <- c("fishing_pressure" = "Annual landings \n(pounds per km² 2000-2006)",
                 "habitat_diversity" = "Habitat diversity \n(Shannon-Wiener)",
                 "prop_rock" = "Proportion rock \n(relative substrate)",
                 "relative_age" = "MPA age \n(years)",
                 "settlement" = "MPA connectivity \n(settlement contribution)",
                 "size" = "MPA Size \n(km²)")

# Get p-values for each model
resist_p_value <- summary(resist_model)$coefficients[, "Pr(>|z|)"]
recovery_p_value <- summary(recovery_model)$coefficients[, "Pr(>|z|)"]

# Create a data frame for p-values
p_value_df <- data.frame(predictor = names(resist_p_value)[-1], # Exclude "(Intercept)"
                         resistance_p_value = resist_p_value[-1], # Exclude "(Intercept)"
                         recovery_p_value = recovery_p_value[-1]) # Exclude "(Intercept)"

df_with_pval <- left_join(kf_invalg_dat_pivot, p_value_df, by = "predictor")

# Plot with p-values
g2 <- ggplot(df_with_pval, aes(x = value, y = stability)) +
  geom_point(aes(color = period, fill = period), size = 2, alpha=0.3, position = position_jitter(height = 0.001)) +
  stat_smooth(
    aes(y = predicted_stability, color = period, fill = period),
    method = "glm",
    formula = y ~ x,
    method.args = list(family = binomial(link = "logit")),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    title = "Kelp forest inverts and algae",
    y = ""
  ) +
  scale_color_manual(values = colors, name = "Resilience \nattribute", labels = c("Resistance", "Recovery")) +
  scale_fill_manual(values = colors, name = "Resilience \nattribute", labels = c("Resistance", "Recovery")) +
  theme_minimal() +
  facet_wrap(~ predictor, scales = "free", labeller = labeller(predictor = label_names), ncol=3) +
  theme_bw() + base_theme +
  xlab("") +
  geom_text(
    data = df_with_pval %>% filter(period == "resistance") %>% distinct(predictor, .keep_all = TRUE),
    aes(label = sprintf("p-val (resist): %.3f", resistance_p_value)),
    x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 2
  ) +
  geom_text(
    data = df_with_pval %>% filter(period == "recovery") %>% distinct(predictor, .keep_all = TRUE),
    aes(label = sprintf("p-val (recov): %.3f", recovery_p_value)),
    x = Inf, y = Inf, hjust = 1.1, vjust = 3.5, size = 2
  )  # +scale_y_continuous(expand = expansion(mult = c(0.4, 0.4)))

g2



################################################################################
#perform logistic regression for kelp forest fishes

kf_fish_dat <- RR_dat_full1 %>% filter(habitat == "Kelp forest fishes")


# Fit logistic regression models for each period
resist_model <- glm(stability ~ size + habitat_diversity + prop_rock + fishing_pressure + relative_age,
                    data = kf_fish_dat,
                    family = "binomial",
                    na.action = na.exclude,
                    subset = period == "resistance")

recovery_model <- glm(stability ~ size + habitat_diversity + prop_rock + fishing_pressure + relative_age,
                      data = kf_fish_dat,
                      family = "binomial",
                      na.action = na.exclude,
                      subset = period == "recovery")

# Subset the data for each period
resistance_data <- subset(kf_fish_dat, period == "resistance")
recovery_data <- subset(kf_fish_dat, period == "recovery")

# Add predicted probabilities to the data
resistance_data$predicted_stability <- predict(resist_model, newdata = resistance_data, type = "response")
recovery_data$predicted_stability <- predict(recovery_model, newdata = recovery_data, type = "response")

# Remove duplicate columns
resistance_data <- resistance_data[!duplicated(names(resistance_data))]
recovery_data <- recovery_data[!duplicated(names(recovery_data))]

# Combine the data frames back together
kf_fish_dat <- rbind(resistance_data, recovery_data) 

# Pivot the predictor variables into a single column
kf_fish_dat_pivot <- kf_fish_dat %>%
  pivot_longer(cols = c(size, habitat_diversity, prop_rock, fishing_pressure, relative_age),
               names_to = "predictor",
               values_to = "value")

library(RColorBrewer)

# Define color palette
colors <- brewer.pal(8, "Dark2")[c(2, 3)]

# Plot partial regression lines with points and smoothed curves
# Define custom labels for predictor variables
label_names <- c("fishing_pressure" = "Annual landings \n(pounds per km² 2000-2006)",
                 "habitat_diversity" = "Habitat diversity \n(Shannon-Wiener)",
                 "prop_rock" = "Proportion rock \n(relative substrate)",
                 "relative_age" = "MPA age \n(years)",
                 #"settlement" = "MPA connectivity \n(settlement contribution)",
                 "size" = "MPA Size \n(km²)")

# Get p-values for each model
resist_p_value <- summary(resist_model)$coefficients[, "Pr(>|z|)"]
recovery_p_value <- summary(recovery_model)$coefficients[, "Pr(>|z|)"]

# Create a data frame for p-values
p_value_df <- data.frame(predictor = names(resist_p_value)[-1], # Exclude "(Intercept)"
                         resistance_p_value = resist_p_value[-1], # Exclude "(Intercept)"
                         recovery_p_value = recovery_p_value[-1]) # Exclude "(Intercept)"

df_with_pval <- left_join(kf_fish_dat_pivot, p_value_df, by = "predictor")

# Plot with p-values
g3 <- ggplot(df_with_pval, aes(x = value, y = stability)) +
  geom_point(aes(color = period, fill = period), size = 2,alpha=0.3, position = position_jitter(height = 0.001)) +
  stat_smooth(
    aes(y = predicted_stability, color = period, fill = period),
    method = "glm",
    formula = y ~ x,
    method.args = list(family = binomial(link = "logit")),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    title = "Kelp forest fishes",
    y = ""
  ) +
  scale_color_manual(values = colors, name = "Resilience \nattribute", labels = c("Resistance", "Recovery")) +
  scale_fill_manual(values = colors, name = "Resilience \nattribute", labels = c("Resistance", "Recovery")) +
  theme_minimal() +
  facet_wrap(~ predictor, scales = "free", labeller = labeller(predictor = label_names), ncol=3) +
  theme_bw() + base_theme +
  xlab("") +
  geom_text(
    data = df_with_pval %>% filter(period == "resistance") %>% distinct(predictor, .keep_all = TRUE),
    aes(label = sprintf("p-val (resist): %.3f", resistance_p_value)),
    x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 2
  ) +
  geom_text(
    data = df_with_pval %>% filter(period == "recovery") %>% distinct(predictor, .keep_all = TRUE),
    aes(label = sprintf("p-val (recov): %.3f", recovery_p_value)),
    x = Inf, y = Inf, hjust = 1.1, vjust = 3.5, size = 2
  ) + theme(legend.position = "none") # +scale_y_continuous(expand = expansion(mult = c(0.4, 0.4)))

g3

################################################################################
#perform logistic regression for rocky intertidal

rocky_dat <- RR_dat_full1 %>% filter(habitat == "Rocky intertidal")


# Fit logistic regression models for each period
resist_model <- glm(stability ~ size + habitat_diversity + prop_rock + settlement,
                    data = rocky_dat,
                    family = "binomial",
                    na.action = na.exclude,
                    subset = period == "resistance")

recovery_model <- glm(stability ~ size + habitat_diversity + prop_rock + settlement,
                      data = rocky_dat,
                      family = "binomial",
                      na.action = na.exclude,
                      subset = period == "recovery")

# Subset the data for each period
resistance_data <- subset(rocky_dat, period == "resistance")
recovery_data <- subset(rocky_dat, period == "recovery")

# Add predicted probabilities to the data
resistance_data$predicted_stability <- predict(resist_model, newdata = resistance_data, type = "response")
recovery_data$predicted_stability <- predict(recovery_model, newdata = recovery_data, type = "response")

# Remove duplicate columns
resistance_data <- resistance_data[!duplicated(names(resistance_data))]
recovery_data <- recovery_data[!duplicated(names(recovery_data))]

# Combine the data frames back together
rocky_dat <- rbind(resistance_data, recovery_data) 

# Pivot the predictor variables into a single column
rocky_dat_pivot <- rocky_dat %>%
  pivot_longer(cols = c(size, habitat_diversity, prop_rock, settlement),
               names_to = "predictor",
               values_to = "value")

library(RColorBrewer)

# Define color palette
colors <- brewer.pal(8, "Dark2")[c(2, 3)]


# Plot partial regression lines with points and smoothed curves
# Define custom labels for predictor variables
label_names <- c(#"fishing_pressure" = "Annual landings \n(pounds per km² 2000-2006)",
                 "habitat_diversity" = "Habitat diversity \n(Shannon-Wiener)",
                 "prop_rock" = "Proportion rock \n(relative substrate)",
                 #"relative_age" = "MPA age \n(years)",
                 "settlement" = "MPA connectivity \n(settlement contribution)",
                 "size" = "MPA Size \n(km²)")

# Get p-values for each model
resist_p_value <- summary(resist_model)$coefficients[, "Pr(>|z|)"]
recovery_p_value <- summary(recovery_model)$coefficients[, "Pr(>|z|)"]

# Create a data frame for p-values
p_value_df <- data.frame(predictor = names(resist_p_value)[-1], # Exclude "(Intercept)"
                         resistance_p_value = resist_p_value[-1], # Exclude "(Intercept)"
                         recovery_p_value = recovery_p_value[-1]) # Exclude "(Intercept)"

df_with_pval <- left_join(rocky_dat_pivot, p_value_df, by = "predictor")

# Plot with p-values
g1 <- ggplot(df_with_pval, aes(x = value, y = stability)) +
  geom_point(aes(color = period, fill = period), size = 2, alpha=0.3, position = position_jitter(height = 0.001)) +
  stat_smooth(
    aes(y = predicted_stability, color = period, fill = period),
    method = "glm",
    formula = y ~ x,
    method.args = list(family = binomial(link = "logit")),
    se = TRUE,
    alpha = 0.2
  ) +
  labs(
    title = "Rocky intertidal",
    y = ""
  ) +
  scale_color_manual(values = colors, name = "Resilience \nattribute", labels = c("Resistance", "Recovery")) +
  scale_fill_manual(values = colors, name = "Resilience \nattribute", labels = c("Resistance", "Recovery")) +
  theme_minimal() +
  facet_wrap(~ predictor, scales = "free", labeller = labeller(predictor = label_names), ncol=3) +
  theme_bw() + base_theme +
  xlab("") +
  geom_text(
    data = df_with_pval %>% filter(period == "resistance") %>% distinct(predictor, .keep_all = TRUE),
    aes(label = sprintf("p-val (resist): %.3f", resistance_p_value)),
    x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, size = 2
  ) +
  geom_text(
    data = df_with_pval %>% filter(period == "recovery") %>% distinct(predictor, .keep_all = TRUE),
    aes(label = sprintf("p-val (recov): %.3f", recovery_p_value)),
    x = Inf, y = Inf, hjust = 1.1, vjust = 3.5, size = 2
  ) + theme(legend.position = "none") # +scale_y_continuous(expand = expansion(mult = c(0.4, 0.4)))

g1


# Create an empty data frame with the same variables as g1 and g3
dummy_data <- data.frame(variable1 = character(),
                         variable2 = character())

# Create a blank plot with the dummy data
blank_plot <- ggplot(dummy_data) + theme_void()

# Add the blank plot to g1 and g3
g1_combined <- g1 + blank_plot
g3_combined <- g3 + blank_plot

# Arrange the plots using ggarrange
arranged_plots <- ggarrange(g1, g2, g3,
                            ncol = 1, heights = c(1,1,1),
                            widths = c(1,1,1),
                            common.legend = TRUE,
                            legend = "right")

g <- ggpubr::annotate_figure(arranged_plots, left = textGrob("Probability of resistance or recovery", 
                                           rot = 90, vjust = 2, hjust=0.5, gp = gpar(cex = 0.8)
                                           ),
                        bottom = textGrob("Predictor value", hjust=0.8, vjust=-1, gp = gpar(cex = 0.8)
                                          ))

# Print the arranged plots
#print(g)



ggsave(g, filename=file.path(figdir, "FigSX_MPA_features_logistic.png"),
       width=7, height=10, units="in", dpi=600, bg="white")


