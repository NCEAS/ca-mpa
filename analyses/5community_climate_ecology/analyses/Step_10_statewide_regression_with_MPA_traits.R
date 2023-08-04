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
                dplyr::select(affiliated_mpa, all_biomass_slope, targeted_biomass_slope) %>%
                mutate(habitat = "Kelp forest fishes")


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

#step 7 - join biomass slopes for KF fishes


RR_dat_full1 <- left_join(RR_dat_full, biomass_RR, by=c("habitat","MPA"="affiliated_mpa"))



#-------------------------------------------------------------------------------
#build model for kelp forest fishes

kf_fish_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Kelp forest fishes') 
hist(kf_fish_resist$logRR)

kf_fish_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == "Kelp forest fishes")
hist(kf_fish_resil$logRR)

#resistance 
kf_fish_resist_mod <-glm(logRR ~ size + 
                                    habitat_diversity + prop_rock + 
                                    fishing_pressure*region + settlement +
                                    targeted_biomass_slope + region, 
                                  data = kf_fish_resist, #add region or lat?
                                  family="gaussian", 
                                  na.action = na.exclude) %>% stepAIC()
summary(kf_fish_resist_mod)

pdp::partial(kf_fish_resil_mod, pred.var = c("habitat_diversity"), plot = TRUE)

rf <- randomForest::randomForest(logRR ~ size + 
                                   habitat_diversity + prop_rock + 
                                   fishing_pressure*region + settlement +
                                   targeted_biomass_slope + region, data = kf_fish_resist, na.action = na.exclude)

pdp::partial(rf, pred.var = c("fishing_pressure"), plot = TRUE)
pdp::partial(rf, pred.var = c("habitat_diversity"), plot = TRUE)
pdp::partial(rf, pred.var = c("region"), plot = TRUE)
pdp::partial(rf, pred.var = c("settlement"), plot = TRUE)

#resilience
kf_fish_resil_mod <- glm(smr ~ size + habitat_richness +
                           habitat_diversity + prop_rock + 
                           fishing_pressure + settlement +
                           targeted_biomass_slope + region, 
                         data = kf_fish_resil, #add region or lat?
                         family="gaussian", 
                         na.action = na.exclude) %>% stepAIC()

summary(kf_fish_resil_mod)


#-------------------------------------------------------------------------------
#build model for kelp forest inverts and algae

kf_invalg_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Kelp forest inverts and algae')

kf_invalg_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == "Kelp forest inverts and algae")

#resistance 
kf_invalg_resist_mod <-glm(logRR ~ size + habitat_richness +
                           habitat_diversity + prop_rock + settlement + region, 
                         data = kf_invalg_resist, #add region or lat?
                         family="gaussian", 
                         na.action = na.exclude) %>% stepAIC()
summary(kf_invalg_resist_mod)

#resilience
kf_invalg_resil_mod <- glm(smr ~ size + habitat_richness +
                           habitat_diversity + prop_rock + 
                           fishing_pressure + settlement, 
                         data = kf_invalg_resil, #add region or lat?
                         family="gaussian", 
                         na.action = na.exclude) 

summary(kf_fish_resil_mod)


####NOTE: rocky intertidal does not have paired reference sites, so
#can't run response ratio for individual MPAs

###############################################################################
###############################################################################
#MODEL 2 - --- RAW DISTANCE REGRESSION for MPAs only

#-------------------------------------------------------------------------------
#build model for kelp forest fishes

kf_fish_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Kelp forest fishes')
hist(kf_fish_resist$smr)

kf_fish_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == "Kelp forest fishes")
hist(kf_fish_resil$smr)

#resistance 
kf_fish_resist_mod <-glm(smr ~ size + habitat_richness +
                           habitat_diversity + prop_rock + 
                           fishing_pressure + settlement +
                           targeted_biomass_slope, 
                         data = kf_fish_resist, #add region or lat?
                         family="gaussian", 
                         na.action = na.exclude)  
summary(kf_fish_resist_mod)
qqnorm(residuals(kf_fish_resist_mod, type="deviance"))
abline(a=0,b=1)


#resilience
kf_fish_resil_mod <- glm(smr ~ size + habitat_richness +
                           habitat_diversity + prop_rock + 
                           fishing_pressure + settlement +
                           targeted_biomass_slope, 
                         data = kf_fish_resil, #add region or lat?
                         family="gaussian", 
                         na.action = na.exclude) 

summary(kf_fish_resil_mod)




#-------------------------------------------------------------------------------
#build model for kelp forest inverts and algae

kf_invalg_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Kelp forest inverts and algae')

kf_invalg_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == "Kelp forest inverts and algae")

#resistance 
kf_invalg_resist_mod <-glm(smr ~ size + habitat_richness +
                             habitat_diversity + prop_rock + 
                             fishing_pressure + settlement, 
                           data = kf_invalg_resist, #add region or lat?
                           family="gaussian", 
                           na.action = na.exclude) %>% stepAIC()
summary(kf_invalg_resist_mod)

#resilience
kf_invalg_resil_mod <- glm(smr ~ size + habitat_richness +
                             habitat_diversity + prop_rock + 
                             fishing_pressure + settlement, 
                           data = kf_invalg_resil, #add region or lat?
                           family="gaussian", 
                           na.action = na.exclude) %>% stepAIC()

summary(kf_fish_resil_mod)



#-------------------------------------------------------------------------------
#build model for rocky intertidal


rocky_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Rocky intertidal')

rocky_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == "Rocky intertidal")

#resistance 
rocky_resist_mod <-glm(smr ~ size + habitat_richness +
                             habitat_diversity + prop_rock + 
                             fishing_pressure + settlement, 
                           data = rocky_resist, #add region or lat?
                           family="gaussian", 
                           na.action = na.exclude)
summary(rocky_resist_mod)

#resilience
rocky_resil_mod <- glm(smr ~ size + habitat_richness +
                             habitat_diversity + prop_rock + 
                             fishing_pressure + settlement, 
                           data = rocky_resil, #add region or lat?
                           family="gaussian", 
                           na.action = na.exclude) %>% stepAIC()

summary(kf_fish_resil_mod)

###############################################################################
###############################################################################
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

RR_dat_full1 <- left_join(RR_dat_full, biomass_RR, by=c("habitat","MPA"="affiliated_mpa")) %>%
                  mutate(stability = ifelse(stability == "no",0,1),
                         relative_age = 2020-implementation_year)



#-------------------------------------------------------------------------------
#build model for kelp fishes

#build model for kelp forest fishes

kf_fish_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Kelp forest fishes') %>% drop_na()

kf_fish_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == "Kelp forest fishes") %>% drop_na()

#resistance 
kf_fish_resist_mod <-glm(stability ~ size + 
                           habitat_diversity + prop_rock + 
                           log(fishing_pressure) + settlement,
                         data = kf_fish_resist, #add region or lat?
                         family="binomial", 
                         na.action = na.exclude) 
                        
summary(kf_fish_resist_mod)



#resilience

kf_fish_resil_mod <- glm(stability ~ size + habitat_richness +
                           habitat_diversity +
                            prop_rock + settlement + 
                           sqrt(fishing_pressure),
                         data = kf_fish_resil, #add region or lat?
                         family="binomial", 
                         na.action = na.exclude) 

summary(kf_fish_resil_mod)


################################################################################
#Plot kelp fish

# Create a data frame with the combined data for both models
data_combined <- rbind(
  data.frame(
    model = "resistance",
    stability = kf_fish_resist$stability,
    size = kf_fish_resist$size,
    habitat_diversity = kf_fish_resist$habitat_diversity,
    prop_rock = kf_fish_resist$prop_rock,
    fishing_pressure = log(kf_fish_resist$fishing_pressure),
    settlement = kf_fish_resist$settlement
  ),
  data.frame(
    model = "recovery",
    stability = kf_fish_resil$stability,
    size = kf_fish_resil$size,
    habitat_diversity = kf_fish_resil$habitat_diversity,
    prop_rock = kf_fish_resil$prop_rock,
    fishing_pressure = sqrt(kf_fish_resil$fishing_pressure),
    settlement = kf_fish_resil$settlement
  )
)

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
                     strip.text = element_text(size=7, hjust=0, face="bold"),
                     strip.background = element_blank()
                     #margins
                     #plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)



library(RColorBrewer)

# Define the color palette
colors <- brewer.pal(8, "Dark2")

# Perform logistic regression for each model separately
resist_model <- glm(stability ~ size + habitat_diversity + prop_rock + log(fishing_pressure) + settlement,
                    data = kf_fish_resist,
                    family = "binomial",
                    na.action = na.exclude)
recov_model <- glm(stability ~ size + habitat_richness + habitat_diversity + prop_rock + settlement + sqrt(fishing_pressure),
                   data = kf_fish_resil,
                   family = "binomial",
                   na.action = na.exclude) 

# Get the p-values for each model
p_values <- list()
for (var in names(data)[-c(1, 2)]) {  # Exclude "model" and "stability" variables
  resist_p_value <- ifelse(var %in% rownames(summary(resist_model)$coefficients),
                           summary(resist_model)$coefficients[rownames(summary(resist_model)$coefficients) == var, "Pr(>|z|)"],
                           NA)
  resil_p_value <- ifelse(var %in% rownames(summary(recov_model)$coefficients),
                          summary(recov_model)$coefficients[rownames(summary(recov_model)$coefficients) == var, "Pr(>|z|)"],
                          NA)
  p_values[[var]] <- list(resistance = resist_p_value, resilience = resil_p_value)
}

kelp_fish_plots <- lapply(names(data)[-c(1, 2)], function(var) {  # Exclude "model" and "stability" variables
  ggplot(data_combined, aes_string(x = var, y = "stability", color = "model")) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), aes(fill = model), alpha = 0.2) +
    geom_point() +
    labs(x = ifelse(var == "habitat_diversity", "Habitat diversity",
                    ifelse(var == "prop_rock", "Proportion rock",
                           ifelse(var == "fishing_pressure", "Historic fishing pressure",
                                  ifelse(var == "settlement", "MPA connectivity", var)
                           )
                    )
    ),
    y = "") +
    scale_color_manual(values = colors[1:2], labels = c("Resistance", "Recovery"),
                       guide = guide_legend(title = "Resilience component",
                                            override.aes = list(color = colors[1:2]))) +
    scale_fill_manual(values = colors[1:2], labels = c("Resistance", "Recovery"),
                      guide = guide_legend(title = "Resilience component",
                                           override.aes = list(fill = colors[1:2]))) +
    theme_bw() + base_theme +
    annotate("text", x = max(data_combined[[var]]) * 0.9, y = max(data_combined$stability) * 0.9,
             label = paste0("p-val (resist): ", ifelse(is.na(p_values[[var]]$resistance), "NA", round(p_values[[var]]$resistance, 3))),
             size = 3) +
    annotate("text", x = max(data_combined[[var]]) * 0.9, y = max(data_combined$stability) * 0.8,
             label = paste0("p-value (recov): ", ifelse(is.na(p_values[[var]]$resilience), "NA", round(p_values[[var]]$resilience, 3))),
             size = 3)
})




# Remove legends from individual plots
plots_nolegend <- lapply(plots, function(plot) plot + theme(legend.position = "none"))

# Arrange the plots in a grid layout without individual legends
g_merge <- grid.arrange(grobs = plots_nolegend, ncol = 2)

# Create a common legend
legend <- cowplot::get_legend(plots[[1]])

# Add the legend to the combined plot
g_with_legend <- cowplot::plot_grid(g_merge, legend, nrow = 1, rel_widths = c(0.8, 0.2))

# Annotate the figure
g_with_legend_kelp_fish <- annotate_figure(g_with_legend, bottom = textGrob("MPA feature"), left = textGrob("Probability of resistance or resilience", rot = 90))


#-------------------------------------------------------------------------------
#build model for invalg


kf_invalg_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Kelp forest inverts and algae') %>% dplyr::select(!(c(all_biomass_slope, targeted_biomass_slope)))%>%
           drop_na()

kf_invalg_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == 'Kelp forest inverts and algae') %>% dplyr::select(!(c(all_biomass_slope, targeted_biomass_slope)))%>%
        drop_na()

#resistance 
kf_invalg_resist_mod <-glm(stability ~ size + 
                           habitat_diversity + prop_rock + 
                           fishing_pressure + region + settlement + relative_age, 
                         data = kf_invalg_resist, #add region or lat?
                         family="binomial", 
                         na.action = na.exclude) 
summary(kf_invalg_resist_mod)

#resilience

kf_invalg_resil_mod <- glm(stability ~ size + habitat_richness +
                           habitat_diversity + prop_rock + 
                           fishing_pressure + settlement + settlement + relative_age, 
                         data = kf_invalg_resil, 
                         family="binomial", 
                         na.action = na.exclude) %>% stepAIC()

summary(kf_invalg_resil_mod)

################################################################################
#Plot kelp invalg

# Create a data frame with the combined data for both models
data_combined <- rbind(
  data.frame(
    model = "resistance",
    stability = kf_invalg_resist$stability,
    size = kf_invalg_resist$size,
    habitat_diversity = kf_invalg_resist$habitat_diversity,
    prop_rock = kf_invalg_resist$prop_rock,
    fishing_pressure = log(as.numeric(kf_invalg_resist$fishing_pressure)),
    settlement = kf_invalg_resist$settlement,
    age = kf_invalg_resist$relative_age
  ),
  data.frame(
    model = "recovery",
    stability = kf_invalg_resil$stability,
    size = kf_invalg_resil$size,
    habitat_diversity = kf_invalg_resil$habitat_diversity,
    prop_rock = kf_invalg_resil$prop_rock,
    fishing_pressure = log(as.numeric(kf_invalg_resil$fishing_pressure)),
    settlement = kf_invalg_resil$settlement,
    age = kf_invalg_resil$relative_age
  )
)

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
                     strip.text = element_text(size=7, hjust=0, face="bold"),
                     strip.background = element_blank()
                     #margins
                     #plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)



library(RColorBrewer)

# Define the color palette
colors <- brewer.pal(8, "Dark2")

# Perform logistic regression for each model separately
resist_model <- glm(stability ~ size + 
                      habitat_diversity + prop_rock + 
                      fishing_pressure + settlement + relative_age, 
                    data = kf_invalg_resist, #add region or lat?
                    family="binomial", 
                    na.action = na.exclude) 
recov_model <-  glm(stability ~ size + 
                      habitat_diversity + prop_rock + 
                      fishing_pressure + settlement + relative_age, 
                    data = kf_invalg_resil, 
                    family="binomial", 
                    na.action = na.exclude)

# Get the p-values for each model
p_values <- list()
for (var in names(data)[-c(1)]) {  # Exclude "model" and "stability" variables
  resist_p_value <- ifelse(var %in% rownames(summary(resist_model)$coefficients),
                           summary(resist_model)$coefficients[rownames(summary(resist_model)$coefficients) == var, "Pr(>|z|)"],
                           NA)
  resil_p_value <- ifelse(var %in% rownames(summary(recov_model)$coefficients),
                          summary(recov_model)$coefficients[rownames(summary(recov_model)$coefficients) == var, "Pr(>|z|)"],
                          NA)
  p_values[[var]] <- list(resistance = resist_p_value, resilience = resil_p_value)
}

kelp_invalg_plots <- lapply(names(data)[-c(1)], function(var) {  # Exclude "model" and "stability" variables
  ggplot(data_combined, aes_string(x = var, y = "stability", color = "model")) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), aes(fill = model), alpha = 0.2) +
    geom_point() +
    labs(x = ifelse(var == "habitat_diversity", "Habitat diversity",
                    ifelse(var == "prop_rock", "Proportion rock",
                           ifelse(var == "fishing_pressure", "Historic fishing pressure",
                                  ifelse(var == "settlement", "MPA connectivity",
                                         ifelse(var == "relative_age", "Relative age", var)
                                  )
                           )
                    )
    ),
    y = "") +
    scale_color_manual(values = colors[1:2], labels = c("Resistance", "Recovery"),
                       guide = guide_legend(title = "Resilience component",
                                            override.aes = list(color = colors[1:2]))) +
    scale_fill_manual(values = colors[1:2], labels = c("Resistance", "Recovery"),
                      guide = guide_legend(title = "Resilience component",
                                           override.aes = list(fill = colors[1:2]))) +
    theme_bw() + base_theme +
    annotate("text", x = max(data_combined[[var]]) * 0.9, y = max(data_combined$stability) * 0.9,
             label = paste0("p-val (resist): ", ifelse(is.na(p_values[[var]]$resistance), "NA", round(p_values[[var]]$resistance, 3))),
             size = 3) +
    annotate("text", x = max(data_combined[[var]]) * 0.9, y = max(data_combined$stability) * 0.8,
             label = paste0("p-value (recov): ", ifelse(is.na(p_values[[var]]$resilience), "NA", round(p_values[[var]]$resilience, 3))),
             size = 3)
})



# Remove legends from individual plots
plots_nolegend <- lapply(kelp_invalg_plots, function(plot) plot + theme(legend.position = "none"))

# Arrange the plots in a grid layout without individual legends
g_merge <- grid.arrange(grobs = plots_nolegend, ncol = 2)

# Create a common legend
legend <- cowplot::get_legend(kelp_invalg_plots[[1]])

# Add the legend to the combined plot
g_with_legend <- cowplot::plot_grid(g_merge, legend, nrow = 1, rel_widths = c(0.8, 0.2))

# Annotate the figure
g_with_legend_kelp_invalg <- annotate_figure(g_with_legend, bottom = textGrob("MPA feature"), left = textGrob("Probability of resistance or resilience", rot = 90))




#-------------------------------------------------------------------------------
#build model for rocky intertidal


rocky_resist <- RR_dat_full1 %>%
  filter(period=="resistance",
         habitat == 'Rocky intertidal') %>% dplyr::select(!(c(all_biomass_slope, targeted_biomass_slope)))%>%
  drop_na()

rocky_resil <- RR_dat_full1 %>%
  filter(period=="recovery",
         habitat == 'Rocky intertidal') %>% dplyr::select(!(c(all_biomass_slope, targeted_biomass_slope)))%>%
  drop_na()

#resistance 
rocky_resist_mod <-glm(stability ~ size +
                              prop_rock + habitat_diversity+
                              settlement + region, 
                           data = rocky_resist, #add region or lat?
                           family="binomial", 
                           na.action = na.exclude) %>% stepAIC()
summary(rocky_resist_mod)

#resilience

rocky_resil_mod <-glm(stability ~ size + prop_rock + 
                         settlement + region, 
                       data = rocky_resil, #add region or lat?
                       family="binomial", 
                       na.action = na.exclude) %>% stepAIC()
summary(rocky_resist_mod)



#-----------------------------------------------------------------------------
#test model with everything

resist <- RR_dat_full1 %>%
  filter(period=="resistance") %>% dplyr::select(!(c(all_biomass_slope, targeted_biomass_slope)))%>%
  drop_na()

resil <- RR_dat_full1 %>%
  filter(period=="recovery") %>% dplyr::select(!(c(all_biomass_slope, targeted_biomass_slope)))%>%
  drop_na()

recovery <-glm(stability ~ habitat*size + habitat*habitat_diversity +
                         habitat*prop_rock + 
                         habitat*settlement + habitat,
                       data = resil, #add region or lat?
                       family="binomial", 
                       na.action = na.exclude) %>% stepAIC()
summary(recovery)






