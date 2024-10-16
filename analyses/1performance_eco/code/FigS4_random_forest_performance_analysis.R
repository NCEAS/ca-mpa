
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(randomForest)

# Directories
datadir <- "analyses/1performance_eco/output"
plotdir <- "analyses/1performance_eco/figures"

# Read data
data_orig <- readRDS(file.path(datadir, "biomass_with_moderators_new2.Rds")) %>% 
  #restrict to targeted species and no-take MPAs
  filter(target_status == "Targeted" & mpa_defacto_class == "smr")%>%
  #drop missing values
  na.omit()%>%
  #filter to include most recent year only
  group_by(habitat, mpa) %>%
  filter(year == max(year)) %>%
  #remove extreme outliers
  filter(fishing_pressure < 2.0e+06, #these are a few extreme outliers
         settlement_habitat < 400)%>%
  ungroup()



# Loop through habitats
################################################################################

# Habitats
habitats <- sort(unique(data_orig$habitat))

# Loop through habitats
i <- 4
for(i in 1:length(habitats)){
  
  set.seed(1985)
  
  # Habitat
  habitat_do <- habitats[i]
  
  # Subset data
  sdata <- data_orig %>% 
    filter(habitat==habitat_do)%>%
    data.frame()
  
  # Fit model
  rf_fit <- randomForest::randomForest(yi ~ size + age_at_survey + habitat_richness + habitat_diversity + fishing_pressure + prop_rock +
                                         settlement_habitat + settlement_mpa_total, data=sdata,
                                       ntree=1501)
  
  # Inspect fit
  preds <- predict(rf_fit, sdata)
  obs <- sdata$yi
  r2 <- 1 - sum((obs-preds)^2)/sum((obs-mean(obs))^2)
  r2_df <- tibble(habitat=habitat_do,
                  r2=r2)
  
  # Inspect importance
  # randomForest::importance(rf_fit, type=2, scale=T)
  # randomForest::varImpPlot(rf_fit)
  hab_imp <-   randomForest::importance(rf_fit, type=2, scale=T) %>% 
    as.data.frame() %>% 
    rownames_to_column(var="variable") %>% 
    mutate(habitat=habitat_do) %>% 
    dplyr::select(habitat, variable, IncNodePurity)
  
  # Inspect marginal effects
  variables <- c("size", "age_at_survey", "habitat_richness", "habitat_diversity", 
                 "fishing_pressure", "prop_rock", "settlement_habitat","settlement_mpa_total")
  for(j in seq_along(variables)){
    parts <- randomForest::partialPlot(x=rf_fit, pred.data=sdata, x.var=variables[j], main=variables[j], plot = F)
    df <- tibble(habitat=habitat_do,
                 variable=variables[j],
                 value=parts$x,
                 effect=parts$y)
    if(j==1){marg_effects <- df}else{marg_effects <- bind_rows(marg_effects, df)}
  }
  
  # Merge
  if(i == 1){
    data_imp <- hab_imp
    data_marg <- marg_effects
    data_r2 <- r2_df
  }else{
    data_imp <- bind_rows(data_imp, hab_imp)
    data_marg <- bind_rows(data_marg, marg_effects)
    data_r2 <- bind_rows(data_r2, r2_df)
  }
  
}



# Format data
################################################################################

# Format r2
data_r2_use <- data_r2 %>% 
  # Format habitat
  mutate(habitat=factor(habitat, levels=c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))) 

# Format variable importance by habitat
data_imp1 <- data_imp %>% 
  # Rename
  dplyr::rename(importance=IncNodePurity) %>% 
  # Format habitat
  mutate(habitat=factor(habitat, levels=c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))) %>% 
  # Format variable
  mutate(variable=recode_factor(variable,
                                "age_at_survey"="MPA age (year)",
                                "size"="MPA area (km²)",
                                "fishing_pressure"="Local pre-MPA landings (lbs)",
                                "habitat_diversity"="Habitat diversity", 
                                "habitat_richness"="Habitat richness",
                                "prop_rock"="Proportion rock",
                                "settlement_habitat" = "Settlement to ecosystem",
                                "settlement_mpa_total" = "Settlement to MPA")) %>% 
  # Scale
  arrange(habitat, desc(importance)) %>%  
  group_by(habitat) %>% 
  mutate(importance_scaled=importance/max(importance)) %>% 
  ungroup() %>% 
  # Add label
  mutate(hab_var=paste(habitat, variable, sep="-"),
         hab_var1=factor(hab_var, levels=hab_var, labels=variable))

# Format marginal effects
data_marg1 <- data_marg %>% 
  # Format habitat
  mutate(habitat=factor(habitat, levels=c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))) %>% 
  # Format variable
  mutate(variable=recode_factor(variable,
                                "age_at_survey"="MPA age (year)",
                                "size"="MPA area (km²)",
                                "fishing_pressure"="Local pre-MPA landings (lbs)",
                                "habitat_diversity"="Habitat diversity", 
                                "habitat_richness"="Habitat richness",
                                "prop_rock"="Proportion rock",
                                "settlement_habitat" = "Settlement to ecosystem",
                                "settlement_mpa_total" = "Settlement to MPA"))


# Calculate average importance for each variable across habitats
variable_importance_rank <- data_imp1 %>%
  mutate(variable=recode_factor(variable,
                                "age_at_survey"="MPA age (year)",
                                "size"="MPA area (km²)",
                                "fishing_pressure"="Local pre-MPA landings (lbs)",
                                "habitat_diversity"="Habitat diversity", 
                                "habitat_richness"="Habitat richness",
                                "prop_rock"="Proportion rock",
                                "settlement_habitat" = "Settlement to ecosystem",
                                "settlement_mpa_total" = "Settlement to MPA"))%>%
  group_by(variable) %>%
  dplyr::summarize(average_importance = mean(importance, na.rm = TRUE)) %>%
  arrange(desc(average_importance)) %>%
  mutate(rank = row_number()) %>%
  dplyr::select(variable, rank) 

#join overall rank order 
data_marg2 <- left_join(data_marg1, variable_importance_rank, by = "variable")



# Format data
################################################################################

# Setup theme
my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=7),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=7),
                   plot.tag=element_text(size=8),
                   plot.title = element_blank(),
                   plot.subtitle = element_blank(),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   # Facets
                   strip.background = element_blank(),
                   strip.text=element_text(size=6, face = "bold")
)

habitat_colors <- c('Surf zone' = '#7f6d95', 
                    'Kelp forest' = '#568736', 
                    'Shallow reef' = '#458eae', 
                    'Deep reef' = '#d98644')

# Variable importance
g1 <- ggplot(data_imp1, aes(y=importance_scaled, 
                            x=tidytext::reorder_within(variable, -importance_scaled, habitat),
                            fill=habitat)) +
  facet_wrap(~habitat, nrow=1, scales="free_x") +
  geom_bar(stat="identity") + 
  # Add r2
  geom_text(data_r2_use, mapping=aes(x=8.5, y=0.95, label=round(r2,2), color=habitat), hjust=1, size=2.8) +
  # Labels
  labs(x="", y="Trait importance\n(scaled to max value)", tag="A") +
  # Scales
  tidytext::scale_x_reordered() +
  # Legend
  scale_fill_manual(values = habitat_colors) +
  scale_color_manual(values = habitat_colors) +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 60, hjust = 1),
        legend.position = "none")
g1


# Marginal effects
g2 <- ggplot(data_marg2, aes(x=value, y=effect, color=habitat)) +
  # Facet
  facet_wrap(~variable, ncol=4, scales="free") +
  # Reference line
  #geom_hline(yintercept = 0, linetype="dashed", color="grey60") +
  # Data
  geom_line(size=1) +
  # Labels
  labs(x="Trait value", y="Partial effect\n(on the log-response ratio)", tag="B",
       color = "Ecosystem") +
  lims(x=c(0, NA)) +
  # Legend
  scale_color_manual(values = habitat_colors) +
  # Theme
  theme_bw() + my_theme +
  theme(legend.position = "bottom",
        legend.margin = ggplot2::margin(-7,0,-2,0)
  )
g2

# Merge
g <- gridExtra::grid.arrange(g1, g2, heights=c(0.42, 0.58))
g




# Export
ggsave(g, filename=file.path(plotdir, "Fig5_random_forest.png"), 
       width=7, height=6.5, units="in", dpi=600)






