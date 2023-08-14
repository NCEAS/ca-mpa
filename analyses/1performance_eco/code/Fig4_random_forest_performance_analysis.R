
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
data_orig <- readRDS(file.path(datadir, "biomass_with_moderators.Rds")) %>% 
  mutate(target_status=ifelse(is.na(target_status), "Targeted", target_status))


# Loop through habitats
################################################################################

# Habitats
habitats <- sort(unique(data_orig$habitat))

# Loop through habitats
i <- 3
for(i in 1:length(habitats)){
  
  set.seed(1985)
  
  # Habitat
  habitat_do <- habitats[i]
  
  # Subset data
  sdata <- data_orig %>% 
    filter(habitat==habitat_do  & target_status=="Targeted" & age_at_survey>0 & !is.na(habitat_richness))
  
  # Fit model
  rf_fit <- randomForest::randomForest(logRR ~ size + age_at_survey + habitat_richness + habitat_diversity + fishing_pressure + prop_rock, data=sdata)
  
  # Inspect importance
  # randomForest::importance(rf_fit, type=2, scale=T)
  # randomForest::varImpPlot(rf_fit)
  hab_imp <-   randomForest::importance(rf_fit, type=2, scale=T) %>% 
    as.data.frame() %>% 
    rownames_to_column(var="variable") %>% 
    mutate(habitat=habitat_do) %>% 
    dplyr::select(habitat, variable, IncNodePurity)
  
  # Inspect marginal effects
  variables <- c("size", "age_at_survey", "habitat_richness", "habitat_diversity", "fishing_pressure", "prop_rock")
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
  }else{
    data_imp <- bind_rows(data_imp, hab_imp)
    data_marg <- bind_rows(data_marg, marg_effects)
  }
  
}


# Format data
################################################################################

# Format variable importance
data_imp1 <- data_imp %>% 
  # Rename
  rename(importance=IncNodePurity) %>% 
  # Format habitat
  mutate(habitat=factor(habitat, levels=c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef"))) %>% 
  # Format variable
  mutate(variable=recode_factor(variable,
                                "age_at_survey"="MPA age (yr)",
                                "size"="MPA area (sqkm)",
                                "fishing_pressure"="Local pre-MPA landings (lbs)",
                                "habitat_diversity"="Habitat diversity", 
                                "habitat_richness"="Habitat richness",
                                "prop_rock"="Proportion rock")) %>% 
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
                                "age_at_survey"="MPA age (yr)",
                                "size"="MPA area (sqkm)",
                                "fishing_pressure"="Local pre-MPA landings (lbs)",
                                "habitat_diversity"="Habitat diversity", 
                                "habitat_richness"="Habitat richness",
                                "prop_rock"="Proportion rock"))




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

# Variable importance
g1 <- ggplot(data_imp1, aes(y=importance_scaled, 
                            x=tidytext::reorder_within(variable, -importance_scaled, habitat),
                            fill=habitat)) +
  facet_wrap(~habitat, nrow=1, scales="free_x") +
  geom_bar(stat="identity") + 
  # Labels
  labs(x="", y="Trait importance\n(scaled to max value)", tag="A") +
  # Scales
  tidytext::scale_x_reordered() +
  # Legend
  scale_fill_discrete(name="", guide="none") +
  # Theme
  theme_bw() + my_theme +
  theme(axis.title.x=element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1))
g1


# Marginal effects
g2 <- ggplot(data_marg1, aes(x=value, y=effect, color=habitat)) +
  # Facet
  facet_wrap(~variable, ncol=3, scales="free") +
  # Reference line
  geom_hline(yintercept = 0, linetype="dashed", color="grey60") +
  # Data
  geom_line() +
  # Labels
  labs(x="Trait value", y="Marginal effect\n(on the log-response ratio)", tag="B") +
  lims(x=c(0, NA)) +
  # Legend
  scale_color_discrete(name="Habitat") +
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
ggsave(g, filename=file.path(plotdir, "Fig4_mpa_trait_impact.png"), 
       width=6.5, height=6.5, units="in", dpi=600)






