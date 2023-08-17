#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, lme4, metafor, emmeans, ggeffects, ggpubr)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) 


################################################################################
#find the latest year for each mpa

filtered_data <- biomass_mod %>%
  filter(age_at_survey > 0)%>%
  filter(target_status == "Targeted")


################################################################################
#Build meta regression

#Step1 approximate the tau between study variance for each year

habitat_year <- filtered_data %>%
  group_by(year) %>%
  do(meta_result =rma(yi, vi, data = .))%>%
  mutate(coef(summary(meta_result)),
         #get sample size from list
         n_mpas = meta_result[["k"]],
         #get tau
         tau2 = meta_result[["tau2"]]) %>%
  data.frame() %>% dplyr::select(year, tau2) 

#Step2 join tau
mod_dat <- left_join(filtered_data, habitat_year, by = "year")


#Step 3 build mixed model
meta_model <- lmer(yi ~ 
                     size+
                     habitat_richness+
                     habitat_diversity+
                     prop_rock +
                     fishing_pressure+
                     age_at_survey+
                     settlement_habitat+
                     settlement_mpa_total+
                     (1 | affiliated_mpa/habitat), 
                   weights = 1 / (vi + tau2),
                    data = mod_dat) #nest habitat within site 

summary(meta_model)

################################################################################
#plot the marginal means

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

# Create a named vector to map original predictor names to display names
display_names <- c("size" = "MPA Size (km²)",
                   "habitat_richness" = "Habitat richness",
                   "habitat_diversity" = "Habitat diversity",
                   "prop_rock" = "Proportion rock",
                   "fishing_pressure" = "Pre-MPA landings",
                   "age_at_survey" = "MPA age (years)",
                   "settlement_habitat" = "Settlement to habitat",
                   "settlement_mpa_total" = "Settlement to MPA")

# Calculate slopes for each predictor
slopes <- sapply(predictor_names, function(predictor_name) {
  effect <- ggpredict(meta_model, terms = predictor_name)
  model <- lm(predicted ~ x, data = effect)
  coef(model)[[2]]  # Extract the slope coefficient
})

# Order predictor names based on slopes
ordered_predictor_names <- predictor_names[order(slopes, decreasing = TRUE)]

# Create a list of ggplot objects, each for a predictor
plot_list <- lapply(ordered_predictor_names, function(predictor_name) {
  effect <- ggpredict(meta_model, terms = predictor_name)
  slope <- slopes[predictor_names == predictor_name]
  
  ggplot(effect, aes(x = x, y = predicted)) +
    geom_ribbon(aes(ymin = conf.low, ymax = conf.high), fill = "gray80", alpha = 0.6) +
    geom_line(color = "black", size = 1) +
    labs(x = display_names[predictor_name], y = "") +
    geom_text(x = Inf, y = Inf, hjust = 1.1, vjust = 1.5, label = sprintf("Coef: %.4f", slope), size=2) +
    theme_bw() +
    theme(legend.position = "none") + my_theme  # Remove legend
})

# Combine plots using facets
g <- gridExtra::grid.arrange(grobs = plot_list, ncol = 2) 

g_annotate <- ggpubr::annotate_figure(g,  left = grid::textGrob("Marginal effect", 
                                                          rot = 90, vjust = 2, gp = grid::gpar(cex = 0.7)))
                                       


ggsave(g_annotate, filename=file.path(fig_dir, "Fig4_meta_regression.png"), bg = "white",
       width=4, height=5, units="in", dpi=600) 


