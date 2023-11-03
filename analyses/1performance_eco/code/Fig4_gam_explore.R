#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, mgcv, gratia, ggeffects, mgcViz, tidymv)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators_new.Rds")) 


################################################################################
#find the latest year for each mpa

filtered_data <- biomass_mod %>%
  filter(age_at_survey > 0)%>%
  filter(target_status == "Targeted")

# Step 1: Approximate the tau between study variance for each year
habitat_year <- filtered_data %>%
  group_by(year) %>%
  do(meta_result = mgcv::gam(yi ~ 1, weights = 1 / vi, data = .)) %>%
  mutate(tau2 = deviance(meta_result) / meta_result$df.residual) %>%
  data.frame() %>%
  dplyr::select(year, tau2)

# Step 2: Join tau
mod_dat <- left_join(filtered_data, habitat_year, by = "year") %>%
  filter(settlement_habitat < 400)
          

# Step 3: Build the meta-GAM
meta_gam_model <- gam(yi ~ 
                        s(size) +
                        s(habitat_richness, k =3) +
                        s(habitat_diversity) +
                        s(prop_rock) +
                        s(fishing_pressure) +
                        s(age_at_survey, k =3)+
                        s(settlement_habitat) +
                        s(settlement_mpa_total) +
                        s(year, bs = "cc"),
                      weights = 1 / (vi + tau2),
                      data = mod_dat) 


# Get the summary of the GAM model
summary_info <- summary(meta_gam_model)

# Extract terms and p-values
gam_terms <- data.frame(summary_info$s.table) %>%
          tibble::rownames_to_column(var = "smooth")





################################################################################
#structure data for plotting

sm_dat <- gratia::smooth_estimates(meta_gam_model) %>%
  add_confint()%>%
  pivot_longer(cols = 6:14, names_to = "var", values_to = "var_val") %>%
  left_join(gam_terms, by = "smooth") %>%
  mutate(smooth = str_replace(smooth, "s\\((.+)\\)", "\\1"),
         smooth = str_replace_all(smooth, "_", " "),
         smooth = str_to_sentence(smooth))


#add residuals to data
resid_dat <- mod_dat %>% na.omit() %>% add_partial_residuals(meta_gam_model) %>%
  #make smooth vars longer
  pivot_longer(cols = 33:ncol(.), names_to = "smooth", values_to = "res_val") %>%
  #make predictors longer
  pivot_longer(cols = c("size","habitat_richness","habitat_diversity",
                        "prop_rock","fishing_pressure","age_at_survey",
                        "settlement_habitat","settlement_mpa_total","year"), 
               names_to = "pred_var", values_to = "pred_val")%>%
  dplyr::select(smooth, res_val, pred_var, pred_val) %>%
  mutate(match_var = paste0("s(",pred_var,")"))%>%
  filter(smooth == match_var) %>%
  dplyr::select(smooth, res_val, pred_val)%>%
  mutate(smooth = str_replace(smooth, "s\\((.+)\\)", "\\1"),
         smooth = str_replace_all(smooth, "_", " "),
         smooth = str_to_sentence(smooth))


################################################################################
#plot

my_theme <-  theme(axis.text=element_text(size=4, color = "black"),
                   axis.text.y = element_text(color = "black",size=6),
                   axis.text.x = element_text(color = "black",size=6),
                   axis.title=element_text(size=7, color = "black"),
                   plot.tag=element_text(size= 7, color = "black"), #element_text(size=8),
                   plot.title =element_text(size=6, face="bold", color = "black"),
                   # Gridlines 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.size  = unit(0.5, "lines"), 
                   legend.text = element_text(size = 6, color = "black"),
                   legend.title = element_text(size = 7, color = "black"),
                   #legend.spacing.y = unit(0.75, "cm"),
                   #facets
                   strip.background = element_blank(),
                   strip.placement = "outside",
                   strip.text = element_text(size = 6 , face="plain", color = "black", vjust=4)
)

pval <- sm_dat %>% dplyr::select(smooth, p.value) %>% distinct() %>%
          mutate(color = ifelse(p.value < 0.05, "red","black"),
                 label = ifelse(p.value < 0.001, "p < 0.001", paste("p = ", round(p.value, 3))))

p <- sm_dat %>%
  ggplot() +
  geom_density_2d_filled(aes(x = pred_val, y = res_val), alpha=0.8, data = resid_dat,
                         contour_var = "ndensity", bins=9,
                        show.legend = TRUE)+
  scale_fill_brewer(palette = "Blues")+
  #scale_fill_brewer()+
  geom_ribbon(aes(ymin = lower_ci, ymax = upper_ci, x = var_val), color = "black", linetype = "dashed", fill = NA) +
  #add residuals
  #geom_point(aes(x = pred_val, y = res_val),
   #          data = resid_dat, colour = "steelblue3") +
  geom_line(aes(x = var_val, y = est), lwd = 0.8) +
  facet_wrap(~smooth, scales = "free",
             labeller = as_labeller(
               c("Age at survey" = "Age at survey \n(years)", "Fishing pressure" = "Pre-implementation landings \n(pounds per km²)",
                 "Habitat diversity" = "Habitat diversity \n(Shannon index)","Habitat richness"= "Habitat richness \n(no. habitats)",
                 "Prop rock" = "Rocky substratum \n(proportion)", "Settlement habitat"="Settlement to \nhabitat",
                 "Settlement mpa total" = "Settlement to \nMPA", "Size" = "Size \n(km²)",
                 "Year" = "Year \n")), 
             strip.position = "bottom"
             )+
  #add summary stats
  geom_text(aes(x = -Inf, y = Inf, 
                label = pval$label),
            hjust = -0.2, vjust = 1.3, size = 2.5, data = pval, color = pval$color) +
  labs(y = "Partial effect", x  = "", fill = "Scaled residual \ndensity")+
  theme_bw() + my_theme 
  
p



ggsave(p, filename=file.path(fig_dir, "Fig4_GAM3.png"), bg = "white",
       width=6, height=5, units="in", dpi=600) 











library(manipulate)

manipulate(
  sm_dat %>%
  ggplot() +
    stat_density_2d(geom = "raster", aes(fill = ..density..,
                                         x = pred_val,
                                         y=res_val), contour = F, 
                    data = resid_dat,
                    h = c(x_bandwidth, y_bandwidth),
                    n = grid_points) +
    facet_wrap(~smooth, scales = "free")+
    scale_fill_distiller(palette = "Viridis", direction = -1),
  x_bandwidth = slider(0.1, 20, 1, step = 0.1),
  y_bandwidth = slider(0.1, 20, 1, step = 0.1),
  grid_points = slider(1, 500, 50)
)

################################################################################
#test reduced gam with surface plot

#see https://stackoverflow.com/questions/65918325/how-to-plot-surface-fit-through-3d-data-in-r

# Step 3: Build the meta-GAM
gam_red <- gam(yi ~ 
                       # s(size) +
                        s(habitat_richness, k =3) +
                      #  s(habitat_diversity) +
                      #  s(prop_rock) +
                      #  s(fishing_pressure) +
                        s(age_at_survey, k =3),
                      #  s(settlement_habitat) +
                      #  s(settlement_mpa_total),
                      #  s(year, bs = "cc"), 
                      weights = 1 / (vi + tau2),
                      data = mod_dat)

summary(gam_red)

# Create a grid of values for age_at_survey and settlement_mpa_total
age.seq <- seq(min(mod_dat$age_at_survey, na.rm=TRUE), max(mod_dat$age_at_survey, na.rm=TRUE), length=25)
rich.seq <- seq(min(mod_dat$habitat_richness, na.rm=TRUE), max(mod_dat$habitat_richness, na.rm=TRUE), length=25)

# Function to predict yi values for given age_at_survey and settlement_mpa_total
predfun <- function(x, y) {
  newdat <- data.frame(age_at_survey = x, habitat_richness = y)
  predict(gam_red, newdata = newdat)
}

# Create a grid of predicted values
fit <- outer(age.seq, rich.seq, Vectorize(predfun))

# Create a 3D surface plot
plot_ly() %>% 
  add_markers(x = ~mod_dat$age_at_survey, y = ~mod_dat$habitat_richness, z = ~mod_dat$yi) %>% 
  add_surface(x = age.seq, y = rich.seq, z = t(fit))



