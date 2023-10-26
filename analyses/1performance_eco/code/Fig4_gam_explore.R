#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, mgcv, ggeffects, mgcViz)

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
mod_dat <- left_join(filtered_data, habitat_year, by = "year")
          

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

summary(meta_gam_model)


viz <- getViz(meta_gam_model)
print(plot(viz, allTerms = T), pages = 1)

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

trt <- plot(viz, allTerms = T) +
  l_points(shape = 19, size =0.5) +
  l_fitLine(linetype = "solid", color = "indianred", size=1.5)  +
  l_ciLine(linetype = "dashed", color = "navyblue", size = 1) +
  l_ciBar() +
  l_rug() +
  theme_bw() +
  my_theme

print(trt, pages = 1)




trt <- plot(viz, allTerms = T) +
  l_dens(type = "cond") +
  l_fitLine(linetype = 2, color = "indianred")  +
  l_ciLine(linetype = 3, color = "navyblue") +
  theme_bw() +
  my_theme

print(trt, pages = 1)

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



