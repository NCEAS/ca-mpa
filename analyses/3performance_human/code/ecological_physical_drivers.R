# Read data
################################################################################

# Clear workspace
rm(list = ls())

#load packages
require(dplyr)
require(stringr)
require(broom)
require(vcdExtra)
require(forcats)
require(MASS)
library(ggeffects)
library(patchwork)


################################################################################

figdir <- "/home/joshsmith/CA_MPA_Project/ca-mpa/analyses/3performance_human/figures" 

# Load data

#charismatic MPAs
data_path <- "/home/joshsmith/CA_MPA_Project/ca-mpa/analyses/3performance_human/output" 
input_file <- "CA_MPA_charisma_key.csv" 
charisma_data <- read.csv(file.path(data_path, input_file))

#MPA attributes
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits/processed"
input_file <- "mpa_attributes_clean.csv" 
mpa_attrib <- read.csv(file.path(data_path, input_file))

#Infrastructure
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits/processed"
input_file <- "mpa_nearby_parks.Rds" 
parks <- readRDS(file.path(data_path,input_file))

#Biological
data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "all_fish_diversity.csv" 
fish_diversity <- read.csv(file.path(data_path, input_file))%>%
  filter(group == 'kelp forest-fish')%>%
  group_by(affiliated_mpa)%>%
  dplyr::summarize(H_fish = mean(mean, na.rm=TRUE))


input_file <- "targeted_nontargeted_fish_biomass.csv"
fish_biomass <- read.csv(file.path(data_path, input_file))%>%
  filter(group == 'kelp')%>%
  group_by(affiliated_mpa, target_status)%>%
  dplyr::summarize(mean_fish_biomass = mean(sum_biomass, na.rm=TRUE))%>%
  pivot_wider(names_from='target_status',values_from="mean_fish_biomass")%>%
  dplyr::mutate(total_fish_biomass = rowSums(across(where(is.numeric))))


bio_drivers <- left_join(fish_diversity,fish_biomass, by='affiliated_mpa')%>%
  dplyr::select(mpa=affiliated_mpa, H_fish, total_fish_biomass)

#Social

data_path <- "/home/shares/ca-mpa/data/sync-data/census_data/processed"
input_file <- "social_vulnerability_index_by_mpa.Rds" 
social_dat <- readRDS(file.path(data_path,input_file))


# Process charismatic MPAs -----------------------------------------------------
#clean up
charisma_data$mpa <- tolower(charisma_data$mpa)
charisma_data$mpa_short <- tolower(charisma_data$mpa_short)
charisma_data$mpa <- recode_factor(charisma_data$mpa, 'año nuevo' = 'ano nuevo')
charisma_data$mpa <- recode_factor(charisma_data$mpa, 'año nuevo smr' = 
                                     'ano nuevo smr')

charisma_data$mpa <- gsub("\\s*\\([^\\)]+\\)","",
                          as.character(charisma_data$mpa))

# Process trait data -----------------------------------------------------------
#select vars
phys_drivers <- mpa_attrib %>%
                  dplyr::select(mpa=name, mpa_class, bioregion, 
                                four_region_north_ci, long, lat,
                                coastal_estuary, implementation_date, 
                                port_size, distance_to_port, size_km2, 
                                shore_span_km, protection,take, sandy_beach_km, 
                                rocky_inter_km, offshore_rock_km,
                                max_kelp_canopy_cdfw_km2, estuary_km2,
                                mpa_within_or_part_of_state_park,
                                mpa_within_or_part_of_nat_marine_sanctuary,
                                total_hard_substrate,total_soft_substrate, 
                                depth_range)
          
#calculate mpa age
phys_drivers <- phys_drivers %>%
                mutate(implementation_year = 
                         format(as.Date(phys_drivers$implementation_date, 
                                        format="%m/%d/%Y"),"%Y"),
                       mpa_age = 2022-as.numeric(implementation_year))

# Process parks ----------------------------------------------------
parks$name <- tolower(parks$name)

parks1 <- parks %>%
          mutate(mpa = gsub(" \\(no-take\\)","",name))

parks1$mpa <- recode_factor(parks1$mpa, "año nuevo smr"="ano nuevo smr")

# Process social data ----------------------------------------------------
social_dat$name <- tolower(social_dat$name)
social_dat$name <- recode_factor(social_dat$name, 'año nuevo' = 'ano nuevo')
social_dat$name <- recode_factor(social_dat$name, 
                                 'año nuevo smr' = 'ano nuevo smr')

social_dat$name <- gsub("\\s*\\([^\\)]+\\)","",as.character(social_dat$name)) 



################################################################################
# join data

traits_join <- left_join(phys_drivers, parks1, by="mpa")
drivers_join <- left_join(charisma_data, traits_join, by="mpa")
social_join <- left_join(drivers_join, social_dat, by = c("mpa"="name"))
all_drivers <- left_join(social_join, bio_drivers, by="mpa")


#clean up

drivers_dat <- all_drivers %>%
                dplyr::select(!(c("long_dd","lat_dd")))

#select data for model

model_dat <- drivers_dat %>%
             dplyr::select(category, 
                           #port_size,
                    distance_to_port, 
                    size_km2, 
                    shore_span_km,
                   take, 
                   sandy_beach_km, 
                   rocky_inter_km,
                    max_kelp_canopy_cdfw_km2, 
                   estuary_km2, 
                   #depth_range,
                    mpa_age, nparks, 
                   'park_area' = area_km2, 
                   n_camp_grounds,
                    n_picnic_areas, 
                   n_parking_lots, 
                   index_wt) %>%
             mutate_if(is.character,as.factor)



################################################################################
# analyze charismatic MPAs using stepwise model selection
char_dat <- model_dat %>%
  filter(!(category=="Inaccessible"))%>%
  mutate(logit_y = ifelse(category=="Charismatic", "1", "0"))%>%
  drop_na()%>%
  dplyr::select(!(category))


char_step_mod <- glm(as.numeric(logit_y) ~., data = char_dat, 
                     family=binomial(link="logit"), na.action = na.exclude)%>%
                stepAIC(trace=FALSE)

summary(char_step_mod)

tab_char_step <- sjPlot::tab_model(char_step_mod, show.aic=T, show.r2=T, title="Stepwise reduced logistic model",auto.label=T,
                              #pred.labels = c("intercept","state parks (yes)","sandy beach","estuary","national marine sanctuary (yes)"),
                              dv.labels = c("iNaturalist"))



# plot predicted probabilities of charismatic MPAs

pred_age_char <- ggpredict(char_step_mod,terms = "mpa_age[all]")%>%
            ggplot(aes(x, predicted)) +
            geom_line() +
            geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
            theme_minimal(base_size = 8)+
            xlab("MPA age")+
            ylab("Probability of charismatic MPA")
       
pred_beach_char <- ggpredict(char_step_mod,terms = "sandy_beach_km[all]")%>%
  ggplot(aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_minimal(base_size = 8)+
  xlab("Sandy beach extent (km)")+
  ylab("Probability of charismatic MPA")

pred_park_char <- ggpredict(char_step_mod,terms = "nparks[all]")%>%
  ggplot(aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_minimal(base_size = 8)+
  xlab("Park density (no. within 1km)")+
  ylab("Probability of charismatic MPA")




################################################################################
# analyze underutilized MPAs using stepwise model selection
under_dat <- model_dat %>%
  filter(!(category=="Charismatic"))%>%
  mutate(logit_y = ifelse(category=="Inaccessible", "1", "0"))%>%
  drop_na()%>%
  dplyr::select(!(c(category, n_camp_grounds)))


under_step_mod <- glm(as.numeric(logit_y) ~., data = under_dat, 
                     family=binomial(link="logit"), na.action = na.exclude)%>%
  stepAIC(trace=FALSE)

summary(char_step_mod)

tab_under_step <- sjPlot::tab_model(under_step_mod, show.aic=T, show.r2=T, 
                      title="Reduced logistic model: underutilized vs. typical",
                      auto.label=T, dv.labels = c("iNaturalist"))



# plot predicted probabilities

pred_distance_under <- ggpredict(under_step_mod,terms = "distance_to_port[all]")%>%
  ggplot(aes(x/1000, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_minimal(base_size = 8)+
  xlab("Distance to port (km)")+
  ylab("Probability of underutilized MPA")

pred_beach_under <- ggpredict(under_step_mod,terms = "sandy_beach_km[all]")%>%
  ggplot(aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_minimal(base_size = 8)+
  xlab("Sandy beach extent (km)")+
  ylab("Probability of underutilized MPA")

pred_park_under <- ggpredict(under_step_mod,terms = "n_parking_lots[all]")%>%
  ggplot(aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_minimal(base_size = 8)+
  xlab("Parking lot density (no. within 1km)")+
  ylab("Probability of underutilized MPA")



#plot all
predicted_coef_plot <- ggarrange(pred_age_char, pred_beach_char, 
                          pred_park_char, pred_distance_under, pred_beach_under, 
                          pred_park_under, nrow=2, ncol=3)


#Export figure
ggsave(here::here("analyses", "3performance_human", "figures", 
"FigX_predicted_probabilities.png"),bg="white", predicted_coef_plot, height=6, width = 6, 
units = "in", dpi = 300)


################################################################################
#Save coef table



model_out <- sjPlot::tab_model(char_step_mod, under_step_mod, show.aic=T, show.r2=T, 
                                    title="Reduced logistic models",
                                    auto.label=T, 
                               dv.labels = c("Charismatic vs. Typical",
                                            "Underutilized vs. Typical"),
                               file=file.path(figdir, "FigX_engagement_drivers.doc")
                               )








