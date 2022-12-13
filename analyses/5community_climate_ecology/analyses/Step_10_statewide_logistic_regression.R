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

mpa_trait <- read.csv("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_clean.csv")

#load model output
mod_out_raw <- read.csv(file.path(datadir,"mpa_betadisp_mod.csv"))


###############################################################################
#Join model output with MPA traits

#Step 1 - select traits of interest

trait_drivers <- mpa_trait %>%
                 dplyr::select(name, long, lat, implementation_date, size_km2, shore_span_km,
                      max_depth_m, protection, depth_range)


mpa_dat <- left_join(mod_out_raw, trait_drivers, by=c("MPA"="name"))%>%
            ## set target as 'no change' == stable
            mutate(stable = ifelse(p.value < 0.05,"no","yes"))%>%
            ##define resist / resilience
            mutate(period=paste(period_1, period_2, sep="-"),
            process=recode_factor(period,
                               "before-during"="Resistance",
                               "before-after"="Resilience"))



###############################################################################
#Build logistic regression

in_resistance <- mpa_dat %>%
        mutate(logit_y = as.numeric(ifelse(stable == "yes","1","0")))%>%
        drop_na()%>%
        dplyr::select(c(logit_y, process, habitat, MPA, MPA_type, size_km2, shore_span_km,
               max_depth_m, depth_range))%>%
        filter(process=="Resistance")%>%
        dplyr::select(!(process))

in_resilience <- mpa_dat %>%
  mutate(logit_y = as.numeric(ifelse(stable == "yes","1","0")))%>%
  drop_na()%>%
  dplyr::select(c(logit_y, process, habitat, MPA, MPA_type, size_km2, shore_span_km,
                  max_depth_m, depth_range))%>%
  filter(process=="Resilience",
         MPA_type == "smr")%>%
  dplyr::select(!(process))


#Do MPAs confer resilience across habitats? 
resil_mod <- glm(as.numeric(logit_y) ~ habitat*MPA_type, data = in_resilience, 
                     family=binomial(link="logit"), na.action = na.exclude)
summary(resil_mod)

#What is the effect of traits on resilience? 
resil_mod <- glm(as.numeric(logit_y) ~ habitat + shore_span_km +
                 size_km2, data = in_resilience, 
                 family=binomial(link="logit"), na.action = na.exclude)
summary(resil_mod)



# plot predicted probabilities o

pred_size <- ggpredict(resil_mod,terms = "size_km2[all]")%>%
  ggplot(aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_minimal(base_size = 8)+
  xlab("MPA size (Km2)")+
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








