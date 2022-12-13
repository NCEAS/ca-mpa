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
                      max_depth_m, protection, depth_range, four_region)


mpa_dat <- left_join(mod_out_raw, trait_drivers, by=c("MPA"="name"))%>%
            ## set target as 'no change' == stable
            mutate(stable = ifelse(p.value < 0.05,"no","yes"))%>%
            ##define resist / resilience
            mutate(period=paste(period_1, period_2, sep="-"),
            process=recode_factor(period,
                               "before-during"="Resistance",
                               "before-after"="Resilience"))%>%
            #remove ref sites, since we don't know anything about their traits
            filter(!(MPA_type == "ref"))%>%
            dplyr::select(!(MPA_type))%>%
            #calculate relative age
            mutate(imp_year = format(as.Date(implementation_date, format="%m/%d/%Y"),"%Y"),
                   rel_age = 2022 - as.numeric(imp_year))



###############################################################################
#Build logistic regression

in_resistance <- mpa_dat %>%
        mutate(logit_y = as.numeric(ifelse(stable == "yes","1","0")))%>%
        drop_na()%>%
        dplyr::select(c(logit_y, process, habitat, MPA, size_km2, shore_span_km,
               max_depth_m, depth_range, rel_age, lat))%>%
        filter(process=="Resistance")%>%
        dplyr::select(!(process))

in_resilience <- mpa_dat %>%
  mutate(logit_y = as.numeric(ifelse(stable == "yes","1","0")))%>%
  drop_na()%>%
  dplyr::select(c(logit_y, process, habitat, MPA,  size_km2, shore_span_km,
                  max_depth_m, depth_range, rel_age, lat, four_region))%>%
  filter(process=="Resilience")%>%
  dplyr::select(!(process))


#What is the effect of traits on resilience? 
resil_mod <- glm(as.numeric(logit_y) ~ four_region, data = in_resilience, 
                 family=binomial(link="logit"), na.action = na.exclude)
summary(resil_mod)


#What is the effect of traits on resistance? 
resil_mod <- glm(as.numeric(logit_y) ~ habitat*shore_span_km +
                   habitat*size_km2 + habitat*depth_range +
                   habitat*rel_age, data = in_resistance, 
                 family=binomial(link="logit"), na.action = na.exclude)
summary(resil_mod)

# plot predicted probabilities o

pred_age <- ggpredict(resil_mod,terms = "rel_age[all]")%>%
  ggplot(aes(x, predicted)) +
  geom_line() +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  theme_minimal(base_size = 8)+
  xlab("MPA age")+
  ylab("Probability of charismatic MPA")


