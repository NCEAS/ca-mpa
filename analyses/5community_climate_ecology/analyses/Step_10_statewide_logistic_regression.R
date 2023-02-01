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

#load habitat data
mpa_attributes_gen <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_general.Rds")
mpa_attributes_hab <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat.Rds")
mpa_attributes_hab_div <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_diversity.Rds")
fishing_effort <- readRDS(here::here("analyses","2performance_fisheries","analyses","blocks","pre_mpa_fishing_pressure_by_mpa.Rds"))
prop_rock <- readRDS("/home/shares/ca-mpa/data/sync-data/mpa_traits/processed/mpa_attributes_habitat_rock.Rds")


#View#load model output
mod_out <- read.csv(file.path(datadir,"mpa_betadisp_mod.csv"))


###############################################################################
#process model output

mod_out1 <- mod_out %>%
            filter(MPA_type == 'smr') %>%
            #stable = 1, shift = 0
            mutate(test_stat = ifelse(p.value>0.05, 0,1))


###############################################################################
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
  dplyr::select(affiliated_mpa, implementation_date, size=size_km2.x,
                habitat_richness, habitat_diversity=habitat_diversity_sw, 
                prop_rock, fishing_pressure = annual_avg_lb_sqkm_20002006
  )%>%
  mutate(affiliated_mpa = recode(affiliated_mpa,
                                 "año nuevo smr" = "ano nuevo smr"))


#step 5 - join traits and mod

logit_dat <- left_join(mod_out1, mpa_traits, by=c("MPA"="affiliated_mpa")) %>%
            mutate(period=paste(period_1, period_2, sep="-"),
            process=recode_factor(period,
                               "before-during"="Resistance",
                               "before-after"="Recovery"),
            age = as.numeric(format(implementation_date,'%Y')))
            #



###############################################################################
#Build logistic regression

resist_all <- logit_dat %>%
        filter(process == "Resistance")%>%
        filter(!(is.na(habitat_diversity)))


resist_mod_all <- glm(test_stat ~ size + habitat_diversity + prop_rock + fishing_pressure +
                        habitat_richness,
                      data = resist_all, 
                 family=binomial(link="logit"), na.action = na.exclude)
summary(resist_mod_all)



recov_all <- logit_dat %>%
  filter(process == "Recovery")%>%
  filter(!(is.na(habitat_diversity)))


recov_mod_all <- glm(test_stat ~ size + habitat_diversity + prop_rock + fishing_pressure +
                       habitat_richness,
                      data = recov_all, 
                      family=binomial(link="logit"), na.action = na.exclude)%>%
                 stepAIC(trace=FALSE)
summary(recov_mod_all)


################################################################################
# Build predicted probabilities

# Theme
my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=8, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)))

pred_size <- ggpredict(recov_mod_all,terms = "size[all]") %>%
  ggplot(aes(x, predicted)) +
  geom_smooth(span=0.25, color='black') +
  geom_point(aes(x=size, y=test_stat), data=recov_all)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  annotate(geom="text", x=0, y=1.05, label="p=0.015", hjust=0, size=2.2) +
  theme_minimal(base_size = 12)+
  xlab("MPA size (km²)")+
  ylab("Probability of recovery")+
  my_theme


pred_rock <- ggpredict(recov_mod_all,terms = "prop_rock[all]") %>%
  ggplot(aes(x, predicted)) +
  geom_smooth(span=0.25, color='black') +
  geom_point(aes(x=prop_rock, y=test_stat), data=recov_all)+
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = .1)+
  annotate(geom="text", x=0, y=1.05, label="p=0.029", hjust=0, size=2.2) +
  theme_minimal(base_size = 12)+
  xlab("Proportion rock habitat")+
  ylab("")+
  my_theme


# Merge plots
g <- gridExtra::grid.arrange(pred_size, pred_rock, nrow=1)
g


# Export
ggsave(g, filename=file.path(figdir, "trait_drivers.png"), 
       width=6.5, height=4.5, units="in", dpi=600)







