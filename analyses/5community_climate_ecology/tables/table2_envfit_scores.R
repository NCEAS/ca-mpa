#Joshua G. Smith
#January 4, 2023

rm(list=ls())

require(dplyr)
require(vegan)
require(here)


#Set paths
datadir <-  "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data" 
tabdir <- here::here("analyses","5community_climate_ecology","tables")

#load EnvFit score
load(file.path(datadir, "env_fit_scores.rda"))


#create helper function

envfit_out <- function(scores) {
  data.frame((scores$vectors)$arrows, (scores$vectors)$r, (scores$vectors)$pvals) %>%
    rename("R square" = X.scores.vectors..r,
           "p-value" = X.scores.vectors..pvals) %>%
    tibble::rownames_to_column(., "Oceanographic variable")
}


#exctract scores
CCFRP <- envfit_out(CCFRP_en) %>%
        mutate(Habitat = "Rocky reef fishes") %>%
        dplyr::select(Habitat, everything())


kf_invalg <- envfit_out(kelp_invalg_en) %>%
  mutate(Habitat = "Kelp forest inverts and algae") %>%
  dplyr::select(Habitat, everything())

kf_fish <- envfit_out(kelp_fish_en) %>%
  mutate(Habitat = "Kelp forest fishes") %>%
  dplyr::select(Habitat, everything())

deep_reef <- envfit_out(deep_reef_en) %>%
  mutate(Habitat = "Deep reef fishes") %>%
  dplyr::select(Habitat, everything())

rocky_int <- envfit_out(rocky_en) %>%
  mutate(Habitat = "Rocky intertidal") %>%
  dplyr::select(Habitat, everything())


envfit_scores <- rbind(rocky_int, CCFRP, kf_invalg, kf_fish, deep_reef)

View(envfit_scores)

write.csv(envfit_scores, file.path(tabdir, "env_fit_scores.csv"),
          row.names=FALSE)












