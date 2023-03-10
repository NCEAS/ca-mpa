#required packages
require(vegan)
require(dplyr)
require(tidyr)
require(gridExtra)
require(usedist)
require(ggplot2)
require(metafor)
require(reshape2)
require(ggfittext)
require(pairwiseAdonis)
require(here)
require(forcats)


# #load data --------------------------------------------------------------

#data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
tab_out <- here::here("analyses","5community_climate_ecology","tables")

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))



################################################################################
#Compare annual centroid distance relative to 2007-2013 centroid

#process group vars by setting 'dummy' start centroid
CCFRP_group_vars2 <- CCFRP_group_vars %>% mutate(year = 
                                                   ifelse(year < 2014, "2007",year),
                                                 desig_state_year = paste(mpa_designation, year))


kelp_invalg_group_vars2 <- kelp_invalg_group_vars %>% mutate(year = 
                                                               ifelse(year < 2014, "2007",year))


kelp_fish_group_vars2 <- kelp_fish_group_vars %>% mutate(year = 
                                                           ifelse(year < 2014, "2007",year))

deep_reef_group_vars2 <- deep_reef_group_vars %>% mutate(year = 
                                                           ifelse(year < 2014, "2007",year))


rocky_group_vars2 <- rocky_group_vars %>% mutate(year = 
                                                    ifelse(year < 2014, "2007",year))




# calculate distances ----------------------------------

cenfun2 <- function(group, x) {
  
  group$desig_state_year <- as.factor(group$desig_state_year)
  levels(group$desig_state_year)
  n <- nlevels(group$desig_state_year)
  start <- levels(group$desig_state_year)[1:1]
  end <- levels(group$desig_state_year)[2:7]
  map2_dfr(start, end, ~ {
    idx1 <- which(group$desig_state_year == .x)
    idx2 <- which(group$desig_state_year == .y)
    tibble(
      centroid_1 = .x,
      centroid_2 = .y,
      distance = dist_between_centroids(x, idx1, idx2)
    )
  })
} #start and end are grouping vars, x is distmat


ccfrp_ref <- cenfun2(group=CCFRP_group_vars2, x=CCFRP_distmat)
ccfrp_ref$group <- c("ccfrp")

cenfun2 <- function(group, x) {
  
  group$desig_state_year <- as.factor(group$desig_state_year)
  levels(group$desig_state_year)
  n <- nlevels(group$desig_state_year)
  start <- levels(group$desig_state_year)[9:9]
  end <- levels(group$desig_state_year)[10:n]
  map2_dfr(start, end, ~ {
    idx1 <- which(group$desig_state_year == .x)
    idx2 <- which(group$desig_state_year == .y)
    tibble(
      centroid_1 = .x,
      centroid_2 = .y,
      distance = dist_between_centroids(x, idx1, idx2)
    )
  })
} #start and end are grouping vars, x is distmat

ccfrp_smr <- cenfun2(group=CCFRP_group_vars2, x=CCFRP_distmat)
ccfrp_smr$group <- c("ccfrp")


####problem with deep reef --- need to figure this out before continuing
deep_reef <- eig_fun(deep_reef_disper)


#combine into df
travel_distance <- as.data.frame(rbind(CCFRP, rocky, kelp_fish,
                                       kelp_invalg))
