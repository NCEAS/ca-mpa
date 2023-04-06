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
fig_dir <- here::here("analyses","5community_climate_ecology","figures")

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))



################################################################################
#Compare annual centroid distance relative to 2007-2013 centroid

#process group vars by setting 'dummy' start centroid
CCFRP_group_vars2 <- CCFRP_group_vars %>% mutate(year = 
                                                   ifelse(year < 2014, 2007,year),
                                                 desig_state_year = paste(mpa_designation, year))


kelp_invalg_group_vars2 <- kelp_invalg_group_vars %>% mutate(year = 
                                                               ifelse(year < 2014, 2007,year),
                                                             desig_state_year = paste(mpa_defacto_designation, year))

kelp_fish_group_vars2 <- kelp_fish_group_vars %>% mutate(year = 
                                                           ifelse(year < 2014, 2007,year),
                                                         desig_state_year = paste(mpa_defacto_designation, year))

deep_reef_group_vars2 <- deep_reef_group_vars %>% mutate(year = 
                                                           ifelse(year < 2014, 2007,year),
                                                         desig_state_year = paste(mpa_defacto_designation, year))


rocky_group_vars2 <- rocky_group_vars %>% mutate(year = 
                                                   ifelse(year < 2014, 2007,year),
                                                 desig_state_year = paste(mpa_designation, year))




# calculate distances ----------------------------------

##need to set level for each group

cenfun2 <- function(group, x) {
  
  group$desig_state_year <- as.factor(group$desig_state_year)
  levels(group$desig_state_year)
  n <- nlevels(group$desig_state_year)
  start <- levels(group$desig_state_year)[1:1]
  end <- levels(group$desig_state_year)[2:8]
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

cenfun3 <- function(group, x) {
  
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


ccfrp_ref <- cenfun2(group=CCFRP_group_vars2, x=CCFRP_distmat)
ccfrp_ref$group <- c("ccfrp")
ccfrp_smr <- cenfun3(group=CCFRP_group_vars2, x=CCFRP_distmat)
ccfrp_smr$group <- c("ccfrp")


kelp_invalg_ref <- cenfun2(group=kelp_invalg_group_vars2, x=kelp_invalg_distmat)
kelp_invalg_ref$group <- c("kelp_invalg")
kelp_invalg_smr <- cenfun3(group=kelp_invalg_group_vars2, x=kelp_invalg_distmat)
kelp_invalg_smr$group <- c("kelp_invalg")


kelp_fish_ref <- cenfun2(group=kelp_fish_group_vars2, x=kelp_fish_distmat)
kelp_fish_ref$group <- c("kelp_fish")
kelp_fish_smr <- cenfun3(group=kelp_fish_group_vars2, x=kelp_fish_distmat)
kelp_fish_smr$group <- c("kelp_fish")



cenfun4 <- function(group, x) {
  
  group$desig_state_year <- as.factor(group$desig_state_year)
  levels(group$desig_state_year)
  n <- nlevels(group$desig_state_year)
  start <- levels(group$desig_state_year)[1:1]
  end <- levels(group$desig_state_year)[2:5]
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

cenfun5 <- function(group, x) {
  
  group$desig_state_year <- as.factor(group$desig_state_year)
  levels(group$desig_state_year)
  n <- nlevels(group$desig_state_year)
  start <- levels(group$desig_state_year)[6:6]
  end <- levels(group$desig_state_year)[7:n]
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



deep_reef_ref <- cenfun4(group=deep_reef_group_vars2, x=deep_reef_distmat)
deep_reef_ref$group <- c("deep_reef")
deep_reef_smr <- cenfun5(group=deep_reef_group_vars2, x=deep_reef_distmat)
deep_reef_smr$group <- c("deep_reef")


rocky_ref <- cenfun2(group=rocky_group_vars2, x=rocky_distmat)
rocky_ref$group <- c("rocky_intertidal")
rocky_smr <- cenfun3(group=rocky_group_vars2, x=rocky_distmat)
rocky_smr$group <- c("rocky_intertidal")


#combine into df
traject_dist <- as.data.frame(rbind(ccfrp_ref,
                                    ccfrp_smr,
                                    kelp_invalg_ref,
                                    kelp_invalg_smr,
                                    kelp_fish_ref,
                                    kelp_fish_smr,
                                    deep_reef_ref,
                                    deep_reef_smr,
                                    rocky_ref,
                                    rocky_smr
                                     )) %>%
                mutate(year = word(centroid_2, 2),
                       MPA_type = word(centroid_2, 1)) %>%
                dplyr::select(group, year, MPA_type, distance)%>%
                mutate(group = recode(group, "ccfrp" = "Rocky reef",
                                               "kelp_invalg" = "Kelp inverts and algae",
                                      "kelp_fish" = "Kelp fish",
                                      "deep_reef" = "Deep reef",
                                      "rocky_intertidal" = "Rocky intertidal"),
                       year = as.numeric(year),
                       MPA_type = recode(MPA_type, "ref" = "Reference", "smr" = "MPA"),
                       MPA_type = as.factor(MPA_type),
                       group = factor(group, levels = c("Rocky intertidal",
                                                           "Kelp inverts and algae",
                                                           "Kelp fish",
                                                           "Rocky reef",
                                                           "Deep reef")))


################################################################################
#plot

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
                   #legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)))

g <- ggplot(transform(traject_dist,
                 group=factor(group,levels=c("Rocky intertidal",
                                             "Kelp inverts and algae",
                                             "Kelp fish",
                                             "Rocky reef",
                                             "Deep reef"))) , aes(x = year, y = distance, group = MPA_type, color=MPA_type))+
  geom_smooth(method = "loess", se=FALSE) +
  geom_point()+
  facet_wrap(~group, ncol=3, scales="free")+
  scale_x_continuous(limits=c(2014,2020)) + scale_y_continuous(limits=c(0,0.6))+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')+
  theme_minimal()+
  xlab("Year")+
  ylab("Distance to pre-heatwave centroid \n(Bray-Curtis)")+
  labs(color = "MPA type")+
  my_theme

g


#ggsave(g, filename=file.path(fig_dir, "FigSX_centroid_trajectory.png"), 
#       width=7, height=5, units="in", dpi=600)









