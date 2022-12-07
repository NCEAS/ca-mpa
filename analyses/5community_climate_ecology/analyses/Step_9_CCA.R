#Joshua G. Smith
#December 7, 2022

rm(list=ls())

require(dplyr)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")

comm_data <- load(file.path(data_path, "comm_data.rda"))
nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
env_fit_scores <- load(file.path(data_path, "env_fit_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))


################################################################################
# step 1 - calculate dissimilarity relative to 2007 by MPA type

#create new grouping vars
CCFRP_group_vars2 <- CCFRP_group_vars %>%
  mutate(desig_state_year = paste(year, 
                                  affiliated_mpa,
                                  mpa_designation))
kelp_fish_group_vars2 <- kelp_fish_group_vars %>%
  mutate(desig_state_year = paste(year, 
                                  affiliated_mpa,
                                  mpa_defacto_designation))
kelp_invalg_group_vars2 <- kelp_invalg_group_vars %>%
  mutate(desig_state_year = paste(year, 
                                  affiliated_mpa,
                                  mpa_defacto_designation))
deep_reef_group_vars2 <- deep_reef_group_vars %>%
  mutate(desig_state_year = paste(year, 
                                  affiliated_mpa,
                                  mpa_defacto_designation))
rocky_group_vars2 <- rocky_group_vars %>%
  mutate(affiliated_mpa2 = ifelse(affiliated_mpa == "none",
                                         paste(site, mpa_designation),
                                         affiliated_mpa),
    desig_state_year = paste(year, 
                                  affiliated_mpa2))


set.seed(1985)

#CCFRP
CCFRP_mean_dist <- meandist(CCFRP_distmat, 
                            grouping = CCFRP_group_vars2$desig_state_year) #calculate mean dissim

CCFRP_mean_dist3 <- melt(CCFRP_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = word(Var1, 2  , -2),
         MPA_2 = word(Var2, 2  , -2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Rocky reef fishes") %>%
  filter(year_1 == 2007 & year_2 >=2008 & MPA_1==MPA_2 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, MPA_type = MPA_type2, 
         dissim = value)


#kelp_invalg
kelp_invalg_mean_dist <- meandist(kelp_invalg_distmat, 
                            grouping = kelp_invalg_group_vars2$desig_state_year) #calculate mean dissim

kelp_invalg_mean_dist3 <- melt(kelp_invalg_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = word(Var1, 2  , -2),
         MPA_2 = word(Var2, 2  , -2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Kelp forest inverts and algae") %>%
  filter(year_1 == 2007 & year_2 >=2008 & MPA_1==MPA_2 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, MPA_type = MPA_type2, 
         dissim = value)


#kelp_fish
kelp_fish_mean_dist <- meandist(kelp_fish_distmat, 
                                  grouping = kelp_fish_group_vars2$desig_state_year) #calculate mean dissim

kelp_fish_mean_dist3 <- melt(kelp_fish_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = word(Var1, 2  , -2),
         MPA_2 = word(Var2, 2  , -2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Kelp forest fishes") %>%
  filter(year_1 == 2007 & year_2 >=2008 & MPA_1==MPA_2 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, MPA_type = MPA_type2, 
         dissim = value)


#deep reef
deep_reef_mean_dist <- meandist(deep_reef_distmat, 
                                grouping = deep_reef_group_vars2$desig_state_year) #calculate mean dissim

deep_reef_mean_dist3 <- melt(deep_reef_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = word(Var1, 2  , -2),
         MPA_2 = word(Var2, 2  , -2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Deep reef fishes") %>%
  filter(year_1 == 2008 & year_2 >=2009 & MPA_1==MPA_2 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, MPA_type = MPA_type2, 
         dissim = value)


#rocky

rocky_mean_dist <- meandist(rocky_distmat, 
                                grouping = rocky_group_vars2$desig_state_year) #calculate mean dissim

rocky_mean_dist3 <- melt(rocky_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = word(Var1, 2  , -2),
         MPA_2 = word(Var2, 2  , -2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Rocky intertidal") %>%
  filter(year_1 == 2007 & year_2 >=2008 & MPA_1==MPA_2 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, MPA_type = MPA_type2, 
         dissim = value)


#Join

full_df <- rbind(CCFRP_mean_dist3, kelp_invalg_mean_dist3, 
                 kelp_fish_mean_dist3, deep_reef_mean_dist3, rocky_mean_dist3)%>%
            mutate(join_ID = paste(year, MPA, MPA_type))



################################################################################
# step 2 - join dissimilarities with SST

CCFRP_envr_dat <- cbind(CCFRP_group_vars, CCFRP_envr_vars) %>%
                  mutate(contrast_ID = paste(year, affiliated_mpa, 
                                             mpa_designation),
                         site_ID = paste(affiliated_mpa, mpa_designation),
                         group = "Rocky reef fishes")%>%
                  dplyr::select(group, year, region3, region4, site_ID,
                                SST, CUTI, BEUTI, MOCI, join_ID = contrast_ID)

rocky_envr_dat <- cbind(rocky_group_vars, rocky_envr_vars) %>%
  mutate(contrast_ID = paste(year, affiliated_mpa, 
                             mpa_designation),
         site_ID = paste(site, mpa_designation),
         group = "Rocky intertidal")%>%
  dplyr::select(group, year, region3, region4, site_ID,
                SST, CUTI, BEUTI, MOCI, join_ID = contrast_ID)

kelp_invalg_envr_dat <- cbind(kelp_invalg_group_vars, kelp_invalg_envr_vars) %>%
  mutate(contrast_ID = paste(year, affiliated_mpa, 
                             mpa_defacto_designation),
         site_ID = paste(affiliated_mpa, mpa_defacto_designation),
         group = "Kelp forest inverts and algae")%>%
  dplyr::select(group, year, region3, region4, site_ID,
                SST, CUTI, BEUTI, MOCI, join_ID = contrast_ID) 

kelp_fish_envr_dat <- cbind(kelp_fish_group_vars, kelp_fish_envr_vars) %>%
  mutate(contrast_ID = paste(year, affiliated_mpa, 
                             mpa_defacto_designation),
         site_ID = paste(affiliated_mpa, mpa_defacto_designation),
         group = "Kelp forest fishes")%>%
  dplyr::select(group, year, region3, region4, site_ID,
                SST, CUTI, BEUTI, MOCI, join_ID = contrast_ID)

deep_reef_envr_dat <- cbind(deep_reef_group_vars, deep_reef_envr_vars) %>%
  mutate(contrast_ID = paste(year, affiliated_mpa, 
                             mpa_defacto_designation),
         site_ID = paste(affiliated_mpa, mpa_defacto_designation),
         group = "Deep reef fishes")%>%
  dplyr::select(group, year, region3, region4, site_ID,
                SST, CUTI, BEUTI, MOCI, join_ID = contrast_ID)


envr_join <- rbind(CCFRP_envr_dat, rocky_envr_dat, kelp_invalg_envr_dat,
                   kelp_fish_envr_dat, deep_reef_envr_dat)%>%
             select(!(join_ID))%>%
              mutate(join_ID = paste(year, site_ID))

con_dat1 <- left_join(full_df,envr_join, by=c('group',
                                              'join_ID'))

CCA_data <- con_dat1 %>% 
  mutate(site = paste(MPA, MPA_type))%>%
  dplyr::select(
  group, year = year.x, site ,dissim,
  SST,CUTI,BEUTI,MOCI
)


################################################################################
# step 3 - build contrast test
  
  
contrast_test_dat <- CCA_data %>% filter(group =="Rocky reef fishes")

ccf<- ccf(contrast_test_dat$dissim, contrast_test_dat$SST,
    lag.max=6)








