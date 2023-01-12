#Joshua G. Smith
#January 5, 2023

rm(list=ls())

librarian::shelf(reshape2, tidyverse, vegan, lmtest, dynlm)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")
tabdir <- here::here("analyses", "5community_climate_ecology", "tables")

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
                                  mpa_designation))
kelp_fish_group_vars2 <- kelp_fish_group_vars %>%
  mutate(desig_state_year = paste(year, 
                                  mpa_defacto_designation))
kelp_invalg_group_vars2 <- kelp_invalg_group_vars %>%
  mutate(desig_state_year = paste(year, 
                                  mpa_defacto_designation))
deep_reef_group_vars2 <- deep_reef_group_vars %>%
  mutate(desig_state_year = paste(year, 
                                  mpa_defacto_designation))
rocky_group_vars2 <- rocky_group_vars %>%
  mutate(affiliated_mpa2 = ifelse(affiliated_mpa == "none",
                                  paste(site, mpa_designation),
                                  affiliated_mpa),
         desig_state_year = paste(year, 
                                  mpa_designation))


set.seed(1985)

#CCFRP
CCFRP_mean_dist <- meandist(CCFRP_distmat, 
                            grouping = CCFRP_group_vars2$desig_state_year) #calculate mean dissim

CCFRP_mean_dist3 <- melt(CCFRP_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Rocky reef fishes") %>%
  filter(year_1 == 2007 & year_2 >=2008 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  dplyr::select(group, year = year_2, MPA_type = MPA_type2, 
         dissim = value)


#kelp_invalg
kelp_invalg_mean_dist <- meandist(kelp_invalg_distmat, 
                                  grouping = kelp_invalg_group_vars2$desig_state_year) #calculate mean dissim

kelp_invalg_mean_dist3 <- melt(kelp_invalg_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Kelp forest inverts and algae") %>%
  filter(year_1 == 2007 & year_2 >=2008 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2,  MPA_type = MPA_type2, 
         dissim = value)


#kelp_fish
kelp_fish_mean_dist <- meandist(kelp_fish_distmat, 
                                grouping = kelp_fish_group_vars2$desig_state_year) #calculate mean dissim

kelp_fish_mean_dist3 <- melt(kelp_fish_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Kelp forest fishes") %>%
  filter(year_1 == 2007 & year_2 >=2008 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA_type = MPA_type2, 
         dissim = value)


#deep reef
deep_reef_mean_dist <- meandist(deep_reef_distmat, 
                                grouping = deep_reef_group_vars2$desig_state_year) #calculate mean dissim

deep_reef_mean_dist3 <- melt(deep_reef_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Deep reef fishes") %>%
  #must use 2012, since deep reef did not survey in 2013
  filter(year_1 == 2008 & year_2 >=2009 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA_type = MPA_type2, 
         dissim = value)


#rocky

rocky_mean_dist <- meandist(rocky_distmat, 
                            grouping = rocky_group_vars2$desig_state_year) #calculate mean dissim

rocky_mean_dist3 <- melt(rocky_mean_dist) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_type1 = word(Var1,-1),
         MPA_type2 = word(Var2,-1),
         group="Rocky intertidal") %>%
  filter(year_1 == 2007 & year_2 >=2008 &
           MPA_type1 == MPA_type2) %>% #extract dissimilarities relative to 2007
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA_type = MPA_type2, 
         dissim = value)


#Join

full_df <- rbind(CCFRP_mean_dist3, kelp_invalg_mean_dist3, 
                 kelp_fish_mean_dist3, deep_reef_mean_dist3, rocky_mean_dist3)%>%
  mutate(join_ID = paste(year, MPA_type),
         year = as.numeric(year))



################################################################################
# step 2 - join dissimilarities with SST

CCFRP_envr_dat <- cbind(CCFRP_group_vars, CCFRP_envr_vars) %>%
  group_by(year, mpa_designation) %>%
  summarize(mean_SST = mean(SST, na.rm=TRUE),
            mean_BT = mean(BT,na.rm=TRUE),
            mean_BEUTI = mean(BEUTI,na.rm=TRUE),
            mean_MOCI = mean(MOCI,na.rm=TRUE)) %>%
  mutate(group = "Rocky reef fishes")%>%
  dplyr::select(group, year, mpa_designation,
                mean_SST, mean_BT, mean_BEUTI, mean_MOCI)

rocky_envr_dat <- cbind(rocky_group_vars, rocky_envr_vars) %>%
  group_by(year, mpa_designation) %>%
  summarize(mean_SST = mean(SST, na.rm=TRUE),
            #mean_BT = mean(BT,na.rm=TRUE),
            mean_BEUTI = mean(BEUTI,na.rm=TRUE),
            mean_MOCI = mean(MOCI,na.rm=TRUE)) %>%
  mutate(group = "Rocky intertidal")%>%
  dplyr::select(group, year, mpa_designation,
                mean_SST, mean_BEUTI, mean_MOCI)

kelp_invalg_envr_dat <- cbind(kelp_invalg_group_vars, kelp_invalg_envr_vars) %>%
  group_by(year, mpa_defacto_designation) %>%
  summarize(mean_SST = mean(SST, na.rm=TRUE),
            mean_BT = mean(BT,na.rm=TRUE),
            mean_BEUTI = mean(BEUTI,na.rm=TRUE),
            mean_MOCI = mean(MOCI,na.rm=TRUE)) %>%
  mutate(group = "Kelp forest inverts and algae")%>%
  dplyr::select(group, year, mpa_designation = mpa_defacto_designation,
                mean_SST, mean_BT, mean_BEUTI, mean_MOCI)

kelp_fish_envr_dat <- cbind(kelp_fish_group_vars, kelp_fish_envr_vars) %>%
  group_by(year, mpa_defacto_designation) %>%
  summarize(mean_SST = mean(SST, na.rm=TRUE),
            mean_BT = mean(BT,na.rm=TRUE),
            mean_BEUTI = mean(BEUTI,na.rm=TRUE),
            mean_MOCI = mean(MOCI,na.rm=TRUE)) %>%
  mutate(group = "Kelp forest fishes")%>%
  dplyr::select(group, year, mpa_designation = mpa_defacto_designation,
                mean_SST, mean_BT, mean_BEUTI, mean_MOCI)

deep_reef_envr_dat <- cbind(deep_reef_group_vars, deep_reef_envr_vars) %>%
  group_by(year, mpa_defacto_designation) %>%
  summarize(mean_SST = mean(SST, na.rm=TRUE),
            mean_BT = mean(BT,na.rm=TRUE),
            mean_BEUTI = mean(BEUTI,na.rm=TRUE),
            mean_MOCI = mean(MOCI,na.rm=TRUE)) %>%
  mutate(group = "Deep reef fishes")%>%
  dplyr::select(group, year, mpa_designation = mpa_defacto_designation,
                mean_SST, mean_BT, mean_BEUTI, mean_MOCI)


envr_join <- rbind(CCFRP_envr_dat, rocky_envr_dat, kelp_invalg_envr_dat,
                   kelp_fish_envr_dat, deep_reef_envr_dat)%>%
              mutate(mean_BT = ifelse(mean_BT == "NaN", NA,mean_BT))

con_dat1 <- left_join(full_df,envr_join, by=c('group',
                                              'year',
                                              'MPA_type'='mpa_designation'))



################################################################################
# step 3 - build Granger causality test

# create helper function

contrast_fun <- function(data, order, group) {
  
  ref_dat <- data %>% filter(MPA_type == "ref")
  smr_dat <- data %>% filter(MPA_type == "smr")
  
  sst_ref <- grangertest(ref_dat$mean_SST ~ ref_dat$dissim,
                              order=order, na.action = na.omit) %>%
            data.frame()%>%
            mutate(MPA_type = "Reference",
              test = "SST") %>%
            dplyr::slice(.,2) %>%
            rename("P" = Pr..F.)
  
  #bt_ref <- grangertest(ref_dat$mean_BT ~ ref_dat$dissim,
   #                  order=order, na.action = na.omit) %>%
  #  data.frame()%>%
   # mutate(MPA_type = "Reference",
    #       test = "BT") %>%
    #dplyr::slice(.,2) %>%
    #rename("P" = Pr..F.)
  
  beuti_ref <- grangertest(ref_dat$mean_BEUTI ~ ref_dat$dissim,
                     order=order, na.action = na.omit) %>%
    data.frame()%>%
    mutate(MPA_type = "Reference",
           test = "BEUTI") %>%
    dplyr::slice(.,2) %>%
    rename("P" = Pr..F.)
  
  moci_ref <- grangertest(ref_dat$mean_MOCI ~ ref_dat$dissim,
                     order=order, na.action = na.omit) %>%
    data.frame()%>%
    mutate(MPA_type = "Reference",
           test = "MOCI") %>%
    dplyr::slice(.,2) %>%
    rename("P" = Pr..F.)
  
  sst_smr <- grangertest(smr_dat$mean_SST ~ smr_dat$dissim,
                         order=order, na.action = na.omit) %>%
    data.frame()%>%
    mutate(MPA_type = "MPA",
           test = "SST") %>%
    dplyr::slice(.,2) %>%
    rename("P" = Pr..F.)
  
  #bt_smr <- grangertest(smr_dat$mean_BT ~ smr_dat$dissim,
  #                      order=order, na.action = na.omit) %>%
  #  data.frame()%>%
  #  mutate(MPA_type = "MPA",
  #         test = "BT") %>%
  #  dplyr::slice(.,2) %>%
  #  rename("P" = Pr..F.)
  
  beuti_smr <- grangertest(smr_dat$mean_BEUTI ~ smr_dat$dissim,
                           order=order, na.action = na.omit) %>%
    data.frame()%>%
    mutate(MPA_type = "MPA",
           test = "BEUTI") %>%
    dplyr::slice(.,2) %>%
    rename("P" = Pr..F.)
  
  moci_smr <- grangertest(smr_dat$mean_MOCI ~ smr_dat$dissim,
                          order=order, na.action = na.omit) %>%
    data.frame()%>%
    mutate(MPA_type = "MPA",
           test = "MOCI") %>%
    dplyr::slice(.,2) %>%
    rename("P" = Pr..F.)
  
  x <- rbind(sst_ref, bt_ref, beuti_ref, moci_ref,
             sst_smr, bt_smr, beuti_smr, moci_smr)
  x$group <- group
  
  print(x)
}


#run granger tests
rocky_reef_dat <- con_dat1 %>% filter(group =="Rocky reef fishes")
rocky_reef_g <- contrast_fun(rocky_reef_dat,3, "Rocky reef fishes")

kelp_fish_dat <- con_dat1 %>% filter(group =="Kelp forest fishes")
kelp_fish_g <- contrast_fun(kelp_fish_dat,3, "Kelp forest fishes")

kelp_invalg_dat <- con_dat1 %>% filter(group =="Kelp forest inverts and algae")
kelp_invalg_g <- contrast_fun(kelp_invalg_dat,3, "Kelp forest inverts and algae")

deep_reef_dat <- con_dat1 %>% filter(group =="Deep reef fishes")
deep_reef_g <- contrast_fun(deep_reef_dat,1, "Deep reef fishes")

#drop BT from function to run rocky intertidal
intertidal_dat <- con_dat1 %>% filter(group =="Rocky intertidal")
intertidal_g <- contrast_fun(intertidal_dat,3, "Rocky intertidal")


granger_contrasts <- rbind(intertidal_g, kelp_invalg_g, kelp_fish_g,
                           rocky_reef_g, deep_reef_g)



write.csv(granger_contrasts, file.path(tabdir, "granger_test.csv"),
          row.names = FALSE)




