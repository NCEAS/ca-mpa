#Joshua G. Smith
#December 7, 2022

rm(list=ls())

librarian::shelf(reshape2, tidyverse, vegan)

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
  filter(year_1 == 2013 & year_2 >=2014 & MPA_1==MPA_2 &
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
  filter(year_1 == 2013 & year_2 >=2014 & MPA_1==MPA_2 &
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
  filter(year_1 == 2013 & year_2 >=2014 & MPA_1==MPA_2 &
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
  filter(year_1 == 2013 & year_2 >=2014 & MPA_1==MPA_2 &
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
  filter(year_1 == 2013 & year_2 >=2014 & MPA_1==MPA_2 &
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
)%>%
  mutate(MPA_type = word(site, -1))%>%
    group_by(group, year, MPA_type)%>%
    dplyr::summarize(dissim = mean(dissim),
                     SST = mean(SST, na.rm=TRUE))


################################################################################
# step 3 - build contrast test
  
# # Test
# contrast_test_dat <- CCA_data %>% filter(group =="Rocky reef fishes")
# ccf_test <- ccf(contrast_test_dat$dissim, contrast_test_dat$SST,
#     lag.max=6)


# creating groups from group and sites
CCA_data_grouped <- CCA_data %>%
  na.omit() %>%
  group_by(group, MPA_type #site
           )

# Get the group names
CCA_data_names <- CCA_data_grouped %>%
  group_keys() %>%
  unite(group_name, group:MPA_type, #:site
        sep = "-") %>%
  pull()

# Split data frame into a list of groups 
CCA_data_list <- CCA_data_grouped %>%
  group_split() %>%
  setNames(CCA_data_names)

# Applying the correlation function using map
# tt <- CCA_data_list %>%
  # map(~ccf(.$dissim, .$SST, lag.max=6, na.action = na.pass))


# Initialize data frame to store outputs
corr_df <- tibble(
  group = replicate(length(CCA_data_list), "a"),
  #site = replicate(length(CCA_data_list), "b"),
  MPA_type = replicate(length(CCA_data_list), "b"),
  acf_values = replicate(length(CCA_data_list), list(1:7)),
  corr_max = replicate(length(CCA_data_list), 0),
  lag_max = replicate(length(CCA_data_list), 0),
  corr_min = replicate(length(CCA_data_list), 0),
  lag_min = replicate(length(CCA_data_list), 0)
)


# Loop through the groups and sites
for (i in 1:length(CCA_data_list)) {
  print(i)
  data_group_name <- names(CCA_data_list[i])
  data_group <- CCA_data_list[[i]] %>%
    arrange(year)
  print(sprintf("Processing: %s", data_group_name))
  
  # Compute the cross-correlation
  ccf_group <- ccf(data_group$dissim, data_group$SST, lag.max=6, plot=FALSE)
  
  # Extract the cross-correlation information
  lag0_ind <-  which(ccf_group$lag == 0)
  positive_lag <- ccf_group$acf[lag0_ind:(lag0_ind*2-1)]
  max_ccf <- max(positive_lag)
  lag_max <- which(ccf_group$acf == max_ccf) - lag0_ind  # removing negative lag
  min_ccf <- min(positive_lag)
  lag_min <- which(ccf_group$acf == min_ccf) - lag0_ind   # removing negative lag
  
  # Add it to the data frame
  corr_df[i, "group"] <- data_group_name
  # corr_df[i, "group"] <- str_split(data_group_name, "-")[[1]][1] # when it was site specific
  #corr_df[i, "site"] <- str_split(data_group_name, "-")[[1]][2]
  # corr_df[i, "MPA_type"] <- str_split(data_group_name, "-")[[1]][2]# when it was site specific
  corr_df[i, "MPA_type"] <- data_group$MPA_type[[1]]
  corr_df[i, "acf_values"][[1]][[1]] <- positive_lag
  corr_df[i, "corr_max"] <- max_ccf
  corr_df[i, "lag_max"] <- lag_max
  corr_df[i, "corr_min"] <- min_ccf
  corr_df[i, "lag_min"] <- lag_min
  
  
  # plot it
  plot(ccf_group, main = data_group_name, xlim = c(0, lag0_ind-1)) # positive only
}


# Write output file
#write_csv(corr_df, file.path(data_path,"cross_correlation_groups.csv"))


#Plot

# expand the data frame so there is one row per acf values (lag 0-6)
corr_df_expanded <- unnest(corr_df)

# any ggplot graph


################################################################################
#######test lag lm

rocky_fish_lag <- CCA_data %>% filter(group=="Rocky reef fishes")%>%
                  dplyr::select(dissim, SST)

alldata=ts.intersect(rocky_fish_lag$dissim,reclag1=lag(rocky_fish_lag$dissim,-1), 
                     reclag2=lag(rocky_fish_lag$dissim,-2), 
                     reclag3 = lag(rocky_fish_lag$dissim, -3))


