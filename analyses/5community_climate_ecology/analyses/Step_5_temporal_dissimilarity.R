#author: "Joshua G. Smith"
#date: '2022-07-13'

rm(list=ls())

#required packages
require(vegan)
require(dplyr)
require(tidyr)
require(metafor)
require(gridExtra)
require(usedist)
require(ggplot2)
require(reshape2)
require(ggfittext)
require(ggdendro)

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"

distmat <- load(file.path(data_path, "distance_matrices_BC.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))





# Take a look at year-to-year dissimilarities -----------------------------------------------

set.seed(1985)

#CCFRP
CCFRP_mean_dist <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars$year) #calculate mean dissim
CCFRP_diag <- diag(CCFRP_mean_dist[,2:11]) #extract mat diagonal
CCFRP_df <- data.frame(Year = row.names(CCFRP_mean_dist[2:11,]), CCFRP_diag) #convert to df
CCFRP_df$group <- c("CCFRP")
colnames(CCFRP_df) <- c('year','dissim','group')

#kelp_swath 
kelp_swath_dist <- meandist(kelp_swath_distmat, grouping = kelp_swath_group_vars$year)
kelp_swath_diag <- diag(kelp_swath_dist[,2:19])
kelp_swath_df <- data.frame(Year = row.names(kelp_swath_dist[2:19,]), kelp_swath_diag)
kelp_swath_df$group <- c("kelp_swath")
colnames(kelp_swath_df) <- c('year','dissim','group')

#kelp_upc
kelp_upc_dist <- meandist(kelp_upc_distmat, grouping = kelp_upc_group_vars$year)
kelp_upc_diag <- diag(kelp_upc_dist[,2:22])
kelp_upc_df <- data.frame(Year = row.names(kelp_upc_dist[2:22,]), kelp_upc_diag)
kelp_upc_df$group <- c("kelp_upc")
colnames(kelp_upc_df) <- c('year','dissim','group')

#kelp_fish
kelp_fish_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars$year)
kelp_fish_diag <- diag(kelp_fish_dist[,2:22])
kelp_fish_df <- data.frame(Year = row.names(kelp_fish_dist[2:22,]), kelp_fish_diag)
kelp_fish_df$group <- c("kelp_fish")
colnames(kelp_fish_df) <- c('year','dissim','group')

#deep reef
deep_reef_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars$year)
deep_reef_diag <- diag(deep_reef_dist[,2:8])
deep_reef_df <- data.frame(Year = row.names(deep_reef_dist[2:8,]), deep_reef_diag)
deep_reef_df$group <- c("deep_reef")
colnames(deep_reef_df) <- c('year','dissim','group')

#rocky
rocky_dist <- meandist(rocky_distmat, grouping = rocky_group_vars$year)
rocky_diag <- diag(rocky_dist[,2:19])
rocky_df <- data.frame(Year = row.names(rocky_dist[2:19,]), rocky_diag)
rocky_df$group <- c("rocky")
colnames(rocky_df) <- c('year','dissim','group')


#Join

full_df <- rbind(CCFRP_df, kelp_swath_df, kelp_upc_df, kelp_fish_df, deep_reef_df, rocky_df)

full_df_2010 <- full_df %>% filter(as.numeric(year)>=2010)

full_df_2010 %>%
  ggplot(aes(x=as.numeric(year), y=dissim, color=group))+
  geom_point(alpha=0.4)+
  geom_line(alpha=0.4)+
  stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = 0.2, ymax = 0.55,
           alpha = .15, fill='red')+
  xlab("year")+
  ylab("dissimilarity")




# dissimilarity relative to 2010 ------------------------------------------


set.seed(1985)

#CCFRP
CCFRP_mean_dist <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars$year) #calculate mean dissim
CCFRP_diag <- CCFRP_mean_dist[4,4:11] #extract mat diagonal
CCFRP_df <- data.frame(Year = row.names(CCFRP_mean_dist[4:11,]), CCFRP_diag) #convert to df
CCFRP_df$group <- c("CCFRP")
colnames(CCFRP_df) <- c('year','dissim','group')

#kelp_swath 
kelp_swath_dist <- meandist(kelp_swath_distmat, grouping = kelp_swath_group_vars$year)
kelp_swath_diag <- kelp_swath_dist[12,12:22]
kelp_swath_df <- data.frame(Year = row.names(kelp_swath_dist[12:22,]), kelp_swath_diag)
kelp_swath_df$group <- c("kelp_swath")
colnames(kelp_swath_df) <- c('year','dissim','group')

#kelp_upc
kelp_upc_dist <- meandist(kelp_upc_distmat, grouping = kelp_upc_group_vars$year)
kelp_upc_diag <- kelp_upc_dist[12,12:22]
kelp_upc_df <- data.frame(Year = row.names(kelp_upc_dist[12:22,]), kelp_upc_diag)
kelp_upc_df$group <- c("kelp_upc")
colnames(kelp_upc_df) <- c('year','dissim','group')

#kelp_invalg
kelp_invalg_dist <- meandist(kelp_invalg_distmat, grouping = kelp_invalg_group_vars$year)
kelp_invalg_diag <- kelp_invalg_dist[12,12:22]
kelp_invalg_df <- data.frame(Year = row.names(kelp_invalg_dist[12:22,]), kelp_invalg_diag)
kelp_invalg_df$group <- c("kelp inverts and algae")
colnames(kelp_invalg_df) <- c('year','dissim','group')

#kelp_fish
kelp_fish_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars$year)
kelp_fish_diag <- kelp_fish_dist[12,12:22]
kelp_fish_df <- data.frame(Year = row.names(kelp_fish_dist[12:22,]), kelp_fish_diag)
kelp_fish_df$group <- c("kelp_fish")
colnames(kelp_fish_df) <- c('year','dissim','group')

#deep reef
deep_reef_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars$year)
deep_reef_diag <- deep_reef_dist[4,4:8]
deep_reef_df <- data.frame(Year = row.names(deep_reef_dist[4:8,]), deep_reef_diag)
deep_reef_df$group <- c("deep_reef")
colnames(deep_reef_df) <- c('year','dissim','group')

#rocky
rocky_dist <- meandist(rocky_distmat, grouping = rocky_group_vars$year)
rocky_diag <- rocky_dist[9,9:19]
rocky_df <- data.frame(Year = row.names(rocky_dist[9:19,]), rocky_diag)
rocky_df$group <- c("rocky")
colnames(rocky_df) <- c('year','dissim','group')


#Join

full_df <- rbind(CCFRP_df, kelp_invalg_df, kelp_fish_df, deep_reef_df, rocky_df)

full_df_2010 <- full_df %>% filter(as.numeric(year)>=2010)

full_df_2010 %>%
  ggplot(aes(x=as.numeric(year), y=dissim, color=group))+
  geom_point(alpha=0.4)+
  geom_line(alpha=0.4)+
  stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = 0.25, ymax = 0.6,
           alpha = .15, fill='red')+
  scale_x_continuous(breaks=2010:2020)+
  xlab("year")+
  ylab("dissimilarity")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))






# Annual dissimilarity by MPA type ---------------------------------------------

#CCFRP
CCFRP_mean_dist2 <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars2$desig_state_year) #calculate mean dissim

CCFRP_mean_dist3 <- melt(CCFRP_mean_dist2) %>% #melt to three column columns
                    mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
                            year_2 = gsub( " .*$", "", Var2),
                            MPA_1 = str_sub(Var1, -3),
                            MPA_2 = str_sub(Var2, -3),
                            group="CCFRP") %>%
                    filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
                    distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
                    select(group, year = year_1, dissim = value)


sim <- anosim(CCFRP_distmat, grouping=CCFRP_group_vars2$desig_state)

#kelp_swath 
kelp_swath_dist2 <- meandist(kelp_swath_distmat, grouping = kelp_swath_group_vars2$desig_state_year)

kelp_swath_dist3 <- melt(kelp_swath_dist2) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = str_sub(Var1, -3),
         MPA_2 = str_sub(Var2, -3),
         group="kelp_swath") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)


#kelp_upc
kelp_upc_dist2 <- meandist(kelp_upc_distmat, grouping = kelp_upc_group_vars2$desig_state_year)

kelp_upc_dist3 <- melt(kelp_upc_dist2) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = str_sub(Var1, -3),
         MPA_2 = str_sub(Var2, -3),
         group="kelp_upc") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)


#kelp_fish
kelp_fish_dist2 <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars2$desig_state_year)

kelp_fish_dist3 <- melt(kelp_fish_dist2) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = str_sub(Var1, -3),
         MPA_2 = str_sub(Var2, -3),
         group="kelp_fish") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)


#deep reef
deep_reef_dist2 <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars2$desig_state_year)

deep_reef_dist3 <- melt(deep_reef_dist2) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = str_sub(Var1, -3),
         MPA_2 = str_sub(Var2, -3),
         group="deep_reef") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)


#rocky
rocky_dist2 <- meandist(rocky_distmat, grouping = rocky_group_vars2$desig_state_year)

rocky_dist3 <- melt(rocky_dist2) %>% #melt to three column columns
  mutate(year_1 = gsub( " .*$", "", Var1), #create selection vars
         year_2 = gsub( " .*$", "", Var2),
         MPA_1 = str_sub(Var1, -3),
         MPA_2 = str_sub(Var2, -3),
         group="rocky") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)



#Join

full_df2 <- rbind(CCFRP_mean_dist3, kelp_swath_dist3, kelp_upc_dist3, kelp_fish_dist3, deep_reef_dist3, rocky_dist3)

#full_df_2010 <- full_df %>% filter(as.numeric(year)>=2010)

full_df2 <- full_df2 %>% filter(year>=2007)

ref_smr_dissim<- full_df2 %>%
  ggplot(aes(x=as.numeric(year), y=dissim, color=group))+
  geom_point(alpha=0.4, aes(shape=group), size=4)+
  geom_line(alpha=0.4)+
  stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = 0.45, ymax = 0.85,
           alpha = .15, fill='red')+
  xlab("year")+
  ylab("dissimilarity")+
  ggtitle("SMR inside vs outside dissimilarity")+
  theme_minimal()



ggsave(here("analyses", "5community_climate_ecology", "figures", "ref_smr_annual_dissimilarity_all_years.png"), ref_smr_dissim, height=4, width = 8, units = "in", 
   dpi = 600, bg="white")




# # MHW dissimilarity by MPA type -------------------------------- --------

#CCFRP
CCFRP_MHW_dist <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars2$desig_state) #calculate mean dissim

CCFRP_MHW_dist2 <- melt(CCFRP_MHW_dist) %>%#melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", Var1), #create selection vars
         MPA_2 = gsub( " .*$", "", Var2),
         MHW_1 = sub("^\\S+\\s+", '', Var1),
         MHW_2 = sub("^\\S+\\s+", '', Var2),
         group="CCFRP") %>%
  filter(MHW_1 == MHW_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(value, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, MHW = MHW_1, dissim = value)%>%
  arrange(factor(MHW, levels = c('before','during','after')))

#kelp_swath
kelp_swath_MHW_dist <- meandist(kelp_swath_distmat, grouping = kelp_swath_group_vars2$desig_state) #calculate mean dissim

kelp_swath_MHW_dist2 <- melt(kelp_swath_MHW_dist) %>%#melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", Var1), #create selection vars
         MPA_2 = gsub( " .*$", "", Var2),
         MHW_1 = sub("^\\S+\\s+", '', Var1),
         MHW_2 = sub("^\\S+\\s+", '', Var2),
         group="kelp_swath") %>%
  filter(MHW_1 == MHW_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(value, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, MHW = MHW_1, dissim = value)%>%
  arrange(factor(MHW, levels = c('before','during','after')))


#kelp_upc
kelp_upc_MHW_dist <- meandist(kelp_upc_distmat, grouping = kelp_upc_group_vars2$desig_state) #calculate mean dissim

kelp_upc_MHW_dist2 <- melt(kelp_upc_MHW_dist) %>%#melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", Var1), #create selection vars
         MPA_2 = gsub( " .*$", "", Var2),
         MHW_1 = sub("^\\S+\\s+", '', Var1),
         MHW_2 = sub("^\\S+\\s+", '', Var2),
         group="kelp_upc") %>%
  filter(MHW_1 == MHW_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(value, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, MHW = MHW_1, dissim = value)%>%
  arrange(factor(MHW, levels = c('before','during','after')))

#kelp_fish
kelp_fish_MHW_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars2$desig_state) #calculate mean dissim

kelp_fish_MHW_dist2 <- melt(kelp_fish_MHW_dist) %>%#melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", Var1), #create selection vars
         MPA_2 = gsub( " .*$", "", Var2),
         MHW_1 = sub("^\\S+\\s+", '', Var1),
         MHW_2 = sub("^\\S+\\s+", '', Var2),
         group="kelp_fish") %>%
  filter(MHW_1 == MHW_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(value, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, MHW = MHW_1, dissim = value)%>%
  arrange(factor(MHW, levels = c('before','during','after')))

#deep_reef
deep_reef_MHW_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars2$desig_state) #calculate mean dissim

deep_reef_MHW_dist2 <- melt(deep_reef_MHW_dist) %>%#melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", Var1), #create selection vars
         MPA_2 = gsub( " .*$", "", Var2),
         MHW_1 = sub("^\\S+\\s+", '', Var1),
         MHW_2 = sub("^\\S+\\s+", '', Var2),
         group="deep_reef") %>%
  filter(MHW_1 == MHW_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(value, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, MHW = MHW_1, dissim = value)%>%
  arrange(factor(MHW, levels = c('before','during','after')))


#rocky
rocky_MHW_dist <- meandist(rocky_distmat, grouping = rocky_group_vars2$desig_state) #calculate mean dissim

rocky_MHW_dist2 <- melt(rocky_MHW_dist) %>%#melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", Var1), #create selection vars
         MPA_2 = gsub( " .*$", "", Var2),
         MHW_1 = sub("^\\S+\\s+", '', Var1),
         MHW_2 = sub("^\\S+\\s+", '', Var2),
         group="rocky") %>%
  filter(MHW_1 == MHW_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(value, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, MHW = MHW_1, dissim = value)%>%
  arrange(factor(MHW, levels = c('before','during','after')))




#Join and plot

MHW_df <- rbind(CCFRP_MHW_dist2, kelp_swath_MHW_dist2, kelp_upc_MHW_dist2, 
                kelp_fish_MHW_dist2, deep_reef_MHW_dist2, rocky_MHW_dist2)

MHW_df %>%
  ggplot(aes(x=factor(MHW, level=c('before','during','after')), y=dissim, color=group, group=group))+
  geom_point(aes(shape=group), size=4)+
  geom_line( aes(color=group))+
  #stat_summary(fun=mean, geom="line",colour="black")+
  #annotate("rect", xmin = 2014, xmax = 2016, ymin = 0.35, ymax = 0.8,
   #        alpha = .15, fill='red')+
  xlab("year")+
  ylab("dissimilarity")+
  theme_minimal()



#ggsave(here("analyses", "5community_climate_ecology", "figures", "ref_smr_annual_dissimilarity.png"), ref_smr_dissim, height=4, width = 8, units = "in", 
#   dpi = 600, bg="white")






# calculate dist between centroids for each year ----------------------------------------

# Question: how far did communities move regardless of MPA status?

#create helper function to calculate distance between centroids. Inputs are 
#grouping vars (group), and distance matrix (x). Be sure that year is formatted
#and arranged in the grouping vars. 

cenfun2 <- function(group, x) {
  
  group$year <- as.factor(group$year)
  levels(group$year)
  n <- nlevels(group$year)
  start <- levels(group$year)[1:(n - 1)]
  end <- levels(group$year)[2:n]
  map2_dfr(start, end, ~ {
    idx1 <- which(group$year == .x)
    idx2 <- which(group$year == .y)
    tibble(
      centroid_1 = .x,
      centroid_2 = .y,
      distance = dist_between_centroids(x, idx1, idx2)
    )
  })
} #start and end are grouping vars, x is distmat



#calculate distances

ccfrp <- cenfun2(group=CCFRP_group_vars, x=CCFRP_distmat)
ccfrp$group <- c("ccfrp")

kelp_upc <- cenfun2(group=kelp_upc_group_vars, x=kelp_upc_distmat)
kelp_upc$group <- c("kelp_upc")

kelp_swath <- cenfun2(group=kelp_swath_group_vars, x=kelp_swath_distmat)
kelp_swath$group <- c("kelp_swath")

kelp_fish <- cenfun2(group=kelp_fish_group_vars, x=kelp_fish_distmat)
kelp_fish$group <- c("kelp_fish")

deep_reef <- cenfun2(group=deep_reef_group_vars, x=deep_reef_distmat)
deep_reef$group <- c("deep_reef")

rocky <- cenfun2(group=rocky_group_vars, x=rocky_distmat)
rocky$group <- c("rocky")

cen_distances <- rbind(ccfrp, kelp_upc, kelp_swath, kelp_fish, deep_reef, rocky)


cen_annual_distance<- cen_distances %>%
  filter(centroid_2>=2010)%>%
  ggplot(aes(x=as.numeric(centroid_2), y=distance, color=group))+
  geom_point(alpha=0.4, aes(shape=group), size=3)+
  geom_line(alpha=0.4)+
  stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = 0, ymax = 0.55,
           alpha = .15, fill='red')+
  xlab("year")+
  ylab("distance")+
  theme_minimal()+theme(aspect.ratio = 1/1.5)

#ggsave(here("analyses", "5community_climate_ecology", "figures", "cen_annual_distances.png"), cen_annual_distance, height=4, width = 8, units = "in", 
#   dpi = 600, bg="white")








# Explore distance between ref and smr by year ----------------------------
#Question: did communities inside and outside of MPAs before more distant after 
#the MHW/=?


#modify helper function
cenfun3 <- function(group, x) {
  
  group$desig_state_year <- as.factor(group$desig_state_year)
  levels(group$desig_state_year)
  n <- nlevels(group$desig_state_year)
  start <- levels(group$desig_state_year)[1:(n - 1)]
  end <- levels(group$desig_state_year)[2:n]
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

CCFRP_mpa <- cenfun3(CCFRP_group_vars2, CCFRP_distmat) %>%
        mutate(year_1 = gsub( " .*$", "", centroid_1 ),
               year_2 = gsub( " .*$", "", centroid_2),
               group="CCFRP")%>%
        filter(year_1 == year_2)
        
kelp_swath_mpa <- cenfun3(kelp_swath_group_vars2, kelp_swath_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group = "kelp_swath")%>%
  filter(year_1 == year_2)

kelp_upc_mpa <- cenfun3(kelp_upc_group_vars2, kelp_upc_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group="kelp_upc")%>%
  filter(year_1 == year_2)

kelp_fish_mpa <- cenfun3(kelp_fish_group_vars2, kelp_fish_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group='kelp_fish')%>%
  filter(year_1 == year_2)

deep_reef_mpa <- cenfun3(deep_reef_group_vars2, deep_reef_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group="deep_reef")%>%
  filter(year_1 == year_2)

rocky_mpa <- cenfun3(rocky_group_vars2, rocky_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group="rocky_mpa")%>%
  filter(year_1 == year_2)

all_mpa <- rbind(CCFRP_mpa, kelp_swath_mpa, kelp_upc_mpa,
                 kelp_fish_mpa, deep_reef_mpa, rocky_mpa)

ref_smr_distance <- all_mpa %>%
  #filter(centroid_2>=2010)%>%
  ggplot(aes(x=as.numeric(year_1), y=distance, color=group))+
  geom_point(alpha=0.4, aes(shape=group), size=3)+
  geom_line(alpha=0.4)+
  stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = 0, ymax = 0.6,
           alpha = .15, fill='red')+
  xlab("year")+
  ylab("distance")+
  ggtitle("SMR inside vs outside distance (Euclidean)")+
  theme_minimal()+theme(aspect.ratio = 1/1.5)

#ggsave(here("analyses", "5community_climate_ecology", "figures", "ref_smr_distance_all_years.png"), ref_smr_distance, height=4, width = 8, units = "in", 
#   dpi = 600, bg="white")









# SIMPER tables Before & after heatwave (no consideration of MPAs)--------------


sim_CCFRP <- with(CCFRP_group_vars2, simper(CCFRP_ord_data, desig_state), ordered=TRUE)
sim_kelp_swath <- with(kelp_swath_group_vars, simper(kelp_swath_ord_data, MHW))
sim_kelp_upc <- with(kelp_upc_group_vars, simper(kelp_upc_ord_data, MHW))
sim_kelp_fish <- with(kelp_fish_group_vars, simper(kelp_fish_ord_data, MHW))
sim_deep_reef <- with(deep_reef_group_vars, simper(deep_reef_ord_data, MHW))
sim_rocky <- with(rocky_group_vars, simper(rocky_ord_data, MHW))






# Cluster analysis --------------------------------------------------------

#Filter 2010 and beyond
CCFRP_10_dist <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars$year)[4:14,4:14]
kelp_invalg_10_dist <- meandist(kelp_invalg_distmat, grouping = kelp_invalg_group_vars$year)[12:22,12:22]
kelp_fish_10_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars$year)[12:22,12:22]
deep_reef_10_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars$year)[4:8,4:8]
rocky_10_dist <- meandist(rocky_distmat, grouping = rocky_group_vars$year)[9:19,9:19]

#run cluster analysis and format for plotting
ccfrp_clust <- hclust(as.dist(CCFRP_10_dist),
                                  method="average")
kelp_invalg_clust <- hclust(as.dist(kelp_invalg_10_dist),
                                  method="average")
kelp_fish_clust <- hclust(as.dist(kelp_fish_10_dist),
                                  method="average")
deep_reef_clust <- hclust(as.dist(deep_reef_10_dist),
                                  method="average")
rocky_clust <- hclust(as.dist(rocky_10_dist),
                            method="average")



#helper function for k clustering
dendro_data_k <- function(hc, k) {
  
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  labclust  <-  cutree(hc, k)[hc$order]
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
    idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  hcdata
}


#define split tree
den_ccfrp <- dendro_data_k(ccfrp_clust, 2)
den_invalg <- dendro_data_k(kelp_invalg_clust, 2)
den_kelp_fish <- dendro_data_k(kelp_fish_clust, 2)
den_deep_reef <- dendro_data_k(deep_reef_clust, 2)
den_rocky <- dendro_data_k(rocky_clust, 2)

#clean up and plot

#CCFRP
ccfrp_seg <- segment(den_ccfrp) %>%
  mutate(yend = ifelse(yend < 0.01, 0.2, yend))

ccfrp_text <- label(den_ccfrp) %>%
  mutate(y = 0.2,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
ccfrp_text$MHW <- factor(ccfrp_text$MHW, levels=c('before', 'during', 'after'))

CCFRP <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=ccfrp_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=12)) +
  scale_y_continuous(limits = c(0.18, 0.4),
                     breaks = seq(0.2, 0.6, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 60, hjust = 1.1, vjust = 1,
            data = ccfrp_text,
            size=5,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("CCFRP")


#kelp invalg
invalg_seg <- segment(den_invalg) %>%
  mutate(yend = ifelse(yend < 0.01, 0.4, yend))

invalg_text <- label(den_invalg) %>%
  mutate(y = 0.4,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
invalg_text$MHW <- factor(invalg_text$MHW, levels=c('before', 'during', 'after'))

invalg <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=invalg_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=12)) +
  scale_y_continuous(limits = c(0.38, 0.55),
                     breaks = seq(0.4, 0.6, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 60, hjust = 1.1, vjust = 1,
            data = invalg_text,
            size=5,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("kelp inverts and algae")




#kelp fish
fish_seg <- segment(den_kelp_fish) %>%
  mutate(yend = ifelse(yend < 0.01, 0.2, yend))

fish_text <- label(den_kelp_fish) %>%
  mutate(y = 0.2,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
fish_text$MHW <- factor(fish_text$MHW, levels=c('before', 'during', 'after'))

kelp_fish <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=fish_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=12)) +
  scale_y_continuous(limits = c(0.17, 0.45),
                     breaks = seq(0.2, 0.45, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 60, hjust = 1.1, vjust = 1,
            data = fish_text,
            size=5,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none") +
  ggtitle("kelp forest fish")




#deep reef
deep_seg <- segment(den_deep_reef) %>%
  mutate(yend = ifelse(yend < 0.01, 0.4, yend))

deep_text <- label(den_deep_reef) %>%
  mutate(y = 0.4,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
deep_text$MHW <- factor(deep_text$MHW, levels=c('before', 'during', 'after'))

deep_reef <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=deep_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=12)) +
  scale_y_continuous(limits = c(0.39, 0.55),
                     breaks = seq(0.2, 0.55, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 60, hjust = 1.1, vjust = 1,
            data = deep_text,
            size=5,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("deep reef")




#rocky
rocky_seg <- segment(den_rocky) %>%
  mutate(yend = ifelse(yend < 0.01, 0.3, yend))

rocky_text <- label(den_rocky) %>%
  mutate(y = 0.3,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
rocky_text$MHW <- factor(rocky_text$MHW, levels=c('before', 'during', 'after'))

rocky <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=rocky_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=12)) +
  scale_y_continuous(limits = c(0.29, 0.4),
                     breaks = seq(0.3, 0.4, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 60, hjust = 1.1, vjust = 1,
            data = rocky_text,
            size=5,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("rocky intertidal")



library(ggpubr)

#create dummy legend as last panel

year <- c(2013, 2016, 2020)
height <- c(1,2,3)
heatwave_period <- c('before','during','after')
legend <- data.frame(year, height, heatwave_period)
legend$heatwave_period <- factor(legend$heatwave_period, levels=c('before', 'during', 'after')) 


p3 <- ggplot(legend, aes(x = year, y = height, color = heatwave_period))+
  geom_point()+
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(1, "cm"),
        legend.text = element_text(size =  12),
        legend.title = element_text(size = 15, face = "bold"))+
  guides(colour = guide_legend(override.aes = list(size=8)))+
  scale_color_manual(values=c('#44b89d','#f56969','#4c78b5'))

p3$labels$colour <- "heatwave period"  

dendro_fig <- ggarrange(CCFRP, kelp_fish, deep_reef, invalg, rocky,
          p3,
          align='h',
          widths = c(1, 1, 1)) #labels=c('A', 'B','C','D', 'E'),
          #common.legend = T)

ggsave(here::here("analyses", "5community_climate_ecology", "figures", "dendro_cluster.png"), dendro_fig, bg="white", dpi = 600, units = "in")




















#extra functions not needed above



set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
  if (fan) {
    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  rep(0, nbLabels)
    hjust[idx]  <-  1
  } else {
    angle       <-  rep(0, nbLabels)
    hjust       <-  0
    if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}
plot_ggdendro <- function(hcdata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          scale.color = NULL,
                          branch.size = 1,
                          label.size  = 3,
                          nudge.label = 0.01,
                          expand.y    = 0.1) {
  
  direction <- match.arg(direction) # if fan = FALSE
  ybreaks   <- pretty(segment(hcdata)$y, n = 5)
  ymax      <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  factor(line),
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 show.legend  =  FALSE,
                 size         =  branch.size)
  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  p <- p +
    geom_text(data        =  label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  label,
                  colour  =  factor(clust),
                  angle   =  angle),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # colors and limits
  if (!is.null(scale.color)) {
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)
  
  p
}
