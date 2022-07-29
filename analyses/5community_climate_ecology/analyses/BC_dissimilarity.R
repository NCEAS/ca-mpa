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


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data/ecological_community_data/year_level"


# load data ---------------------------------------------------------------

#load CCFRP
input_file <- "CCFRP_mpa_year.csv" 
CCFRP_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central')%>%
  select(-total)

#load kelp upc
input_file <- "kelp_upc_mpa_year.csv" 
kelp_upc_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central',
         mpa_defacto_designation=='smr'|mpa_defacto_designation=='ref')

#load kelp swath
input_file <- "kelp_swath_mpa_year.csv" 
kelp_swath_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central')
#load kelp_fish
input_file <- "kelp_fish_mpa_year.csv" 
kelp_fish_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central')

#load deep reef
input_file <- "deep_reef_mpa_year.csv" 
deep_reef_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central')

#load rocky intertidal
input_file <- "rocky_mpa_year.csv" 
rocky_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central')




#CCFRP processing--------------------------------------------------------------
CCFRP_process <- CCFRP_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(10:ncol(.)), na.rm = T)) %>%
  filter(!(sum==0))%>%
  dplyr::select(-sum)%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  arrange(year, desig_state)

#define grouping vars
CCFRP_group_vars <- CCFRP_process %>%
  dplyr::select(1:9)

#define data for ordination
CCFRP_ord_data <- CCFRP_process %>%
  dplyr::select(10:ncol(.))


#calculate relative abundance
CCFRP_rel <- decostand(CCFRP_ord_data, method = "max") %>%
  dplyr::select(where(~any(. !=0)))

#generate a BC dissim matrix
CCFRP_distmat <- vegdist(CCFRP_rel, method = "bray", na.rm=T) 




#kelp swath processing---------------------------------------------------------
kelp_swath <- kelp_swath_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(8:ncol(.), na.rm = T))) %>%
  filter(!(sum==0))%>% #remove rows containing only zeros
  dplyr::select(-sum, unidentified_mobile_invert_species, 
                no_organisms_present_in_this_sample)%>%
  mutate(desig_state = paste(mpa_defacto_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  filter(mpa_defacto_designation=="smr" | mpa_defacto_designation=="ref")%>%
  arrange(desig_state)

#define grouping vars
kelp_swath_group_vars <- kelp_swath%>%
  dplyr::select(1:8)

#define data for ordination
kelp_swath_ord_data <- kelp_swath%>%
  ungroup()%>%
  dplyr::select(9:ncol(.))
#%>%    #remove all-zero columns
#mutate_if(is.character, as.numeric)



#calculate relative abundance
kelp_swath_rel <- decostand(kelp_swath_ord_data, method = "max")

#generate a BC dissim matrix
kelp_swath_distmat <- 
  vegdist(kelp_swath_rel, method = "bray", na.rm=T) #generates a BC dissim matrix




#kelp upc processing---------------------------------------------------------

kelp_upc <- kelp_upc_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(8:ncol(.), na.rm = T))) %>%
  #filter(!(sum==0))%>% #remove rows containing only zeros
  dplyr::select(-sum, bare_rock,unidentified_fish,
                bare_sand, shell_debris)%>%
  mutate(desig_state = paste(mpa_defacto_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  filter(mpa_defacto_designation=="smr" | mpa_defacto_designation=="ref")%>%
  arrange(desig_state)

kelp_upc[is.na(kelp_upc)] = 0                  

#define grouping vars
kelp_upc_group_vars <- kelp_upc%>%
  dplyr::select(1:8)

#define data for ordination
kelp_upc_ord_data <- kelp_upc%>%
  ungroup()%>%
  dplyr::select(9:ncol(.))


#calculate relative abundance
kelp_upc_rel <- decostand(kelp_upc_ord_data, method = "max")

#generate a BC dissim matrix
kelp_upc_distmat <- 
  vegdist(kelp_upc_rel, method = "bray", na.rm=T) #generates a BC dissim matrix



#kelp fish processing---------------------------------------------------------

kelp_fish <- kelp_fish_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(8:ncol(.), na.rm = T))) %>%
  filter(!(sum==0))%>% #remove rows containing only zeros
  dplyr::select(-sum) %>%
  dplyr:: select(where(~ any(. != 0)))%>%
  mutate(desig_state = paste(mpa_defacto_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  filter(mpa_defacto_designation=="smr" | mpa_defacto_designation=="ref")%>%
  arrange(desig_state)

#define grouping vars
kelp_fish_group_vars <- kelp_fish %>%
  dplyr::select(1:8)

#define data for ordination
kelp_fish_ord_data <- kelp_fish %>%
  ungroup() %>%
  dplyr::select(9:ncol(.))

#calculate relative abundance
kelp_fish_rel <- decostand(kelp_fish_ord_data, method = "max")


#generate a BC dissim matrix
kelp_fish_distmat <- 
  vegdist(kelp_fish_rel, method = "bray", na.rm=T) #generates a BC dissim matrix

#deep reef processing---------------------------------------------------------

deep_reef <- deep_reef_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(8:ncol(.), na.rm = T))) %>%
  filter(!(sum==0))%>% #remove rows containing only zeros
  dplyr::select(-c(sum, schooling_10_15_cm_sebastes_sp,
                   schooling_10_15_cm_sebastes_sp,
                   young_of_year_10_cm_sebastes_sp,
                   synodus_lucioceps_or_ophiodon_elongatus,
                   sebastes_melanops_or_mystinus_or_diaconus))%>%
  mutate(desig_state = paste(mpa_defacto_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  filter(mpa_defacto_designation=="smr" | mpa_defacto_designation=="ref")%>%
  arrange(desig_state)

#define grouping vars
deep_reef_group_vars <- deep_reef%>%
  dplyr::select(1:8)

#define data for ordination
deep_reef_ord_data <- deep_reef%>%
  ungroup()%>%
  dplyr::select(9:ncol(.))

#calculate relative abundance
deep_reef_rel <- decostand(deep_reef_ord_data, method="max")

#generate a BC dissim mat
deep_reef_distmat <- 
  vegdist(deep_reef_rel, method = "bray", na.rm=T) #generates a BC dissim matrix

#Intertidal processing---------------------------------------------------------

rocky_counts <- rocky_counts %>%
  mutate(MHW = ifelse(year>=2014 & year<=2016, "during",ifelse(year<2014, "before","after")))%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, MHW, everything())%>%
  filter(mpa_designation=="smr" | mpa_designation=="ref")%>%
  mutate(MHW=factor(MHW)) %>% 
  #mutate(MHW=fct_relevel(MHW,c("before","after"))) %>%
  arrange(desig_state)


#define grouping vars
rocky_group_vars <- rocky_counts%>%
  dplyr::select(1:9)

#define data for ordination
rocky_ord_data <- rocky_counts %>%
  ungroup() %>%
  dplyr::select(10:ncol(.))

#calculate relative abundance
rocky_rel <- decostand(rocky_ord_data, method = "max")

#generate a BC dissim matrix
rocky_distmat <- 
  vegdist(rocky_rel, method = "bray", na.rm=T) #generates a BC dissim matrix




# Insert dummy var --------------------------------------------------------

#add dummy var
CCFRP_group_vars2 <- CCFRP_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_designation))
kelp_swath_group_vars2 <- kelp_swath_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))
kelp_upc_group_vars2 <- kelp_upc_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))
kelp_fish_group_vars2 <- kelp_fish_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))
deep_reef_group_vars2 <- deep_reef_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))
rocky_group_vars2 <- rocky_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_designation))




# Take a look as dissimilarities -----------------------------------------------

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


