#author: "Joshua G. Smith"
#date: '2022-08-02'

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

#use central region only for comprehensive sampling
#use two years prior and two years after MHW (2012 - 2018)

#load CCFRP
input_file <- "CCFRP_mpa_year.csv" 
CCFRP_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central',
         year>=2012 & year<=2019)%>%
  select(-total)

#load kelp upc
input_file <- "kelp_upc_mpa_year.csv" 
kelp_upc_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central',
         mpa_defacto_designation=='smr'|mpa_defacto_designation=='ref',
         year>=2012 & year<=2019)

#load kelp swath
input_file <- "kelp_swath_mpa_year.csv" 
kelp_swath_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central',
         year>=2012 & year<=2019)

#load kelp_fish
input_file <- "kelp_fish_mpa_year.csv" 
kelp_fish_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central',
         year>=2012 & year<=2019)

#load deep reef
input_file <- "deep_reef_mpa_year.csv" 
deep_reef_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central',
         year>=2012 & year<=2019
         )

#load rocky intertidal
input_file <- "rocky_mpa_year.csv" 
rocky_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central',
         year>=2012 & year<=2019)




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


# calculate distance traveled  ----------------------------------------

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



#calculate distances for each year

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

travel_distance <- cen_distances %>%
                         group_by(group)%>%
                         summarise(total_distance=sum(distance))






#calculate distance before to after
cenfun3 <- function(group, x) {
  
  group$MHW <- as.factor(group$MHW)
  levels(group$MHW)
  n <- nlevels(group$MHW)
  start <- levels(group$MHW)[1:(n - 1)]
  end <- levels(group$MHW)[2:n]
  map2_dfr(start, end, ~ {
    idx1 <- which(group$MHW == .x)
    idx2 <- which(group$MHW == .y)
    tibble(
      centroid_1 = .x,
      centroid_2 = .y,
      distance = dist_between_centroids(x, idx1, idx2)
    )
  })
} #start and end are grouping vars, x is distmat



#calculate distances for each year

ccfrp <- cenfun3(group=CCFRP_group_vars, x=CCFRP_distmat) %>% filter(centroid_1 == 'after')
ccfrp$group <- c("ccfrp")

kelp_upc <- cenfun3(group=kelp_upc_group_vars, x=kelp_upc_distmat) %>% filter(centroid_1 == 'after')
kelp_upc$group <- c("kelp_upc")

kelp_swath <- cenfun3(group=kelp_swath_group_vars, x=kelp_swath_distmat) %>% filter(centroid_1 == 'after')
kelp_swath$group <- c("kelp_swath")

kelp_fish <- cenfun3(group=kelp_fish_group_vars, x=kelp_fish_distmat) %>% filter(centroid_1 == 'after')
kelp_fish$group <- c("kelp_fish")

deep_reef <- cenfun3(group=deep_reef_group_vars, x=deep_reef_distmat)# %>% filter(centroid_1 == 'after')
deep_reef$group <- c("deep_reef")

rocky <- cenfun3(group=rocky_group_vars, x=rocky_distmat) %>% filter(centroid_1 == 'after')
rocky$group <- c("rocky")

cen_MHW_distances <- rbind(ccfrp, kelp_upc, kelp_swath, kelp_fish, deep_reef, rocky)%>%
  group_by(group)%>%
  summarise(before_to_after=sum(distance))


distance_df <- left_join(travel_distance, cen_MHW_distances, by="group")%>%
                pivot_longer(cols=c('total_distance','before_to_after'))
             

distance_df %>%
  ggplot(aes(x=forcats::fct_reorder2(group, name=='total_distance', value, .desc=T), y=as.numeric(value), fill=name))+
  geom_bar(stat='identity', position='dodge')+
  #stat_summary(fun=mean, geom="line",colour="black", size=1)+
  #annotate("rect", xmin = 2014, xmax = 2016, ymin = 0, ymax = 0.55,
      #     alpha = .15, fill='red')+
  xlab("year")+
  ylab("distance")+
  theme_minimal()+theme(aspect.ratio = 1/1.5)

#ggsave(here("analyses", "5community_climate_ecology", "figures", "cen_annual_distances.png"), cen_annual_distance, height=4, width = 8, units = "in", 
#   dpi = 600, bg="white")

















# Explore distance by ref and smr by year ----------------------------

#step 1 - calculate distance for ref
#CCFRP processing--------------------------------------------------------------
CCFRP_process <- CCFRP_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(10:ncol(.)), na.rm = T)) %>%
  filter(!(sum==0))%>%
  dplyr::select(-sum)%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  arrange(year)%>%
  filter(mpa_designation=='ref')

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
  filter(mpa_defacto_designation=="ref")%>%
  arrange(year, desig_state)

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
  filter(mpa_defacto_designation=="ref")%>%
  arrange(year, desig_state)

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
  filter( mpa_defacto_designation=="ref")%>%
  arrange(year, desig_state)

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
  filter( mpa_defacto_designation=="ref")%>%
  arrange(year, desig_state)

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

rocky_counts1 <- rocky_counts %>%
  mutate(MHW = ifelse(year>=2014 & year<=2016, "during",ifelse(year<2014, "before","after")))%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, MHW, everything())%>%
  filter( mpa_designation=="ref")%>%
  mutate(MHW=factor(MHW)) %>% 
  #mutate(MHW=fct_relevel(MHW,c("before","after"))) %>%
  arrange(year, desig_state)


#define grouping vars
rocky_group_vars <- rocky_counts1%>%
  dplyr::select(1:9)

#define data for ordination
rocky_ord_data <- rocky_counts1 %>%
  ungroup() %>%
  dplyr::select(10:ncol(.))

#calculate relative abundance
rocky_rel <- decostand(rocky_ord_data, method = "max")

#generate a BC dissim matrix
rocky_distmat <- 
  vegdist(rocky_rel, method = "bray", na.rm=T) #generates a BC dissim matrix












CCFRP_mpa <- cenfun2(CCFRP_group_vars, CCFRP_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "ref",
         group="CCFRP") 

kelp_swath_mpa <- cenfun2(kelp_swath_group_vars, kelp_swath_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "ref",
         group = "kelp_swath")

kelp_upc_mpa <- cenfun2(kelp_upc_group_vars, kelp_upc_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "ref",
         group="kelp_upc")


kelp_fish_mpa <- cenfun2(kelp_fish_group_vars, kelp_fish_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "ref",
         group='kelp_fish')
  

deep_reef_mpa <- cenfun2(deep_reef_group_vars, deep_reef_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "ref",
         group="deep_reef")


rocky_mpa <- cenfun2(rocky_group_vars, rocky_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "ref",
         group="rocky_mpa")


ref_desig <- rbind(CCFRP_mpa, kelp_swath_mpa, kelp_upc_mpa,
                 kelp_fish_mpa, deep_reef_mpa, rocky_mpa)








# step2 calculate distance for smr ----------------------------------------


#step 1 - sort by year and mpa_designation status
#CCFRP processing--------------------------------------------------------------
CCFRP_process <- CCFRP_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(10:ncol(.)), na.rm = T)) %>%
  filter(!(sum==0))%>%
  dplyr::select(-sum)%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  arrange(year)%>%
  filter(mpa_designation=='smr')

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
  filter(mpa_defacto_designation=="smr")%>%
  arrange(year, desig_state)

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
  filter(mpa_defacto_designation=="smr")%>%
  arrange(year, desig_state)

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
  filter( mpa_defacto_designation=="smr")%>%
  arrange(year, desig_state)

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
  filter( mpa_defacto_designation=="smr")%>%
  arrange(year, desig_state)

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

rocky_counts2 <- rocky_counts %>%
  mutate(MHW = ifelse(year>=2014 & year<=2016, "during",ifelse(year<2014, "before","after")))%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, MHW, everything())%>%
  filter(mpa_designation=="smr")%>%
  mutate(MHW=factor(MHW)) %>% 
  #mutate(MHW=fct_relevel(MHW,c("before","after"))) %>%
  arrange(year, desig_state)

#define grouping vars
rocky_group_vars <- rocky_counts2%>%
  dplyr::select(1:9)

#define data for ordination
rocky_ord_data <- rocky_counts2 %>%
  ungroup() %>%
  dplyr::select(10:ncol(.))

#calculate relative abundance
rocky_rel <- decostand(rocky_ord_data, method = "max")

#generate a BC dissim matrix
rocky_distmat <- 
  vegdist(rocky_rel, method = "bray", na.rm=T) #generates a BC dissim matrix




CCFRP_mpa <- cenfun2(CCFRP_group_vars, CCFRP_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "smr",
         group="CCFRP") 

kelp_swath_mpa <- cenfun2(kelp_swath_group_vars, kelp_swath_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "smr",
         group = "kelp_swath")

kelp_upc_mpa <- cenfun2(kelp_upc_group_vars, kelp_upc_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "smr",
         group="kelp_upc")


kelp_fish_mpa <- cenfun2(kelp_fish_group_vars, kelp_fish_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "smr",
         group='kelp_fish')


deep_reef_mpa <- cenfun2(deep_reef_group_vars, deep_reef_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "smr",
         group="deep_reef")


rocky_mpa <- cenfun2(rocky_group_vars, rocky_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         mpa_designation = "smr",
         group="rocky_mpa")


smr_desig <- rbind(CCFRP_mpa, kelp_swath_mpa, kelp_upc_mpa,
                   kelp_fish_mpa, deep_reef_mpa, rocky_mpa)


mpa_table <- rbind(ref_desig, smr_desig) %>%
              group_by(group, mpa_designation)%>%
              summarize(total_distance = sum(distance))





#calculate distances for each time period

cenfun4 <- function(group, x) {
  
  group$desig_state <- as.factor(group$desig_state)
  levels(group$desig_state)
  n <- nlevels(group$desig_state)
  start <- levels(group$desig_state)[1:(n - 1)]
  end <- levels(group$desig_state)[2:n]
  map2_dfr(start, end, ~ {
    idx1 <- which(group$desig_state == .x)
    idx2 <- which(group$desig_state == .y)
    tibble(
      centroid_1 = .x,
      centroid_2 = .y,
      distance = dist_between_centroids(x, idx1, idx2)
    )
  })
}

ccfrp <- cenfun4(group=CCFRP_group_vars, x=CCFRP_distmat) %>% filter(centroid_1 == 'smr after')
ccfrp$group <- c("ccfrp")

kelp_upc <- cenfun4(group=kelp_upc_group_vars, x=kelp_upc_distmat) %>% filter(centroid_1 == 'smr after')
kelp_upc$group <- c("kelp_upc")

kelp_swath <- cenfun4(group=kelp_swath_group_vars, x=kelp_swath_distmat) %>% filter(centroid_1 == 'smr after')
kelp_swath$group <- c("kelp_swath")

kelp_fish <- cenfun4(group=kelp_fish_group_vars, x=kelp_fish_distmat) %>% filter(centroid_1 == 'smr after')
kelp_fish$group <- c("kelp_fish")

deep_reef <- cenfun4(group=deep_reef_group_vars, x=deep_reef_distmat) %>% filter(centroid_1 == 'smr after')
deep_reef$group <- c("deep_reef")

rocky <- cenfun4(group=rocky_group_vars, x=rocky_distmat) %>% filter(centroid_1 == 'smr after')
rocky$group <- c("rocky")

cen_MHW_smr <- rbind(ccfrp, kelp_upc, kelp_swath, kelp_fish, deep_reef, rocky)%>%
  group_by(group)%>%
  summarise(before_to_after=sum(distance))%>%
  mutate(mpa_designation = "smr")

#centroids for rref

ccfrp <- cenfun4(group=CCFRP_group_vars, x=CCFRP_distmat) %>% filter(centroid_1 == 'ref after')
ccfrp$group <- c("ccfrp")

kelp_upc <- cenfun4(group=kelp_upc_group_vars, x=kelp_upc_distmat) %>% filter(centroid_1 == 'ref after')
kelp_upc$group <- c("kelp_upc")

kelp_swath <- cenfun4(group=kelp_swath_group_vars, x=kelp_swath_distmat) %>% filter(centroid_1 == 'ref after')
kelp_swath$group <- c("kelp_swath")

kelp_fish <- cenfun4(group=kelp_fish_group_vars, x=kelp_fish_distmat) %>% filter(centroid_1 == 'ref after')
kelp_fish$group <- c("kelp_fish")

deep_reef <- cenfun4(group=deep_reef_group_vars, x=deep_reef_distmat) %>% filter(centroid_1 == 'ref after')
deep_reef$group <- c("deep_reef")

rocky <- cenfun4(group=rocky_group_vars, x=rocky_distmat) %>% filter(centroid_1 == 'ref after')
rocky$group <- c("rocky")

cen_MHW_ref <- rbind(ccfrp, kelp_upc, kelp_swath, kelp_fish, deep_reef, rocky)%>%
  group_by(group)%>%
  summarise(before_to_after=sum(distance))%>%
  mutate(mpa_designation = "ref")

MHW_table <- rbind(cen_MHW_ref, cen_MHW_smr)

MHW_table$group <- recode_factor(MHW_table$group, 'rocky'='rocky_mpa')
MHW_table$group <- recode_factor(MHW_table$group, 'ccfrp'='CCFRP')


distance_MPA_table <- left_join(MHW_table, mpa_table, by=c("group","mpa_designation"))%>%
  pivot_longer(cols=c('total_distance','before_to_after'))

#save(distance_MPA_table, file = file.path("/home/shares/ca-mpa/data/sync-data/processed_data/ecological_community_data" ,"distance_MPA_table.RData"))

 travel_distance <- distance_MPA_table%>%
   ggplot(aes(x=forcats::fct_reorder2(group, name=='total_distance', value, .desc=T), y=as.numeric(value), fill=name))+
   geom_bar(stat='identity', position='dodge')+
   #stat_summary(fun=mean, geom="line",colour="black", size=1)+
   #annotate("rect", xmin = 2014, xmax = 2016, ymin = 0, ymax = 0.55,
   #     alpha = .15, fill='red')+
   xlab("year")+
   ylab("distance")+
   facet_wrap(~mpa_designation)+
   geom_hline(data = distance_MPA_table %>% filter(mpa_designation == "ref" & name=="before_to_after"), aes(yintercept = median(value),
                                                                                                            color='#00BFC4'),
              linetype="dashed")+
   geom_hline(data = distance_MPA_table %>% filter(mpa_designation == "ref" & name=="total_distance"), aes(yintercept = median(value),
                                                                                                           color='#F8766D'),
              linetype="dashed")+
   geom_hline(data = distance_MPA_table %>% filter(mpa_designation == "smr" & name=="before_to_after"), aes(yintercept = median(value), 
                                                                                                            color='#00BFC4'),
              linetype="dashed")+
   geom_hline(data = distance_MPA_table %>% filter(mpa_designation == "smr" & name=="total_distance"), aes(yintercept = median(value),
                                                                                                           color='#F8766D'),
              linetype="dashed")+
   theme_minimal(base_size = 8)+theme(aspect.ratio = 1/1.5,
                         strip.text.x = element_text(
                           size = 16, color = "black"
                         ))+
   guides(color = "none",
          fill=guide_legend(title="distance"))

#ggsave(here("analyses", "5community_climate_ecology", "figures", "travel_vs_MHW_distance.png"), travel_distance, height=4, width = 8, units = "in", 
#  dpi = 600, bg="white")








