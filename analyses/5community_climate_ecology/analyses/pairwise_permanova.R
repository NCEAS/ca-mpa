
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
  filter(MHW=='before' | MHW=='after',
         year>=2007)%>%
  dplyr::select(desig_state, everything())%>%
  arrange(desig_state)

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
  filter(MHW=='before' | MHW=='after',
         year>=2007)%>%
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
  filter(MHW=='before' | MHW=='after',
         year>=2007)%>%
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
  filter(MHW=='before' | MHW=='after',
         year>=2007)%>%
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
  filter(MHW=='before' | MHW=='after',
         year>=2007)%>%
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
  filter(MHW=='before' | MHW=='after')%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, MHW, everything())%>%
  filter(mpa_designation=="smr" | mpa_designation=="ref",
         year>=2007)%>%
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

#NOTE: DO NOT RE ARRANGE GROUP VARS HERE

#add dummy var
CCFRP_group_vars2 <- CCFRP_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_designation)) %>%


kelp_swath_group_vars2 <- kelp_swath_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))%>%


kelp_upc_group_vars2 <- kelp_upc_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))%>%


kelp_fish_group_vars2 <- kelp_fish_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))%>%


deep_reef_group_vars2 <- deep_reef_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_defacto_designation))%>%


rocky_group_vars2 <- rocky_group_vars %>%
  mutate(desig_state_year = paste(year,mpa_designation))%>%






# significance of dissimilarity before vs. after MHW by MPA status--------------

CCFRP_adonis <- pairwise.adonis(CCFRP_distmat, factors=CCFRP_group_vars2$desig_state) %>% mutate(group="CCFRP")
kelp_swath_adonis <- pairwise.adonis(kelp_swath_distmat, factors=kelp_swath_group_vars2$desig_state)  %>% mutate(group="kelp_swath")
kelp_upc_adonis <-  pairwise.adonis(kelp_upc_distmat, factors=kelp_upc_group_vars2$desig_state)  %>% mutate(group="kelp_upc")
kelp_fish_adonis <- pairwise.adonis(kelp_fish_distmat, factors=kelp_fish_group_vars2$desig_state)  %>% mutate(group="kelp_fish")
deep_reef_adonis <- pairwise.adonis(deep_reef_distmat, factors=deep_reef_group_vars2$desig_state)  %>% mutate(group="deep_reef")
rocky_adonis <- pairwise.adonis(rocky_distmat, factors=rocky_group_vars2$desig_state)  %>% mutate(group="rocky")

adonis_output <- rbind(CCFRP_adonis, kelp_swath_adonis,
                       kelp_upc_adonis, kelp_fish_adonis,
                       deep_reef_adonis, rocky_adonis) 

adonis_output_wide <- adonis_output %>%
                      select(pairs, p.value, group)%>%
                      pivot_wider(names_from=group, values_from=p.value)

                    

#create table


adonis_plot <- adonis_output%>%
                mutate(signif = ifelse(p.value<0.05,"yes","no")) %>%
                filter(!(pairs=="ref before vs smr after" | pairs=="ref after vs smr before")) %>%
                arrange(factor(pairs, levels=c("ref before vs smr before",
                                        "ref after vs smr after",
                                        "smr after vs smr before",
                                        "ref after vs ref before")))
     

#pp = ggplot(adonis_plot, aes(x = group, y = pairs, fill = p.value, label = p.value,size=10))

permanova_fig <- adonis_plot%>%
ggplot() +
  geom_tile(aes(x = group, y = factor(pairs, levels=c("ref after vs ref before",
                                                      "smr after vs smr before",
                                                      "ref after vs smr after",
                                                      "ref before vs smr before")),
                color="black", fill=signif)) +
  geom_text(aes(size=10, x = group, y = pairs, label = p.value)) + 
  scale_x_discrete(name = '', expand = c(0, 0)) +
  scale_y_discrete(name = '', expand = c(0, 0)) +
  scale_color_manual(values="black")+
  scale_fill_manual(values=c('#c1f2f3','#f0c8c5'))+
  theme_bw() +
  theme(legend.position = 'none'
  )


ggsave(here("analyses", "5community_climate_ecology", "figures", "pairwise_permanova_output.png"), permanova_fig, height=4, width = 8, units = "in", 
   dpi = 600, bg="white")










