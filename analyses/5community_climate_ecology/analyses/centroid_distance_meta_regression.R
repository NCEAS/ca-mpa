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


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data/ecological_community_data/year_level"


# load data ---------------------------------------------------------------

#load CCFRP
input_file <- "CCFRP_mpa_year.csv" 
CCFRP_counts <- read.csv(file.path(data_path, input_file))%>%
  filter(region4=='central')

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
  filter(MHW=='before'|MHW=='after')#remove rows containing only zeros

#define grouping vars
CCFRP_group_vars <- CCFRP_process %>%
  dplyr::select(1:9)

#define data for ordination
CCFRP_ord_data <- CCFRP_process %>%
  dplyr::select(10:ncol(.))


#calculate relative abundance
CCFRP_rel <- decostand(CCFRP_ord_data, method = "hellinger") %>%
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
  filter(MHW=='before'|MHW=='after')#remove rows containing only zeros #drop sum columns and non-species categories

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
kelp_swath_rel <- decostand(kelp_swath_ord_data, method = "hellinger")

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
  filter(MHW=='before'|MHW=='after')#drop sum columns and non-species categories


kelp_upc[is.na(kelp_upc)] = 0                  

#define grouping vars
kelp_upc_group_vars <- kelp_upc%>%
  dplyr::select(1:8)

#define data for ordination
kelp_upc_ord_data <- kelp_upc%>%
  ungroup()%>%
  dplyr::select(9:ncol(.))


#calculate relative abundance
kelp_upc_rel <- decostand(kelp_upc_ord_data, method = "hellinger")

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
  filter(MHW=='before'|MHW=='after')#drop sum columns and non-species categories  #drop sum columns and non-species categories


#define grouping vars
kelp_fish_group_vars <- kelp_fish %>%
  dplyr::select(1:8)

#define data for ordination
kelp_fish_ord_data <- kelp_fish %>%
  ungroup() %>%
  dplyr::select(9:ncol(.))

#calculate relative abundance
kelp_fish_rel <- decostand(kelp_fish_ord_data, method = "hellinger")

#kelp_fish_rel <- kelp_fish_rel %>% slice_sample(n=2000) # testing if work on smaller

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
  filter(MHW=='before'|MHW=='after')#drop sum columns and non-species categories

#define grouping vars
deep_reef_group_vars <- deep_reef%>%
  dplyr::select(1:8)

#define data for ordination
deep_reef_ord_data <- deep_reef%>%
  ungroup()%>%
  dplyr::select(9:ncol(.))

#calculate relative abundance
deep_reef_rel <- decostand(deep_reef_ord_data, method="hellinger")

#generate a BC dissim mat
deep_reef_distmat <- 
  vegdist(deep_reef_rel, method = "bray", na.rm=T) #generates a BC dissim matrix



#Intertidal processing---------------------------------------------------------

rocky_counts <- rocky_counts %>%
  mutate(MHW = ifelse(year>=2014 & year<=2016, "during",ifelse(year<2014, "before","after")))%>%
  mutate(desig_state = paste(mpa_designation,MHW))%>%
  dplyr::select(desig_state, MHW, everything())%>%
  filter(mpa_designation=="smr" | mpa_designation=="ref")%>%
  filter(MHW=='before'|MHW=='after') %>%
  mutate(MHW=factor(MHW)) %>% 
  #mutate(MHW=fct_relevel(MHW,c("before","after"))) %>%
  arrange(MHW)





#define grouping vars
rocky_group_vars <- rocky_counts%>%
  dplyr::select(1:9)

#define data for ordination
rocky_ord_data <- rocky_counts %>%
  ungroup() %>%
  dplyr::select(10:ncol(.))

#calculate relative abundance
rocky_rel <- decostand(rocky_ord_data, method = "hellinger")

#generate a BC dissim matrix
rocky_distmat <- 
  vegdist(rocky_rel, method = "bray", na.rm=T) #generates a BC dissim matrix







# Plot --------------------------------------------------------------------


CCFRP_disper <- betadisper(CCFRP_distmat, type="centroid", group=CCFRP_group_vars$MHW)
A<- plot(CCFRP_disper, main="CCFRP", col=c('red','blue'))


kelp_swath_disper <- betadisper(kelp_swath_distmat, type="centroid", group=kelp_swath_group_vars$MHW)
B<- plot(kelp_swath_disper, main="kelp swath", col=c('red','blue'))



kelp_upc_disper <- betadisper(kelp_upc_distmat, type="centroid", group=kelp_upc_group_vars$MHW)
C<- plot(kelp_upc_disper, main="kelp upc", col=c('red','blue'))


kelp_fish_disper <- betadisper(kelp_fish_distmat, type="centroid", group=kelp_fish_group_vars$MHW)
D<- plot(kelp_fish_disper, main="kelp fish", col=c('red','blue'))


deep_reef_disper <- betadisper(deep_reef_distmat, type="centroid", group=deep_reef_group_vars$MHW)
E <- plot(deep_reef_disper, main="deep reef", col=c('red','blue'))


rocky_disper <- betadisper(rocky_distmat, type="centroid", group=rocky_group_vars$MHW)
F <- plot(rocky_disper, main="rocky", col=c('red','blue'))



# calculate dist between centroids ----------------------------------------
dist_between_mat <- as.data.frame(matrix(ncol=2, nrow=6))
colnames(dist_between_mat) <- c("dist_between","group")


dist_between_mat[1,1] <- dist_between_centroids(CCFRP_distmat, 1:54,55:86)
dist_between_mat[1,2] <- c("CCFRP")

dist_between_mat[2,1] <- dist_between_centroids(kelp_swath_distmat, 1:210,211:244)
dist_between_mat[2,2]<- c("kelp swath")


dist_between_mat[3,1] <- dist_between_centroids(kelp_upc_distmat, 1:210,211:244)
dist_between_mat[3,2] <- c("kelp upc")

dist_between_mat[4,1] <- dist_between_centroids(kelp_fish_distmat, 1:206,207:241)
dist_between_mat[4,2] <- c("kelp fish")


dist_between_mat[5,1] <- dist_between_centroids(deep_reef_distmat, 1:15,16:32)
dist_between_mat[5,2] <- c("deep reef")


dist_between_mat[6,1]  <- dist_between_centroids(rocky_distmat, 1:191,192:245)
dist_between_mat[6,2] <- c("rocky")



# test for significant dispersion between periods -------------------------

vegan::permutest(CCFRP_disper)
vegan::permutest(kelp_swath_disper)
vegan::permutest(kelp_fish_disper)
vegan::permutest(kelp_upc_disper)
vegan::permutest(deep_reef_disper)
vegan::permutest(rocky_disper)


#permanovas
rocky_perm <- adonis(formula = rocky_distmat ~ MHW, data = rocky_group_vars, permutations = 99) 
deep_reef_perm <- adonis(formula = deep_reef_distmat ~ MHW, data = deep_reef_group_vars, permutations = 99) 
kelp_swath_perm <- adonis(formula = kelp_swath_distmat ~ MHW, data = kelp_swath_group_vars, permutations = 99) 
kelp_fish_perm <- adonis(formula = kelp_fish_distmat ~ MHW, data = kelp_fish_group_vars, permutations = 99) 
kelp_upc_perm <- adonis(formula = kelp_upc_distmat ~ MHW, data = kelp_upc_group_vars, permutations = 99) 
CCFRP_perm <- adonis(formula = CCFRP_distmat ~ MHW, data = CCFRP_group_vars, permutations = 99) 

#save output
rocky_output <-as.data.frame(rocky_perm$aov.tab)[1,]%>%
    mutate(group='rocky')
CCFRP_output <-as.data.frame(CCFRP_perm$aov.tab)[1,]%>%
  mutate(group='CCFRP')
kelp_swath_output <-as.data.frame(kelp_swath_perm$aov.tab)[1,]%>%
  mutate(group='kelp swath')
kelp_upc_output <-as.data.frame(kelp_upc_perm$aov.tab)[1,]%>%
  mutate(group='kelp upc')
kelp_fish_output <-as.data.frame(kelp_fish_perm$aov.tab)[1,]%>%
  mutate(group='kelp fish')
deep_reef_output <-as.data.frame(deep_reef_perm$aov.tab)[1,]%>%
  mutate(group='deep reef')

aov_output <- rbind(rocky_output, CCFRP_output, kelp_swath_output, kelp_upc_output, kelp_fish_output,deep_reef_output)%>%
              dplyr::select(group, everything())%>%
              arrange(desc(F.Model))

rownames(aov_output) <- NULL


#pdf("/home/joshsmith/CA_MPA_Project/ca-mpa/analyses/5community_climate_ecology/tables/permanova_table.pdf")       # Export PDF
#grid.table(aov_output)
#dev.off()




# extract params for meta regression ----------------------------------------

#CCFRP
CCFRP_params <- as.data.frame(cbind(mean=tapply(CCFRP_disper$distances, CCFRP_disper$group, mean), 
                      sd=tapply(CCFRP_disper$distances, CCFRP_disper$group, sd), 
                      n=table(CCFRP_disper$group)))%>%
                mutate(group="CCFRP") %>%
                dplyr::select(group, distance=mean, sd, n) 

CCFRP_params <- cbind(period = rownames(CCFRP_params), CCFRP_params) %>%
                pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 

   

#Kelp swath
kelp_swath_params <- as.data.frame(cbind(mean=tapply(kelp_swath_disper$distances, kelp_swath_disper$group, mean), 
                                    sd=tapply(kelp_swath_disper$distances, kelp_swath_disper$group, sd), 
                                    n=table(kelp_swath_disper$group)))%>%
                mutate(group="kelp swath") %>%
                dplyr::select(group, distance=mean, sd, n) 

kelp_swath_params <- cbind(period = rownames(kelp_swath_params), kelp_swath_params) %>%
                 pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 



#Kelp upc
kelp_upc_params <- as.data.frame(cbind(mean=tapply(kelp_upc_disper$distances, kelp_upc_disper$group, mean), 
                                         sd=tapply(kelp_upc_disper$distances, kelp_upc_disper$group, sd), 
                                         n=table(kelp_upc_disper$group)))%>%
                  mutate(group="kelp upc") %>%
                  dplyr::select(group, distance=mean, sd, n) 

kelp_upc_params <- cbind(period = rownames(kelp_upc_params), kelp_upc_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 





#Kelp fish
kelp_fish_params <- as.data.frame(cbind(mean=tapply(kelp_fish_disper$distances, kelp_fish_disper$group, mean), 
                                       sd=tapply(kelp_fish_disper$distances, kelp_fish_disper$group, sd), 
                                       n=table(kelp_fish_disper$group)))%>%
                     mutate(group="kelp fish") %>%
                      dplyr::select(group, distance=mean, sd, n) 

kelp_fish_params <- cbind(period = rownames(kelp_fish_params), kelp_fish_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 





#deep reef
deep_reef_params <- as.data.frame(cbind(mean=tapply(deep_reef_disper$distances, deep_reef_disper$group, mean), 
                                        sd=tapply(deep_reef_disper$distances, deep_reef_disper$group, sd), 
                                        n=table(deep_reef_disper$group)))%>%
                     mutate(group="deep reef") %>%
                     dplyr::select(group, distance=mean, sd, n) 

deep_reef_params <- cbind(period = rownames(deep_reef_params), deep_reef_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 




#rocky 

rocky_params <- as.data.frame(cbind(mean=tapply(rocky_disper$distances, rocky_disper$group, mean), 
                                        sd=tapply(rocky_disper$distances, rocky_disper$group, sd), 
                                        n=table(rocky_disper$group)))%>%
                 mutate(group="rocky") %>%
                  dplyr::select(group, distance=mean, sd, n) 

rocky_params <- cbind(period = rownames(rocky_params), rocky_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 


#combine

meta_params <- rbind(CCFRP_params, 
                     kelp_swath_params, kelp_upc_params, kelp_fish_params,
                     deep_reef_params,
                     rocky_params)

meta_dist_params <- left_join(meta_params, dist_between_mat, by="group")
                    









# Construct meta regression -----------------------------------------------


#calculate effect size

#https://www.r-bloggers.com/2018/04/v-is-for-meta-analysis-variance/

meta_es <- meta_dist_params %>%
           mutate(SMD = dist_between/
                    (sqrt(((n_before-1)*sd_before^2 + (n_after-1)*sd_after^2) / 
                            (n_before+n_after-2))), #divide distance by pooled SD
                  vi= ((n_before+n_after)/(n_before*n_after)) + (SMD^2/(2*(n_before+n_after)))) 

test <- escalc("SMD", yi=SMD, vi=vi, data=meta_es) 




mlabfun <- function(text, y) {
  bquote(paste(.(text),
               " (Q = ", .(formatC(y$QE, digits=2, format="f")),
               #", df = ", .(y$k - y$p),
               ", p ", .(formatC(y$pval, digits=2, format="f")), #"; ",
               #I^2, " = ", .(formatC(y$I2, digits=1, format="f")), "%, ",
               #tau^2, " = ", .(formatC(y$tau2, digits=2, format="f")), 
               ")"
  )
  )}


forest(test$yi, test$vi, #xlim=c(-0.7,1),
       #ilab.xpos=c(-9.5,-8,-6,-4.5), 
       cex=0.75, 
       #ylim=c(0, 9),
       slab=paste(dat$group),
       order=-test$yi
       #header="Monitoring Group"
)


res <- rma(yi, vi, method="REML", verbose=TRUE, digits=5, data=test)

addpoly(res, row=0.5, cex=0.75, mlab=mlabfun("RE Model", y=res), #col='red'
)
text(-4.5, 0.5, pos=4, cex=0.7, mlabfun("RE Model", y=res))



pooled <- coef(summary(res)) %>%
          mutate(group="pooled",
                 yi=estimate)

#set order
test<- test %>% arrange(desc(-yi))

#add labels
test$community <- c("inverts and algae","fish","inverts and algae","fish","fish","inverts and algae")

test$group <- as.character(test$group)
#Then turn it back into a factor with the levels in the correct order
test$group <- factor(test$group, levels=unique(test$group))

#plot
(figure<-ggplot(test, aes(x=group,yi, color=community),y=yi)+
  geom_point(size=2)+
  geom_errorbar(aes(ymin=yi-vi, ymax=yi+vi), width=.3, size=0.7) +
  geom_vline(xintercept = 0.7, color = "black", size = 0.4, linetype = "dashed") +
  #scale_x_discrete(expand = c(-.03, 1.6) )+
  geom_point(aes(x=0.5, y=yi), data=pooled, shape=18, size = 5, colour="black")+
  geom_errorbar(aes(x=0.5, ymin=yi-se, ymax=yi+se), data=pooled, colour="black", width=.2) +
  ylab("standardized distance")+
  xlab("")+
  coord_flip()+
  theme_minimal(base_size=14)
)



#export distance plot

#ggsave(here("analyses", "5community_climate_ecology", "figures", "distance_meta.png"), figure, height=6, width = 8, units = "in", 
#       dpi = 300, bg="white")









