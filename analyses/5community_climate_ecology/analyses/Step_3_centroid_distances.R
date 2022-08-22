#author: "Joshua G. Smith"
#date: '2022-07-13'

rm(list=ls())

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



# #load data --------------------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))




# Step 1 - examine hull shape and dispersion -----------------------------------


CCFRP_disper <- betadisper(CCFRP_distmat, type="centroid", 
                           group=CCFRP_group_vars$desig_state)
A<- plot(CCFRP_disper, main="CCFRP", col=c('red','blue'))


kelp_swath_disper <- betadisper(kelp_swath_distmat, type="centroid", 
                                group=kelp_swath_group_vars$desig_state)
B<- plot(kelp_swath_disper, main="kelp swath", col=c('red','blue'))



kelp_upc_disper <- betadisper(kelp_upc_distmat, type="centroid", 
                              group=kelp_upc_group_vars$desig_state)
C<- plot(kelp_upc_disper, main="kelp upc", col=c('red','blue'))


kelp_fish_disper <- betadisper(kelp_fish_distmat, type="centroid", 
                               group=kelp_fish_group_vars$desig_state)
D<- plot(kelp_fish_disper, main="kelp fish", col=c('red','blue'))


deep_reef_disper <- betadisper(deep_reef_distmat, type="centroid", 
                               group=deep_reef_group_vars$desig_state)
E <- plot(deep_reef_disper, main="deep reef", col=c('red','blue'))


rocky_disper <- betadisper(rocky_distmat, type="centroid", 
                           group=rocky_group_vars$desig_state)
F <- plot(rocky_disper, main="rocky", col=c('red','blue'))



# step 2 calculate dist between centroids inside and outide of MPAs-------------

#create helper function to calculate centroid distance
cenfun <- function(group, x) {
  
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

#CCFRP
ccfrp_cen <- cenfun(group=CCFRP_group_vars, x=CCFRP_distmat) %>% 
  filter(centroid_1 == 'smr after' |
         centroid_1 == 'ref after')%>%
  mutate(group="CCFRP")
                            
kelp_swath_cen <- cenfun(group=kelp_swath_group_vars, x=kelp_swath_distmat) %>% 
  filter(centroid_1 == 'smr after' |
   centroid_1 == 'ref after')%>%
  mutate(group='kelp swath')
  
kelp_upc_cen <- cenfun(group=kelp_upc_group_vars, x=kelp_upc_distmat) %>% 
  filter(centroid_1 == 'smr after' |
  centroid_1 == 'ref after')%>%
  mutate(group='kelp upc')

kelp_fish_cen <-cenfun(group=kelp_fish_group_vars, x=kelp_fish_distmat) %>% 
  filter(centroid_1 == 'smr after' |
 centroid_1 == 'ref after')%>%
  mutate(group='kelp fish')

deep_reef_cen <-cenfun(group=deep_reef_group_vars, x=deep_reef_distmat) %>% 
  filter(centroid_1 == 'smr after' |
  centroid_1 == 'ref after')%>%
  mutate(group='deep reef')

rocky_cen <-cenfun(group=rocky_group_vars, x=rocky_distmat) %>% 
  filter(centroid_1 == 'smr after' |
  centroid_1 == 'ref after')%>%
  mutate(group='rocky')


cen_distances <- rbind(ccfrp_cen, kelp_swath_cen, kelp_upc_cen,
                       kelp_fish_cen, deep_reef_cen, rocky_cen)

cen_distances2 <- cen_distances %>%
                  mutate(mpa_type = word(centroid_1, start = 1))%>%
                  select(!(1:2)) %>%
                  pivot_wider(names_from = mpa_type, values_from = distance) %>%
                  select(group, distance_between_ref='ref',
                         distance_between_smr = 'smr')

cen_distances2 <- as.data.frame(cen_distances2)


# test for significant dispersion between periods -----------------------------

vegan::permutest(CCFRP_disper)
vegan::permutest(kelp_swath_disper)
vegan::permutest(kelp_fish_disper)
vegan::permutest(kelp_upc_disper)
vegan::permutest(deep_reef_disper)
vegan::permutest(rocky_disper)


#permanovas
rocky_perm <- adonis2(formula = rocky_distmat ~ MHW +  mpa_designation, data = rocky_group_vars, permutations = 99) 
deep_reef_perm <- adonis2(formula = deep_reef_distmat ~ MHW+desig_state, data = deep_reef_group_vars, permutations = 99) 
kelp_swath_perm <- adonis2(formula = kelp_swath_distmat ~ MHW+desig_state, data = kelp_swath_group_vars, permutations = 99) 
kelp_fish_perm <- adonis2(formula = kelp_fish_distmat ~ MHW+desig_state, data = kelp_fish_group_vars, permutations = 99) 
kelp_upc_perm <- adonis2(formula = kelp_upc_distmat ~ MHW+desig_state, data = kelp_upc_group_vars, permutations = 99) 
CCFRP_perm <- adonis2(formula = CCFRP_distmat ~ MHW+desig_state, data = CCFRP_group_vars, permutations = 99) 

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

aov_output <- rbind(rocky_output, CCFRP_output, kelp_swath_output, 
                    kelp_upc_output, kelp_fish_output,deep_reef_output)%>%
  dplyr::select(group, everything())
  #arrange(desc(F.Model))

rownames(aov_output) <- NULL


#pdf("/home/joshsmith/CA_MPA_Project/ca-mpa/analyses/5community_climate_ecology/tables/permanova_table.pdf")       # Export PDF
#grid.table(aov_output)
#dev.off()




# extract params for meta regression ----------------------------------------

#CCFRP
CCFRP_params <- as.data.frame(cbind(mean=tapply(CCFRP_disper$distances, 
                                                CCFRP_disper$group, mean), 
                                    sd=tapply(CCFRP_disper$distances, 
                                              CCFRP_disper$group, sd), 
                                    n=table(CCFRP_disper$group)))%>%
  mutate(group="CCFRP") %>%
  dplyr::select(group, distance=mean, sd, n) 

CCFRP_params <- cbind(period = rownames(CCFRP_params), CCFRP_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 



#Kelp swath
kelp_swath_params <- as.data.frame(cbind(mean=tapply(
  kelp_swath_disper$distances, kelp_swath_disper$group, mean), 
                                         sd=tapply(kelp_swath_disper$distances, 
                                                   kelp_swath_disper$group, sd), 
                                         n=table(kelp_swath_disper$group)))%>%
  mutate(group="kelp swath") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_swath_params <- cbind(period = rownames(kelp_swath_params),
                           kelp_swath_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 



#Kelp upc
kelp_upc_params <- as.data.frame(cbind(mean=tapply(kelp_upc_disper$distances, 
                                                   kelp_upc_disper$group, mean), 
                                       sd=tapply(kelp_upc_disper$distances,
                                                 kelp_upc_disper$group, sd), 
                                       n=table(kelp_upc_disper$group)))%>%
  mutate(group="kelp upc") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_upc_params <- cbind(period = rownames(kelp_upc_params), 
                         kelp_upc_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 





#Kelp fish
kelp_fish_params <- as.data.frame(cbind(mean=tapply(kelp_fish_disper$distances,
                                                    kelp_fish_disper$group, 
                                                    mean), 
                                        sd=tapply(kelp_fish_disper$distances,
                                                  kelp_fish_disper$group, sd), 
                                        n=table(kelp_fish_disper$group)))%>%
  mutate(group="kelp fish") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_fish_params <- cbind(period = rownames(kelp_fish_params), kelp_fish_params) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 





#deep reef
deep_reef_params <- as.data.frame(cbind(mean=tapply(deep_reef_disper$distances,
                                                    deep_reef_disper$group, mean), 
                                        sd=tapply(deep_reef_disper$distances,
                                                  deep_reef_disper$group, sd), 
                                        n=table(deep_reef_disper$group)))%>%
  mutate(group="deep reef") %>%
  dplyr::select(group, distance=mean, sd, n) 

deep_reef_params <- cbind(period = rownames(deep_reef_params), deep_reef_params)
%>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 




#rocky 

rocky_params <- as.data.frame(cbind(mean=tapply(rocky_disper$distances, 
                                                rocky_disper$group, mean), 
                                    sd=tapply(rocky_disper$distances, 
                                              rocky_disper$group, sd), 
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

meta_dist_params <- left_join(meta_params, cen_distances2, by="group")



# Construct meta regression -----------------------------------------------


#calculate effect size

#https://www.r-bloggers.com/2018/04/v-is-for-meta-analysis-variance/
meta_dist_params <- meta_dist_params%>%
    clean_names()

meta_es <- meta_dist_params %>%
  mutate(SMD_ref = distance_between_ref/
           (sqrt(((n_ref_before-1)*sd_ref_before^2 + (n_ref_after-1)*sd_ref_after^2) / 
                   (n_ref_before+n_ref_after-2))), #divide distance by pooled SD
         ref_vi= ((n_ref_before+n_ref_after)/(n_ref_before*n_ref_after)) + (SMD_ref^2/(2*(n_ref_before+n_ref_after))),
         SMD_smr = distance_between_smr/
           (sqrt(((n_smr_before-1)*sd_smr_before^2 + (n_smr_after-1)*sd_smr_after^2) / 
                   (n_smr_before+n_smr_after-2))), 
         smr_vi= ((n_smr_before+n_smr_after)/(n_smr_before*n_smr_after)) + (SMD_smr^2/(2*(n_smr_before+n_smr_after)))
           ) 



REF_es <- escalc("SMD", yi=SMD_ref, vi=ref_vi, data=meta_es) 
SMR_es <- escalc("SMD", yi=SMD_smr, vi = smr_vi, data=meta_es)



res_ref <- rma(yi, vi, method="REML", verbose=TRUE, digits=5, data=REF_es)
res_smr <- rma(yi, vi, method="REML", verbose=TRUE, digits=5, data=SMR_es)


pooled_ref <- coef(summary(res_ref)) %>%
  mutate(group="pooled",
         yi=estimate)

pooled_smr <- coef(summary(res_smr)) %>%
  mutate(group="pooled",
         yi=estimate)

#set order
test_ref<- REF_es %>% arrange(desc(-yi))
test_smr <- SMR_es %>% arrange(desc(-yi))

#add labels
test_ref$community <- c("inverts and algae","fish","inverts and algae","fish","fish","inverts and algae")
test_smr$community <- c("inverts and algae","fish","inverts and algae","fish","fish","inverts and algae")

test_ref$group <- as.character(test_ref$group)
test_smr$group <- as.character(test_smr$group)
#Then turn it back into a factor with the levels in the correct order
test_smr$group <- factor(test_smr$group, levels=unique(test_smr$group))
test_ref$group <- factor(test_ref$group, levels=unique(test_ref$group))

#plot
(figure<-ggplot(test_smr, aes(x=group,yi),y=yi)+
    geom_point(size=2, color='#EB6977', position = position_jitter(seed = 123, width =0.2))+
    geom_errorbar(aes(ymin=yi-vi, ymax=yi+vi), color='#EB6977',width=.3, size=0.7, data=test_smr, position = position_jitter(seed = 123, width =0.2)) +
    geom_point(data=test_ref, color='#13A0DD')+
    geom_errorbar(aes(ymin=yi-vi, ymax=yi+vi), width=.3, size=0.7, data=test_ref, color='#13A0DD')+
    geom_vline(xintercept = 0.7, color = "black", size = 0.4, linetype = "dashed") +
    scale_x_discrete(expand = c(0.2, 0.1) )+
    geom_point(aes(x=0.5, y=yi), data=pooled_ref, shape=18, size = 3, colour='#13A0DD')+
    geom_errorbar(aes(x=0.5, ymin=yi-se, ymax=yi+se), data=pooled_ref, colour='#13A0DD', width=.2) +
    geom_point(aes(x=0.18, y=yi), data=pooled_smr, shape=18, size = 3, colour='#EB6977')+
    geom_errorbar(aes(x=0.18, ymin=yi-se, ymax=yi+se), data=pooled_smr, colour='#EB6977', width=.2) +
    ylab("standardized distance")+
    xlab("")+
    coord_flip()+
    theme_minimal(base_size=14) + theme(aspect.ratio = 1/1.5)
)



#export distance plot

#ggsave(here("analyses", "5community_climate_ecology", "figures", "distance_MPA.png"), figure, height=4, width = 5, units = "in", 
 #      dpi = 600, bg="white")











# Step 4 - centroid distance by MHW period (no MPAs) ---------------------------


#create helper function to calculate centroid distance
cenfun <- function(group, x) {
  
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
}



#calculate distances
#CCFRP
ccfrp_MHW <- cenfun(group=CCFRP_group_vars, x=CCFRP_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='CCFRP')

kelp_swath_MHW <- cenfun(group=kelp_swath_group_vars, x=kelp_swath_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='kelp swath')

kelp_upc_MHW <- cenfun(group=kelp_upc_group_vars, x=kelp_upc_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='kelp upc')

kelp_fish_MHW <-cenfun(group=kelp_fish_group_vars, x=kelp_fish_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='kelp fish')

deep_reef_MHW <-cenfun(group=deep_reef_group_vars, x=deep_reef_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='deep reef')

rocky_MHW <-cenfun(group=rocky_group_vars, x=rocky_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='rocky')


cen_distances_MHW <- rbind(ccfrp_MHW, kelp_swath_MHW, kelp_upc_MHW,
                       kelp_fish_MHW, deep_reef_MHW, rocky_MHW) %>%
                    select(!(c(centroid_1, centroid_2)))


#calculate dispersion params

CCFRP_disper_MHW <- betadisper(CCFRP_distmat, type="centroid", 
                           group=CCFRP_group_vars$MHW)

kelp_swath_disper_MHW <- betadisper(kelp_swath_distmat, type="centroid",
                                group=kelp_swath_group_vars$MHW)

kelp_upc_disper_MHW <- betadisper(kelp_upc_distmat, type="centroid",
                              group=kelp_upc_group_vars$MHW)

kelp_fish_disper_MHW <- betadisper(kelp_fish_distmat, type="centroid", 
                               group=kelp_fish_group_vars$MHW)

deep_reef_disper_MHW <- betadisper(deep_reef_distmat, type="centroid", 
                               group=deep_reef_group_vars$MHW)

rocky_disper_MHW <- betadisper(rocky_distmat, type="centroid", 
                           group=rocky_group_vars$MHW)

#extract params
#CCFRP
CCFRP_params_MHW <- as.data.frame(cbind(mean=tapply(CCFRP_disper_MHW$distances, 
                                                CCFRP_disper_MHW$group, mean), 
                                    sd=tapply(CCFRP_disper_MHW$distances, 
                                              CCFRP_disper_MHW$group, sd), 
                                    n=table(CCFRP_disper_MHW$group)))%>%
  mutate(group="CCFRP") %>%
  dplyr::select(group, distance=mean, sd, n) 

CCFRP_params_MHW <- cbind(period = rownames(CCFRP_params_MHW), 
                          CCFRP_params_MHW) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 


#Kelp swath
kelp_swath_params_MHW <- as.data.frame(cbind(mean=tapply(kelp_swath_disper_MHW$distances, 
                                                     kelp_swath_disper_MHW$group, mean), 
                                         sd=tapply(kelp_swath_disper_MHW$distances, 
                                                   kelp_swath_disper_MHW$group, sd), 
                                         n=table(kelp_swath_disper_MHW$group)))%>%
  mutate(group="kelp swath") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_swath_params_MHW <- cbind(period = rownames(kelp_swath_params_MHW), kelp_swath_params_MHW) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 



#Kelp upc
kelp_upc_params_MHW <- as.data.frame(cbind(mean=tapply(kelp_upc_disper_MHW$distances, kelp_upc_disper_MHW$group, mean), 
                                       sd=tapply(kelp_upc_disper_MHW$distances, kelp_upc_disper_MHW$group, sd), 
                                       n=table(kelp_upc_disper_MHW$group)))%>%
  mutate(group="kelp upc") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_upc_params_MHW <- cbind(period = rownames(kelp_upc_params_MHW), kelp_upc_params_MHW) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 





#Kelp fish
kelp_fish_params_MHW <- as.data.frame(cbind(mean=tapply(kelp_fish_disper_MHW$distances, kelp_fish_disper_MHW$group, mean), 
                                        sd=tapply(kelp_fish_disper_MHW$distances, kelp_fish_disper_MHW$group, sd), 
                                        n=table(kelp_fish_disper_MHW$group)))%>%
  mutate(group="kelp fish") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_fish_params_MHW <- cbind(period = rownames(kelp_fish_params_MHW), kelp_fish_params_MHW) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 





#deep reef
deep_reef_params_MHW <- as.data.frame(cbind(mean=tapply(deep_reef_disper_MHW$distances, deep_reef_disper_MHW$group, mean), 
                                        sd=tapply(deep_reef_disper_MHW$distances, deep_reef_disper_MHW$group, sd), 
                                        n=table(deep_reef_disper_MHW$group)))%>%
  mutate(group="deep reef") %>%
  dplyr::select(group, distance=mean, sd, n) 

deep_reef_params_MHW <- cbind(period = rownames(deep_reef_params_MHW), deep_reef_params_MHW) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 




#rocky 

rocky_params_MHW <- as.data.frame(cbind(mean=tapply(rocky_disper_MHW$distances, rocky_disper_MHW$group, mean), 
                                    sd=tapply(rocky_disper_MHW$distances, rocky_disper_MHW$group, sd), 
                                    n=table(rocky_disper_MHW$group)))%>%
  mutate(group="rocky") %>%
  dplyr::select(group, distance=mean, sd, n) 

rocky_params_MHW <- cbind(period = rownames(rocky_params_MHW), rocky_params_MHW) %>%
  pivot_wider(names_from=c('period'), values_from = c('distance','sd','n')) 


#combine

meta_params_MHW <- rbind(CCFRP_params_MHW, 
                     kelp_swath_params_MHW, kelp_upc_params_MHW, kelp_fish_params_MHW,
                     deep_reef_params_MHW,
                     rocky_params_MHW)

meta_dist_params_MHW <- left_join(meta_params_MHW, cen_distances_MHW, by="group")




#calculate effect size

#https://www.r-bloggers.com/2018/04/v-is-for-meta-analysis-variance/

meta_es_MHW <- meta_dist_params_MHW %>%
  mutate(SMD = distance/
           (sqrt(((n_before-1)*sd_before^2 + (n_after-1)*sd_after^2) / 
                   (n_before+n_after-2))), #divide distance by pooled SD
         vi= ((n_before+n_after)/(n_before*n_after)) + (SMD^2/(2*(n_before+n_after)))) 

test <- escalc("SMD", yi=SMD, vi=vi, data=meta_es_MHW) 



res <- rma(yi, vi, method="REML", verbose=TRUE, digits=5, data=test)



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
(figure <- ggplot(test, aes(x=group,yi, color=community),y=yi) +
    geom_point(size=2) +
    geom_errorbar(aes(ymin=yi-vi, ymax=yi+vi), width=.3, size=0.7) +
    geom_vline(xintercept = 0.7, color = "black", size = 0.4, linetype = "dashed") +
    #scale_x_discrete(expand = c(-.03, 1.6) ) +
    geom_point(aes(x=0.4, y=yi), data=pooled, shape=18, size = 5, colour="black") +
    geom_errorbar(aes(x=0.4, ymin=yi-se, ymax=yi+se), data=pooled, colour="black", width=.2) +
    scale_x_discrete(expand = expansion(mult = c(0.2, 0.1))) +
    ylab("standardized distance") +
    xlab("") +
    coord_flip() +
    theme_minimal(base_size=14) + theme(aspect.ratio = 1/1.5)
)










