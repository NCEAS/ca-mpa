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
require(reshape2)
require(ggfittext)



# #load data --------------------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))




# Plot --------------------------------------------------------------------


CCFRP_disper <- betadisper(CCFRP_distmat, type="centroid", group=CCFRP_group_vars$desig_state)
A<- plot(CCFRP_disper, main="CCFRP", col=c('red','blue'))


kelp_swath_disper <- betadisper(kelp_swath_distmat, type="centroid", group=kelp_swath_group_vars$desig_state)
B<- plot(kelp_swath_disper, main="kelp swath", col=c('red','blue'))



kelp_upc_disper <- betadisper(kelp_upc_distmat, type="centroid", group=kelp_upc_group_vars$desig_state)
C<- plot(kelp_upc_disper, main="kelp upc", col=c('red','blue'))


kelp_fish_disper <- betadisper(kelp_fish_distmat, type="centroid", group=kelp_fish_group_vars$desig_state)
D<- plot(kelp_fish_disper, main="kelp fish", col=c('red','blue'))


deep_reef_disper <- betadisper(deep_reef_distmat, type="centroid", group=deep_reef_group_vars$desig_state)
E <- plot(deep_reef_disper, main="deep reef", col=c('red','blue'))


rocky_disper <- betadisper(rocky_distmat, type="centroid", group=rocky_group_vars$desig_state)
F <- plot(rocky_disper, main="rocky", col=c('red','blue'))



# calculate dist between centroids ----------------------------------------
dist_between_mat <- as.data.frame(matrix(ncol=3, nrow=6))
colnames(dist_between_mat) <- c("dist_between_ref","distance_between_smr","group")


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


# test for significant dispersion between periods -------------------------

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

aov_output <- rbind(rocky_output, CCFRP_output, kelp_swath_output, kelp_upc_output, kelp_fish_output,deep_reef_output)%>%
  dplyr::select(group, everything())
  #arrange(desc(F.Model))

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
    geom_point(aes(x=0.5, y=yi), data=pooled_ref, shape=18, size = 5, colour='#13A0DD')+
    geom_errorbar(aes(x=0.5, ymin=yi-se, ymax=yi+se), data=pooled_ref, colour='#13A0DD', width=.2) +
    geom_point(aes(x=0.18, y=yi), data=pooled_smr, shape=18, size = 5, colour='#EB6977')+
    geom_errorbar(aes(x=0.18, ymin=yi-se, ymax=yi+se), data=pooled_smr, colour='#EB6977', width=.2) +
    ylab("standardized distance")+
    xlab("")+
    coord_flip()+
    theme_minimal(base_size=14) + theme(aspect.ratio = 1/1.5)
)



#export distance plot

#ggsave(here("analyses", "5community_climate_ecology", "figures", "distance_MPA.png"), figure, height=4, width = 8, units = "in", 
#       dpi = 600, bg="white")





# Calculate MPA differences before, then after ----------------------------

dist_between_MPA_mat <- as.data.frame(matrix(ncol=3, nrow=6))
colnames(dist_between_MPA_mat) <- c("dist_between_before","distance_between_after","group")

#CCFRP
dist_between_MPA_mat[1,1] <- dist_between_centroids(CCFRP_distmat, 17:43,60:86) #ref to SMR before
dist_between_MPA_mat[1,2] <- dist_between_centroids(CCFRP_distmat, 1:16,44:59) # ref to SMR after
dist_between_MPA_mat[1,3] <- c("CCFRP")

#kelp eswath
dist_between_MPA_mat[2,1] <- dist_between_centroids(kelp_swath_distmat, 20:128,144:244) #ref to SMR before
dist_between_MPA_mat[2,2] <- dist_between_centroids(kelp_swath_distmat, 129:143,1:19) #ref to SMR after
dist_between_MPA_mat[2,3]<- c("kelp swath")

#kelp upc
dist_between_MPA_mat[3,1] <- dist_between_centroids(kelp_upc_distmat, 20:128,144:244) #ref to SMR before
dist_between_MPA_mat[3,2] <- dist_between_centroids(kelp_upc_distmat, 1:19,129:143)  #ref to SMR after
dist_between_MPA_mat[3,3] <- c("kelp upc")

#kelp fish
dist_between_MPA_mat[4,1] <- dist_between_centroids(kelp_fish_distmat, 21:126,142:241)#ref to SMR before
dist_between_MPA_mat[4,2] <- dist_between_centroids(kelp_fish_distmat, 1:20,127:141) #ref to SMR after
dist_between_MPA_mat[4,3] <- c("kelp fish")

#deep reef
dist_between_MPA_mat[5,1] <- dist_between_centroids(deep_reef_distmat, 11:17,25:32) #ref before to after
dist_between_MPA_mat[5,2] <- dist_between_centroids(deep_reef_distmat, 1:10, 18:24) #smr before to after
dist_between_MPA_mat[5,3] <- c("deep reef")

#rocky
dist_between_MPA_mat[6,1]  <- dist_between_centroids(rocky_distmat, 34:144,166:245) #ref before to after
dist_between_MPA_mat[6,2]  <- dist_between_centroids(rocky_distmat, 1:33,145:165) #smr before to after
dist_between_MPA_mat[6,3] <- c("rocky")



#clean before plotting
dist_between_MPA_mat.2 <- dist_between_MPA_mat %>%
                        select(before = dist_between_before,
                               after = distance_between_after,
                               group)%>%
                        melt(id.var='group', variable.name="distance")


#join with beta params
beta_params <- meta_params %>%
                clean_names()%>%
                select(group, sd_ref_after, sd_ref_before, 
                       sd_smr_after, sd_smr_before, n_ref_after, n_ref_before, 
                       n_smr_after, n_smr_before)

dist_data <- left_join(dist_between_MPA_mat.2, beta_params, by="group") %>%
            mutate(se = if_else(distance=='before',
                                sqrt(((sd_ref_before^2)/n_ref_before))+((sd_smr_before^2)/n_smr_before),
                                sqrt(((sd_ref_after^2)/n_ref_after))+((sd_smr_after^2)/n_smr_after)))
            



(dist_between_plot <- dist_data %>%
  ggplot(aes(x=group, y=value, fill=distance))+
  geom_bar(stat='identity', position="dodge", color="black", width=0.9)+
  geom_errorbar(aes(ymin=value-se, ymax=value+se), position=position_dodge(0.9), width=.3, size=0.4) +
  #scale_fill_manual(values=c('#95B4CC','#ff392e'))+
  #scale_y_continuous(labels=label_number(accuracy=0.01), 
      #               breaks = seq(0,0.45,len=10))+
  #coord_flip()+
  labs(
    title = "Distance between SMR and reference centroids",
    x = "monitoring group",
    y = "distance (Euclidean)"
  )+
  #theme(legend.text = element_text(size=16))+
  labs(fill='Marine heatwave')+
  scale_fill_brewer(palette=14)+
  theme_minimal(base_size=14)) + theme(aspect.ratio = 1/1.5)


#ggsave(here("analyses", "5community_climate_ecology", "figures", "distance_MPA_barplot.png"), dist_between_plot, height=4, width = 8, units = "in", 
    #   dpi = 300, bg="white")







# -------------------------------------------------------------------------



# SIMPER tables Before & after heatwave (no consideration of MPAs)--------------


sim_CCFRP <- with(CCFRP_group_vars, simper(CCFRP_ord_data, MHW), ordered=TRUE)
sim_kelp_swath <- with(kelp_swath_group_vars, simper(kelp_swath_ord_data, MHW))
sim_kelp_upc <- with(kelp_upc_group_vars, simper(kelp_upc_ord_data, MHW))
sim_kelp_fish <- with(kelp_fish_group_vars, simper(kelp_fish_ord_data, MHW))
sim_deep_reef <- with(deep_reef_group_vars, simper(deep_reef_ord_data, MHW))
sim_rocky <- with(rocky_group_vars, simper(rocky_ord_data, MHW))


#collect tables

CCFRP_a_b_table <- as.data.frame(summary(sim_CCFRP)$after_before)%>%
                    mutate(group="CCFRP",
                           contrib = cumsum-lag(cumsum, default=0),
                           perc_change = (ava-avb)/avb,
                           sign = ifelse(perc_change > 0, "positive","negative"))
                    
kelp_swath_a_b_table <- as.data.frame(summary(sim_kelp_swath)$after_before)%>%
                    mutate(group="kelp_swath",
                           contrib = cumsum-lag(cumsum, default=0),
                           perc_change = (ava-avb)/avb,
                           sign = ifelse(perc_change > 0, "positive","negative"))

kelp_upc_a_b_table <- as.data.frame(summary(sim_kelp_upc)$after_before)%>%
                    mutate(group="kelp_upc",
                           contrib = cumsum-lag(cumsum, default=0),
                           perc_change = (ava-avb)/avb,
                           sign = ifelse(perc_change > 0, "positive","negative"))

kelp_fish_a_b_table <- as.data.frame(summary(sim_kelp_fish)$after_before)%>%
                    mutate(group="kelp_fish",
                           contrib = cumsum-lag(cumsum, default=0),
                           perc_change = (ava-avb)/avb,
                           sign = ifelse(perc_change > 0, "positive","negative"))

deep_reef_a_b_table <- as.data.frame(summary(sim_deep_reef)$after_before)%>%
                    mutate(group="deep_reef",
                           contrib = cumsum-lag(cumsum, default=0),
                           perc_change = (ava-avb)/avb,
                           sign = ifelse(perc_change > 0, "positive","negative"))

rocky_a_b_table <- as.data.frame(summary(sim_rocky)$after_before)%>%
                    mutate(group="rocky",
                           contrib = cumsum-lag(cumsum, default=0),
                           perc_change = (ava-avb)/avb,
                           sign = ifelse(perc_change > 0, "positive","negative"))


simper_a_b_table <- rbind(CCFRP_a_b_table, kelp_swath_a_b_table, kelp_upc_a_b_table, kelp_fish_a_b_table, deep_reef_a_b_table, rocky_a_b_table)%>%
                    tibble::rownames_to_column(var = "species") %>%
                    filter(cumsum <0.80)%>%
                    select(group, species, avg_before = avb, avg_after=ava, cumsum, contrib, perc_change, sign)

#plot

#set order
simper_a_b_table$group <- factor(simper_a_b_table$group, levels = c('CCFRP', 'kelp_swath', 
                                                                    'rocky','kelp_fish',
                                                                    'kelp_upc','deep_reef'))

ggplot(simper_a_b_table, aes(x = group, y = contrib, label = species, fill=sign)) +
  geom_bar(stat = "identity", color="black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color="black", fontface="bold")+
  theme_minimal(base_size = 21)+
  scale_fill_discrete(name = "direction of change")+
  ylab("cumulative contribution")
  
  #scale_fill_brewer(palette=1)+







# SIMPER tables w/ MPAs added (all combinations)--------------------------------

sim_CCFRP_MPA <- with(CCFRP_group_vars, simper(CCFRP_ord_data, desig_state))
sim_kelp_swath_MPA <- with(kelp_swath_group_vars, simper(kelp_swath_ord_data, desig_state))
sim_kelp_upc_MPA <- with(kelp_upc_group_vars, simper(kelp_upc_ord_data, desig_state))
sim_kelp_fish_MPA <- with(kelp_fish_group_vars, simper(kelp_fish_ord_data, desig_state))
sim_deep_reef_MPA <- with(deep_reef_group_vars, simper(deep_reef_ord_data, desig_state))
sim_rocky_MPA <- with(rocky_group_vars, simper(rocky_ord_data, desig_state))





