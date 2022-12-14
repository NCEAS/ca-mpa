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
require(pairwiseAdonis)



# #load data --------------------------------------------------------------

#data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))




# Step 1 - examine hull shape and dispersion -----------------------------------


CCFRP_disper <- betadisper(CCFRP_distmat, type="centroid", 
                           group=CCFRP_group_vars$desig_state)
A<- plot(CCFRP_disper, main="CCFRP", col=c('red','blue'),
         hull=TRUE, ellipse=FALSE, label=FALSE)


kelp_swath_disper <- betadisper(kelp_swath_distmat, type="centroid", 
                                group=kelp_swath_group_vars$desig_state)
B<- plot(kelp_swath_disper, main="kelp swath", col=c('red','blue'))



kelp_upc_disper <- betadisper(kelp_upc_distmat, type="centroid", 
                              group=kelp_upc_group_vars$desig_state)
C<- plot(kelp_upc_disper, main="kelp upc", col=c('red','blue'))


kelp_fish_disper <- betadisper(kelp_fish_distmat, type="centroid", 
                               group=kelp_fish_group_vars$desig_state)
D<- plot(kelp_fish_disper, main="kelp fish", col=c('red','blue'))

kelp_invalg_disper <- betadisper(kelp_invalg_distmat, type="centroid", 
                               group=kelp_invalg_group_vars$desig_state)
G<- plot(kelp_invalg_disper, main="kelp inverts and algae", col=c('red','blue'))


deep_reef_disper <- betadisper(deep_reef_distmat, type="centroid", 
                               group=deep_reef_group_vars$desig_state)
E <- plot(deep_reef_disper, main="deep reef", col=c('red','blue'))


rocky_disper <- betadisper(rocky_distmat, type="centroid", 
                           group=rocky_group_vars$desig_state)
F <- plot(rocky_disper, main="rocky", col=c('red','blue'))


################################################################################
#calculate dist between centroids using betadisper output

#create helper function to handle negative eigenvalues and
#convert to square mat

eig_fun <- function(disper_mat) {
  
  x = melt(as.matrix(sqrt(dist(disper_mat$centroids[,disper_mat$eig>0]^2)-
                            dist(disper_mat$centroids[,disper_mat$eig<0]^2))))
  tibble::rownames_to_column(x, "distance")
  
  x2 <- x %>% filter(Var1 == "ref before" & Var2 == "ref after" |
                       Var1 == "ref before" & Var2 == "ref during" |
                       Var1 == "smr before" & Var2 == "smr after"|
                       Var1 == "smr before" & Var2 == "smr during") %>%
    mutate(MPA = gsub( " .*$", "", Var1)) # %>%
  #select(MPA, value)%>%
  #pivot_wider(names_from="MPA")%>%
  #mutate(logRR = log(smr/ref))
  
  sd = melt(as.matrix(tapply(disper_mat$distances, disper_mat$group, sd)))%>%
    tibble::rownames_to_column() %>% mutate(s_d = value)%>%
    dplyr::select(Var1, s_d)
  
  n = melt(table(disper_mat$group))%>%
    tibble::rownames_to_column() %>% mutate(n = value)%>%
    dplyr::select(Var1, n) 
  
  e_hat <- left_join(sd, n, by='Var1') %>%
    pivot_wider(names_from='Var1', values_from = c('s_d','n'))%>%
    mutate(sd_ref_pooled_before_after = sqrt(
      ((`n_ref before`-1)*`s_d_ref before`^2 + (`n_ref after`-1)*`s_d_ref after`^2)/
        (`n_ref before`+`n_ref after`-2)
    ),
    sd_smr_pooled_before_after = sqrt(
      ((`n_smr before`-1)*`s_d_smr before`^2 + (`n_smr after`-1)*`s_d_smr after`^2)/
        (`n_smr before`+`n_smr after`-2)
    ),
    sd_ref_pooled_before_during = sqrt(
      ((`n_ref before`-1)*`s_d_ref before`^2 + (`n_ref during`-1)*`s_d_ref during`^2)/
        (`n_ref before`+`n_ref during`-2)
    ),
    sd_smr_pooled_before_during = sqrt(
      ((`n_smr before`-1)*`s_d_smr before`^2 + (`n_smr during`-1)*`s_d_smr during`^2)/
        (`n_smr before`+`n_smr during`-2)
    )
    ) %>%
    dplyr::select(`sd_ref_pooled_before_after`,`sd_ref_pooled_before_during`,
                  `sd_smr_pooled_before_after`,`sd_smr_pooled_before_during`)%>%
    pivot_longer(cols=c(`sd_ref_pooled_before_after`,`sd_ref_pooled_before_during`,
                        `sd_smr_pooled_before_after`,`sd_smr_pooled_before_during`),
                 values_to ="sd_pooled")
  cbind(x2, e_hat)
}


ccfrp_travel <- eig_fun(CCFRP_disper) %>% mutate(group = "Rocky reef fishes")
kelp_invalg_travel <- eig_fun(kelp_invalg_disper) %>% mutate(group = "Kelp forest inverts and algae")
kelp_fish_travel <- eig_fun(kelp_fish_disper) %>% mutate(group = "Kelp forest fishes")
deep_reef_travel <- eig_fun(deep_reef_disper) %>% mutate(group = "Deep reef fishes")
rocky_travel <- eig_fun(rocky_disper) %>% mutate(group = "Rocky intertidal")

travel_distance <- as.data.frame(rbind(ccfrp_travel, kelp_invalg_travel, kelp_fish_travel,
                         deep_reef_travel, rocky_travel))

travel_distance$MPA <- recode_factor(travel_distance$MPA, 'smr'='MPA')
travel_distance$MPA <- recode_factor(travel_distance$MPA, 'ref'='Reference')

dist_mpa <- travel_distance %>% filter(MPA=='MPA')
mean_mpa <- mean(dist_mpa$value)

dist_ref <- travel_distance %>% filter(MPA=='Reference')
mean_ref <- mean(dist_ref$value)



################################################################################
#pairwise PERMANOVA to test for sig differences


#pariwise permanova
ccfrp_pair_perm <- pairwise.adonis2(CCFRP_distmat ~ desig_state, 
                                    data = CCFRP_group_vars, permutations = 999)

dr_pair_perm <- pairwise.adonis2(deep_reef_distmat ~ desig_state, 
                                 data = deep_reef_group_vars, permutations = 999) 
kelp_fish_pair_perm <- pairwise.adonis2(kelp_fish_distmat ~ desig_state, 
                                        data = kelp_fish_group_vars,permutations = 999) 
kelp_invalg_pair_perm <- pairwise.adonis2(kelp_invalg_distmat ~ desig_state, 
                                          data = kelp_invalg_group_vars, permutations = 999) 
rocky_pair_perm <- pairwise.adonis2(rocky_distmat ~ desig_state,
                                    data = rocky_group_vars, permutations = 999) 

#create helper function to collect pairwise output 

perm_fun <- function(perm_table, group_name){
  ref_after = (as.data.frame(perm_table[["ref before_vs_ref after"]])%>%
              mutate(group = group_name,
                     MPA_type = 'REF',
                     period = 'before-to-after') %>%
              filter(row_number()==1) %>%
              dplyr::select(group, MPA_type, period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))
  ref_during = (as.data.frame(perm_table[["ref before_vs_ref during"]])%>%
              mutate(group = group_name,
                     MPA_type = 'REF',
                     period = 'before-to-during') %>%
              filter(row_number()==1) %>%
              dplyr::select(group, MPA_type,period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))
  
  smr_after = (as.data.frame(perm_table[["smr before_vs_smr after"]])%>%
                 mutate(group = group_name,
                        MPA_type = 'MPA',
                        period = 'before-to-after') %>%
                 filter(row_number()==1) %>%
                 dplyr::select(group, MPA_type,period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))
  smr_during = (as.data.frame(perm_table[["smr before_vs_smr during"]])%>%
                  mutate(group = group_name,
                         MPA_type = 'MPA',
                         period = 'before-to-during') %>%
                  filter(row_number()==1) %>%
                  dplyr::select(group, MPA_type, period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))

  rbind(ref_after, ref_during, smr_after, smr_during)
}


#collect output
ccfrp_op <- perm_fun(ccfrp_pair_perm, group="Rocky reef fishes")
kelp_fish_op <- perm_fun(kelp_fish_pair_perm, group='Kelp forest fishes')
kelp_invalg_perm <- perm_fun(kelp_invalg_pair_perm, group = "Kelp forest inverts and algae")
rocky_op <- perm_fun(rocky_pair_perm, group="Rocky intertidal")
deep_reef_op <- perm_fun(dr_pair_perm, group = "Deep reef fishes")

perm_output <- rbind(ccfrp_op, kelp_fish_op, deep_reef_op,
                     kelp_invalg_perm, rocky_op)

rownames(perm_output) <- NULL

perm_output$MPA_type <- recode_factor(perm_output$MPA_type, "REF"="Reference")


#create helper function to collect MPA vs REF output
perm_fun <- function(perm_table, group_name){
  ref_after = (as.data.frame(perm_table[["ref before_vs_smr before"]])%>%
                 mutate(group = group_name,
                        MPA_type = 'REF',
                        period = 'before-to-after') %>%
                 filter(row_number()==1) %>%
                 dplyr::select(group, MPA_type, period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))
  ref_during = (as.data.frame(perm_table[["ref before_vs_ref during"]])%>%
                  mutate(group = group_name,
                         MPA_type = 'REF',
                         period = 'before-to-during') %>%
                  filter(row_number()==1) %>%
                  dplyr::select(group, MPA_type,period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))
  
  smr_after = (as.data.frame(perm_table[["smr before_vs_smr after"]])%>%
                 mutate(group = group_name,
                        MPA_type = 'MPA',
                        period = 'before-to-after') %>%
                 filter(row_number()==1) %>%
                 dplyr::select(group, MPA_type,period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))
  smr_during = (as.data.frame(perm_table[["smr before_vs_smr during"]])%>%
                  mutate(group = group_name,
                         MPA_type = 'MPA',
                         period = 'before-to-during') %>%
                  filter(row_number()==1) %>%
                  dplyr::select(group, MPA_type, period, Df, SumOfSqs, R2, `F`,`Pr(>F)`))
  
  rbind(ref_after, ref_during, smr_after, smr_during)
}



#write.csv(perm_output, "/home/joshsmith/CA_MPA_Project/ca-mpa/analyses/5community_climate_ecology/tables/pairwise_permanova_table.csv")       # Export PDF
#grid.table(perm_output)
#dev.off()


################################################################################
#plot betadisper distance between centroids

#join distance and PERMANOVA tables
distance1 <- travel_distance%>%
  mutate(period = ifelse(Var1 == "ref before" & Var2== "ref during" |
                           Var1== "smr before" & Var2== "smr during","before-to-during",
                         ifelse(Var1 == "ref before" & Var2== "ref after" |
                                  Var1== "smr before" & Var2== "smr after","before-to-after","")),
         period=fct_relevel(period, c("before-to-during","before-to-after")),
         MPA = tidytext::reorder_within(MPA, value, period),
         MPA_type = toupper(gsub( " .*$", "", Var1)))

distance1$MPA_type <- recode_factor(distance1$MPA_type, "In"="MPA")
distance1$MPA_type <- recode_factor(distance1$MPA_type, "Out"="Reference")

#perm_output$group <- recode_factor(perm_output$group, 	
#                                   "Kelp forest inverts and algae" = "kelp inverts and algae")
#perm_output$group <- recode_factor(perm_output$group, 	
#                                   "Kelp forest fish" = "kelp fish")
#perm_output$group <- recode_factor(perm_output$group, 	
#                                   "Deep reef" = "deep reef")
#perm_output$group <- recode_factor(perm_output$group, 	
#                                   "Rocky intertidal" = "rocky intertidal")

sig_distance <- left_join(distance1, perm_output, 
                          by=c("group","MPA_type","period"))%>%
                mutate(sig = ifelse(`Pr(>F)`<0.05, "*",""))%>%
                rename("p-val" = `Pr(>F)`) %>%
                mutate(period = factor(period, levels=c("before-to-during",
                                                        "before-to-after")))
sig_distance$period <- recode_factor(sig_distance$period, "before-to-during"="Before-to-during")
sig_distance$period <- recode_factor(sig_distance$period, "before-to-after"="Before-to-after")



my_theme <-  theme(axis.text=element_text(size=8),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=10),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=9, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   #facets
                   strip.text = element_text(size=6),
                   #margins
                   #plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)

sig_distance$MPA_type <- recode_factor(sig_distance$MPA_type, "In"="MPA")
sig_distance$MPA_type <- recode_factor(sig_distance$MPA_type, "Out"="Reference")

p1 <- 
  sig_distance %>%
  rename("Period"=period)%>%
  filter(Period == "Before-to-during")%>%
  mutate(group = factor(group, levels = c("Rocky intertidal","Kelp forest inverts and algae",
                                          "Kelp forest fishes","Rocky reef fishes","Deep reef fishes")),
         MPA_type = factor(MPA_type, levels = c("MPA","Reference")))%>%
  arrange(MPA_type, -value, group)%>%
  mutate(mpa_ordered = fct_inorder(paste(MPA_type, group, sep = "."))) |> 
  ggplot(aes(x = group, y = value, color = MPA_type, group = mpa_ordered)) +
  geom_point(position = position_dodge(width=0.8),
             size=3)+
  geom_errorbar(aes(ymin=value-sd_pooled,
                    ymax = value+sd_pooled), stat="identity",
                position = position_dodge(width=0.8), size=0.3, width=.3)+
  #scale_color_manual(name='MPA',
   #                  breaks=c('In', 'Out'),
    #                 values=c('In'='#EB6977', 'Out'='#13A0DD'))+
  #add significance level
  geom_text(aes(label=sig), size=5, hjust=-1, vjust=0.8,
            position = position_dodge(width=0.8),
            show.legend = FALSE)+
  ylab("")+
  xlab("")+
  scale_y_continuous(limits=c(-0.05,0.3))+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)
    )+
 labs(color = "Site type")+
  scale_color_brewer(palette="Set1") +
  #geom_vline(xintercept=c(1.5, 2.5,3.5,4.5), color="grey",alpha=.4)+
  #coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_blank())+
  ggtitle("Resistance \n(Before-to-during)")+
  my_theme+
  theme(aspect.ratio=1)



p2 <- 
  sig_distance %>%
  rename("Period"=period)%>%
  filter(Period == "Before-to-after")%>%
  mutate(group = factor(group, levels = c("Rocky intertidal","Kelp forest inverts and algae",
                                          "Kelp forest fishes","Rocky reef fishes","Deep reef fishes")))%>%
  arrange(MPA_type, -value, group)%>%
  mutate(mpa_ordered = fct_inorder(paste(MPA_type, group, sep = "."))) |> 
  ggplot(aes(x = group, y = value, color = MPA_type, group = mpa_ordered)) +
  geom_point(position = position_dodge(width=0.8),
             size=3)+
  geom_errorbar(aes(ymin=value-sd_pooled,
                    ymax = value+sd_pooled), stat="identity",
                position = position_dodge(width=0.8), size=0.3, width=.3)+
  #scale_color_manual(name='MPA',
  #                  breaks=c('In', 'Out'),
  #                 values=c('In'='#EB6977', 'Out'='#13A0DD'))+
  #add significance level
  geom_text(aes(label=sig), size=5, hjust=-1, vjust=0.8,
            position = position_dodge(width=0.8),
            show.legend = FALSE)+
  ylab("")+
  xlab("")+
  labs(color = "Site type")+
  scale_y_continuous(limits=c(-0.05,0.3))+
  scale_x_discrete(labels = function(x) 
    stringr::str_wrap(x, width = 15)
  )+
  scale_color_brewer(palette="Set1") +
  #geom_vline(xintercept=c(1.5, 2.5,3.5,4.5), color="grey",alpha=.4)+
  #coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_blank())+
  ggtitle("Resilience \n(Before-to-after)")+
  my_theme+
  theme(aspect.ratio=1)



library(grid)
g <- ggpubr::ggarrange(p1, p2, nrow=1, ncol=2, 
                       common.legend = TRUE, legend = "right")



g_title <- ggpubr::annotate_figure(g, left = textGrob("Distance (Bray-Curtis)", 
                                                     rot = 90, vjust = 2, hjust=0.3, gp = gpar(cex = 0.8)),
                                  bottom = textGrob("Habitat", hjust=1, vjust=-11, gp = gpar(cex = 0.8)))



#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "betadisp_plot3.png"), g_title, height=6, width = 8, units = "in", 
#   dpi = 600, bg="white")





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

kelp_invalg_cen <-cenfun(group=kelp_invalg_group_vars, x=kelp_invalg_distmat) %>% 
  filter(centroid_1 == 'smr after' |
           centroid_1 == 'ref after')%>%
  mutate(group='kelp inverts and algae')

deep_reef_cen <-cenfun(group=deep_reef_group_vars, x=deep_reef_distmat) %>% 
  filter(centroid_1 == 'smr after' |
  centroid_1 == 'ref after')%>%
  mutate(group='deep reef')

rocky_cen <-cenfun(group=rocky_group_vars, x=rocky_distmat) %>% 
  filter(centroid_1 == 'smr after' |
  centroid_1 == 'ref after')%>%
  mutate(group='rocky')


cen_distances <- rbind(ccfrp_cen, kelp_swath_cen, kelp_upc_cen,
                       kelp_fish_cen, kelp_invalg_cen, deep_reef_cen, rocky_cen)

cen_distances2 <- cen_distances %>%
                  mutate(mpa_type = word(centroid_1, start = 1))%>%
                  select(!(1:2)) %>%
                  pivot_wider(names_from = mpa_type, values_from = distance) %>%
                  select(group, distance_between_ref='ref',
                         distance_between_smr = 'smr')

cen_distances2 <- as.data.frame(cen_distances2)


# test for significant dispersion between periods -----------------------------

vegan::permutest(CCFRP_disper)
  TukeyHSD(CCFRP_disper)
vegan::permutest(kelp_swath_disper)
vegan::permutest(kelp_fish_disper)
vegan::permutest(kelp_upc_disper)
vegan::permutest(kelp_invalg_disper)
vegan::permutest(rocky_disper)
vegan::permutest(deep_reef_disper)


#permanovas
rocky_perm <- adonis2(formula = rocky_distmat ~ MHW +  mpa_designation, data = rocky_group_vars, permutations = 99) 
deep_reef_perm <- adonis2(formula = deep_reef_distmat ~ MHW+desig_state, data = deep_reef_group_vars, permutations = 99) 
kelp_swath_perm <- adonis2(formula = kelp_swath_distmat ~ MHW+desig_state, data = kelp_swath_group_vars, permutations = 99) 
kelp_fish_perm <- adonis2(formula = kelp_fish_distmat ~ MHW+desig_state, data = kelp_fish_group_vars, permutations = 99) 
kelp_invalg_perm <- adonis2(formula = kelp_invalg_distmat ~ MHW+desig_state, data = kelp_invalg_group_vars, permutations = 99) 
kelp_upc_perm <- adonis2(formula = kelp_upc_distmat ~ MHW+desig_state, data = kelp_upc_group_vars, permutations = 99) 
CCFRP_perm <- adonis2(formula = CCFRP_distmat ~ MHW+desig_state, data = CCFRP_group_vars, permutations = 99) 





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



#Kelp invalg
kelp_invalg_params <- as.data.frame(cbind(mean=tapply(kelp_invalg_disper$distances, 
                                                   kelp_invalg_disper$group, mean), 
                                       sd=tapply(kelp_invalg_disper$distances,
                                                 kelp_invalg_disper$group, sd), 
                                       n=table(kelp_invalg_disper$group)))%>%
  mutate(group="kelp inverts and algae") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_invalg_params <- cbind(period = rownames(kelp_invalg_params), 
                         kelp_invalg_params) %>%
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

deep_reef_params <- cbind(period = rownames(deep_reef_params), deep_reef_params)%>%
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
                     kelp_swath_params, kelp_upc_params, kelp_invalg_params, kelp_fish_params,
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
test_ref$group <- as.character(test_ref$group)
test_smr$group <- as.character(test_smr$group)
#Then turn it back into a factor with the levels in the correct order
test_smr$group <- factor(test_smr$group, levels=unique(test_smr$group))
test_ref$group <- factor(test_ref$group, levels=unique(test_ref$group))

test_ref <- test_ref %>% filter(!(group=='kelp upc' | group=='kelp swath'))
test_smr <- test_smr %>% filter(!(group=='kelp upc' | group=='kelp swath'))

test_ref$group <- recode_factor(test_ref$group, 'rocky'='rocky intertidal')
test_smr$group <- recode_factor(test_smr$group, 'rocky'='rocky intertidal')


#plot

(figure<-ggplot(test_smr, aes(x=group,yi),y=yi)+
    geom_point(aes(color='SMR'), size=1, position = position_jitter(seed = 123, width =0.2))+
    geom_errorbar(aes(ymin=yi-vi, ymax=yi+vi, color='SMR'),width=.2, size=0.3, data=test_smr, position = position_jitter(seed = 123, width =0.2)) +
    geom_point(data=test_ref, size=1, aes(color='REF'))+
    geom_errorbar(aes(ymin=yi-vi, ymax=yi+vi, color="REF"), width=.2, size=0.2, data=test_ref)+
    geom_vline(xintercept = 0.7, color = "black", size = 0.4, linetype = "dashed") +
    scale_x_discrete(expand = c(0.2, 0.1) )+
    geom_point(aes(x=0.5, y=yi, color='REF'), data=pooled_ref, shape=18, size = 2)+
    geom_errorbar(aes(x=0.5, ymin=yi-se, ymax=yi+se, color='REF'), data=pooled_ref, width=.2, size=0.3) +
    geom_point(aes(x=0.18, y=yi, color="SMR"), data=pooled_smr, shape=18, size = 2)+
    geom_errorbar(aes(x=0.18, ymin=yi-se, ymax=yi+se, color="SMR"), data=pooled_smr, width=.2, size=0.3) +
    ylab("Euclidean distance (community change)")+
    xlab("Community")+
    coord_flip()+
    theme_minimal(base_size=8) + theme(aspect.ratio = 1/1)+
    scale_color_manual(name='MPA type',
                       breaks=c('SMR', 'REF'),
                       values=c('SMR'='#EB6977', 'REF'='#13A0DD')
                       )
)



#export distance plot

#ggsave(here("analyses", "5community_climate_ecology", "figures", "distance_MPA.png"), figure, height=4, width = 5, units = "in", 
#       dpi = 600, bg="white")











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

kelp_invalg_MHW <-cenfun(group=kelp_invalg_group_vars, x=kelp_invalg_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='kelp inverts and algae')

deep_reef_MHW <-cenfun(group=deep_reef_group_vars, x=deep_reef_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='deep reef')

rocky_MHW <-cenfun(group=rocky_group_vars, x=rocky_distmat) %>% 
  filter(centroid_1 == 'after' & centroid_2 =='before')%>%
  mutate(group='rocky intertidal')


cen_distances_MHW <- rbind(ccfrp_MHW, kelp_swath_MHW, kelp_upc_MHW,
                       kelp_fish_MHW, kelp_invalg_MHW, deep_reef_MHW, rocky_MHW) %>%
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

kelp_invalg_disper_MHW <- betadisper(kelp_invalg_distmat, type="centroid", 
                                   group=kelp_invalg_group_vars$MHW)

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


#Kelp invalg
kelp_invalg_params_MHW <- as.data.frame(cbind(mean=tapply(kelp_invalg_disper_MHW$distances, kelp_invalg_disper_MHW$group, mean), 
                                           sd=tapply(kelp_invalg_disper_MHW$distances, kelp_invalg_disper_MHW$group, sd), 
                                           n=table(kelp_invalg_disper_MHW$group)))%>%
  mutate(group="kelp inverts and algae") %>%
  dplyr::select(group, distance=mean, sd, n) 

kelp_invalg_params_MHW <- cbind(period = rownames(kelp_invalg_params_MHW), kelp_invalg_params_MHW) %>%
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
                     kelp_invalg_params_MHW,
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
test<- test %>% arrange(desc(-yi)) %>% filter(group=='CCFRP'|
                                                group=='k')

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






























# calculate vector dist between centroids for each year ------------------------

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

kelp_invalg <- cenfun2(group=kelp_invalg_group_vars, x=kelp_invalg_distmat)
kelp_invalg$group <- c("kelp inverts and algae")

deep_reef <- cenfun2(group=deep_reef_group_vars, x=deep_reef_distmat)
deep_reef$group <- c("deep reef")

rocky <- cenfun2(group=rocky_group_vars, x=rocky_distmat)
rocky$group <- c("rocky intertidal")

cen_distances <- rbind(ccfrp, kelp_invalg, kelp_fish, deep_reef, rocky)

cen_distances <- cen_distances %>% 
  group_by(centroid_2)%>%
  dplyr::mutate(ymean = mean(distance))



cen_annual_distance<- cen_distances %>%
  filter(centroid_2>=2010)%>%
  ggplot(aes(x=as.numeric(centroid_2), y=distance, color=group))+
  geom_point(alpha=0.4, aes(shape=group), size=3)+
  geom_line(alpha=0.4)+
  geom_smooth(aes(y=ymean), span=0.4, color='black')+
  #stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = 0, ymax = 0.35,
           alpha = .15, fill='red')+
  xlab("Year")+
  ylab("Distance")+
  theme_minimal()+theme(aspect.ratio = 1/1.5
  )+
  scale_x_continuous(breaks= scales::pretty_breaks())

#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "cen_annual_distances.png"), cen_annual_distance, height=4, width = 8, units = "in", 
#   dpi = 600, bg="white")


#barplot mean distances before, during, after MHW

cen_distances2 <- cen_distances %>% 
                  mutate(MHW = ifelse(centroid_2 < 2015, "before",
                                      ifelse(centroid_2 > 2017, "after",
                                             "during")))
cen_distances2$MHW <- factor(cen_distances2$MHW, levels = c("before", "during","after"))

cen_distances3 <- cen_distances2 %>% 
                    filter(centroid_2>=2010)%>%
                    group_by(group, MHW) %>% 
                                       dplyr::summarise(
                                         n = n(),
                                         m = mean(distance),
                                         stdv = sd(distance),
                                         se=stdv/sqrt(n),
                                         ci=se * qt((1-0.05)/2 + .5, n-1)
                                       )
                                         

dist_by_period <- cen_distances3 %>%
  ggplot(aes(x=group, y=m, fill=MHW))+
  geom_bar(position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin=m-se, ymax=m+se),
                width=.2,                    # Width of the error bars
                position=position_dodge(.9))+
  xlab("Group")+
  ylab("Distance")+
  scale_fill_manual(values=c('#44b89d','#f56969','#4c78b5'))
  #theme_minimal()+theme(aspect.ratio = 1/1.5





# Explore distance between ref and smr by year ----------------------------
#Question: did communities inside and outside of MPAs before more distant after 
#the MHW/=?

#create new grouping vars
CCFRP_group_vars2 <- CCFRP_group_vars %>%
  mutate(desig_state_year = paste(mpa_designation,
                                  year))
kelp_fish_group_vars2 <- kelp_fish_group_vars %>%
  mutate(desig_state_year = paste(mpa_defacto_designation,
                                  year))
kelp_invalg_group_vars2 <- kelp_invalg_group_vars %>%
  mutate(desig_state_year = paste(mpa_defacto_designation,
                                  year))
deep_reef_group_vars2 <- deep_reef_group_vars %>%
  mutate(desig_state_year = paste(mpa_defacto_designation,
                                  year))
rocky_group_vars2 <- rocky_group_vars %>%
  mutate(desig_state_year = paste(mpa_designation,
                                  year))


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
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="CCFRP")

kelp_invalg_mpa <- cenfun3(kelp_invalg_group_vars2, kelp_invalg_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="kelp inverts and algae")

kelp_fish_mpa <- cenfun3(kelp_fish_group_vars2, kelp_fish_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="kelp fish")

deep_reef_mpa <- cenfun3(deep_reef_group_vars2, deep_reef_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="deep reef")

rocky_mpa <- cenfun3(rocky_group_vars2, rocky_distmat) %>%
  mutate(MPA_type = gsub( " .*$", "", centroid_1),
         year_1 = str_replace(centroid_1, "^\\S* ", ""),
         year_2 = str_replace(centroid_2, "^\\S* ", ""),
         group="rocky intertidal")

all_mpa <- rbind(CCFRP_mpa, kelp_invalg_mpa, 
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


cen_distances <- all_mpa %>% 
  mutate(MHW = ifelse(year_2 < 2014, "before",
                      ifelse(year_2 > 2016, "after",
                             "during")))
cen_distances$MHW <- factor(cen_distances$MHW, levels = c("before", "during","after"))
cen_distances$MPA_type <- recode_factor(cen_distances$MPA_type, "ref"='REF', 'smr'="SMR")

cen_distances2 <- cen_distances %>% 
  #filter(year_2>=2010)%>%
  group_by(group, MHW, MPA_type) %>% 
  dplyr::summarise(
    n = n(),
    m = mean(distance),
    stdv = sd(distance),
    se=stdv/sqrt(n),
    ci=se * qt((1-0.05)/2 + .5, n-1)
  )








dist_by_mpa_period <- cen_distances2 %>%
  ggplot(aes(x=factor(MPA_type), y=m, fill=MHW
  ))+
  geom_col(width=0.5, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=m-se, ymax=m+se),
                # Width of the error bars
                position=position_dodge(width=0.5),width = 0.3, size=.2)+
  xlab("Group")+
  ylab("Distance")+
  scale_fill_manual(values=c('#44b89d','#f56969','#4c78b5'))










dist_by_mpa_period <- cen_distances2 %>%
  ggplot(aes(x=factor(MHW), y=m, fill=MPA_type
             ))+
  geom_col(width=0.5, position=position_dodge(width=0.5)) +
  geom_errorbar(aes(ymin=m-1.96*se, ymax=m+1.96*se),
                          # Width of the error bars
                position=position_dodge(width=0.5),width = 0.3, size=.2)+
  scale_y_continuous(expand = c(0,0),
                     limits = c(0,0.41))+
  facet_grid(~group) +
  xlab("Group")+
  ylab("Distance")+
  theme_bw()+
  scale_fill_manual(name = "MPA Type",
                     values=c('#13A0DD','#EB6977')
  )
  #scale_fill_manual(values=c('#44b89d','#f56969','#4c78b5'))

#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "dist_by_mpa_period_barplot.png"), dist_by_mpa_period, height=6, width = 8, units = "in", 
#   dpi = 600, bg="white")



a1 <- aov(cen_distances$distance ~ cen_distances$MHW + cen_distances$group + cen_distances$MPA_type)
posthoc <- TukeyHSD(x=a1, conf.level=0.95)



















