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
require(here)
require(forcats)


# #load data --------------------------------------------------------------

#data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
tab_out <- here::here("analyses","5community_climate_ecology","tables")

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))



# Step 1 - examine hull shape and dispersion -----------------------------------


CCFRP_disper <- betadisper(CCFRP_distmat, type="centroid", 
                           group=CCFRP_group_vars$desig_state)
plot(CCFRP_disper, main="CCFRP", col=c('red','blue'),
         hull=T, ellipse=F, label=F)


kelp_swath_disper <- betadisper(kelp_swath_distmat, type="centroid", 
                                group=kelp_swath_group_vars$desig_state)
plot(kelp_swath_disper, main="kelp swath", col=c('red','blue'))



kelp_upc_disper <- betadisper(kelp_upc_distmat, type="centroid", 
                              group=kelp_upc_group_vars$desig_state)
plot(kelp_upc_disper, main="kelp upc", col=c('red','blue'))


kelp_fish_disper <- betadisper(kelp_fish_distmat, type="centroid", 
                               group=kelp_fish_group_vars$desig_state)
plot(kelp_fish_disper, main="kelp fish", col=c('red','blue'))

kelp_invalg_disper <- betadisper(kelp_invalg_distmat, type="centroid", 
                               group=kelp_invalg_group_vars$desig_state)
plot(kelp_invalg_disper, main="kelp inverts and algae", col=c('red','blue'))


deep_reef_disper <- betadisper(deep_reef_distmat, type="centroid", 
                               group=deep_reef_group_vars$desig_state)
plot(deep_reef_disper, main="deep reef", col=c('red','blue'))


rocky_disper <- betadisper(rocky_distmat, type="centroid", 
                           group=rocky_group_vars$desig_state)
plot(rocky_disper, main="rocky", col=c('red','blue'))


################################################################################
#calculate dist between centroids using betadisper output

#create helper function to handle negative eigenvalues and
#convert to square matrix

eig_fun <- function(disper_mat) {
  #set positive-definite eigenvalues
  x = melt(as.matrix(sqrt(dist(disper_mat$centroids[,disper_mat$eig>0]^2)-
                            dist(disper_mat$centroids[,disper_mat$eig<0]^2))))
  tibble::rownames_to_column(x, "distance")
  #select pairwise comparisons (resistance = before-during, recovery= before-after)
  x2 <- x %>% filter(Var1 == "ref before" & Var2 == "ref after" |
                       Var1 == "ref before" & Var2 == "ref during" |
                       Var1 == "smr before" & Var2 == "smr after"|
                       Var1 == "smr before" & Var2 == "smr during") %>%
    mutate(MPA = gsub( " .*$", "", Var1)) # %>%
  
  #calculate pairswise standard deviation 
  sd = melt(as.matrix(tapply(disper_mat$distances, disper_mat$group, sd)))%>%
    tibble::rownames_to_column() %>% mutate(s_d = value)%>%
    dplyr::select(Var1, s_d)
  
  #calculate number of reps (sites) used in pairwise comparison 
  n = melt(table(disper_mat$group))%>%
    tibble::rownames_to_column() %>% mutate(n = value)%>%
    dplyr::select(Var1, n) 
  #calculate pooled standard error 
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
    ),
    se_pooled_smr_during = sd_smr_pooled_before_during*(sqrt(1/`n_smr before` + 1/`n_smr during`)),
    se_pooled_smr_after = sd_smr_pooled_before_after*(sqrt(1/`n_smr before` + 1/`n_smr after`)),
    se_pooled_ref_during = sd_ref_pooled_before_during*(sqrt(1/`n_ref before` + 1/`n_ref during`)),
    se_pooled_ref_after = sd_ref_pooled_before_after*(sqrt(1/`n_ref before` + 1/`n_ref after`)),
    ) %>%
    dplyr::select(`sd_ref_pooled_before_after`,`sd_ref_pooled_before_during`,
                  `sd_smr_pooled_before_after`,`sd_smr_pooled_before_during`,
                  #standard error
                  `se_pooled_smr_during`, `se_pooled_smr_after`,`se_pooled_ref_during`,`se_pooled_ref_after`)%>%
    pivot_longer(cols=c(`sd_ref_pooled_before_after`,`sd_ref_pooled_before_during`,
                        `sd_smr_pooled_before_after`,`sd_smr_pooled_before_during`),
                 values_to ="sd_pooled") %>%
    mutate(period1 = word(name,-2, sep="_"),
           period2 = word(name,-1, sep="_"),
           MPA_type =  word(name,-4, sep="_"),
           se_pooled = ifelse(MPA_type == "ref" & period2=="during",
                              se_pooled_ref_during,ifelse(
                                MPA_type == "ref" & period2=="after",
                                se_pooled_ref_after, ifelse(
                                  MPA_type == "smr" & period2 == "during",
                                  se_pooled_smr_during,
                                  ifelse(MPA_type == "smr" & period2 == "after",
                                         se_pooled_smr_after,NA)
                                )
                              )))%>%
    dplyr::select(name, sd_pooled, se_pooled)
  
  cbind(x2, e_hat)
}


ccfrp_travel <- eig_fun(CCFRP_disper) %>% mutate(group = "Shallow reef")
kelp_invalg_travel <- eig_fun(kelp_invalg_disper) %>% mutate(group = "Kelp forest inverts and algae")
kelp_fish_travel <- eig_fun(kelp_fish_disper) %>% mutate(group = "Kelp forest fishes")
deep_reef_travel <- eig_fun(deep_reef_disper) %>% mutate(group = "Deep reef")
rocky_travel <- eig_fun(rocky_disper) %>% mutate(group = "Rocky intertidal")

#combine results
travel_distance <- as.data.frame(rbind(ccfrp_travel, kelp_invalg_travel, kelp_fish_travel,
                         deep_reef_travel, rocky_travel))

#clean up names
travel_distance$MPA <- recode_factor(travel_distance$MPA, 'smr'='MPA')
travel_distance$MPA <- recode_factor(travel_distance$MPA, 'ref'='Reference')

dist_mpa <- travel_distance %>% filter(MPA=='MPA')
mean_mpa <- mean(dist_mpa$value)

dist_ref <- travel_distance %>% filter(MPA=='Reference')
mean_ref <- mean(dist_ref$value)



################################################################################
#pairwise PERMANOVA to test for sig differences

#pariwise permanova
set.seed(1985)
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
ccfrp_op <- perm_fun(ccfrp_pair_perm, group="Shallow reef")
kelp_fish_op <- perm_fun(kelp_fish_pair_perm, group='Kelp forest fishes')
kelp_invalg_perm <- perm_fun(kelp_invalg_pair_perm, group = "Kelp forest inverts and algae")
rocky_op <- perm_fun(rocky_pair_perm, group="Rocky intertidal")
deep_reef_op <- perm_fun(dr_pair_perm, group = "Deep reef")

perm_output <- rbind(ccfrp_op, kelp_fish_op, deep_reef_op,
                     kelp_invalg_perm, rocky_op)

rownames(perm_output) <- NULL

perm_output$MPA_type <- recode_factor(perm_output$MPA_type, "REF"="Reference")



#write.csv(perm_output, "/home/joshsmith/CA_MPA_Project/ca-mpa/analyses/5community_climate_ecology/tables/pairwise_permanova_table.csv")       # Export PDF
#grid.table(perm_output)
#dev.off()


################################################################################
#plot betadisper distance between centroids

#join distance and PERMANOVA results
distance1 <- travel_distance%>%
  mutate(period = ifelse(Var1 == "ref before" & Var2== "ref during" |
                           Var1== "smr before" & Var2== "smr during","before-to-during",
                         ifelse(Var1 == "ref before" & Var2== "ref after" |
                                  Var1== "smr before" & Var2== "smr after","before-to-after","")),
         period=forcats::fct_relevel(period, c("before-to-during","before-to-after")),
         MPA = tidytext::reorder_within(MPA, value, period),
         MPA_type = toupper(gsub( " .*$", "", Var1)))

distance1$MPA_type <- recode_factor(distance1$MPA_type, "REF"="Reference")
distance1$MPA_type <- recode_factor(distance1$MPA_type, "SMR"="MPA")


sig_distance <- left_join(distance1, perm_output, 
                          by=c("group","MPA_type","period"))%>%
                mutate(sig = ifelse(`Pr(>F)`<0.05, "*",""))%>%
                dplyr::rename("p-val" = `Pr(>F)`) %>%
                mutate(period = factor(period, levels=c("before-to-during",
                                                        "before-to-after")))
sig_distance$period <- recode_factor(sig_distance$period, "before-to-during"="Before-to-during")
sig_distance$period <- recode_factor(sig_distance$period, "before-to-after"="Before-to-after")



my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(#angle = 90, 
                                              hjust = 0.5, color = "black"),
                   axis.text.x = element_text(color = "black"),
                   axis.title=element_text(size=6),
                   plot.tag=element_text(size=7, color = "black"),
                   plot.title =element_text(size=7, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.text=element_text(size=7),
                   legend.title=element_text(size=7),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   #facets
                   strip.text = element_text(size=5),
                   #margins
                   #plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)

sig_distance$MPA_type <- recode_factor(sig_distance$MPA_type, "In"="MPA")
sig_distance$MPA_type <- recode_factor(sig_distance$MPA_type, "Out"="Reference")

p1 <- 
  sig_distance %>%
  dplyr::rename("Period"=period)%>%
  filter(Period == "Before-to-during")%>%
  mutate(group = factor(group, levels = c("Rocky intertidal","Kelp forest inverts and algae",
                                          "Kelp forest fishes","Shallow reef","Deep reef")),
         MPA_type = factor(MPA_type, levels = c("MPA","Reference")),
         sig_y = value+se_pooled+0.01)%>%
  arrange(MPA_type, -value, group)%>%
  mutate(mpa_ordered = fct_inorder(paste(MPA_type, group, sep = "."))) |> 
  ggplot(aes(x = group, y = value, color = MPA_type, group = mpa_ordered)) +
  geom_point(position = position_dodge(width=0.5),
             size=2)+
  geom_errorbar(aes(ymin=value-se_pooled,
                    ymax = value+se_pooled), stat="identity",
                position = position_dodge(width=0.5), size=0.3, width=.3)+
  #add significance level
  geom_text(aes(x = group, y=sig_y, label=sig), size=3, #hjust=-1, vjust=0.5,
            position = position_dodge(width=0.5),
            show.legend = FALSE)+
  annotate("segment",x =0.62, y = 0.215, xend = 0.62, yend = 0.01,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), size=3, linejoin = "mitre",
           color="lightgray")+
  annotate("text", size=1.8,
           x=0.6, y=0.12, angle=90,
           label = "Resistance",
           fontface = 'italic',
           color = 'black')+
  annotate("text", size=2,
           x=0.64, y=-0.008,
           label = "High",
           fontface = 'italic',
           color = 'black')+
  annotate("text", size=2,
           x=0.61, y=0.225, 
           label = "Low",
           fontface = 'italic',
           color = 'black')+
  ylab("")+
  xlab("")+
  scale_y_continuous(limits=c(-0.01,0.23))+
  scale_x_discrete(labels = function(x) {
    ifelse(x == "Kelp forest inverts and algae", 
           stringr::str_wrap(x, width = 9), 
           stringr::str_wrap(x, width = 8))
  })+
 labs(color = "Site type", tag = "A")+
  scale_color_brewer(palette="Set1") +
  #geom_vline(xintercept=c(1.5, 2.5,3.5,4.5), color="grey",alpha=.4)+
  #coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_blank())+
  ggtitle("Resistance \n(Before-to-during)")+
  my_theme+
  theme(aspect.ratio=1)

p1


p2 <- 
  sig_distance %>%
  dplyr::rename("Period"=period)%>%
  filter(Period == "Before-to-after")%>%
  mutate(group = factor(group, levels = c("Rocky intertidal","Kelp forest inverts and algae",
                                          "Kelp forest fishes","Shallow reef","Deep reef")),
         MPA_type = factor(MPA_type, levels = c("MPA","Reference")),
         sig_y = value+se_pooled+0.01)%>%
  arrange(MPA_type, -value, group)%>%
  mutate(mpa_ordered = fct_inorder(paste(MPA_type, group, sep = "."))) |> 
  ggplot(aes(x = group, y = value, color = MPA_type, group = mpa_ordered)) +
  geom_point(position = position_dodge(width=0.5),
             size=2)+
  geom_errorbar(aes(ymin=value-se_pooled,
                    ymax = value+se_pooled), stat="identity",
                position = position_dodge(width=0.5), size=0.3, width=.3)+
  #add significance level
  geom_text(aes(x = group, y=sig_y, label=sig), size=3, #hjust=-1, vjust=0.5,
            position = position_dodge(width=0.5),
            show.legend = FALSE)+
  annotate("segment",x =0.62, y = 0.215, xend = 0.62, yend = 0.01,
           arrow = arrow(type = "closed", length = unit(0.01, "npc")), size=3, linejoin = "mitre",
           color="lightgray")+
  annotate("text", size=1.8,
           x=0.6, y=0.12, angle=90,
           label = "Recovery",
           fontface = 'italic',
           color = 'black')+
  annotate("text", size=2,
           x=0.64, y=-0.008,
           label = "High",
           fontface = 'italic',
           color = 'black')+
  annotate("text", size=2,
           x=0.61, y=0.225, 
           label = "Low",
           fontface = 'italic',
           color = 'black')+
  ylab("")+
  xlab("")+
  scale_y_continuous(limits=c(-0.01,0.23))+
  scale_x_discrete(labels = function(x) {
    ifelse(x == "Kelp forest inverts and algae", 
           stringr::str_wrap(x, width = 9), 
           stringr::str_wrap(x, width = 8))
  })+
  labs(color = "Site type", tag="B")+
  scale_color_brewer(palette="Set1") +
  #geom_vline(xintercept=c(1.5, 2.5,3.5,4.5), color="grey",alpha=.4)+
  #coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_blank())+
  ggtitle("Recovery \n(Before-to-after)")+
  my_theme+
  theme(aspect.ratio=1)
p2


library(grid)
g <- ggpubr::ggarrange(p1, p2, nrow=1, ncol=2, 
                       common.legend = TRUE, legend = "right")



g_title <- ggpubr::annotate_figure(g, left = textGrob("Distance (Bray-Curtis)", 
                                                     rot = 90, vjust = 3, hjust=0.3, gp = gpar(cex = 0.6)),
                                  bottom = textGrob("Habitat", hjust=1, vjust=-5, gp = gpar(cex = 0.6)))



ggsave(here::here("analyses", "5community_climate_ecology", "figures", "Fig3_centroid_shifts_revised.png"), g_title, height=4, width = 7, units = "in", 
  dpi = 600, bg="white")


################################################################################
#export table

perm_tab <- sig_distance %>%
            dplyr::select(habitat = group, MPA_type, period, distance  = value,
                          Df, SumOfSqs, R2, `F`, `p-val`, sig) %>%
            mutate(habitat = factor(habitat, levels = c("Rocky intertidal",
                                                        "Kelp forest inverts and algae",
                                                        "Kelp forest fishes",
                                                        "Rocky reef fishes",
                                                        "Deep reef fishes")),
                   period = factor(period, levels = c("Before-to-during",
                                                      "Before-to-after")),
                   habitat = factor(habitat, levels = c("Rocky intertidal",
                                                        "Kelp forest inverts and algae",
                                                        "Kelp forest fishes",
                                                        "Rocky reef fishes",
                                                        "Deep reef fishes")))%>%
          rename("MPA type" = "MPA_type",
                 "Habitat" = "habitat",
                 "Transition" = "period",
                 "B-C distance" = "distance",
                 "Sum of squares" = "SumOfSqs",
                 "R-squared" = "R2",
                 "Psuedo F" = "F",
                 "P(perm)" = "p-val") %>%
          dplyr::select(Habitat, "MPA type", Transition, "B-C distance", "Sum of squares",
                        "R-squared", "Psuedo F","P(perm)") 

#write.csv(perm_tab, file.path(tab_out, "pairwise_permanova.csv"))





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
  filter(centroid_2>=2007)%>%
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


#modify helper function to calculate year-to-year centroid distances 
#by MPA type
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



















