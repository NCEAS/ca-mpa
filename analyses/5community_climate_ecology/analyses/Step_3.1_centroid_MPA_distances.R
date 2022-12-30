#author: "Joshua G. Smith"
#date: '2022-07-13'

rm(list=ls())

#required packages
require(vegan)
require(dplyr)
require(tidyr)
require(stringr)
require(gridExtra)
require(usedist)
require(ggplot2)
require(reshape2)
require(ggfittext)
require(pairwiseAdonis)
require(purrr)
require(broom)



#load data ---------------------------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))


# define test centroids  -------------------------------------------------------

rocky_group_vars1 <- rocky_group_vars %>%
                      mutate(affiliated_mpa2 = ifelse(affiliated_mpa == "none",
                                                      site, affiliated_mpa),
                             site_yr_ID = paste(affiliated_mpa2, desig_state))

CCFRP_group_vars1 <- CCFRP_group_vars %>%
                      mutate(site_yr_ID = paste(affiliated_mpa, desig_state))

kelp_fish_group_vars1 <- kelp_fish_group_vars %>%
                         mutate(site_yr_ID = paste(affiliated_mpa, desig_state))

kelp_invalg_group_vars1 <- kelp_invalg_group_vars %>%
                         mutate(site_yr_ID = paste(affiliated_mpa, desig_state))

deep_reef_group_vars1 <- deep_reef_group_vars %>%
                        mutate(site_yr_ID = paste(affiliated_mpa, desig_state))

# Step 1 - examine hull shape and dispersion -----------------------------------

CCFRP_disper <- betadisper(CCFRP_distmat, type="centroid", 
                           group=CCFRP_group_vars1$site_yr_ID)

rocky_disper <- betadisper(rocky_distmat, type = "centroid",
                           group = rocky_group_vars1$site_yr_ID)

kelp_fish_disper <- betadisper(kelp_fish_distmat, type="centroid",
                               group=kelp_fish_group_vars1$site_yr_ID)

kelp_invalg_disper <- betadisper(kelp_invalg_distmat, type = "centroid",
                                 group = kelp_invalg_group_vars1$site_yr_ID)

deep_reef_disper <- betadisper(deep_reef_distmat, type="centroid",
                               group=deep_reef_group_vars1$site_yr_ID)

# Step 2 - calculate distances and pooled s.d.----------------------------------

#create helper function

eig_fun <- function(disper_mat) {
  
  x = melt(as.matrix(sqrt(dist(disper_mat$centroids[,disper_mat$eig>0]^2)-
                            dist(disper_mat$centroids[,disper_mat$eig<0]^2))))
  tibble::rownames_to_column(x, "distance")
  
  x2 <- x %>% mutate(
    MPA1 = word(Var1 , 1  , -3),
    MPA_type1 = word(Var1 , -2),
    MPA_period1 = word(Var1, -1),
    MPA2 = word(Var2 , 1  , -3),
    MPA_type2 = word(Var2 , -2),
    MPA_period2 = word(Var2, -1))
  
  x3 <- x2 %>% filter(MPA1 == MPA2 &
                        MPA_type1 == MPA_type2,
                      MPA_period1 == "before" & MPA_period2 == "after" |
                        MPA_period1 == "before" & MPA_period2 == "during") %>%
    mutate(join_ID = paste(MPA1, MPA_type1, MPA_period1, MPA_period2))
  
  sd = melt(as.matrix(tapply(disper_mat$distances, disper_mat$group, sd)))%>%
    tibble::rownames_to_column() %>% mutate(s_d = value)%>%
    dplyr::select(Var1, s_d)
  
  n = melt(table(disper_mat$group))%>%
    tibble::rownames_to_column() %>% mutate(n = value)%>%
    dplyr::select(Var1, n) 
  
  e_hat <- left_join(sd, n, by='Var1') %>%
    #select(!(c(value, rowname)))%>%
    mutate(MPA_type = factor(word(Var1, -2)),
           MPA_period = factor(word(Var1, -1)),
           MPA = factor(word(Var1 , 1  , -3)))%>%
    select(!(Var1))%>%
    pivot_wider(names_from='MPA_period', values_from = c('s_d','n'))%>%
    mutate(sd_pooled_before_after = sqrt(
      ((`n_before`-1)*`s_d_before`^2 + (`n_after`-1)*`s_d_after`^2)/
        (`n_before`+`n_after`-2)
    ),
    sd_pooled_before_during = sqrt(
      ((`n_before`-1)*`s_d_before`^2 + (`n_during`-1)*`s_d_during`^2)/
        (`n_before`+`n_during`-2)
    )
    ) %>%
    pivot_longer(cols=c(`sd_pooled_before_after`,`sd_pooled_before_during`),
                 values_to ="sd_pooled")%>%
    mutate(sub = gsub("_", " ", name),
           MPA_period1 =factor(word(sub, -2, sep = ' ')),
           MPA_period2 = factor(word(sub, -1)),
           join_ID = paste(MPA, MPA_type, MPA_period1, MPA_period2))%>%
    select(join_ID, sd_pooled)
  
  left_join(x3, e_hat, by='join_ID')
}


CCFRP <- eig_fun(CCFRP_disper) %>% 
                mutate(group = "Rocky reef fishes")
rocky <- eig_fun(rocky_disper) %>% 
                mutate(group = "Rocky intertidal")
kelp_fish <- eig_fun(kelp_fish_disper) %>% 
                mutate(group = "Kelp forest fishes")
kelp_invalg <- eig_fun(kelp_invalg_disper) %>% 
                mutate(group = "Kelp forest inverts and algae")


####problem with deep reef --- need to figure this out before continuing
deep_reef <- eig_fun(deep_reef_disper)


#combine into df
travel_distance <- as.data.frame(rbind(CCFRP, rocky, kelp_fish,
                                       kelp_invalg))




################################################################################
#Step 3 -- use permanova to test significance

#pariwise permanova
ccfrp_pair_perm <- pairwise.adonis2(CCFRP_distmat ~ site_yr_ID, 
                                   data = CCFRP_group_vars1, permutations = 999)

rocky_pair_perm <- pairwise.adonis2(rocky_distmat ~ site_yr_ID, 
                                   data = rocky_group_vars1, permutations = 999)

kelp_fish_pair_perm <- pairwise.adonis2(kelp_fish_distmat ~ site_yr_ID, 
                               data = kelp_fish_group_vars1, permutations = 999)


kelp_invalg_pair_perm <- pairwise.adonis2(kelp_invalg_distmat ~ site_yr_ID, 
                             data = kelp_invalg_group_vars1, permutations = 999)


##### EXTRACT OUTPUT
# Remove the different list item
rocky_pair_perm$parent_call <- NULL


# make it a data frame

extract_fun <- function(mod, habitat) {
  mod$parent_call <- NULL
  mod %>% map_dfr(~tidy(.x), .id="name")%>%
                      filter(term == "site_yr_ID")%>%
                      mutate(group_1 = str_extract(name, "[^_]+"),
                             group_2 = gsub(".*\\_", "", name),
                             period_1 = gsub(".*\\ ", "", group_1),
                             period_2 = gsub(".*\\ ", "", group_2),
                             MPA_int_1 = word(group_1 , 1  , -2),
                             MPA_1 = word(MPA_int_1, 1  , -2),
                             MPA_int_2 = word(group_2 , 1  , -2),
                             MPA_2 = word(MPA_int_2, 1  , -2),
                             MPA_type_1 = word(MPA_int_1,-1),
                             MPA_type_2 = word(MPA_int_1,-1),
                             habitat = habitat
                             )%>%
                      filter(MPA_int_1 == MPA_int_2,
                             period_1 == "before") %>%
                     dplyr::select(habitat, "MPA" = MPA_1, "MPA_type" = MPA_type_1,
                                   period_1, period_2,
                                   SumOfSqs, R2, "F.stat"=statistic,
                                   p.value)
}



CCFRP_fun <- extract_fun(ccfrp_pair_perm, "Rocky reef fishes")
rocky_fun <- extract_fun(rocky_pair_perm, "Rocky intertidal")
kelp_fish_fun <- extract_fun(kelp_fish_pair_perm, "Kelp forest fishes")
kelp_invalg_fun <- extract_fun(kelp_invalg_pair_perm, "Kelp forest inverts and algae")

mod_out <- rbind(CCFRP_fun, rocky_fun, kelp_fish_fun, kelp_invalg_fun)

################################################################################
##join distance with significance level

#prep distance
b_join <- travel_distance %>%
          select(join_ID, group, "distance"=value, sd_pooled)

#prep model
a_join <- mod_out %>%
          mutate(join_ID = paste(MPA, MPA_type, period_1, period_2))

#join
mpa_output <- left_join(a_join, b_join, by=c("habitat"="group","join_ID"))%>%
              select(!(join_ID)) %>%
              mutate(transition = ifelse(period_2 == "during","resistance","resilience"),
                     sig = ifelse(`p.value`<0.05, "*",""))


#write.csv(mpa_output, 
# "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data/mpa_betadisp_mod.csv",
# row.names = FALSE)


################################################################################
#plot


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

mpa_output$MPA_type <- recode_factor(mpa_output$MPA_type, "ref"="Reference")
mpa_output$MPA_type <- recode_factor(mpa_output$MPA_type, "smr"="MPA")

p1 <- 
  mpa_output %>%
  filter(transition == "resistance")%>%
  mutate(group = factor(habitat, levels = c("Rocky intertidal","Kelp forest inverts and algae",
                                          "Kelp forest fishes","Rocky reef fishes")),
         MPA_type = factor(MPA_type, levels = c("MPA","Reference")))%>%
  arrange(MPA_type, -distance, habitat)%>%
  mutate(mpa_ordered = fct_inorder(paste(MPA_type, habitat, sep = "."))) |> 
  ggplot(aes(x = MPA, y = distance, color = MPA_type, group = mpa_ordered)) +
  geom_point(position = position_dodge(width=0.8),
             size=3)+
  geom_errorbar(aes(ymin=distance-sd_pooled,
                    ymax = distance+sd_pooled), stat="identity",
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
  facet_wrap(~habitat, scales = "free")+
  #geom_vline(xintercept=c(1.5, 2.5,3.5,4.5), color="grey",alpha=.4)+
  #coord_flip()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        legend.key=element_blank())+
  ggtitle("Resistance \n(Before-to-during)")+
  my_theme+
  theme(aspect.ratio=1)






library(grid)
g <- ggpubr::ggarrange(p1, p2, nrow=1, ncol=2, 
                       common.legend = TRUE, legend = "right")



g_title <- ggpubr::annotate_figure(g, left = textGrob("Distance (Bray-Curtis)", 
                                                      rot = 90, vjust = 2, hjust=0.3, gp = gpar(cex = 0.8)),
                                   bottom = textGrob("Habitat", hjust=1, vjust=-11, gp = gpar(cex = 0.8)))












