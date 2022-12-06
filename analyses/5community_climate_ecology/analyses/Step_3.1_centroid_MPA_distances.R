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



#load data ---------------------------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"

nmds_scores <- load(file.path(data_path, "bray_nmds_scores.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))
envr_vars <- load(file.path(data_path, "envr_vars.rda"))
eco_dist <- load(file.path(data_path, "distance_matrices_BC.rda"))


# define levels ----------------------------------------------------------------

rocky_group_vars1 <- rocky_group_vars %>%
                      mutate(affiliated_mpa2 = ifelse(affiliated_mpa == "none",
                                                      site, affiliated_mpa),
                             site_yr_ID = paste(affiliated_mpa2, desig_state))

CCFRP_group_vars1 <- CCFRP_group_vars %>%
                      mutate(site_yr_ID = paste(affiliated_mpa, desig_state))



# Step 1 - examine hull shape and dispersion -----------------------------------

CCFRP_disper <- betadisper(CCFRP_distmat, type="centroid", 
                           group=CCFRP_group_vars1$site_yr_ID)

rocky_disper <- betadisper(rocky_distmat, type = "centroid",
                           group = rocky_group_vars1$site_yr_ID)


# Step 2 - calculate distance -----------------------------------

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


CCFRP <- eig_fun(CCFRP_disper)
rocky <- eig_fun(rocky_disper)





