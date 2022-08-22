#author: "Joshua G. Smith"
#date: '2022-08-19'

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
require(mvabund)



# #load data --------------------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"

comm_data <- load(file.path(data_path, "comm_data.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))


# run SIMPER tables before & after MHW------------------------------------------


sim_CCFRP <- with(CCFRP_group_vars, simper(CCFRP_ord_data, MHW), 
                  ordered=TRUE)
sim_kelp_swath <- with(kelp_swath_group_vars, simper(kelp_swath_ord_data, MHW),
                  ordered=TRUE)
sim_kelp_upc <- with(kelp_upc_group_vars, simper(kelp_upc_ord_data, MHW),
                  ordered=TRUE)
sim_kelp_fish <- with(kelp_fish_group_vars, simper(kelp_fish_ord_data, MHW),
                  ordered=TRUE)
sim_deep_reef <- with(deep_reef_group_vars, simper(deep_reef_ord_data, MHW),
                  ordered=TRUE)
sim_rocky <- with(rocky_group_vars, simper(rocky_ord_data, MHW),
                  oredered=TRUE)

#collect output

CCFRP_a_b_table <- as.data.frame(summary(sim_CCFRP)$before_after)%>%
  mutate(group="CCFRP",
         contrib = cumsum-lag(cumsum, default=0),
         perc_change = (avb-ava)/ava,
         sign = ifelse(perc_change > 0, "positive","negative"))

kelp_swath_a_b_table <- as.data.frame(summary(sim_kelp_swath)$before_after)%>%
  mutate(group="kelp_swath",
         contrib = cumsum-lag(cumsum, default=0),
         perc_change = (avb-ava)/ava,
         sign = ifelse(perc_change > 0, "positive","negative"))

kelp_upc_a_b_table <- as.data.frame(summary(sim_kelp_upc)$before_after)%>%
  mutate(group="kelp_upc",
         contrib = cumsum-lag(cumsum, default=0),
         perc_change = (avb-ava)/ava,
         sign = ifelse(perc_change > 0, "positive","negative"))

kelp_fish_a_b_table <- as.data.frame(summary(sim_kelp_fish)$before_after)%>%
  mutate(group="kelp_fish",
         contrib = cumsum-lag(cumsum, default=0),
         perc_change = (avb-ava)/ava,
         sign = ifelse(perc_change > 0, "positive","negative"))

deep_reef_a_b_table <- as.data.frame(summary(sim_deep_reef)$before_after)%>%
  mutate(group="deep_reef",
         contrib = cumsum-lag(cumsum, default=0),
         perc_change = (avb-ava)/ava,
         sign = ifelse(perc_change > 0, "positive","negative"))

rocky_a_b_table <- as.data.frame(summary(sim_rocky)$before_after)%>%
  mutate(group="rocky",
         contrib = cumsum-lag(cumsum, default=0),
         perc_change = (avb-ava)/ava,
         sign = ifelse(perc_change > 0, "positive","negative"))


simper_a_b_table <- rbind(CCFRP_a_b_table, kelp_swath_a_b_table, 
                          kelp_upc_a_b_table, kelp_fish_a_b_table, 
                          deep_reef_a_b_table, rocky_a_b_table)%>%
  tibble::rownames_to_column(var = "species") %>%
  filter(cumsum <0.80)%>%
  select(group, species, avg_before = avb, avg_after=ava, cumsum, contrib, perc_change, sign)




# Examine output ----------------------------------------------------------



#set order
simper_a_b_table$group <- factor(simper_a_b_table$group,
                                 levels = c('CCFRP', 'kelp_swath', 
                                            'rocky','kelp_fish',
                                            'kelp_upc','deep_reef'))

ggplot(simper_a_b_table, aes(x = group, y = contrib, label = species, fill=sign)) +
  geom_bar(stat = "identity", color="black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color="black", 
            fontface="bold")+
  theme_minimal(base_size = 21)+
  scale_fill_discrete(name = "direction of change")+
  ylab("cumulative contribution")



# SIMPER tables w/ MPAs added (all combinations)--------------------------------

sim_CCFRP_MPA <- with(CCFRP_group_vars, simper(CCFRP_ord_data, desig_state))
sim_kelp_swath_MPA <- with(kelp_swath_group_vars, simper(kelp_swath_ord_data, 
                                                         desig_state))
sim_kelp_upc_MPA <- with(kelp_upc_group_vars, simper(kelp_upc_ord_data, 
                                                     desig_state))
sim_kelp_fish_MPA <- with(kelp_fish_group_vars, simper(kelp_fish_ord_data, 
                                                       desig_state))
sim_deep_reef_MPA <- with(deep_reef_group_vars, simper(deep_reef_ord_data, 
                                                       desig_state))
sim_rocky_MPA <- with(rocky_group_vars, simper(rocky_ord_data, desig_state))






# mvabund -----------------------------------------------------------------

#tutorial https://environmentalcomputing.net/statistics/mvabund/



#prep data by remove 'during' period 


CCFRP_join <- cbind(CCFRP_group_vars, CCFRP_ord_data)%>%
              filter(MHW == 'before' | MHW == 'after')

kelp_swath_join <- cbind(kelp_swath_group_vars, kelp_swath_ord_data)%>%
              filter(MHW == 'before' | MHW == 'after')

kelp_upc_join <- cbind(kelp_upc_group_vars, kelp_upc_ord_data)%>%
  filter(MHW == 'before' | MHW == 'after')

kelp_fish_join <- cbind(kelp_fish_group_vars, kelp_fish_ord_data)%>%
  filter(MHW == 'before' | MHW == 'after')

deep_reef_join <- cbind(deep_reef_group_vars, deep_reef_ord_data)%>%
  filter(MHW == 'before' | MHW == 'after')

rocky_join <- cbind(rocky_group_vars, rocky_ord_data)%>%
  filter(MHW == 'before' | MHW == 'after')


#format as mvabund objects
CCFRP_spp <- mvabund(CCFRP_join[,10:102])
kelp_swath_spp <- mvabund(kelp_swath_join[,10:177])
kelp_upc_spp <- mvabund(kelp_upc_join[,10:81])
kelp_fish_spp <- mvabund(kelp_fish_join[,10:108])
deep_reef_spp <- mvabund(deep_reef_join[,11:112])
rocky_spp <- mvabund(rocky_join[,10:58])



#check mean to variance relationships
mvabund::meanvar.plot(CCFRP_spp)
mvabund::meanvar.plot(kelp_swath_spp)
mvabund::meanvar.plot(kelp_upc_spp)
mvabund::meanvar.plot(kelp_fish_spp)
mvabund::meanvar.plot(deep_reef_spp)
mvabund::meanvar.plot(rocky_spp)



#test for species composition across heatwave periods

CCFRP_glm <- manyglm(CCFRP_spp ~ CCFRP_join$MHW)
kelp_swath_glm <- manyglm(kelp_swath_spp ~ kelp_swath_join$MHW)







CCFRP_aov <- anova(mod1, p.uni="adjusted")

mv_CCFRP <- as.data.frame(CCFRP_aov[["uni.p"]]) 

mv_CCFRP_1 <- mv_CCFRP %>%
              pivot_longer(cols=1:ncol(.), names_to="species")%>%
              drop_na()%>%
              filter(value <= 0.05)





