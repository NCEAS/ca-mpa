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
require(stringr)
require(gt)



# #load data --------------------------------------------------------------

#old dat
#data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"

comm_data <- load(file.path(data_path, "comm_data.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))



# run SIMPER tables before & after MHW------------------------------------------

sim_CCFRP <- with(CCFRP_group_vars, simper(CCFRP_ord_data, MHW), 
                  ordered=TRUE)

#SIMPER does not work on standardized data
#kelp_invalg_ord_data[is.na(kelp_invalg_ord_data)] = 0
#sim_kelp_invalg <- with(kelp_invalg_group_vars, simper(kelp_invalg_ord_data, MHW),
#                  ordered=TRUE)

sim_kelp_upc <- with(kelp_upc_group_vars, simper(kelp_upc_ord_data, MHW),
                 ordered=TRUE)

sim_kelp_swath <- with(kelp_swath_group_vars, simper(kelp_swath_ord_data, MHW),
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

#kelp_invalg_a_b_table <- as.data.frame(summary(sim_kelp_invalg)$before_after)%>%
#  mutate(group="kelp_invalg",
#         contrib = cumsum-lag(cumsum, default=0),
#         perc_change = (avb-ava)/ava,
#         sign = ifelse(perc_change > 0, "positive","negative"))

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


simper_a_b_table <- rbind(CCFRP_a_b_table, 
                          #kelp_invalg_a_b_table,
                          kelp_swath_a_b_table,
                          kelp_upc_a_b_table,
                          kelp_fish_a_b_table, 
                          deep_reef_a_b_table, rocky_a_b_table)%>%
  tibble::rownames_to_column(var = "species") %>%
  filter(cumsum <0.80)%>%
  select(group, species, avg_before = avb, avg_after=ava, cumsum, contrib, perc_change, sign)




# Examine output ----------------------------------------------------------

# Clean up spp names 
simper_a_b_table$species <- gsub('[[:digit:]]+', '', simper_a_b_table$species)

#load KF taxon table
taxon_tab <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","Kelp-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")
rocky_taxon <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","RockyIntertidal-LongTerm-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")%>%
  select(marine_species_code, marine_species_name)

rocky_taxon$marine_species_code <- tolower(rocky_taxon$marine_species_code)

taxon_short <- taxon_tab %>%
  select(common_name, ScientificName_accepted) %>%
  mutate(join_ID = sub(" ","_", ScientificName_accepted))


taxon_short$join_ID <- gsub("/","_",taxon_short$join_ID)

taxon_short$join_ID <- tolower(taxon_short$join_ID)


simper_table <- left_join(simper_a_b_table, taxon_short, by=c("species"="join_ID"))%>%
  mutate(species_ID = ifelse(is.na(common_name),species, common_name))%>%
  filter(!(species_ID == 'chain-bladder kelp'))%>%
  distinct()

simper_table <- left_join(simper_table, rocky_taxon, by=c("species_ID"="marine_species_code"))


simper_table$species_ID <- recode_factor(simper_table$species_ID,
                                         "sebastes_mystinus_or_diaconus"=
                                           "blue rockfish") 

simper_table <- simper_table%>% mutate(species_ID = ifelse(group=='rocky',marine_species_name,as.character(species_ID)))  


simper_table$species_ID <- gsub("_"," ",simper_table$species_ID)
simper_table$species_ID <- gsub("Adult"," ",simper_table$species_ID)


simper_table$species_ID <- str_to_title(simper_table$species_ID)

simper_table <- simper_table %>%
                mutate(monitoring = ifelse(group == 'kelp_upc'|
                                             group== 'kelp_swath'|
                                             group=='kelp_fish',
                                           'Kelp Forest',
                                           as.character(group)))
simper_table$monitoring <- recode_factor(simper_table$monitoring,
                                         'deep_reef'='Deep Reef')
simper_table$monitoring <- recode_factor(simper_table$monitoring,
                                         'rocky'='Rocky Intertidal')


#add monitoring group level


#rename

simper_table$group <- recode_factor(simper_table$group, 'kelp_swath'=
                                      'kelp and mobile inverts')

simper_table$group <- recode_factor(simper_table$group, 'kelp_upc'=
                                      'sessile inverts and algae')

simper_table$group <- recode_factor(simper_table$group, 'kelp_fish'=
                                      'Fish')

simper_table$group <- recode_factor(simper_table$group, 'deep_reef'=
                                      'Fish')

simper_table$group <- recode_factor(simper_table$group, 'CCFRP'=
                                      'Fish')

simper_table$group <- recode_factor(simper_table$group, 'rocky'=
                                      'Inverts and algae')




simper_table$monitoring <- factor(simper_table$monitoring,
                             levels = c('CCFRP', #'kelp_invalg', 
                                        'Deep Reef',
                                        'Kelp Forest',
                                        'Rocky Intertidal'))




###############################################################################
#plot SIMPER
st <- ggplot(simper_table, aes(x = group, y = contrib, label = stringr::str_wrap(species_ID, 20), fill=sign)) +
  geom_bar(stat = "identity", color="black") +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color="black", 
            fontface="bold", lineheight = 0.75) +
  theme_minimal(base_size = 21) +
  scale_fill_discrete(name = "direction of change") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  facet_grid(.~monitoring, scales = "free", switch = "x", space = "free_x") + 
  theme_classic() +
  theme(strip.placement = "outside", panel.grid.major = element_blank()) +
  xlab("Monitoring Group") +
  ylab("Cumulative Contribution (top 80%)")

st

# Saving the plot with dimensions that facilitate the text wrapping
#ggsave("analyses/5community_climate_ecology/figures/simper_table_plot.png", width = 15, height = 12, dpi = 300)

###############################################################################
#SIMPER output table

#gt option
simper_out_gt <- simper_table %>%
              select('monitoring group'=monitoring, "community type"=group, 
                     'species' = species_ID,
                     'cumulative contrib.'=cumsum, 'individual contrib.' = contrib)%>%
              dplyr::group_by(`monitoring group`, `community type`)%>%
              dplyr::arrange(`cumulative contrib.`,.by_group=TRUE)%>%
              mutate(`cumulative contrib.` = round(`cumulative contrib.`,2),
                     `individual contrib.` = round(`individual contrib.`,2))%>%
  gt(rowname_col = 'community type',
     groupname_col = 'monitoring group',
     )%>%
  tab_header(title = md("Cumulative contribution of species to community 
                        structure changes"))


gtsave(simper_out_gt, "analyses/5community_climate_ecology/tables/simper_table.docx")

#flex option
simper_out_flex <- simper_table %>%
  select('monitoring group'=monitoring, "community type"=group, 
         'species' = species_ID,
         'cumulative contrib.'=cumsum, 'individual contrib.' = contrib)%>%
  dplyr::group_by(`monitoring group`, `community type`)%>%
  dplyr::arrange(`cumulative contrib.`,.by_group=TRUE)%>%
  mutate(`cumulative contrib.` = round(`cumulative contrib.`,2),
         `individual contrib.` = round(`individual contrib.`,2))

ftab <- flextable::qflextable(simper_out_flex) %>%
        #theme_zebra()%>%
       #theme_tron_legacy()
        #theme_vader()%>%
        #theme_box()
        theme_alafoli()


        library(officer)
sect_properties <- prop_section(
          page_size = page_size(
            orient = "landscape",
            width = 8.3, height = 11
          ),
          type = "continuous",
          page_margins = page_mar()
        )

save_as_docx(ftab,
             path = "analyses/5community_climate_ecology/tables/simper_out.docx",
             pr_section = sect_properties)
        




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



#fit glms for mean-var relationships

CCFRP_glm <- manyglm(CCFRP_spp ~ CCFRP_join$MHW)
kelp_swath_glm <- manyglm(kelp_swath_spp ~ kelp_swath_join$MHW)
kelp_upc_glm <- manyglm(kelp_upc_spp ~ kelp_swath_join$MHW)
kelp_fish_glm <- manyglm(kelp_fish_spp ~ kelp_fish_join$MHW)
deep_reef_glm <- manyglm(deep_reef_spp ~ deep_reef_join$MHW)
rocky_glm <- manyglm(rocky_spp ~ rocky_join$MHW)


#test for species composition across heatwave periods
CCFRP_aov <- anova(CCFRP_glm, p.uni="adjusted")
CCFRP_out <- as.data.frame(CCFRP_aov[["uni.p"]])

kelp_swath_aov <- anova(kelp_swath_glm, p.uni="adjusted")
kelp_swath_out <- as.data.frame(kelp_swath_aov[["uni.p"]])

kelp_upc_aov <- anova(kelp_upc_glm, p.uni = "adjusted")
kelp_upc_out <- as.data.frame(kelp_upc_aov[["uni.p"]])

kelp_fish_aov <- anova(kelp_fish_glm, p.uni="adjusted")
kelp_fisf_out <- as.data.frame(kelp_fish_aov[["uni.p"]])

deep_reef_aov <- anova(deep_reef_glm, p.uni="adjusted")
deep_reef_out <- as.data.frame(deep_reef_aov[["uni.p"]])

rocky_aov <- anova(rocky_glm, p.uni='adjusted')
rocky_out <- as.data.frame(rocky_aov[["uni.p"]])

#examine output
CCFRP_sig <- CCFRP_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group="CCFRP")

kelp_swath_sig <- kelp_swath_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group="kelp_swath")

kelp_upc_sig <- kelp_upc_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group="kelp_upc")

kelp_fish_sig <- kelp_fish_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group=="kelp_fish")
 
deep_reef_sig <- deep_reef_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group=="deep_reef")

rocky_sig <- rocky_out %>%
  pivot_longer(cols=1:ncol(.), names_to="species")%>%
  drop_na()%>%
  filter(value <= 0.05) %>%
  mutate(group=="rocky")

aov_out <- rbind(CCFRP_sig, kelp_swath_sig, kelp_upc_sig, kelp_fish_sig,
                 deep_reef_sig, rocky_sig
                 )





