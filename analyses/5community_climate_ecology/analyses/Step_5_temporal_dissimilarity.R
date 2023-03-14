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
require(ggdendro)
require(here)

#Old
#data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"

data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"

distmat <- load(file.path(data_path, "distance_matrices_BC.rda"))
group_vars <- load(file.path(data_path, "group_vars.rda"))





# Take a look at year-to-year dissimilarities -----------------------------------------------

set.seed(1985)

#CCFRP
CCFRP_mean_dist <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars$year) #calculate mean dissim
CCFRP_diag <- diag(CCFRP_mean_dist[,2:14]) #extract mat diagonal
CCFRP_df <- data.frame(Year = row.names(CCFRP_mean_dist[2:14,]), CCFRP_diag) #convert to df
CCFRP_df$group <- c("CCFRP")
colnames(CCFRP_df) <- c('year','dissim','group')

#kelp_swath 
kelp_swath_dist <- meandist(kelp_swath_distmat, grouping = kelp_swath_group_vars$year)
kelp_swath_diag <- diag(kelp_swath_dist[,2:14])
kelp_swath_df <- data.frame(Year = row.names(kelp_swath_dist[2:14,]), kelp_swath_diag)
kelp_swath_df$group <- c("kelp_swath")
colnames(kelp_swath_df) <- c('year','dissim','group')

#kelp_upc
kelp_upc_dist <- meandist(kelp_upc_distmat, grouping = kelp_upc_group_vars$year)
kelp_upc_diag <- diag(kelp_upc_dist[,2:14])
kelp_upc_df <- data.frame(Year = row.names(kelp_upc_dist[2:14,]), kelp_upc_diag)
kelp_upc_df$group <- c("kelp_upc")
colnames(kelp_upc_df) <- c('year','dissim','group')

#kelp_invalg
kelp_invalg_dist <- meandist(kelp_invalg_distmat, grouping = kelp_invalg_group_vars$year)
kelp_invalg_diag <- diag(kelp_invalg_dist[,2:14])
kelp_invalg_df <- data.frame(Year = row.names(kelp_invalg_dist[2:14,]), kelp_invalg_diag)
kelp_invalg_df$group <- c("kelp inverts and algae")
colnames(kelp_invalg_df) <- c('year','dissim','group')

#kelp_fish
kelp_fish_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars$year)
kelp_fish_diag <- diag(kelp_fish_dist[,2:14])
kelp_fish_df <- data.frame(Year = row.names(kelp_fish_dist[2:14,]), kelp_fish_diag)
kelp_fish_df$group <- c("kelp fish")
colnames(kelp_fish_df) <- c('year','dissim','group')

#deep reef
deep_reef_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars$year)
deep_reef_diag <- diag(deep_reef_dist[,2:8])
deep_reef_df <- data.frame(Year = row.names(deep_reef_dist[2:8,]), deep_reef_diag)
deep_reef_df$group <- c("deep reef")
colnames(deep_reef_df) <- c('year','dissim','group')

#rocky
rocky_dist <- meandist(rocky_distmat, grouping = rocky_group_vars$year)
rocky_diag <- diag(rocky_dist[,2:14])
rocky_df <- data.frame(Year = row.names(rocky_dist[2:14,]), rocky_diag)
rocky_df$group <- c("rocky intertidal")
colnames(rocky_df) <- c('year','dissim','group')


#Join

full_df <- rbind(CCFRP_df,kelp_invalg_df, kelp_fish_df, deep_reef_df, rocky_df)

full_df_2010 <- full_df %>% filter(as.numeric(year)>=2007) %>%
                group_by(year)%>%
                dplyr::mutate(ymean=mean(dissim))

dissim_plot <- full_df_2007 %>%
  ggplot(aes(x=as.numeric(year), y=dissim, color=group))+
  geom_point(alpha=0.4)+
  geom_line(alpha=0.4)+
  geom_smooth(aes(y=ymean),span=0.4, color='black')+
  #stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')+
  scale_x_continuous(breaks=2007:2020)+
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 12))+
  xlab("Year")+
  ylab("Dissimilarity")

#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "annual_dissimilarity.png"), dissim_plot, height=4, width = 8, units = "in", 
#       dpi = 600, bg="white")


# dissimilarity relative to 2010 ------------------------------------------

set.seed(1985)

#CCFRP
CCFRP_mean_dist <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars$year) #calculate mean dissim
CCFRP_diag <- CCFRP_mean_dist[4,4:14] #extract mat diagonal
CCFRP_df <- data.frame(Year = row.names(CCFRP_mean_dist[4:14,]), CCFRP_diag) #convert to df
CCFRP_df$group <- c("CCFRP")
colnames(CCFRP_df) <- c('year','dissim','group')

#kelp_swath 
kelp_swath_dist <- meandist(kelp_swath_distmat, grouping = kelp_swath_group_vars$year)
kelp_swath_diag <- kelp_swath_dist[12,12:22]
kelp_swath_df <- data.frame(Year = row.names(kelp_swath_dist[12:22,]), kelp_swath_diag)
kelp_swath_df$group <- c("kelp_swath")
colnames(kelp_swath_df) <- c('year','dissim','group')

#kelp_upc
kelp_upc_dist <- meandist(kelp_upc_distmat, grouping = kelp_upc_group_vars$year)
kelp_upc_diag <- kelp_upc_dist[12,12:22]
kelp_upc_df <- data.frame(Year = row.names(kelp_upc_dist[12:22,]), kelp_upc_diag)
kelp_upc_df$group <- c("kelp_upc")
colnames(kelp_upc_df) <- c('year','dissim','group')

#kelp_invalg
kelp_invalg_dist <- meandist(kelp_invalg_distmat, grouping = kelp_invalg_group_vars$year)
kelp_invalg_diag <- kelp_invalg_dist[12,12:22]
kelp_invalg_df <- data.frame(Year = row.names(kelp_invalg_dist[12:22,]), kelp_invalg_diag)
kelp_invalg_df$group <- c("kelp inverts and algae")
colnames(kelp_invalg_df) <- c('year','dissim','group')

#kelp_fish
kelp_fish_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars$year)
kelp_fish_diag <- kelp_fish_dist[12,12:22]
kelp_fish_df <- data.frame(Year = row.names(kelp_fish_dist[12:22,]), kelp_fish_diag)
kelp_fish_df$group <- c("kelp fish")
colnames(kelp_fish_df) <- c('year','dissim','group')

#deep reef
deep_reef_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars$year)
deep_reef_diag <- deep_reef_dist[4,4:8]
deep_reef_df <- data.frame(Year = row.names(deep_reef_dist[4:8,]), deep_reef_diag)
deep_reef_df$group <- c("deep reef")
colnames(deep_reef_df) <- c('year','dissim','group')

#rocky
rocky_dist <- meandist(rocky_distmat, grouping = rocky_group_vars$year)
rocky_diag <- rocky_dist[9,9:19]
rocky_df <- data.frame(Year = row.names(rocky_dist[9:19,]), rocky_diag)
rocky_df$group <- c("rocky intertidal")
colnames(rocky_df) <- c('year','dissim','group')


#Join

full_df <- rbind(CCFRP_df, kelp_invalg_df, kelp_fish_df, deep_reef_df, rocky_df)

full_df_2010 <- full_df %>% filter(as.numeric(year)>=2010) %>%
  group_by(year)%>%
  dplyr::mutate(ymean=mean(dissim))

dissim_2010 <- full_df_2010 %>%
  ggplot(aes(x=as.numeric(year), y=dissim, color=group))+
  geom_point(alpha=0.4)+
  geom_line(alpha=0.4)+
  geom_smooth(aes(y=ymean), span=0.4, color='black')+
  #stat_summary(fun=mean, geom="line",colour="black", size=1)+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')+
  scale_x_continuous(breaks=2010:2020)+
  xlab("Year")+
  ylab("Dissimilarity")+
  theme_bw() +
  theme(axis.line = element_line(color='black'),
        plot.background = element_blank(),
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        text = element_text(size = 12))

#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "dissim_relative_to_2010.png"),
#       dissim_2010, height=4, width = 8, units = "in", 
#       dpi = 600, bg="white")






# dissimilarity relative to 2010 by MPA type -----------------------------------

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


set.seed(1985)

#CCFRP
CCFRP_mean_dist <- meandist(CCFRP_distmat, 
                            grouping = CCFRP_group_vars2$desig_state_year) #calculate mean dissim


CCFRP_mean_dist3 <- melt(CCFRP_mean_dist) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="CCFRP") %>%
  filter(year_1 == '2010' & year_2 >=2011 & MPA_1==MPA_2) %>% #extract dissimilarities relative to 2010
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, dissim = value)


#kelp_swath 
kelp_invalg_dist <- meandist(kelp_invalg_distmat, grouping = kelp_invalg_group_vars2$desig_state_year)

kelp_invalg_mean_dist3 <- melt(kelp_invalg_dist) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="kelp inverts and algae") %>%
  filter(year_1 == '2010' & year_2 >=2011 & MPA_1==MPA_2) %>% #extract dissimilarities relative to 2010
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, dissim = value)


#kelp_fish
kelp_fish_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars2$desig_state_year)

kelp_fish_mean_dist3 <- melt(kelp_fish_dist) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="kelp fish") %>%
  filter(year_1 == '2010' & year_2 >=2011 & MPA_1==MPA_2) %>% #extract dissimilarities relative to 2010
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, dissim = value)

#deep reef
deep_reef_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars2$desig_state_year)

deep_reef_mean_dist3 <- melt(deep_reef_dist) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="deep reef") %>%
  filter(year_1 == '2009' & year_2 >=2011 & MPA_1==MPA_2) %>% #extract dissimilarities relative to 2010
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, dissim = value)

#rocky
rocky_dist <- meandist(rocky_distmat, grouping = rocky_group_vars2$desig_state_year)

rocky_mean_dist3 <- melt(rocky_dist) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="rocky intertidal") %>%
  filter(year_1 == '2010' & year_2 >=2011 & MPA_1==MPA_2) %>% #extract dissimilarities relative to 2010
  #distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_2, MPA = MPA_2, dissim = value)


#Join

full_df <- rbind(CCFRP_mean_dist3, kelp_invalg_mean_dist3, 
                 kelp_fish_mean_dist3, deep_reef_mean_dist3, rocky_mean_dist3)


#sub <- CCFRP_mean_dist3 %>% filter(year>=2014 & year <=2017 & MPA=="ref")
#sub2 <- CCFRP_mean_dist3 %>% filter(year>=2014 & year <=2017 & MPA=="smr")

CCFRP_stabil <- 
  CCFRP_mean_dist3 %>%
  ggplot(aes(x = as.numeric(year), y = dissim, color = MPA))+
  geom_point()+
  geom_smooth(span=0.4)+
  scale_color_manual(values=c("#619CFF","#F8766D"))+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  xlab("")+
  ylab("")+
  ggtitle("CCFRP")+
  theme_bw(base_size=8)+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')
#stat_poly_line(linetype="dashed") +
#stat_poly_eq(aes(label = paste(after_stat(eq.label),
# after_stat(rr.label), sep = "*\", \"*")))
#stat_poly_line(linetype="dashed", data=sub, color="black")+
#stat_poly_line(linetype="dashed", data=sub2, color="black")

kelp_fish_stabil <- 
  kelp_fish_mean_dist3 %>%
  ggplot(aes(x = as.numeric(year), y = dissim, color = MPA))+
  geom_point()+
  geom_smooth(span=0.4)+
  scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
              axis.text.x = element_text(size=10),
              axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("kelp forest fish")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')

deep_reef_stabil <- 
  deep_reef_mean_dist3 %>%
  ggplot(aes(x = as.numeric(year), y = dissim, color = MPA))+
  geom_point()+
  geom_smooth(span=0.4)+
  scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("deep reef")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')

kelp_invalg_stabil <- 
  kelp_invalg_mean_dist3 %>%
  ggplot(aes(x = as.numeric(year), y = dissim, color = MPA))+
  geom_point()+
  geom_smooth(span=0.4)+
  scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("kelp inverts and algae")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')

rocky_stabil <- 
  rocky_mean_dist3 %>%
  ggplot(aes(x = as.numeric(year), y = dissim, color = MPA))+
  geom_point()+
  geom_smooth(span=0.4)+
  scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("rocky intertidal")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')



#create dummy legend as last panel
year <- c(2013, 2016, 2020)
height <- c(1,2,3)
MPA <- c('ref','smr','ref')
legend <- data.frame(year, height, MPA)

p6 <- ggplot(legend, aes(x = year, y = height, color = MPA))+
  geom_point()+
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size =  8),
        legend.title = element_text(size = 10, face = "bold"))+
  guides(colour = guide_legend(override.aes = list(size=6))
  )+
  scale_color_manual(values=c("#619CFF","#F8766D"))

p6$labels$colour <- "MPA type"  

stability_fig <- ggarrange(CCFRP_stabil, kelp_fish_stabil, 
                        deep_reef_stabil, kelp_invalg_stabil, rocky_stabil,
                        p6,
                        align='h'
) 

stability_fig2<- annotate_figure(stability_fig,
                              bottom = text_grob("Year", color = "black",
                                                 size = 12, vjust=-1),
                              left = text_grob("Stability (dissimilarity relative to 2010)", rot=90, size=12, vjust=2)
)


#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "stability_relative_to_2010.png"),
#       stability_fig2, height=6, width = 8, units = "in", 
#      dpi = 600, bg="white")






# Annual dissimilarity between REF and SMR  ------------------------------------

#CCFRP
CCFRP_mean_dist2 <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars2$desig_state_year) #calculate mean dissim

CCFRP_mean_dist3 <- melt(CCFRP_mean_dist2) %>% #melt to three column columns
                    mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
                            MPA_2 = gsub( " .*$", "", X2),
                            year_1 = str_sub(X1, -4),
                            year_2 = str_sub(X2, -4),
                            group="CCFRP") %>%
                    filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
                    distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
                    select(group, year = year_1, dissim = value)

#kelp invalg
kelp_invalg_mean_dist2 <- meandist(kelp_invalg_distmat, grouping = kelp_invalg_group_vars2$desig_state_year) #calculate mean dissim

kelp_invalg_mean_dist3 <- melt(kelp_invalg_mean_dist2) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="kelp inverts and algae") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)



#kelp_fish
kelp_fish_mean_dist2 <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars2$desig_state_year) #calculate mean dissim

kelp_fish_mean_dist3 <- melt(kelp_fish_mean_dist2) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="kelp forest fish") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)


#deep reef
deep_reef_mean_dist2 <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars2$desig_state_year) #calculate mean dissim

deep_reef_dist3 <- melt(deep_reef_mean_dist2) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="deep reef") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)


#rocky
rocky_mean_dist2 <- meandist(rocky_distmat, grouping = rocky_group_vars2$desig_state_year) #calculate mean dissim

rocky_mean_dist3 <- melt(rocky_mean_dist2) %>% #melt to three column columns
  mutate(MPA_1 = gsub( " .*$", "", X1), #create selection vars
         MPA_2 = gsub( " .*$", "", X2),
         year_1 = str_sub(X1, -4),
         year_2 = str_sub(X2, -4),
         group="rocky intertidal") %>%
  filter(year_1 == year_2 & MPA_1 !=MPA_2) %>% #drop matches
  distinct(year_2, .keep_all=TRUE)%>% #grab distinct disimilarities
  select(group, year = year_1, dissim = value)



#Join


full_df <- rbind(CCFRP_mean_dist3, kelp_invalg_mean_dist3, 
                 kelp_fish_mean_dist3, deep_reef_dist3, rocky_mean_dist3)



CCFRP_stabil <- 
  CCFRP_mean_dist3 %>%
  filter(year>=2010)%>%
  ggplot(aes(x = as.numeric(year), y = dissim))+
  geom_point()+
  geom_smooth(span=0.4, color='#5B5B5B')+
  #scale_color_manual(values=c("#619CFF","#F8766D"))+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  xlab("")+
  ylab("")+
  ggtitle("CCFRP")+
  theme_bw(base_size=8)+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')
#stat_poly_line(linetype="dashed") +
#stat_poly_eq(aes(label = paste(after_stat(eq.label),
# after_stat(rr.label), sep = "*\", \"*")))
#stat_poly_line(linetype="dashed", data=sub, color="black")+
#stat_poly_line(linetype="dashed", data=sub2, color="black")

kelp_fish_stabil <- 
  kelp_fish_mean_dist3 %>%
  filter(year>=2010)%>%
  ggplot(aes(x = as.numeric(year), y = dissim))+
  geom_point()+
  geom_smooth(span=0.4, color='#5B5B5B')+
  #scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("kelp forest fish")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')

deep_reef_stabil <- 
  deep_reef_dist3 %>%
  filter(year>=2010)%>%
  ggplot(aes(x = as.numeric(year), y = dissim))+
  geom_point()+
  geom_smooth(span=0.4, color='#5B5B5B')+
  #scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("deep reef")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')

kelp_invalg_stabil <- 
  kelp_invalg_mean_dist3 %>%
  filter(year>=2010)%>%
  ggplot(aes(x = as.numeric(year), y = dissim))+
  geom_point()+
  geom_smooth(span=0.4, color='#5B5B5B')+
  #scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("kelp inverts and algae")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')

rocky_stabil <- 
  rocky_mean_dist3 %>%
  filter(year>=2010)%>%
  ggplot(aes(x = as.numeric(year), y = dissim))+
  geom_point()+
  geom_smooth(span=0.4, color='#5B5B5B')+
  #scale_color_manual(values=c("#619CFF","#F8766D"))+
  theme_bw(base_size=8)+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none",
        axis.text.x = element_text(size=10),
        axis.text.y = element_text(size=10))+
  xlab("")+
  ylab("")+
  ggtitle("rocky intertidal")+
  annotate("rect", xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf,
           alpha = .15, fill='red')



stability_fig <- ggarrange(CCFRP_stabil, kelp_fish_stabil, 
                           deep_reef_stabil, kelp_invalg_stabil, rocky_stabil,
                          align='h'
) 

stability_fig2<- annotate_figure(stability_fig,
                                 bottom = text_grob("Year", color = "black",
                                                    size = 12, vjust=-1),
                                 left = text_grob("Annual MPA dissimilarity", rot=90, size=12, vjust=2)
)


#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "stability_relative_to_2010.png"),
#       stability_fig2, height=6, width = 8, units = "in", 
#      dpi = 600, bg="white")













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








# Explore distance between ref and smr by year ----------------------------
#Question: did communities inside and outside of MPAs before more distant after 
#the MHW/=?


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
        mutate(year_1 = gsub( " .*$", "", centroid_1 ),
               year_2 = gsub( " .*$", "", centroid_2),
               group="CCFRP")%>%
        filter(year_1 == year_2)
        
kelp_swath_mpa <- cenfun3(kelp_swath_group_vars2, kelp_swath_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group = "kelp_swath")%>%
  filter(year_1 == year_2)

kelp_upc_mpa <- cenfun3(kelp_upc_group_vars2, kelp_upc_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group="kelp_upc")%>%
  filter(year_1 == year_2)

kelp_invalg_mpa <- cenfun3(kelp_invalg_group_vars2, kelp_invalg_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group="kelp_upc")%>%
  filter(year_1 == year_2)

kelp_fish_mpa <- cenfun3(kelp_fish_group_vars2, kelp_fish_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group='kelp_fish')%>%
  filter(year_1 == year_2)

deep_reef_mpa <- cenfun3(deep_reef_group_vars2, deep_reef_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group="deep_reef")%>%
  filter(year_1 == year_2)

rocky_mpa <- cenfun3(rocky_group_vars2, rocky_distmat) %>%
  mutate(year_1 = gsub( " .*$", "", centroid_1 ),
         year_2 = gsub( " .*$", "", centroid_2),
         group="rocky_mpa")%>%
  filter(year_1 == year_2)

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









# SIMPER tables Before & after heatwave (no consideration of MPAs)--------------


sim_CCFRP <- with(CCFRP_group_vars2, simper(CCFRP_ord_data, desig_state), ordered=TRUE)
sim_kelp_swath <- with(kelp_swath_group_vars, simper(kelp_swath_ord_data, MHW))
sim_kelp_upc <- with(kelp_upc_group_vars, simper(kelp_upc_ord_data, MHW))
sim_kelp_fish <- with(kelp_fish_group_vars, simper(kelp_fish_ord_data, MHW))
sim_deep_reef <- with(deep_reef_group_vars, simper(deep_reef_ord_data, MHW))
sim_rocky <- with(rocky_group_vars, simper(rocky_ord_data, MHW))






# Cluster analysis --------------------------------------------------------

#Filter 2007 and beyond
CCFRP_07_dist <- meandist(CCFRP_distmat, grouping = CCFRP_group_vars$year)[1:14,1:14]
kelp_invalg_07_dist <- meandist(kelp_invalg_distmat, grouping = kelp_invalg_group_vars$year)[1:14,1:14]
kelp_fish_07_dist <- meandist(kelp_fish_distmat, grouping = kelp_fish_group_vars$year)[1:14,1:14]
deep_reef_07_dist <- meandist(deep_reef_distmat, grouping = deep_reef_group_vars$year)[2:8,2:8]
rocky_07_dist <- meandist(rocky_distmat, grouping = rocky_group_vars$year)[1:14,1:14]

#rocky_dist_un2 <- meandist(rocky_dist_un, grouping = rocky_group_vars$year)[1:14,1:14]

#run cluster analysis and format for plotting
ccfrp_clust <- hclust(as.dist(CCFRP_07_dist),
                                  method="average")
kelp_invalg_clust <- hclust(as.dist(kelp_invalg_07_dist),
                                  method="average")
kelp_fish_clust <- hclust(as.dist(kelp_fish_07_dist),
                                  method="average")
deep_reef_clust <- hclust(as.dist(deep_reef_07_dist),
                                  method="average")
rocky_clust <- hclust(as.dist(rocky_07_dist),
                            method="average")
plot(rocky_clust)

rocky_clust_un <- hclust(as.dist(rocky_dist_un2), method="average")

#helper function for k clustering
dendro_data_k <- function(hc, k) {
  
  hcdata    <-  ggdendro::dendro_data(hc, type = "rectangle")
  seg       <-  hcdata$segments
  labclust  <-  cutree(hc, k)[hc$order]
  segclust  <-  rep(0L, nrow(seg))
  heights   <-  sort(hc$height, decreasing = TRUE)
  height    <-  mean(c(heights[k], heights[k - 1L]), na.rm = TRUE)
  
  for (i in 1:k) {
    xi      <-  hcdata$labels$x[labclust == i]
    idx1    <-  seg$x    >= min(xi) & seg$x    <= max(xi)
    idx2    <-  seg$xend >= min(xi) & seg$xend <= max(xi)
    idx3    <-  seg$yend < height
    idx     <-  idx1 & idx2 & idx3
    segclust[idx] <- i
  }
  
  idx                    <-  which(segclust == 0L)
  segclust[idx]          <-  segclust[idx + 1L]
  hcdata$segments$clust  <-  segclust
  hcdata$segments$line   <-  as.integer(segclust < 1L)
  hcdata$labels$clust    <-  labclust
  
  hcdata
}


#define split tree
den_ccfrp <- dendro_data_k(ccfrp_clust, 2)
den_invalg <- dendro_data_k(kelp_invalg_clust, 2)
den_kelp_fish <- dendro_data_k(kelp_fish_clust, 2)
den_deep_reef <- dendro_data_k(deep_reef_clust, 2)
den_rocky <- dendro_data_k(rocky_clust, 2)

#clean up and plot

#CCFRP
ccfrp_seg <- segment(den_ccfrp) %>%
  mutate(yend = ifelse(yend < 0.01, 0.21, yend))

ccfrp_text <- label(den_ccfrp) %>%
  mutate(y = 0.21,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
ccfrp_text$MHW <- factor(ccfrp_text$MHW, levels=c('before', 'during', 'after'))

CCFRP <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=ccfrp_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=6)) +
  scale_y_continuous(limits = c(0.18, 0.4),
                     breaks = seq(0.2, 0.6, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 90, hjust = 1.1, vjust = 0.5,
            data = ccfrp_text,
            size=2,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("Rocky reef fishes")+
  theme(plot.title = element_text(size = 7, face = "bold"))


#kelp invalg
invalg_seg <- segment(den_invalg) %>%
  mutate(yend = ifelse(yend < 0.01, 0.4, yend))

invalg_text <- label(den_invalg) %>%
  mutate(y = 0.4,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
invalg_text$MHW <- factor(invalg_text$MHW, levels=c('before', 'during', 'after'))

invalg <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=invalg_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=6)) +
  scale_y_continuous(limits = c(0.38, 0.58),
                     breaks = seq(0.4, 0.6, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 90, hjust = 1.1, vjust = 0.5,
            data = invalg_text,
            size=2,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("Kelp invertebrates and algae")+
  theme(plot.title = element_text(size = 7, face = "bold"))




#kelp fish
fish_seg <- segment(den_kelp_fish) %>%
  mutate(yend = ifelse(yend < 0.01, 0.22, yend))

fish_text <- label(den_kelp_fish) %>%
  mutate(y = 0.22,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
fish_text$MHW <- factor(fish_text$MHW, levels=c('before', 'during', 'after'))

kelp_fish <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=fish_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=6)) +
  scale_y_continuous(limits = c(0.185, 0.50),
                     breaks = seq(0.2, 0.50, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 90, hjust = 1.1, vjust = 0.5,
            data = fish_text,
            size=2,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none") +
  ggtitle("Kelp forest fishes")+
  theme(plot.title = element_text(size = 7, face = "bold"))




#deep reef
deep_seg <- segment(den_deep_reef) %>%
  mutate(yend = ifelse(yend < 0.01, 0.227, yend))

deep_text <- label(den_deep_reef) %>%
  mutate(y = 0.217,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
deep_text$MHW <- factor(deep_text$MHW, levels=c('before', 'during', 'after'))

deep_reef <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=deep_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=6)) +
  scale_y_continuous(limits = c(0.18, 0.55),
                     breaks = seq(0.2, 0.55, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 90, hjust = 0.9, vjust = 0.5,
            data = deep_text,
            size=2,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("Deep reef fishes")+
  theme(plot.title = element_text(size = 7, face = "bold"))




#rocky
rocky_seg <- segment(den_rocky) %>%
  mutate(yend = ifelse(yend < 0.01, 0.295, yend))

rocky_text <- label(den_rocky) %>%
  mutate(y = 0.295,
         MHW = ifelse(as.numeric(label) < 2014, "before",
                      ifelse(as.numeric(label) > 2016, "after","during")))
rocky_text$MHW <- factor(rocky_text$MHW, levels=c('before', 'during', 'after'))

rocky <- ggplot() + 
  geom_segment(aes(x = x,
                   y = y,
                   xend = xend,
                   yend = yend,
                   color = as.factor(clust)),
               #color = as.factor("clust"),
               show.legend = FALSE,
               data=rocky_seg)  +
  theme_dendro() + 
  theme(axis.line.y = element_line(),
        axis.ticks.y = element_line(),
        axis.text.y = element_text(size=6)) +
  scale_y_continuous(limits = c(0.28, 0.45),
                     breaks = seq(0.3, 0.45, by = 0.05)) +
  geom_text(aes(x = x,
                y = y,
                label = label,
                color = MHW),
            angle = 90, hjust = 1.1, vjust = 0.5,
            data = rocky_text,
            size=2,
            key_glyph = "point")+
  scale_color_manual(values=c('#C0C0C0', '#444444','#4c78b5','#44b89d','#f56969'))+
  labs(color='heatwave period')+
  guides(color="none")+
  ggtitle("Rocky intertidal")+
  theme(plot.title = element_text(size = 7, face = "bold"))



library(ggpubr)

#create dummy legend as last panel

year <- c(2013, 2016, 2020)
height <- c(1,2,3)
heatwave_period <- c('Before (2007-2013)','During (2014-2016)','After (2017-2020)')
legend <- data.frame(year, height, heatwave_period)
legend$heatwave_period <- factor(legend$heatwave_period, levels=c('Before (2007-2013)','During (2014-2016)','After (2017-2020)')) 


p3 <- ggplot(legend, aes(x = year, y = height, color = heatwave_period))+
  geom_point()+
  lims(x = c(0,0), y = c(0,0))+
  theme_void()+
  theme(legend.position = c(0.5,0.5),
        legend.key.size = unit(0.5, "cm"),
        legend.text = element_text(size =  6),
        legend.title = element_text(size = 8, face = "bold"))+
  guides(colour = guide_legend(override.aes = list(size=4))
         )+
  scale_color_manual(values=c('#44b89d','#f56969','#4c78b5'))

p3$labels$colour <- "Heatwave period"  

dendro_fig <- ggarrange(rocky, invalg, kelp_fish, CCFRP, deep_reef, 
          p3,
          align='h'
          ) #labels=c('A', 'B','C','D', 'E'),
          #common.legend = T)

dendro_fig2<- annotate_figure(dendro_fig,
              
                bottom = text_grob("Year", color = "black",
                               size = 8, vjust=-2),
                left = text_grob("Dissimilarity", rot=90, size=8)
)


ggsave(here::here("analyses", "5community_climate_ecology", "figures", "dendro_cluster3.png"), 
       dendro_fig2, width=6,height=5, bg="white", dpi = 600, units = "in")




















#extra functions not needed above



set_labels_params <- function(nbLabels,
                              direction = c("tb", "bt", "lr", "rl"),
                              fan       = FALSE) {
  if (fan) {
    angle       <-  360 / nbLabels * 1:nbLabels + 90
    idx         <-  angle >= 90 & angle <= 270
    angle[idx]  <-  angle[idx] + 180
    hjust       <-  rep(0, nbLabels)
    hjust[idx]  <-  1
  } else {
    angle       <-  rep(0, nbLabels)
    hjust       <-  0
    if (direction %in% c("tb", "bt")) { angle <- angle + 45 }
    if (direction %in% c("tb", "rl")) { hjust <- 1 }
  }
  list(angle = angle, hjust = hjust, vjust = 0.5)
}
plot_ggdendro <- function(hcdata,
                          direction   = c("lr", "rl", "tb", "bt"),
                          fan         = FALSE,
                          scale.color = NULL,
                          branch.size = 1,
                          label.size  = 3,
                          nudge.label = 0.01,
                          expand.y    = 0.1) {
  
  direction <- match.arg(direction) # if fan = FALSE
  ybreaks   <-pretty(segment(hcdata)$y, n = 5)
  ymax      <- max(segment(hcdata)$y)
  
  ## branches
  p <- ggplot() +
    geom_segment(data         =  segment(hcdata),
                 aes(x        =  x,
                     y        =  y,
                     xend     =  xend,
                     yend     =  yend,
                     linetype =  factor(line),
                     colour   =  factor(clust)),
                 lineend      =  "round",
                 show.legend  =  FALSE,
                 size         =  branch.size)
  
  ## orientation
  if (fan) {
    p <- p +
      coord_polar(direction = -1) +
      scale_x_continuous(breaks = NULL,
                         limits = c(0, nrow(label(hcdata)))) +
      scale_y_reverse(breaks = ybreaks)
  } else {
    p <- p + scale_x_continuous(breaks = NULL)
    if (direction %in% c("rl", "lr")) {
      p <- p + coord_flip()
    }
    if (direction %in% c("bt", "lr")) {
      p <- p + scale_y_reverse(breaks = ybreaks)
    } else {
      p <- p + scale_y_continuous(breaks = ybreaks)
      nudge.label <- -(nudge.label)
    }
  }
  
  # labels
  labelParams <- set_labels_params(nrow(hcdata$labels), direction, fan)
  hcdata$labels$angle <- labelParams$angle
  
  p <- p +
    geom_text(data        =  label(hcdata),
              aes(x       =  x,
                  y       =  y,
                  label   =  label,
                  colour  =  factor(clust),
                  angle   =  angle),
              vjust       =  labelParams$vjust,
              hjust       =  labelParams$hjust,
              nudge_y     =  ymax * nudge.label,
              size        =  label.size,
              show.legend =  FALSE)
  
  # colors and limits
  if (!is.null(scale.color)) {
    p <- p + scale_color_manual(values = scale.color)
  }
  
  ylim <- -round(ymax * expand.y, 1)
  p    <- p + expand_limits(y = ylim)
  
  p
}
