###

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
require(tidyverse)
require(sf)
require(ggplot2)

# Directories
plotdir <- "analyses/2performance_fisheries/analyses/dive_logbooks/output/figures" 
outdir <- "analyses/2performance_fisheries/analyses/dive_logbooks/output"
shp_dir <- "/Users/Joshua/Documents/Research/Postdoc/NCEAS/Project files/Data/spatial_dat/"

# Read logbook data
data_orig <- readRDS("/Users/Joshua/Documents/Research/Postdoc/NCEAS/Project files/GitHub/ca-mpa/analyses/2performance_fisheries/analyses/dive_logbooks/output/data/CDFW_2000_2020_dive_logbooks_processed.Rds")

#read spatial dat
#read spatial data
island_buff <- st_read(file.path(shp_dir,"islands_1nm_poly/wk478kp5122.shp"))%>%
  mutate(land_name = ifelse(id=="0","San Miguel",
                            ifelse(id=="1","Santa Rosa",
                                   ifelse(id=="2", "Santa Cruz",
                                          ifelse(id=="3", "Anacapa",
                                                 ifelse(id=="4", "San Nicolas",
                                                        ifelse(id=="5", "Santa Barbara",
                                                               ifelse(id=="6", "Santa Catalina",
                                                                      ifelse(id=="7", "San Clemente","")))))))))

CA_buff <- st_read(file.path(shp_dir,"CA_1nm_poly/jf154jt0733.shp"))%>%
  mutate(land= "Mainland",
         id="8")%>%
  dplyr::select(id, bufferdist="dist1nmi_i",geometry,land_name='land')%>%
  relocate(id, bufferdist,geometry,land_name)

land_poly <- rbind(island_buff, CA_buff)


# MPAs
mpas <- wcfish::mpas_ca %>% 
  sf::st_as_sf() %>% 
  filter(type!="SMP")

#buid data
################################################################################

#Step 1 - calculate CPUE as catch pound per hour

data <- data_orig %>%
        mutate(cpue = catch_lbs / hours)

#Step 2 - filter only reliable data

data2 <- data %>% filter(reliable_yn == "yes")

data3 <- do.call(data.frame,                      # Replace Inf in data by NA
                 lapply(data2,
                        function(x) replace(x, is.infinite(x), NA))) %>%
  sf::st_as_sf(coords=c("long_dd", "lat_dd"), crs=sf::st_crs(mpas), remove=F)

my_theme <-  theme(axis.text=element_text(size=6),
                   axis.title=element_text(size=8),
                   legend.text=element_text(size=6),
                   legend.title=element_text(size=8),
                   strip.text=element_text(size=8),
                   plot.title=element_text(size=10),
                   # Gridlines
                   panel.border = element_blank(),
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.background = element_rect(fill=alpha('blue', 0)))

#Step 3 - determine no. of MPAs for each island

mpa_assign <- st_join(land_poly, mpas)%>%
              filter(region=='SCSR')%>%
              select(land_name, mpa_name='name',type)

sc <- land_poly %>% filter(land_name=='San Clemente' | land_name == "San Nicolas") %>% mutate(n_mpa = 0)%>%
              select(land_name, n_mpa, geometry)
n_mpas <- mpa_assign %>%
          group_by(land_name, .drop=FALSE)%>%
          summarise(n_mpa = n())%>%
          rbind(sc)
  
fish_location1 <- st_join(data3, land_poly)
    

catch_summary <- fish_location1 %>%
                  group_by(land_name)%>%
                  summarize(n_dives = n(),
                            avg_cpue = mean(cpue, na.rm=TRUE),
                            sd_cpue = sd(cpue, na.rm=TRUE),
                            se_cpue = sd_cpue/sqrt(n_dives))%>%
                  mutate(total_dives = sum(n_dives),
                         proportion_dives = n_dives/total_dives*100)

catch_summary1 <- st_join(catch_summary, n_mpas) %>%
                  select(!(land_name.y),land_name='land_name.x')

################################################################################
# Examine percent fishing at MPAs BEFORE implementation -------------------


#examine percent hours inside vs outside
perc_time <- data2 %>%
             filter(mpa_period == "pre")%>%
             group_by(inside_pre)%>%
             summarize(total_hours = sum(hours, na.rm=TRUE))%>%
             mutate('dive time (hours)'= total_hours /sum(total_hours)*100)

perc_fleet <- data2 %>%
             filter(mpa_period == "pre")%>%
             distinct(inside_pre, fisher_id)%>%
             group_by(inside_pre)%>%
             summarize(perc = n())%>%
             mutate('fleet effort'= perc /sum(perc)*100)

perc_landings <- data2 %>%
            filter(mpa_period == "pre")%>%
            group_by(inside_pre)%>%
            summarize(total_landings = sum(catch_lbs, na.rm=TRUE))%>%
            mutate('total landings (pounds)'= total_landings/sum(total_landings)*100)
           
angler_hours <- data2 %>%
            filter(mpa_period == "pre")%>%
            group_by(inside_pre, fisher_id)%>%
            summarize(hours = sum(hours))


       
effort_data_pre <- left_join(perc_time, perc_fleet, by="inside_pre")
effort_data_pre1 <- left_join(effort_data_pre, perc_landings, by="inside_pre")%>%
                  pivot_longer(names_to="measure", cols=c('dive time (hours)',
                                                          'fleet effort',
                                                          'total landings (pounds)'))%>%
                  select('MPA'='inside_pre','measure','percent'='value')%>%
                  mutate(MPA = fct_recode(MPA, "inside pre-imp."="yes"),
                         MPA = fct_recode(MPA, "outside pre-imp."="no"))
  
effort_data_pre1$measure <- factor(effort_data_pre1$measure,
                                   level=c('total landings (pounds)',
                                           'dive time (hours)',
                                           'fleet effort'))

#plot
p <- effort_data_pre1%>%
     ggplot(aes(x=measure, y=percent, fill=MPA))+
     geom_bar(stat="identity", width=0.5, position=position_dodge(width=0.5))+
     geom_text(aes(label = paste0(round(percent),"%")),hjust=-0.2, position=position_dodge(width=0.5), size=2)+
     scale_fill_manual(values=c('#00BFC4','#F8766D'))+
     ylab("Percent of total")+
     xlab("Measure")+
     theme_bw()+
     coord_flip()+
     my_theme
p


#ggsave(p, filename=file.path(plotdir, "FigX_percent_effort.png"), 
#       width=6.5, height=4, units="in", dpi=600)



################################################################################
# Examine CPUE at MPAs BEFORE implementation -------------------


#clean up Inf and replace with NA
data3 <- do.call(data.frame,                      # Replace Inf in data by NA
                 lapply(data2,
                        function(x) replace(x, is.infinite(x), NA)))                                      # Print updated data frame

#claculate CPUE means pre- 
CPUE <- data3 %>%
  filter(mpa_period == "pre")%>%
  group_by(inside_pre)%>%
  dplyr::summarize(mean.cpue = mean(cpue, na.rm=TRUE),
                   n = n(),
                   stdv = sd(cpue, na.rm=TRUE),
                   se=stdv/sqrt(n),
                   ci=se * qt((1-0.05)/2 + .5, n-1))%>%
  mutate(inside_pre = fct_recode(inside_pre, "inside"="yes"),
                inside_pre = fct_recode(inside_pre, "outside"="no"))%>%
  rename('MPA'='inside_pre')


#plot
p <- CPUE%>%
  ggplot(aes(x=MPA, y=mean.cpue, fill=MPA))+
  geom_bar(stat="identity", width=0.5, position="dodge")+
  geom_errorbar(aes(ymin=mean.cpue-ci, ymax=mean.cpue+ci),
                position="dodge", width=0.2)+
  scale_fill_manual(values=c('#00BFC4','#F8766D'))+
  ylab("Average CPUE (pounds per hour)")+
  xlab("Fishing location (prior to MPA implementation)")+
  coord_flip()+
  theme_bw()
p


#ggsave(p, filename=file.path(plotdir, "FigX_cpue_pre_MPA.png"), 
#       width=6, height=4, units="in", dpi=600)



################################################################################
# Examine CPUE over time






# percent effort by location
################################################################################

plot_sum <- catch_summary1 %>%
            filter(!(land_name=='NA'),)%>%
            select(land_name, "mean cpue"=avg_cpue, se_cpue, "proportion dives"=proportion_dives, n_mpa)%>%
            pivot_longer(cols=c('mean cpue','proportion dives'))%>%
            mutate(se_cpue = ifelse(name =='proportion dives',NA,se_cpue ),
                   n_mpa = ifelse(name =='proportion dives',NA,n_mpa ))
            

effort_island <- plot_sum%>%
  ggplot(aes(x=forcats::fct_reorder2(land_name, name=='mean cpue', value, .desc = T),
             y=value, fill=name))+
  geom_bar(stat="identity", width=0.5, position=position_dodge(width=0.5))+
  geom_errorbar(aes(ymin=value-se_cpue, ymax=value+se_cpue),
                position=position_dodge(width = 0.5), width=0.2)+
  geom_text(aes(label = ifelse(is.na(n_mpa), "", paste0("nMPAs =",n_mpa))),
            hjust=-0.2, vjust=-2, position=position_dodge(width=0.5), size=2)+
  scale_fill_manual(values=c('#00BFC4','#F8766D'))+
  ylab("CPUE or proportion of dives")+
  xlab("Location")+
  guides(fill=guide_legend(title=""))+
  #coord_flip()+
  theme_bw()+
  my_theme


ggsave(effort_island, filename=file.path(plotdir, "FigX_cpue_effort_by_island_bar.png"), 
       width=7.5, height=6, units="in", dpi=600)









