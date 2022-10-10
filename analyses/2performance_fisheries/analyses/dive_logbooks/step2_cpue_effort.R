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

# Read logbook data
data_orig <- readRDS("/Users/Joshua/Documents/Research/Postdoc/NCEAS/Project files/GitHub/ca-mpa/analyses/2performance_fisheries/analyses/dive_logbooks/output/data/CDFW_2000_2020_dive_logbooks_processed.Rds")


#Step 1 - calculate CPUE as catch pound per hour

data <- data_orig %>%
        mutate(cpue = catch_lbs / hours)

#Step 2 - filter only reliable data

data2 <- data %>% filter(reliable_yn == "yes")

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



















