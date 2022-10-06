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



################################################################################
# Examine percent fishing at MPAs BEFORE implementation -------------------


#examine percent hours inside vs outside
perc_time <- data2 %>%
             filter(mpa_period == "pre")%>%
             group_by(inside_pre)%>%
             summarize(total_hours = sum(hours, na.rm=TRUE))%>%
             mutate('time (hours)'= total_hours /sum(total_hours)*100)

perc_fleet <- data2 %>%
             filter(mpa_period == "pre")%>%
             distinct(inside_pre, fisher_id)%>%
             group_by(inside_pre)%>%
             summarize(perc = n())%>%
             mutate('fleet'= perc /sum(perc)*100)

perc_landings <- data2 %>%
            filter(mpa_period == "pre")%>%
            group_by(inside_pre)%>%
            summarize(total_landings = sum(catch_lbs, na.rm=TRUE))%>%
            mutate('catch'= total_landings/sum(total_landings)*100)
             
       
effort_data_pre <- left_join(perc_time, perc_fleet, by="inside_pre")
effort_data_pre1 <- left_join(effort_data_pre, perc_landings, by="inside_pre")%>%
                  pivot_longer(names_to="measure", cols=c('time (hours)',
                                                          'fleet',
                                                          'catch'))%>%
                  select('MPA'='inside_pre','measure','percent'='value')%>%
                  mutate(MPA = fct_recode(MPA, "inside"="yes"),
                         MPA = fct_recode(MPA, "outside"="no"))
  
#plot
p <- effort_data_pre1%>%
     ggplot(aes(x=measure, y=percent, fill=MPA))+
     geom_bar(stat="identity", width=0.5, position="dodge")+
     scale_fill_manual(values=c('#00BFC4','#F8766D'))+
     ylab("Percent of total")+
     xlab("Measure")+
     coord_flip()+
     theme_bw()
p


#ggsave(p, filename=file.path(plotdir, "FigX_percent_effort.png"), 
#       width=6.5, height=3.7, units="in", dpi=600)



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



















