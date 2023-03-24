
rm(list=ls())


require(tidyverse)
require(vegan)
require(reshape2)


#set data dir
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_rocky-intertidal/Community analysis"
input_file <- "intertidal_site_counts.csv"

#load data
rocky_counts_raw <- read.csv(file.path(data_path, input_file)) %>%
  janitor::clean_names()%>%
  #assign sites as MPA
  mutate(mpa_designation = ifelse(mpa_designation=="NONE","REF","MPA"),
         #create grouping variable
         identifier = paste(site, mpa_designation, year))%>%
  dplyr::select(identifier, everything())%>%
  #filter central coast only for 2004 and beyond
  filter(region3 == "central") %>%
  #drop inanimate 
  dplyr::select(!(c(rock, sand, tar))) 



################################################################################
#step 3--- define grouping variables and data for matrix

#define data for matrix
rocky_mat_data <- rocky_counts_raw %>% ungroup() %>% dplyr::select(identifier, 9:ncol(.))

#convert group var to row name
rownames(rocky_mat_data) <- rocky_mat_data[,1]
rocky_mat_data <- rocky_mat_data[,-1]

rocky_group_vars <- rocky_counts_raw %>% dplyr::select(identifier)

################################################################################
#step 4--- create dissim matrix using Bray

bc_mat <- as.matrix(vegan::vegdist(rocky_mat_data, method="bray"))

#join group vars
dist_dat <- cbind(rocky_group_vars, bc_mat)

#create header names to match square matrix
colnames(dist_dat)[2:ncol(dist_dat)] <- dist_dat[,1]

#convert to three column format
dist_long <- setNames(melt(dist_dat), c('site_MPA_year1', 'site_MPA_year2', 'dissim')) 


dist_long1 <- dist_long %>% 
  #bust apart the grouping var
  mutate(site_1 = word(site_MPA_year1,1,-2),
         site_2 = word(site_MPA_year2,1,-2),
         year_1 = word(site_MPA_year1,-1),
         year_2 = word(site_MPA_year2,-1),
         MPA_1 = word(site_MPA_year1,-2),
         MPA_2 = word(site_MPA_year2,-2)) %>%
  #filter site-by-site comparisons only
  filter(site_1==site_2) %>%
  #select baseline period
  filter(year_1 >= 2004 & year_1 <= 2006)%>%
  #make year numeric for plotting
  mutate(year_x = as.numeric(year_2))%>%
  #plot starts at 2006
  filter(year_x >= 2006) %>%
  dplyr::select(MPA_2, year_1, year_x, dissim)%>%
  #convert dissim to sim
  mutate(sim = 1-dissim)%>%
  #drop duplicates
  distinct()


################################################################################
#plot

ggplot(dist_long1, aes(x=year_x, y=sim, group=MPA_2, color=MPA_2))+
  stat_summary(fun = mean, geom = "line", aes(group = MPA_2))+
  stat_summary(fun.data = mean_se, geom = "errorbar", aes(group=MPA_2))+
  theme_classic()+
  labs(x = "Year", y="Stability (relative to 2004-2006)")



#