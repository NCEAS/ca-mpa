
rm(list=ls())


require(tidyverse)
require(vegan)

#set data dir
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_rocky-intertidal/Community analysis"
input_file <- "intertidal_site_counts.csv"

#load data
rocky_counts_raw <- read.csv(file.path(data_path, input_file)) %>%
  janitor::clean_names()%>%
  #assign sites as MPA or REF
  mutate(mpa_designation = ifelse(mpa_designation=="NONE","REF",mpa_designation))%>%
  #filter central coast only for 2004 and beyond
  filter(region3 == "central",
         year >= 2004) %>%
  #drop inanimate 
  dplyr::select(!(c(rock, sand, tar)))


#step 1 --- Set 2004-2006 baseline year by assigning 2006 as dummy var for that period
#original sites remain rows in the dataframe
baseline_dat <- rocky_counts_raw %>%
  #create dummy year for sites surveyed 2004-2006
  mutate(year = ifelse(year >= 2004 & year <2006, 2006, year))




################################################################################
#Stability for SMRs only

rocky_counts_MPA <- baseline_dat %>% 
  #recode SMCAs and SMRs as 'MPA'
  mutate(mpa_designation = recode(mpa_designation, "SMR" = "MPA",
                                  "SMCA" = "MPA"))%>%
  #drop special closures
  filter(mpa_designation == "MPA" | mpa_designation == 'REF') %>%
  #create grouping variable for distance matrix
  mutate(mpa_year = paste(mpa_designation, year))%>%
  select(mpa_designation, everything()) 

#define group var as mpa_year
rocky_group_vars <- rocky_counts_SMR %>% dplyr::select(2)

#define data for matrix
rocky_mat_data <- rocky_counts_SMR %>% ungroup() %>% dplyr::select(9:ncol(.))

#generate a BC dissim matrix 
rocky_distmat <- vegan::vegdist(rocky_mat_data, method = "bray", na.rm=T)

#dissimilarity relative to 2004-2006
rocky_dist <- meandist(rocky_distmat, grouping = rocky_group_vars$mpa_year) #calcualte mean dissimilarity for grouping var (mpa_type, year)

#select distances for MPAs relative to 2006 -- 2006 is first row in df
rocky_diag_MPA <- rocky_dist[1,1:15]
rocky_df_MPA <- data.frame(Year = row.names(rocky_dist[1:15,]), rocky_diag_ref)

#select distances for REFs relative to 2006 -- 2006 is row 16 for MPAs in df
rocky_diag_REF <- rocky_dist[16,16:30]
rocky_df_REF <- data.frame(Year = row.names(rocky_dist[16:30,]), rocky_diag_smr)
colnames(rocky_df_REF) <- c('Year','rocky_diag_ref')

#combine into single dataframe
rocky_mat <- rbind(rocky_df_MPA, rocky_df_REF) %>%
  rename(group = Year,
         dissimilarity = rocky_diag_ref) %>%
  #convert dissim to sim
  mutate(sim = 1-dissimilarity,
         MPA_type = word(group,1),
         Year = as.numeric(word(group, -1)))


#plot
ggplot(rocky_mat, aes(x = Year, y=sim, group=MPA_type, color=MPA_type))+
  geom_point()+
  geom_line()











