
rm(list=ls())

################################################################################
#load raw data
data_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_ccfrp/CCFRP_derived_data_tables_DataONE"
input_file <- "CCFRP_derived_effort_table.csv" 
CPUE_raw <- read.csv(file.path(data_path, input_file))

#reshape and clean up
CCFRP_CPUE <- CPUE_raw %>%
  mutate(affiliated_mpa = paste(Area,"smr"), #all ccfrp MPAs are defacto SMRs per PI
         mpa_designation = ifelse(MPA_Status == "MPA","smr", MPA_Status))

CCFRP_CPUE$mpa_designation <- tolower(CCFRP_CPUE$mpa_designation)
CCFRP_CPUE$mpa_designation <- tolower(CCFRP_CPUE$mpa_designation)

CCFRP_CPUE1 <- CCFRP_CPUE %>%
  dplyr::select(year=Year, affiliated_mpa, mpa_designation, Grid_Cell_ID, 
                Common_Name, CPUE_catch_per_angler_hour)%>%
  mutate(mpa_class="smr", 
         group="ccfrp") %>%
  group_by(group, year, affiliated_mpa, mpa_class, mpa_designation, 
           Grid_Cell_ID, Common_Name)%>%
  dplyr::summarize(CPUE_mean = mean(CPUE_catch_per_angler_hour))%>%  #some species in the raw data have duplicate entries.
  ungroup() %>%
  tidyr::pivot_wider(names_from = Common_Name, 
                     values_from = CPUE_mean) %>%
  janitor::clean_names()

CCFRP_CPUE1$affiliated_mpa <- tolower(CCFRP_CPUE1$affiliated_mpa)

CCFRP_CPUE1$affiliated_mpa <- recode_factor(CCFRP_CPUE1$affiliated_mpa, 
                                            "southeast farallon islands smr" =
                                              "southeast farallon island smr") #clean up names

CCFRP_CPUE1$affiliated_mpa <- recode_factor(CCFRP_CPUE1$affiliated_mpa,
                                            "swamis smr" = "swami's smca") #clean up names 



################################################################################
#drop species and combine to higher taxa
#### see reference taxonomy 
#table https://docs.google.com/spreadsheets/d/1vxy0XVOrlNhXD-i9tWL_F9G5h8S3S4OV/edit#gid=2031917236

CCFRP_combined <- CCFRP_CPUE1 %>%
  #merge species
  mutate(blue_rockfish_merge = rowSums(select(.,'blue_rockfish','deacon_rockfish')),
         mackerel_family_merge = rowSums(select(., 'mackerel_family_scombridae','pacific_chub_mackerel','jack_mackerel')),
         sanddabs_merge = rowSums(select(., 'pacific_sanddab','sanddab_spp','speckled_sanddab')))%>%
  select(!(c('blue_rockfish','deacon_rockfish','mackerel_family_scombridae','pacific_chub_mackerel',
             'pacific_sanddab','sanddab_spp','speckled_sanddab','jack_mackerel'))) %>%
  #drop species
  select(!(c('northern_anchovy',
             'olive_or_yellowtail_rockfish',
             'pacific_sardine',
             'petrale_sole',
             'pile_perch',
             'rubberlip_seaperch',
             'sand_sole',
             'spiny_dogfish',
             'senorita',
             'starry_skate',
             'tubesnout',
             'silversides_family_atherinopsidae',
             'un_id_blue_rockfish',
             'unknown',
             'unknown_rockfish',
             'wolf_eel',
             'yelloweye_rockfish',
             'bull_sculpin',
             'pacific_barracuda',
             'total')))%>%
  #drop string
  rename(blue_rockfish = blue_rockfish_merge,
         mackerel_family = mackerel_family_merge,
         sanddabs = sanddabs_merge)


#add regions
data_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
four_region <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

regions <- four_region %>%
  dplyr::select(name, region3=bioregion, region4 = four_region_north_ci)

CCFRP_CPUE <- left_join(CCFRP_combined, regions, by=c("affiliated_mpa"="name"))

#clean up

CCFRP_CPUE <- CCFRP_CPUE %>%
  mutate(MHW = ifelse(year>=2014 & year<=2016, "during",ifelse(year<2014, "before","after")))%>%
  dplyr::select(year, group, region3, region4, MHW, everything())

#drop species that were never observed on the central coast
CCFRP_zero_drop <- CCFRP_CPUE %>% filter(region3=='central') %>%
  select(!(where(~ any(. != 0))))

CCFRP_cen <- CCFRP_CPUE %>% filter(region3=='central') %>%
  select(where(~ any(. != 0)))

#Export
#path_aurora <- "/home/shares/ca-mpa/data/sync-data/processed_data/ecological_community_data" 
#write.csv(CCFRP_CPUE,file.path(path_aurora, "CCFRP_species_counts.csv"), row.names = FALSE)

CCFRP_mpa_year <- CCFRP_cen %>%
  pivot_longer(cols=10:46, names_to="species",values_to="CPUE")%>%
  group_by(year, group, region3, region4, MHW, affiliated_mpa, mpa_class, mpa_designation, species)%>%
  dplyr::summarize(mean_CPUE = mean(CPUE))%>%
  ungroup()%>%
  pivot_wider(names_from = "species", values_from = "mean_CPUE")



################################################################################

#calculate alpha diversity

CCFRP_dat <- CCFRP_mpa_year[,9:45]
CCFRP_group_vars <- CCFRP_mpa_year[,1:8]

#CCFRP

ccfrp_richness <- data.frame(S.obs = apply(CCFRP_dat>0, 1, sum))
ccfrp_evenness <- diversity(CCFRP_dat)/log(specnumber(CCFRP_dat))
ccfrp_shannon <- diversity(CCFRP_dat, index="shannon")
ccfrp_abund <- rowSums(CCFRP_dat)

ccfrp_alphadiv <- cbind(CCFRP_group_vars, ccfrp_richness, ccfrp_shannon, ccfrp_evenness, ccfrp_abund)%>%
  mutate(MHW = ifelse(year < 2014,"before",
                      ifelse(year>2016,"after","during")))


#Plot

my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=10),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=7, face="bold"),
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
                   plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)


color_set <- c("smr" = "#EB6977","ref" = "#13A0DD")


s1 <-ggplot(ccfrp_alphadiv, aes(x = mpa_designation, y=ccfrp_shannon))+
  geom_boxplot(aes(x=MHW, fill=mpa_designation), show.legend = FALSE)+
  #facet_wrap(~MHW)+
  theme_bw()+my_theme+
  labs(x="", 
       y="Shannon", 
       title="Rock reef fish")+
  scale_fill_manual(values = color_set)









