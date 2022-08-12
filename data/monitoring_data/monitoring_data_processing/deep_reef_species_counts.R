


data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data/ecological_community_data/year_level"

input_file <- "deep_reef_mpa_year.csv" 
deep_reef_counts <- read.csv(file.path(data_path, input_file))



deep_reef <- deep_reef_counts %>%
  rowwise() %>%
  dplyr::mutate(sum = sum(across(8:ncol(.), na.rm = T))) %>%
  filter(!(sum==0))%>% #remove rows containing only zeros
  dplyr::select(-c(sum, schooling_10_15_cm_sebastes_sp,
                   schooling_10_15_cm_sebastes_sp,
                   young_of_year_10_cm_sebastes_sp,
                   synodus_lucioceps_or_ophiodon_elongatus,
                   sebastes_melanops_or_mystinus_or_diaconus))%>%
  mutate(desig_state = paste(mpa_defacto_designation,MHW))%>%
  dplyr::select(desig_state, everything())%>%
  #filter(mpa_defacto_designation=="smr" | mpa_defacto_designation=="ref")%>%
  #filter(MHW=='before'|MHW=='after')%>%#drop sum columns and non-species categories
  arrange(desig_state)


deep_reef_ag <- deep_reef %>%
                select(!(c(desig_state,region3, affiliated_mpa, mpa_defacto_class, mpa_defacto_designation, MHW)))%>%
                group_by(year, region4) %>% 
                summarise(across(everything(), list(sum)))

colClean <- function(x){ colnames(x) <- gsub("_1", "", colnames(x)); x }                 

deep_reef_cleaned<- colClean(deep_reef_ag)



#write.csv(deep_reef_cleaned,"/home/shares/ca-mpa/data/sync-data/processed_data/deep_reef_fish_counts.csv", row.names = FALSE)

                


