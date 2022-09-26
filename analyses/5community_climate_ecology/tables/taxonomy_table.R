rm(list=ls())

require(tidyverse)

input_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/year_level_with_envr_vars"
export_path <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/ecological_community_data/cc_species_tables"

comm_data <- load(file.path(input_path, "comm_data.rda"))
group_vars <- load(file.path(input_path, "group_vars.rda"))


CCFRP_dat <- cbind(CCFRP_group_vars, CCFRP_ord_data)
kelp_invalg_dat <- cbind(kelp_invalg_group_vars, kelp_invalg_ord_data)
kelp_fish_dat <- cbind(kelp_fish_group_vars, kelp_fish_ord_data)
deep_reef_dat <- cbind(deep_reef_group_vars, deep_reef_ord_data)
rocky_dat <- cbind(rocky_group_vars, rocky_ord_data)


CCFRP_spp <- CCFRP_dat %>%
              group_by(year)%>%
              dplyr::summarise(across(where(is.numeric), list(mean), na.rm=TRUE))%>%
              data.frame()
CCFRP_t <- setNames(data.frame(t(CCFRP_spp[ , -1])), CCFRP_spp[ ,1])
row.names(CCFRP_t) <- sub("\\_1", "", row.names(CCFRP_t))
              

kelp_invalg <- kelp_invalg_dat %>%
  group_by(year)%>%
  dplyr::summarise(across(where(is.numeric), list(mean), na.rm=TRUE))%>%
  data.frame()
kelp_invalg_t <- setNames(data.frame(t(kelp_invalg[ , -1])), kelp_invalg[ ,1])
row.names(kelp_invalg_t) <- sub("\\_1", "", row.names(kelp_invalg_t))


kelp_fish <- kelp_fish_dat %>%
  group_by(year)%>%
  dplyr::summarise(across(where(is.numeric), list(mean), na.rm=TRUE))%>%
  data.frame()
kelp_fish_t <- setNames(data.frame(t(kelp_fish[ , -1])), kelp_fish[ ,1])
row.names(kelp_fish_t) <- sub("\\_1", "", row.names(kelp_fish_t))

deep_reef <- deep_reef_dat %>%
  group_by(year)%>%
  dplyr::summarise(across(where(is.numeric), list(mean), na.rm=TRUE))%>%
  data.frame()
deep_reef_t <- setNames(data.frame(t(deep_reef[ , -1])), deep_reef[ ,1])
row.names(deep_reef_t) <- sub("\\_1", "", row.names(deep_reef_t))


rocky <- rocky_dat %>%
  group_by(year)%>%
  dplyr::summarise(across(where(is.numeric), list(mean), na.rm=TRUE))%>%
  data.frame()
rocky_t <- setNames(data.frame(t(rocky[ , -1])), rocky[ ,1])
row.names(rocky_t) <- sub("\\_1", "", row.names(rocky_t))


rocky_taxon <- readxl::read_excel(
  file.path("/home/shares/ca-mpa/data/sync-data/monitoring/taxonomy_tables","RockyIntertidal-LongTerm-Taxonomy.xlsx"), sheet=1, skip = 0, na="NA")%>%
  select(marine_species_code, marine_species_name)

rocky_taxon$marine_species_code <- tolower(rocky_taxon$marine_species_code)


rocky_final <- left_join(rownames_to_column(rocky_t), rocky_taxon, by=c("rowname" = "marine_species_code"))



library(xlsx)
write.xlsx(CCFRP_t, file.path(export_path, "species_table.xlsx"), sheetName="CCFRP", row.names=TRUE)
write.xlsx(kelp_invalg_t, file.path(export_path, "species_table.xlsx"), sheetName="kelp_invalg", append=TRUE, row.names=TRUE) 
write.xlsx(kelp_fish_t, file.path(export_path, "species_table.xlsx"), sheetName="kelp_fish", append=TRUE, row.names=TRUE) 
write.xlsx(deep_reef_t, file.path(export_path, "species_table.xlsx"), sheetName="deep_reef", append=TRUE, row.names=TRUE)  
write.xlsx(rocky_final, file.path(export_path, "species_table.xlsx"), sheetName="rocky_intertidal", append=TRUE, row.names=TRUE)  



