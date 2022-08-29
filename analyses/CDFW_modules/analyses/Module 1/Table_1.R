

require(dplyr)
require(gt)



# Load data ---------------------------------------------------------------

monitoring_dat <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/envr_anomalies_at_mpas.Rds") %>%
  select(group, mpa_name)%>% unique()

rocky_sites <- read.csv("/home/shares/ca-mpa/data/sync-data/monitoring/monitoring_rocky-intertidal/CA_MPA_sites_20210907b.csv")%>%
                filter(!(mpa_name=="NONE"))%>%
                mutate(group="rocky")%>%
                select(group, mpa_name) %>%
                unique()
rocky_sites$mpa_name <- tolower(rocky_sites$mpa_name)

monitoring_dat <- rbind(monitoring_dat, rocky_sites)
monitoring_dat$group <- recode_factor(monitoring_dat$group, "surf_zone"="surf")
monitoring_dat$mpa_name <- recode_factor(monitoring_dat$mpa_name, "swamis smca"="swami's smca")

trait_path <- "/home/shares/ca-mpa/data/sync-data/mpa_traits"
input_file <- "mpa-attributes.xlsx" 
defacto_smr <- readxl::read_excel(file.path(data_path, input_file), sheet=5, skip = 0, na="NA")%>%
  dplyr::select(group, affiliated_mpa, mpa_class)

trait_table <-  readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")%>%
  dplyr::select(name, mpa_class, bioregion, four_region_north_ci)


# Join with trait table ---------------------------------------------------


mpa_defacto_table <- left_join(monitoring_dat, defacto_smr, by=c("mpa_name"="affiliated_mpa",
                                                                 "group"))%>%
                      rename("mpa_defacto_class"="mpa_class")%>%
                      pivot_wider(names_from="group",values_from="mpa_defacto_class")
           
mpa_defacto_table2 <- left_join(trait_table,mpa_defacto_table, by=c("name"="mpa_name"
                                                                     ))%>%
                      rename(
                             "mpa type" = mpa_class) %>%
                      ungroup()%>%
  #dplyr::mutate(four_region_north_ci = forcats::fct_relevel(four_region_north_ci,"north","central","north islands","south"))%>%
                      mutate(four_region_north_ci = factor(four_region_north_ci, levels=c("north","central","north islands","south")))%>%
                      arrange(four_region_north_ci)%>%
                      select("MPA name" = name, 
                             "CA four regions" = four_region_north_ci,
                             "mpa type",
                             "kelp forest" = kelp,
                             "rocky intertidal" = rocky,
                             "surf zone" = surf,
                             "deep reef" = deep_reef,
                             "CCFRP" = ccfrp)
                      
                      
mpa_defacto_table2$"mpa type" <- toupper(mpa_defacto_table2$"mpa type")

mpa_defacto_table2 <- sapply(mpa_defacto_table2, as.character) # since your values are `factor`
mpa_defacto_table2[is.na(mpa_defacto_table2)] <- " "


# Create beautiful table --------------------------------------------------


# constants ----
n = 0
c_col = c("#1e3048", "#274060", "#2f5375", "#4073a0", "#5088b9")
c_col_light_blue = c("#edf2fb", "#e2eafc", "#d7e3fc", "#ccdbfd", "#c1d3fe")
c_container_width = px(800)
c_table_width = px(650)
c_rn = 150
c_save = TRUE
c_format = "html"


mpa_defacto_table2 %>% 
  head(c_rn)%>%
  gt(
    groupname_col = "CA four regions",
    rowname_col = "MPA name"
  )  %>%
  cols_align(
    align = "right",
    columns = c("kelp forest","rocky intertidal","surf zone","deep reef","CCFRP")
  ) %>% 
  cols_align(
    align = "left",
    columns = c("mpa type")
  ) %>% 
  cols_width(
    c("mpa type") ~ px(100),
    c("kelp forest") ~ px(75),
    c("rocky intertidal") ~ px(75),
    c("surf zone") ~ px(75),
    c("deep reef") ~ px(75),
    c("CCFRP") ~ px(75),
  )%>%
  opt_row_striping()%>%
  tab_style(
    style = list(
      cell_fill(color = "#D3D3D3"),
      cell_text(style = "italic")
      ),
    locations = cells_body(
      col = "mpa type"
      #rows=1:124
    )
  ) 








