#title: "Site table shint"
#author: "Joshua G. Smith"
#date: "5/30/2023"

###Note: these data were submitted by LT PIs. An alternative approach could 
#use the monitoring data to identify site-years. 

librarian::shelf(ggplot2, tidyverse)

#Directories
basedir <- "/home/shares/ca-mpa/data/sync-data"

# Read data
# Read site locations
kelp_sites <- readxl::read_excel(file.path(basedir, "monitoring/site_tables/raw/ltm-site-table.xlsx"), sheet=1) %>%
  janitor::clean_names()

rocky_lt <- readxl::read_excel(file.path(basedir, "monitoring/site_tables/raw/ltm-site-table.xlsx"), sheet=2) %>%
  janitor::clean_names()

rocky_bio <- readxl::read_excel(file.path(basedir, "monitoring/site_tables/raw/ltm-site-table.xlsx"), sheet=3) %>%
  janitor::clean_names()

deep_reef <- readxl::read_excel(file.path(basedir, "monitoring/site_tables/raw/ltm-site-table.xlsx"), sheet=4) %>%
  janitor::clean_names()

surf <- readxl::read_excel(file.path(basedir, "monitoring/site_tables/raw/ltm-site-table.xlsx"), sheet=5) %>%
  janitor::clean_names()

CCFRP <- readxl::read_excel(file.path(basedir, "monitoring/site_tables/raw/ltm-site-table.xlsx"), sheet=6) %>%
  janitor::clean_names()

beach <- readxl::read_excel(file.path(basedir, "monitoring/site_tables/raw/ltm-site-table.xlsx"), sheet=7) %>%
  janitor::clean_names()

################################################################################
#clean data
kelp_build1 <- kelp_sites %>%
                rename_with(~gsub("^x(\\d+)_0$", "\\1", .), starts_with("x")) %>%
                dplyr::select(2:30)%>%
                pivot_longer(cols = 8:29,
                names_to = "year",
                values_to = "dummy_var") %>%
                #drop missing years
                filter(!(is.na(dummy_var)))%>%
                mutate(group = "Kelp forest")%>%
                dplyr::select(group, everything())

rocky_lt_build1 <- rocky_lt %>%
                rename_with(~gsub("^x(\\d+)_0$", "\\1", .), starts_with("x")) %>%
                dplyr::select(2:49)%>%
                pivot_longer(cols = 8:48,
               names_to = "year",
               values_to = "dummy_var") %>%
              #drop missing years
              filter(!(is.na(dummy_var))) %>%
              mutate(group = "Intertidal long-term")%>%
              dplyr::select(group, everything())

rocky_bio_build1 <- rocky_bio %>%
              rename_with(~gsub("^x(\\d+)_0$", "\\1", .), starts_with("x")) %>%
              dplyr::select(2:30)%>%
              pivot_longer(cols = 8:29,
               names_to = "year",
               values_to = "dummy_var") %>%
              #drop missing years
              filter(!(is.na(dummy_var))) %>%
              mutate(group = "Intertidal biodiversity")%>%
              dplyr::select(group, everything())

deep_reef_build1 <- deep_reef %>%
              rename_with(~gsub("^x(\\d+)_0$", "\\1", .), starts_with("x")) %>%
              dplyr::select(2:25)%>%
              pivot_longer(cols = 7:24,
               names_to = "year",
               values_to = "dummy_var") %>%
              #drop missing years
              filter(!(is.na(dummy_var))) %>%
              mutate(group = "Deep reef",
                     site_name = NA)%>%
              dplyr::select(group,mlpa_region, site_name, everything())%>%
              mutate(dummy_var = str_replace_all(dummy_var, "X", "1"))

surf_build1 <- surf %>%
            rename_with(~gsub("^x(\\d+)_0$", "\\1", .), starts_with("x")) %>%
            dplyr::select(2:50)%>%
            dplyr::select(!(mpa_designation_jec_edits))%>%
            pivot_longer(cols = 8:48,
               names_to = "year",
               values_to = "dummy_var") %>%
            #drop missing years
            filter(!(is.na(dummy_var))) %>%
            mutate(group = "Surf zone")%>%
            dplyr::select(group, everything())

ccfrp_build1 <- CCFRP %>%
  rename_with(~gsub("^x(\\d+)_0$", "\\1", .), starts_with("x")) %>%
  dplyr::select(2:22)%>%
  pivot_longer(cols = 8:21,
               names_to = "year",
               values_to = "dummy_var") %>%
  #drop missing years
  filter(!(is.na(dummy_var))) %>%
  mutate(group = "CCFRP")%>%
  dplyr::select(group, everything())

beach_build1 <- beach %>%
  rename_with(~gsub("^x(\\d+)_0$", "\\1", .), starts_with("x")) %>%
  dplyr::select(2:49)%>%
  pivot_longer(cols = 8:48,
               names_to = "year",
               values_to = "dummy_var") %>%
  #drop missing years
  filter(!(is.na(dummy_var))) %>%
  mutate(group = "Sandy beach")%>%
  dplyr::select(group, everything())

site_tab <- rbind(kelp_build1, rocky_lt_build1, rocky_bio_build1,
                  deep_reef_build1, surf_build1, ccfrp_build1, beach_build1) %>%
            dplyr::select(!(dummy_var))

write.csv(site_tab, file.path(basedir, "monitoring/site_tables/processed/monitoring_site_table.csv"),row.names = FALSE)






















