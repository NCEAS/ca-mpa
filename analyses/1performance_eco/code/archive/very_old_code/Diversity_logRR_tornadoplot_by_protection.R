rm(list = ls())


require(ggpubr)
require(tidytext)
require(dplyr)
require(ggplot2)


data_path <- "/home/shares/ca-mpa/data/sync-data"
input_file <- "Ecol_perform_metrics_means_4regions.csv" 

means.data <- read.csv(file.path(data_path, input_file))
means.data$mpa_class <- recode_factor(means.data$mpa_class, none="ref")


data_path <- "/home/shares/ca-mpa/data/sync-data"
input_file <- "mpa-attributes.xlsx" 

attrib.data <- readxl::read_excel(file.path(data_path, input_file), 
                                  sheet=1, na = c("NA", "."), trim_ws = T)
attrib.data <- attrib.data %>%
  dplyr::select(name, protection) %>%
  rename(affiliated_mpa = name)

means.final <- left_join(means.data, attrib.data)


##========meta analysis for 2016-2019 -- DIVERSITY =======

means.final$mpa_designation <- recode_factor(means.final$mpa_designation, smr="mpa")
means.final$mpa_designation <- recode_factor(means.final$mpa_designation, smca="mpa")


region.yr.means<- means.final%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         #mpa_class == "ref"| mpa_class =="smr" | mpa_class == "none",
         year=='2016' | year=='2017'|year=='2018' | year=='2019'
  )%>%
  group_by(group,mlpa_region, mpa_designation,protection,variable,indicator)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n)
  )


