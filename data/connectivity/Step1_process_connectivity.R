#title: "Connectivity processing"
#author: "Joshua G. Smith"
#date: "8/17/2023"

rm(list=ls())

#required packages
librarian::shelf(tidyverse, here, janitor)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"

#load raw connectivity dat
settle_dat <- read.csv(file.path(data_path, "/connectivity/raw/Settlement_connectivity_by_habitat.csv")) %>%
                      janitor::clean_names()


################################################################################
#process connectivity

settle_build1 <- settle_dat %>%
                  rename(settlement_rocky_intertidal = rocky_intertidal,
                         settlement_kelp_forest_shallow_reef = shallow_rocky_reef_kelp_and_rock,
                         settlement_deep_reef = rock_30_100m,
                         settlement_100_200m = rock_100_200m) %>%
                  #calculate mpa total
                  group_by(mpa)%>%
                  mutate(settlement_mpa_total = sum(settlement_rocky_intertidal,
                                                    settlement_kelp_forest_shallow_reef,
                                                    settlement_deep_reef,
                                                    settlement_100_200m),
                         #match typical naming
                         affiliated_mpa = str_trim(str_replace(mpa, "\\(No-Take\\)", "")))  %>%
                  dplyr::select(affiliated_mpa, everything()) %>%
                  ungroup()



saveRDS(settle_build1, file = file.path(data_path,"/connectivity/processed/settlement_by_mpa.Rds"))



                    