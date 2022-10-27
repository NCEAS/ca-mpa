
#title: "CA MPA Performance PCA"
#author: "Joshua G. Smith"
#date: "10/26/2022"

rm(list=ls())

#load required packages
library(dplyr)
library(vegan)

#set dir
data_path <- "analyses/1performance_eco/output"


# Load fish biomass data -------------------------------------------------------
input_file <- "MPA_targeted_nontargeted_biomass_with_mods.csv" 
meta.data <- read.csv(file.path(data_path, input_file)) %>%
  dplyr::select(!(X))%>%
  filter(!(group=='ccfrp' & target_status=='nontargeted'))#drop nontargeted ccfrp

#clean
meta.data$group <- recode_factor(meta.data$group, "deep_reef"='deep reef')        

meta.data$mpa_class <- tolower(meta.data$mpa_class)
meta.data$mpa_designation <- tolower(meta.data$mpa_designation)


#reshape to wide format

mpa.means<- meta.data%>%
  filter( 
          #group=="kelp",
          mpa_class=='smr'| mpa_class=='ref'
  )

mpa.means$mpa_designation <- recode_factor(mpa.means$mpa_designation, smca="smr") #recode to match defacto SMR


targeted_wide <- mpa.means %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(sum_biomass),
              values_fn =  mean
  )%>%
  filter(!(smr=="NULL"|ref=="NULL"))%>%
  mutate(RR = log(smr/ref))%>%
  na.omit()

PCR_dat <- as.data.frame(targeted_wide) %>%
  mutate(affiliated_mpa = as.factor(affiliated_mpa),
         region4 = as.factor(region4),
         mpa_age = as.numeric(mpa_age),
         year= as.factor(year),
         RR_sign = ifelse(RR > 0,"positive","negative"))%>%
  mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(., -Inf)))%>%
  filter(!(affiliated_mpa=="begg rock smr"))


PCR_targeted_dat <- PCR_dat %>% filter(target_status == 'targeted')
PCR_nontargeted_dat <- PCR_dat %>% filter(target_status == 'nontargeted')


################################################################################
#PCR

PCR_dat_kelp <- PCR_targeted_dat %>% filter(
                                            year=='2019'|
                                              year=='2020') 

pcr_target<- prcomp(~ mpa_age + size_km2 + lat, scale=TRUE, data = PCR_dat_kelp)
summary(pcr_target)

#examine output
p<-autoplot(pcr_target, data=PCR_dat_kelp, loadings=TRUE,
            loadings.label=TRUE, size="RR",
            #colour="RR_sign"
            ) + scale_size_continuous(range = c(-3,6))
ggplotly(p) 


#plot using ggplot
names(pcr_target)
pcr_target$x[1:6,1:3]












