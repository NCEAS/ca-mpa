#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "5/30/2023"

rm(list=ls())

#required packages
librarian::shelf(ggplot2, tidyverse, here, metafor)

#set directories
data_path <- "/home/shares/ca-mpa/data/sync-data/"
biomass_dat <-  paste0(data_path,"monitoring/processed_data/biomass_processed")
fig_dir <- here::here("analyses","1performance_eco","figures")
tab_dir <- here::here("analyses","1performance_eco","tables")
dat_path <- here::here("analyses","1performance_eco","output")

#read data
biomass_dat <- read.csv(file.path(dat_path, "biomass_dat.csv"))

################################################################################
#start simple
dat <- escalc(measure="ROM", m1i=mean_kg_smr, m2i=mean_kg_ref, sd1i=sd_kg_smr, 
              sd2i=sd_kg_ref, n1i=n_smr, n2i=n_ref, data=biomass_mod %>% filter(habitat == "Shallow reef")) 

# Create a mixed-effects model with random intercepts
model <- rma.mv(yi = logRR,
                V = V,
                random = ~ 1 + factor(year) | affiliated_mpa,
                method = "REML",
                mods = ~ habitat + region4 + habitat*region4,
                data = dat)

# Generate the forest plot with separate panels for each region
forest(model, slab = dat$habitat, xlim = c(-2, 2), 
       rows = length(unique(dat$region4)), at = c(-1, 1))


################################################################################

dat <- escalc(measure="ROM", m1i=mean_kg_smr, m2i=mean_kg_ref, sd1i=sd_kg_smr, sd2i=sd_kg_ref, n1i=n_smr, n2i=n_ref, data=biomass_mod, slab=paste(habitat)) 

# Create a mixed-effects model with random intercepts
model <- rma.mv(yi = logRR,
                V = V,
                random = ~ 1 + factor(year) | affiliated_mpa,
                method = "REML",
                mods = ~ habitat + region4 + habitat*region4,
                data = dat)

# Generate the forest plot with separate panels for each region
forest(model, slab = dat$habitat, xlim = c(-2, 2), 
       rows = length(unique(dat$region4)), at = c(-1, 1))


################################################################################

