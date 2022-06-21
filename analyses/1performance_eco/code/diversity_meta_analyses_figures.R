# Create Figure from Meta-Analysis
# Cori 

# Setup ------------------------------------------------------------------------
#required packages

library(ggplot2)
library(metafor)
library(forestplot)
library(dplyr)
library(tidyr)
library(reshape2)
require(ggpubr)
require(tidytext)
require(plyr)
require(Rmisc)
require(boot)
require(broom)
require(waldo)
require(ggpmisc)
require(ggtext)
library(forcats)


## Load Data ----

### Targeted/Non-Targeted ----
data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_diversity.csv" 
meta.data <- read.csv(file.path(data_path, input_file))

### All Combined ----
input_file <- "all_fish_diversity.csv" 
meta.data.all <- read.csv(file.path(data_path, input_file))

# Function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, y) {
  bquote(paste(.(text),
               " (Q = ", .(formatC(y$QE, digits=2, format="f")),
               #", df = ", .(y$k - y$p),
               ", p ", .(formatC(y$pval, digits=2, format="f")), #"; ",
               #I^2, " = ", .(formatC(y$I2, digits=1, format="f")), "%, ",
               #tau^2, " = ", .(formatC(y$tau2, digits=2, format="f")), 
               ")"))}


# Targeted Species -------------------------------------------------------------
## Create summary df----
region.yr.means.tar <- meta.data %>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020"),
         mpa_defacto_class=='smr',
         target_status == 'targeted')


region.yr.means.tar <- region.yr.means.tar %>%
  group_by(group,region4, mpa_defacto_class, mpa_defacto_designation, target_status)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd = sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n = n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )

## Calculate SMD ---------------------------------------------------------------
dat.tar <- escalc(measure="SMD", 
                  m1i=yr.mean_smr, 
                  m2i=yr.mean_ref, 
                  sd1i=sd_smr, 
                  sd2i=sd_ref, 
                  n1i=n_smr, 
                  n2i=n_ref, 
                  data=region.yr.means.tar, 
                  slab=paste(group)) 

dat.tar <- dat.tar %>%
  filter(!is.na(vi))    


## Fit random effects models ---------------------------------------------------

### All groups ----
res.tar <- rma(yi,vi, method="REML", data=dat.tar, slab=paste(group)) 

### Subgroups ----
res.tar.n <- rma(yi, vi, method="REML", subset=(region4=="north"), 
                 verbose=TRUE, digits=5, data=dat.tar, 
                 slab=paste(group, region4))

res.tar.c <- rma(yi, vi, method="REML", subset=(region4=="central"),
                 verbose=TRUE, digits=5, data=dat.tar, 
                 slab=paste(group, region4))

res.tar.s <- rma(yi, vi, method="REML", subset=(region4=="south"), 
                 verbose=TRUE, digits=5, data=dat.tar, 
                 slab=paste(group, region4))

res.tar.i <- rma(yi, vi, method="REML", subset=(region4=="north islands"), 
                 verbose=TRUE, digits=5, data=dat.tar, 
                 slab=paste(group, region4))

### Meta ----
### fit meta-regression model to test for subgroup differences
res.tar.meta <- rma(yi, vi, mods = ~ region4, data=dat.tar)


# Non-Targeted -----------------------------------------------------------------
## Create summary data frame ----
region.yr.means.non <- meta.data%>%
  filter(year=='2016' | year=='2017'|year=='2018' | year=='2019' | year =='2020',
         mpa_defacto_class=='smr',
         target_status == 'nontargeted')

region.yr.means.non <- region.yr.means.non %>%
  group_by(group,region4, mpa_defacto_class, mpa_defacto_designation,target_status)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n))

## Calculate SMD ---------------------------------------------------------------
dat.non <- escalc(measure="SMD", 
                  m1i=yr.mean_smr, 
                  m2i=yr.mean_ref, 
                  sd1i=sd_smr, 
                  sd2i=sd_ref, 
                  n1i=n_smr, 
                  n2i=n_ref, 
                  data=region.yr.means.non, 
                  slab=paste(group)) %>% 
  filter(!is.na(vi))

## Fit random effects models ---------------------------------------------------

### All groups ----
res.non <- rma(yi,vi, method="REML", data=dat.non, slab=paste(group)) 

### Subgroups ----
res.non.n <- rma(yi, vi, method="REML", subset=(region4=="north"), 
                 verbose=TRUE, digits=5, data=dat.non, slab=paste(group, region4))

res.non.c <- rma(yi, vi, method="REML", subset=(region4=="central"),
                 verbose=TRUE, digits=5, data=dat.non, slab=paste(group, region4))

res.non.s <- rma(yi, vi, method="REML", subset=(region4=="south"),
                 verbose=TRUE, digits=5, data=dat.non, slab=paste(group, region4))

res.non.i <- rma(yi, vi, method="REML", subset=(region4=="north islands"), 
                 verbose=TRUE, digits=5, data=dat.non, slab=paste(group, region4))

### Meta ----
### fit meta-regression model to test for subgroup differences
res.non.meta <- rma(yi, vi, mods = ~ region4, data=dat.non)


# Combined (All Targeted and Non-Targeted) --------------------------------------
## Create summary df
region.yr.means<- meta.data%>%
  filter(year %in% c("2016", "2017", "2018", "2019", "2020"),
         mpa_defacto_class=='smr')

region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )

## Calculate SMD ---------------------------------------------------------------
dat <- escalc(measure="SMD", 
              m1i=yr.mean_smr, 
              m2i=yr.mean_ref, 
              sd1i=sd_smr, 
              sd2i=sd_ref, 
              n1i=n_smr, 
              n2i=n_ref, 
              data=region.yr.means, 
              slab=paste(group)) 

dat <- dat %>%
  filter(!is.na(vi))    

## Fit random effects models ---------------------------------------------------
### All groups ----
res <- rma(yi,vi, method="REML", data=dat, slab=paste(group)) 

### Subgroups ----
### fit random-effects model in the three subgroups
res.n <- rma(yi, vi, method="REML", subset=(region4=="north"), verbose=TRUE, digits=5, data=dat, slab=paste(group, region4))
res.c <- rma(yi, vi, method="REML", subset=(region4=="central"),data=dat, slab=paste(group, region4))
res.s <- rma(yi, vi, method="REML", subset=(region4=="south"), data=dat, slab=paste(group, region4))
res.i <- rma(yi, vi, method="REML", subset=(region4=="north islands"), verbose=TRUE, digits=5, data=dat, slab=paste(group, region4))

### Meta ----
### fit meta-regression model to test for subgroup differences
res.meta <- rma(yi, vi, mods = ~ region4, data=dat)


# Create Figure ----------------------------------------------------------------
## Create data frames ----

### Combined ----
combined <- data.frame(ES = res$yi,
                       SE = res$vi,
                       region = dat$region4,
                       hab.group = dat$group,
                       type = "SMD",
                       analysis = "combined")

combined_models <- rbind(data.frame(ES = res$b,
                                    SE = res$se,
                                    region = "all"),
                         data.frame(ES = res.n$b,
                                    SE = res.n$se,
                                    region = "north"),
                         data.frame(ES = res.c$b,
                                    SE = res.c$se,
                                    region = "central"),
                         data.frame(ES = res.s$b,
                                    SE = res.s$se,
                                    region = "south"),
                         data.frame(ES = res.i$b,
                                    SE = res.i$se,
                                    region = "north islands")) %>% 
  mutate(hab.group = "RE Model",
         type = "REM",
         analysis = "combined")

### Targeted ----
targeted <- data.frame(ES = res.tar$yi,
                       SE = res.tar$vi,
                       region = dat.tar$region4,
                       hab.group = dat.tar$group,
                       type = "SMD",
                       analysis = "targeted")

targeted_models <- rbind(data.frame(ES = res.tar$b,
                                    SE = res.tar$se,
                                    region = "all"),
                         data.frame(ES = res.tar.n$b,
                                    SE = res.tar.n$se,
                                    region = "north"),
                         data.frame(ES = res.tar.c$b,
                                    SE = res.tar.c$se,
                                    region = "central"),
                         data.frame(ES = res.tar.s$b,
                                    SE = res.tar.s$se,
                                    region = "south"),
                         data.frame(ES = res.tar.i$b,
                                    SE = res.tar.i$se,
                                    region = "north islands")) %>% 
  mutate(hab.group = "RE Model",
         type = "REM",
         analysis = "targeted")

### Non-Targeted ----
nontargeted <- data.frame(ES = res.non$yi,
                          SE = res.non$vi,
                          region = dat.non$region4,
                          hab.group = dat.non$group,
                          type = "SMD",
                          analysis = "non-targeted")

nontargeted_models <- rbind(data.frame(ES = res.non$b,
                                       SE = res.non$se,
                                       region = "all"),
                            data.frame(ES = res.non.n$b,
                                       SE = res.non.n$se,
                                       region = "north"),
                            data.frame(ES = res.non.c$b,
                                       SE = res.non.c$se,
                                       region = "central"),
                            data.frame(ES = res.non.s$b,
                                       SE = res.non.s$se,
                                       region = "south"),
                            data.frame(ES = res.non.i$b,
                                       SE = res.non.i$se,
                                       region = "north islands")) %>% 
  mutate(hab.group = "RE Model",
         type = "REM",
         analysis = "non-targeted")

## Mega data frame ----
all_results <- rbind(combined,
                     combined_models,
                     targeted,
                     targeted_models,
                     nontargeted,
                     nontargeted_models) %>% 
  mutate(hab.group = as.factor(hab.group)) %>% 
  mutate(hab.group = fct_relevel(hab.group,
                                 "all", "RE Model", "surf-bruv", "surf-seine",
                                 "kelp-fish", "deep_reef", "ccfrp")) %>% 
  mutate(ES_points = case_when(type == "SMD" ~ ES)) %>% 
  mutate(poly_points = case_when(type == "REM" ~ ES))

## Build Plot ----

ggplot(data = all_results %>% 
         filter(analysis %in% c("targeted", "non-targeted"),
                !(region == "all"))) +
  geom_vline(xintercept = 0, color = "grey60", size = 0.4, linetype = "dotted") +
  geom_point(aes(x = ES_points, y = hab.group, color = analysis), position = position_dodge(width=0.2)) +
  geom_errorbar(aes(xmin = ES_points-SE, xmax = ES_points+SE, y = hab.group, color = analysis),
                width = 0.1, size = 0.2, position = position_dodge(width=0.2)) +
  scale_x_continuous(limits = c(-3, 3), expand = c(0,0)) +
  labs(x = "Standardized Mean Difference",
       y = "Habitat Monitoring Group",
       color = NULL) +
  theme_classic() +
  facet_wrap(~region, ncol = 1)

