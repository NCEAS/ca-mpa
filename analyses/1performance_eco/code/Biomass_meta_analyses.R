

#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "3/30/2022"

  
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


# Load ecological data ----------------------------------------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "targeted_nontargeted_fish_biomass.csv" 
meta.data <- read.csv(file.path(data_path, input_file))

meta.data$mpa_class <- tolower(meta.data$mpa_class)
meta.data$mpa_designation <- tolower(meta.data$mpa_designation)




# Analysis-specific data cleaning -----------------------------------------



  #note: for surf zone, some SMCA's were used as reference sites for SMRs. Here we recode the affiliated_mpa names of the SMCAs to serve as SMR-affiliated reference sites. 

#remove surf to edit
#surf.data <- meta.data %>%
#            filter(group=="surf")%>%
#            mutate(affiliated_mpa = ifelse(affiliated_mpa=="mackerricher smca", "ten mile smr", affiliated_mpa),
#                   affiliated_mpa = ifelse(affiliated_mpa=="greyhound rock smca", "ano nuevo smr", affiliated_mpa),
#                   affiliated_mpa = ifelse(affiliated_mpa=="carmel bay smca", "asilomar smr", affiliated_mpa))

#surf.data$mpa_designation <- recode_factor(surf.data$mpa_designation, smca="ref") #make all smca's 'ref', since SMCAs will be filtered out of mpa_class

#meta.data.clipped <- meta.data %>%
#                    filter(group=="deep_reef"|group=="ccfrp"|group=="kelp")

#meta.data<-rbind(meta.data.clipped, surf.data)




# #PART 1 - meta analyses for Biomass of targeted fish** ------------------

region.yr.means<- meta.data%>%
  filter(year=='2016' | year=='2017'|year=='2018' | year=='2019' | year =='2020',
         mpa_class=='smr'| mpa_class=='ref',
         target_status == 'targeted'
  )

region.yr.means$mpa_designation <- recode_factor(region.yr.means$mpa_designation, smca="smr") #recode to match defacto SMR

region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_class, mpa_designation,target_status)%>%
  dplyr::summarize(yr.mean = mean(sum_biomass,na.rm=TRUE), 
                   sd=sd(sum_biomass), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n)
  )



#region means error structure 2 -- sigma calculated from MPA means summarized across years. 
#region.MPAlevel.means<- meta.data%>% 
#  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
#         indicator == "diversity",
#         mpa_designation == "ref"|mpa_designation =="smr",
#         year=='2016' | year=='2017'|year=='2018' | year=='2019')%>%
#  dplyr::group_by(group,mlpa_region, affiliated_mpa, mpa_designation,variable,indicator)%>%
#  dplyr::summarize(group.mean = mean(mean,na.rm=TRUE))  #no. MPA 'sites' where year is unit of rep

#region.means<- region.MPAlevel.means%>%
#  dplyr::group_by(group,mlpa_region,mpa_designation, variable,indicator)%>%
#  dplyr::summarize(group.avg = mean(group.mean,na.rm=TRUE),
#                   sd=sd(group.mean),
#                   n=n()) %>% #no. MPA 'sites' where year is unit of rep
#  pivot_wider(names_from = mpa_designation, #convert to wide format for {metafor}
#              values_from = c(group.avg, sd, n)
#names_glue = "{mpa_designation}_{value}")
#  )



### calculate SMD sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)

dat <- escalc(measure="SMD", m1i=yr.mean_smr, m2i=yr.mean_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=region.yr.means, slab=paste(group)) 

dat <- dat %>%
  filter(!is.na(vi))    

#dat <- escalc(measure="SMD", m1i=group.avg_smr, m2i=group.avg_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=smd.final, slab=paste(group)) 

#fit random effects model
res <- rma(yi,vi, method="REML", data=dat, slab=paste(group)) 


### a little helper function to add Q-test, I^2, and tau^2 estimate info
### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, y) {
  bquote(paste(.(text),
               " (Q = ", .(formatC(y$QE, digits=2, format="f")),
               #", df = ", .(y$k - y$p),
               ", p ", .(formatC(y$pval, digits=2, format="f")), #"; ",
               #I^2, " = ", .(formatC(y$I2, digits=1, format="f")), "%, ",
               #tau^2, " = ", .(formatC(y$tau2, digits=2, format="f")), 
               ")"
  )
  )}


### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
forest(res, xlim=c(-8, 6), #at=log(c(0.05, 0.25, 1, 4)), #atransf=exp,
       #ilab=cbind(dat$tpos, dat$tneg, dat$cpos, dat$cneg),
       #ilab.xpos=c(-9.5,-8,-6,-4.5), 
       cex=0.75, 
       ylim=c(-3, 30),
       order=order(factor(dat$region4, level=c("south","north islands", "central","north")),dat$yi), 
       rows=c(3:6,10:12,16:19,23:26),
       mlab=mlabfun("RE Model", res),
       slab=paste(dat$group),
       #showweights = TRUE,
       #psize=1.3, 
       header="Region | Monitoring Group")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)

### add additional column headings to the plot

### add text for the subgroups
text(-8, c(27,20,13,7), pos=4, c("North",
                                 "Central",
                                 "N. Channel Islands",
                                 "South"))


### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.n <- rma(yi, vi, method="REML", subset=(region4=="north"), verbose=TRUE, digits=5, data=dat, slab=paste(group, region4))
res.c <- rma(yi, vi, method="REML", subset=(region4=="central"),data=dat, slab=paste(group, region4))
res.s <- rma(yi, vi, method="REML", subset=(region4=="south"), data=dat, slab=paste(group, region4))
res.i <- rma(yi, vi, method="REML", subset=(region4=="north islands"), verbose=TRUE, digits=5, data=dat, slab=paste(group, region4))

### add summary polygons for the three subgroups

addpoly(res.n, row=21.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.n))
text(-8, 21.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.n))

addpoly(res.c, row= 14.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.c))
text(-8, 14.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.c))

addpoly(res.i, row= 8.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.i))
text(-8, 8.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.i))

addpoly(res.s, row= 1,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.s))
text(-8, 1, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.s))


### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ region4, data=dat)

### add text for the test of subgroup differences
text(-8, -2.5, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                             Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                             ", p = ", .(formatC(res$pval, digits=2, format="f")))))

text(x =-0.2, y = 28.8, "REF",  pos=2, col="blue", font=2)
text(x =1.3, y = 28.8, "SMR",  pos=2, col="red ", font=2)
text(x =-4, y = 30.2, "targeted fish biomass 2016-20",  cex=1, pos=2, font=3)









# Biomass of nontargeted --------------------------------------------------


region.yr.means<- meta.data%>%
  filter(year=='2016' | year=='2017'|year=='2018' | year=='2019' | year =='2020',
         mpa_class=='smr'| mpa_class=='ref',
         target_status == 'nontargeted'
  )

region.yr.means$mpa_designation <- recode_factor(region.yr.means$mpa_designation, smca="smr") #recode to match defacto SMR

region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_class, mpa_designation,target_status)%>%
  dplyr::summarize(yr.mean = mean(sum_biomass,na.rm=TRUE), 
                   sd=sd(sum_biomass), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n)
  )




dat <- escalc(measure="SMD", m1i=yr.mean_smr, m2i=yr.mean_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=region.yr.means, slab=paste(group)) 

dat <- dat %>%
  filter(!is.na(vi))    

#dat <- escalc(measure="SMD", m1i=group.avg_smr, m2i=group.avg_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=smd.final, slab=paste(group)) 

#fit random effects model
res <- rma(yi,vi, method="REML", data=dat, slab=paste(group)) 


### a little helper function to add Q-test, I^2, and tau^2 estimate info
### a little helper function to add Q-test, I^2, and tau^2 estimate info
mlabfun <- function(text, y) {
  bquote(paste(.(text),
               " (Q = ", .(formatC(y$QE, digits=2, format="f")),
               #", df = ", .(y$k - y$p),
               ", p ", .(formatC(y$pval, digits=2, format="f")), #"; ",
               #I^2, " = ", .(formatC(y$I2, digits=1, format="f")), "%, ",
               #tau^2, " = ", .(formatC(y$tau2, digits=2, format="f")), 
               ")"
  )
  )}


### set up forest plot (with 2x2 table counts added; the 'rows' argument is
### used to specify in which rows the outcomes will be plotted)
forest(res, xlim=c(-8, 6), #at=log(c(0.05, 0.25, 1, 4)), #atransf=exp,
       #ilab=cbind(dat$tpos, dat$tneg, dat$cpos, dat$cneg),
       #ilab.xpos=c(-9.5,-8,-6,-4.5), 
       cex=0.75, 
       ylim=c(-3, 30),
       order=order(factor(dat$region4, level=c("south","north islands", "central","north")),dat$yi), 
       rows=c(3:6,10:12,16:19,23:25),
       mlab=mlabfun("RE Model", res),
       slab=paste(dat$group),
       #showweights = TRUE,
       #psize=1.3, 
       header="Region | Monitoring Group")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)

### add additional column headings to the plot

### add text for the subgroups
text(-8, c(26,20,13,7), pos=4, c("North",
                                 "Central",
                                 "N. Channel Islands",
                                 "South"))


### set par back to the original settings
par(op)

### fit random-effects model in the three subgroups
res.n <- rma(yi, vi, method="REML", subset=(region4=="north"), verbose=TRUE, digits=5, data=dat, slab=paste(group, region4))
res.c <- rma(yi, vi, method="REML", subset=(region4=="central"),data=dat, slab=paste(group, region4))
res.s <- rma(yi, vi, method="REML", subset=(region4=="south"), data=dat, slab=paste(group, region4))
res.i <- rma(yi, vi, method="REML", subset=(region4=="north islands"), verbose=TRUE, digits=5, data=dat, slab=paste(group, region4))

### add summary polygons for the three subgroups

addpoly(res.n, row=21.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.n))
text(-8, 21.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.n))

addpoly(res.c, row= 14.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.c))
text(-8, 14.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.c))

addpoly(res.i, row= 8.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.i))
text(-8, 8.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.i))

addpoly(res.s, row= 1.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.i))
text(-8, 1.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.i))


### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ region4, data=dat)

### add text for the test of subgroup differences
text(-8, -2.5, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                             Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                             ", p = ", .(formatC(res$pval, digits=2, format="f")))))

text(x =-0.2, y = 28.8, "REF",  pos=2, col="blue", font=2)
text(x =1.3, y = 28.8, "SMR",  pos=2, col="red ", font=2)
text(x =-3.6, y = 30.2, "nontargeted fish biomass 2016-20",  cex=1, pos=2, font=3)













