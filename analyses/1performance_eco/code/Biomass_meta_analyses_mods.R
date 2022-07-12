

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
input_file <- "targeted_nontargeted_fish_biomass_with_mods.csv" 
meta.data <- read.csv(file.path(data_path, input_file)) %>%
              filter(!(group=='ccfrp' & target_status=='nontargeted')) #drop nontargeted ccfrp

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
         mpa_class=='smr'| mpa_class=='ref'
  )

region.yr.means$mpa_designation <- recode_factor(region.yr.means$mpa_designation, smca="smr") #recode to match defacto SMR

region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_class, mpa_designation,target_status)%>%
  dplyr::summarize(yr.mean = mean(sum_biomass,na.rm=TRUE), 
                   sd=sd(sum_biomass), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n(),
                   mean_age=mean(mpa_age),
                   mean_distance=mean(distance_to_port),
                   mean_size = mean(size_km2)) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd, n, mean_age, mean_distance, mean_size)
  )





mpa.means<- meta.data%>%
  filter( year=='2019' | year =='2020',
         mpa_class=='smr'| mpa_class=='ref'
  )

mpa.means$mpa_designation <- recode_factor(mpa.means$mpa_designation, smca="smr") #recode to match defacto SMR

#step 1 - take mean of each MPA across all years
mpa.means <- mpa.means %>%
  group_by(group,region4, affiliated_mpa, mpa_class, mpa_designation,target_status)%>%
  dplyr::summarize(mpa.mean = mean(sum_biomass,na.rm=TRUE), 
                   sd=sd(sum_biomass), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n(),
                   mean_age=mean(mpa_age),
                   mean_distance=mean(distance_to_port),
                   mean_size = mean(size_km2))

#step 2 - take mean of mpas
region.yr.means <- mpa.means %>%
  group_by(group,region4, mpa_class, mpa_designation,target_status)%>%
  dplyr::summarize(yr.mean = mean(mpa.mean,na.rm=TRUE), 
                   sd_mpa=sd(mpa.mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n(),
                   mean_age_mpa=mean(mean_age),
                   mean_distance_mpa=mean(mean_distance),
                   mean_size_mpa = mean(mean_size)) %>%
  pivot_wider(names_from = mpa_designation,
              values_from = c(yr.mean, sd_mpa, n, mean_age_mpa, mean_distance_mpa, mean_size_mpa)
  )







### calculate SMD sampling variances (and use
### the 'slab' argument to store study labels as part of the data frame)

#dat <- escalc(measure="ROM", m1i=yr.mean_smr, m2i=yr.mean_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=region.yr.means, slab=paste(group)) 

dat <- escalc(measure="ROM", m1i=yr.mean_smr, m2i=yr.mean_ref, sd1i=sd_mpa_smr, sd2i=sd_mpa_ref, n1i=n_smr, n2i=n_ref, data=region.yr.means, slab=paste(group)) 

dat <- dat %>%
  filter(!is.na(vi))    

#dat <- escalc(measure="SMD", m1i=group.avg_smr, m2i=group.avg_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=smd.final, slab=paste(group)) 

#fit random effects model
res.overall <- rma(yi,vi, method="REML", data=dat, slab=paste(group)) 


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

cols <- c("red", "blue")[match(dat$target_status, c("targeted", "nontargeted"))]

forest(dat$yi, dat$vi, xlim=c(-8, 6), #at=log(c(0.05, 0.25, 1, 4)), #atransf=exp,
       #ilab=cbind(dat$tpos, dat$tneg, dat$cpos, dat$cneg),
       #ilab.xpos=c(-9.5,-8,-6,-4.5), 
       cex=0.75, 
       ylim=c(-3, 51),
       order=order(factor(dat$region4, level=c("south","north islands", "central","north"))), 
       rows=c(3:9,16:20,27:33,40:45),
       mlab=mlabfun("biomass overall", res.overall),
       slab=paste(dat$group),
       ilab = dat$target_status,
       ilab.xpos=-6,
       #col=cols,
       #showweights = TRUE,
       #psize=1.3, 
       header="Monitoring Group | Fished Status")

#with(forest_plot, points(x,y,col=cols))

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)

### add additional column headings to the plot

### add text for the subgroups
text(-8, c(46.5,34.5,21.5,10.5), pos=4, c("North",
                                 "Central",
                                 "N. Islands",
                                 "South"))

text(-6.8, c(46.5,34.5,21.5,10.5), pos=4, c("Fished Status",
                                          "Fished Status",
                                          "Fished Status",
                                          "Fished Status"))


### set par back to the original settings
par(op)


#subset data for polygons
dat_target <- dat %>%
              filter(target_status=='targeted')
dat_nontarget <- dat %>%
              filter(target_status=='nontargeted')


### fit random-effects model in the three subgroups
res.n.target <- rma(yi, vi, method="REML", subset=(region4=="north"), verbose=TRUE, digits=5, data=dat_target)
res.n.nontarget <- rma(yi, vi, method="REML", subset=(region4=="north"), verbose=TRUE, digits=5, data=dat_nontarget)

res.c.target <- rma(yi, vi, method="REML", subset=(region4=="central"),data=dat_target)
res.c.nontarget <- rma(yi, vi, method="REML", subset=(region4=="central"),data=dat_nontarget)

res.s.target <- rma(yi, vi, method="REML", subset=(region4=="south"), data=dat_target)
res.s.nontarget <- rma(yi, vi, method="REML", subset=(region4=="south"), data=dat_nontarget)

res.i.target <- rma(yi, vi, method="REML", subset=(region4=="north islands"), verbose=TRUE, digits=5, data=dat_target)
res.i.nontarget <- rma(yi, vi, method="REML", subset=(region4=="north islands"), verbose=TRUE, digits=5, data=dat_nontarget)


### add summary polygons for the three subgroups

addpoly(res.n.target, row=38, cex=0.75, mlab=mlabfun("RE Model for targeted", y=res.n.target), #col='red'
        )
text(-8, 38, pos=4, cex=0.7, mlabfun("RE Model for targeted", y=res.n.target))

addpoly(res.n.nontarget, row=37, cex=0.75, mlab=mlabfun("RE Model for nontargeted", y=res.n.nontarget), #col='blue'
        )
text(-8, 37, pos=4, cex=0.7, mlabfun("RE Model for nontargeted", y=res.n.nontarget))




addpoly(res.c.target, row= 25,cex=0.75, mlab=mlabfun("RE Model for targeted", y=res.c.target), #col='red'
        )
text(-8, 25, pos=4, cex=0.7, mlabfun("RE Model for targeted", y=res.c.target))

addpoly(res.c.nontarget, row= 24,cex=0.75, mlab=mlabfun("RE Model for nontargeted", y=res.c.nontarget), #col='blue'
        )
text(-8, 24, pos=4, cex=0.7, mlabfun("RE Model for nontargeted", y=res.c.nontarget))




addpoly(res.i.target, row= 14,cex=0.75, mlab=mlabfun("RE Model for targeted", y=res.i.target), #col='red'
        )
text(-8, 14, pos=4, cex=0.7, mlabfun("RE Model for targeted", y=res.i.target))

addpoly(res.i.nontarget, row= 13,cex=0.75, mlab=mlabfun("RE Model for nontargeted", y=res.i.nontarget), #col='blue'
        )
text(-8, 13, pos=4, cex=0.7, mlabfun("RE Model for nontargeted", y=res.i.nontarget))


addpoly(res.s.target, row= 1,cex=0.75, mlab=mlabfun("RE Model for targeted", y=res.s.target), #col='red'
        )
text(-8, 1, pos=4, cex=0.7, mlabfun("RE Model for targeted", y=res.s.target))

addpoly(res.s.nontarget, row= 0,cex=0.75, mlab=mlabfun("RE Model for nontargeted", y=res.s.nontarget), #col='blue'
        )
text(-8, 0, pos=4, cex=0.7, mlabfun("RE Model for nontargeted", y=res.s.nontarget))



### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ region4+target_status+mean_age_mpa_smr, data=dat)

### add text for the test of subgroup differences

res.target <- rma(yi,vi, method="REML", data=dat_target, slab=paste(group))
res.nontarget <- rma(yi,vi, method="REML", data=dat_nontarget, slab=paste(group))

addpoly(res.target, row= -3,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.target))
text(-8, -3, pos=4, cex=0.7, mlabfun("targeted biomass", y=res.target))

addpoly(res.nontarget, row= -4,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.nontarget))
text(-8, -4, pos=4, cex=0.7, mlabfun("nontargeted biomass", y=res.nontarget))

addpoly(res.overall, row=-2, cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.overall))
text(-8, -2, pos=4, cex=0.7, mlabfun("biomass overall", y=res.overall))

abline(h=-1)
abline(h=12, lty=1, col='#D3D3D3')
abline(h=23, lty=1, col='#D3D3D3')
abline(h=36, lty=1, col='#D3D3D3')



text(x =-0.2, y = 50, "REF",  pos=2, font=2)
text(x =1.1, y = 50, "SMR",  pos=2, font=2)
text(x =-2.1, y = 51.5, "targeted and nontargeted fish biomass 2019-20",  cex=1, pos=2, font=3)





#test moderator effect on overall biomass
(res.overall.mods <- rma(yi, vi, mods = ~ region4*mean_age_mpa_smr+target_status, data=dat))


#test moderator effect on targeted biomass
(res.target.mods <- rma(yi, vi, mods = ~ region4*mean_age_mpa_smr+region4+mean_age_mpa_smr, data=dat_target))


#test moderator effect on nontargeted biomass
(res.nontarget.mods <- rma(yi, vi, mods = ~ region4*mean_age_mpa_smr+region4+mean_age_mpa_smr, data=dat_nontarget))




