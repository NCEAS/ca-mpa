

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
input_file <- "targeted_nontargeted_fish_diversity.csv" 
meta.data <- read.csv(file.path(data_path, input_file))



# #PART 1 - meta analyses for diversity of targeted fish** ------------------

region.yr.means<- meta.data%>%
  filter(year=='2016' | year=='2017'|year=='2018' | year=='2019' | year =='2020',
         mpa_defacto_class=='smr',
         target_status == 'targeted'
  )


region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_defacto_class, mpa_defacto_designation,target_status)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )


dat <- escalc(measure="SMD", m1i=yr.mean_smr, m2i=yr.mean_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=region.yr.means, slab=paste(group)) 

dat <- dat %>%
  filter(!is.na(vi))    


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
       ylim=c(-3, 32),
       order=order(factor(dat$region4, level=c("south","north islands", "central","north")),dat$yi), 
       rows=c(3:7,11:13,17:21,25:28),
       mlab=mlabfun("RE Model", res),
       slab=paste(dat$group),
       #showweights = TRUE,
       #psize=1.3, 
       header="Region | Monitoring Group")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)

### add additional column headings to the plot

### add text for the subgroups
text(-8, c(29,22,14,8), pos=4, c("North",
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

addpoly(res.n, row=23.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.n))
text(-8, 23.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.n))

addpoly(res.c, row= 15.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.c))
text(-8, 15.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.c))

addpoly(res.i, row= 9.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.i))
text(-8, 9.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.i))

addpoly(res.s, row= 1,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.s))
text(-8, 1, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.s))


### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ region4, data=dat)

### add text for the test of subgroup differences
text(-8, -2.5, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                             Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                             ", p = ", .(formatC(res$pval, digits=2, format="f")))))

text(x =-0.2, y = 30.8, "REF",  pos=2, col="blue", font=2)
text(x =1.1, y = 30.8, "SMR",  pos=2, col="red ", font=2)
text(x =-4.3, y = 32.2, "targeted fish diversity 2016-20",  cex=1, pos=2, font=3)





# Diversity of nontargeted --------------------------------------------------


# #PART 1 - meta analyses for diversity of targeted fish** ------------------

region.yr.means<- meta.data%>%
  filter(year=='2016' | year=='2017'|year=='2018' | year=='2019' | year =='2020',
         mpa_defacto_class=='smr',
         target_status == 'nontargeted'
  )


region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_defacto_class, mpa_defacto_designation,target_status)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )


dat <- escalc(measure="SMD", m1i=yr.mean_smr, m2i=yr.mean_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=region.yr.means, slab=paste(group)) 

dat <- dat %>%
  filter(!is.na(vi))    


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
       ylim=c(-3, 28),
       order=order(factor(dat$region4, level=c("south","north islands", "central","north")),dat$yi), 
       rows=c(3:6,10:11,15:18,22:24),
       mlab=mlabfun("RE Model", res),
       slab=paste(dat$group),
       #showweights = TRUE,
       #psize=1.3, 
       header="Region | Monitoring Group")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)

### add additional column headings to the plot

### add text for the subgroups
text(-8, c(25,19,12,7), pos=4, c("North",
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

addpoly(res.n, row=20.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.n))
text(-8, 20.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.n))

addpoly(res.c, row= 13.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.c))
text(-8, 13.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.c))

addpoly(res.i, row= 8.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.i))
text(-8, 8.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.i))

addpoly(res.s, row= 1.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.s))
text(-8, 1.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.s))


### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ region4, data=dat)

### add text for the test of subgroup differences
text(-8, -2, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                             Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                             ", p = ", .(formatC(res$pval, digits=2, format="f")))))

text(x =-0.2, y = 27, "REF",  pos=2, col="blue", font=2)
text(x =1.1, y = 27, "SMR",  pos=2, col="red ", font=2)
text(x =-3.8, y = 28, "nontargeted fish diversity 2016-20",  cex=1, pos=2, font=3)














# All fish diversity --------------------------------------------------


# #PART 1 - meta analyses for diversity of targeted fish** ------------------

data_path <- "/home/shares/ca-mpa/data/sync-data/processed_data"
input_file <- "all_fish_diversity.csv" 
meta.data <- read.csv(file.path(data_path, input_file))




region.yr.means<- meta.data%>%
  filter(year=='2016' | year=='2017'|year=='2018' | year=='2019' | year =='2020',
         mpa_defacto_class=='smr'
  )


region.yr.means <- region.yr.means %>%
  group_by(group,region4, mpa_defacto_class, mpa_defacto_designation)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean), # standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(names_from = mpa_defacto_designation,
              values_from = c(yr.mean, sd, n)
  )


dat <- escalc(measure="SMD", m1i=yr.mean_smr, m2i=yr.mean_ref, sd1i=sd_smr, sd2i=sd_ref, n1i=n_smr, n2i=n_ref, data=region.yr.means, slab=paste(group)) 

dat <- dat %>%
  filter(!is.na(vi))    


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
       ylim=c(-3, 34),
       order=order(factor(dat$region4, level=c("south","north islands", "central","north")),dat$yi), 
       rows=c(3:7,11:13,17:21,25:29),
       mlab=mlabfun("RE Model", res),
       slab=paste(dat$group),
       #showweights = TRUE,
       #psize=1.3, 
       header="Region | Monitoring Group")

### set font expansion factor (as in forest() above) and use a bold font
op <- par(cex=0.75, font=2)

### add additional column headings to the plot

### add text for the subgroups
text(-8, c(30,22,14,8), pos=4, c("North",
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

addpoly(res.n, row=23.5, cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.n))
text(-8, 23.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.n))

addpoly(res.c, row= 15.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.c))
text(-8, 15.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.c))

addpoly(res.i, row= 9.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.i))
text(-8, 9.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.i))

addpoly(res.s, row= 1.5,cex=0.75, mlab=mlabfun("RE Model for Subgroup", y=res.s))
text(-8, 1.5, pos=4, cex=0.7, mlabfun("RE Model for Subgroup", y=res.s))


### fit meta-regression model to test for subgroup differences
res <- rma(yi, vi, mods = ~ region4, data=dat)

### add text for the test of subgroup differences
text(-8, -2, pos=4, cex=0.75, bquote(paste("Test for Subgroup Differences: ",
                                           Q[M], " = ", .(formatC(res$QM, digits=2, format="f")), ", df = ", .(res$p - 1),
                                           ", p = ", .(formatC(res$pval, digits=2, format="f")))))

text(x =-0.2, y = 32.5, "REF",  pos=2, col="blue", font=2)
text(x =1.1, y = 32.5, "SMR",  pos=2, col="red ", font=2)
text(x =-4.8, y = 34, "all fish diversity 2016-20",  cex=1, pos=2, font=3)


