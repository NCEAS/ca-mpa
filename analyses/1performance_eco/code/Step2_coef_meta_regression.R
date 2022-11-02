
#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "3/30/2022"
#revised: "10/24/2022

rm(list=ls())

#required packages

library(ggplot2)
library(metafor)
library(dplyr)
library(tidyr)
library(reshape2)
require(ggpubr)
require(plyr)
require(Rmisc)
require(ggpmisc)
require(ggtext)
require(nlme)
require(purrr)
require(tidyr)
require(broom)
require(forestmangr)

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
  filter( target_status=='targeted',
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

targeted_dat <- as.data.frame(targeted_wide) %>%
                mutate(affiliated_mpa = as.factor(affiliated_mpa),
                       region4 = as.factor(region4),
                       mpa_age = as.numeric(mpa_age),
                       year= as.factor(year))%>%
             mutate_if(is.numeric, list(~na_if(., Inf))) %>% 
              mutate_if(is.numeric, list(~na_if(., -Inf)))%>%
            filter(!(affiliated_mpa=="begg rock smr"))


################################################################################
#examine data
DataExplorer::plot_histogram(targeted_dat)

psych::pairs.panels(targeted_dat)

lw <- length_weight("Oreochromis niloticus")


################################################################################
#perform regression
fitted_models <- targeted_dat %>%
  nest(data = -group) %>%
  mutate(
    mod = map(data, ~lm(RR ~ mpa_age + size_km2 + lat , data = .x)),
    tidied = map(mod, conf.int=TRUE, tidy))%>%
  unnest(tidied)

lm_model_out <- round_df(fitted_models, digits=3)
lm_model_out1 <- lm_model_out %>% filter(!(term=="(Intercept)")) #remove the intercept 



################################################################################
#analyze partial regressions
#lm_kelp <- targeted_dat %>% filter(group=='kelp')
#kelp_partial <- avPlots(lm(RR~mpa_age + size_km2 + lat , data = lm_kelp))

#lm_ccfrp <- targeted_dat %>% filter(group=='ccfrp')
#ccfrp_partial <- avPlots(lm(RR~mpa_age + size_km2 + lat , data = lm_ccfrp))

#lm_deep_reef <- targeted_dat %>% filter(group=='deep reef')
#deep_reef_partial <- avPlots(lm(RR~mpa_age + size_km2 + lat , data = lm_deep_reef))

#lm_surf <- targeted_dat %>% filter(group=='surf')
#surf_partial <- avPlots(lm(RR~mpa_age + size_km2 + lat , data = lm_surf))



#lm_model_out1$group = with(lm_model_out1, factor(group, levels=group[order(ave(estimate, term, FUN=min),estimate)]))


################################################################################
#compute meta regression for overall pooled coefficients

#rma 
targeted_dat$V <- 0
meta_regress <- rma.mv(RR, V, mods = ~ mpa_age + size_km2 + lat, 
                       random = ~ 1| group,
                    data = targeted_dat)
summary(meta_regress)

forest(meta_regress)


#mixed effects model with random ~ group
lm_mix <- lme4::lmer(RR ~ mpa_age + size_km2 + lat + (1|group) , 
                  
                     data = targeted_dat)



CI_int <- as.data.frame(confint(lm_mix)) %>%
          janitor::clean_names()%>%
          tibble::rownames_to_column()%>%
          mutate(term = rowname,
                 CI_lower = x2_5_percent,
                 CI_upper = x97_5_percent)%>%
          filter(!(c(term == '.sig01'|
                       term == '.sigma'|
                       term == '(Intercept)')))
CI_int$term <- recode_factor(CI_int$term, "mpa_age"="MPA age")
CI_int$term <- recode_factor(CI_int$term, "size_km2"="MPA size")
CI_int$term <- recode_factor(CI_int$term, "lat"="latitude")




################################################################################
#Build rma for coefficients

#latitude
rma_lat <- rma(yi=estimate, sei=std.error, method="REML", subset=(term=="lat"), verbose=TRUE, digits=5, data=lm_model_out1)
pooled_lat <- coef(summary(rma_lat)) %>%
  mutate(group="pooled",
         term="latitude",
         yi=estimate)%>%
  select(group, estimate, "conf.low"=ci.lb, "conf.high"=ci.ub, term)

#mpa_age
rma_age <- rma(yi=estimate, sei=std.error, method="REML", subset=(term=="mpa_age"), verbose=TRUE, digits=5, data=lm_model_out1)
pooled_age <- coef(summary(rma_age)) %>%
  mutate(group="pooled",
         term="MPA age",
         yi=estimate)%>%
  select(group, estimate, "conf.low"=ci.lb, "conf.high"=ci.ub, term)


#size_km2
rma_size <- rma(yi=estimate, sei=std.error, method="REML", subset=(term=="size_km2"), verbose=TRUE, digits=5, data=lm_model_out1)
pooled_size <- coef(summary(rma_size)) %>%
  mutate(group="pooled",
         term="MPA size",
         yi=estimate)%>%
  select(group, estimate, "conf.low"=ci.lb, "conf.high"=ci.ub, term)



################################################################################

################################################################################
# prepare for plotting

#sort
lm_model_plot <- rbind.fill(lm_model_out1, pooled_lat, pooled_age, pooled_size)
lm_model_plot1 <- lm_model_plot %>%
                  mutate(sorting_var = ifelse(group=="pooled","pooled","coef"))

#define colors
group.colors <- c('deep reef' = "#1B9E77", ccfrp = "#D95F02", kelp ="#7570B3", surf = "#E7298A", pooled = "black")
group.labs <- expression("deep reef", "ccfrp","kelp","surf",
                         italic("pooled"))
group.shape <- c('deep reef'= 16, ccfrp=16, kelp=16, surf=16, pooled = 18)

#clean up names
lm_model_plot1$term <- recode_factor(lm_model_plot1$term, "lat"="latitude")
lm_model_plot1$term <- recode_factor(lm_model_plot1$term, "mpa_age"="MPA age")
lm_model_plot1$term <- recode_factor(lm_model_plot1$term, "size_km2"="MPA size")

#add p-val
lm_model_plot2 <- lm_model_plot1 %>%
  mutate(p_label = case_when(
    p.value > 0.05 ~ "",
    p.value > 0.01 ~ "*",
    p.value > 0.001 ~ "**")
  )

lm_model_plot3 <- as.data.frame(lm_model_plot2) %>%
                  arrange(term, sorting_var, desc(estimate))%>%
                  mutate(group = as.factor(group),
                         term= as.factor(term),
                         sorting_num = seq(1:nrow(.)))
                  
#lock in factor level ordering
lm_model_plot3$sorting_num <- factor(lm_model_plot3$sorting_num, levels = lm_model_plot3$sorting_num)

A <- lm_model_plot3 %>%
  ggplot(aes(x=tidytext::reorder_within(term, as.numeric(desc(sorting_num)), group), y=estimate, color=group)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                width = 0,size  = 0.5,
                position = position_dodge(width=0.5), stat="identity") +
  geom_hline(yintercept = 0, color = "black", size = 0.5, linetype="dashed") +
  geom_point(position=position_dodge(width=0.5), size=1.5) + 
  geom_text(aes(label=p_label), nudge_x = 0.15, size=5, show.legend = FALSE)+
  #scale_x_discrete(expand = c(0.2, 0.1) )+
  coord_flip() +
  theme_bw()+
  scale_x_discrete(breaks = c('size_km2',
                              'lat',
                              'mpa_age'),
                   labels = c('MPA size (km2)',
                              'latitude',
                              'MPA age'))+
  xlab("")+
  ylab("Beta coefficients with 95% CI")+
  ggtitle("Targeted fish biomass")+
  theme(legend.title=element_blank())+
  theme_bw(base_size = 8)+
  scale_fill_manual(values=c(group.colors))+
  #scale_shape_manual(values = c(16,16,16,16,22))+
  scale_color_manual(name="",
                     values=c(group.colors),
                     labels = group.labs)+
  facet_wrap(~term, ncol=1, scales="free_y")



biomass_coef <- ggarrange(A, B, common.legend = TRUE)


#ggsave(here::here("analyses", "1performance_eco", "figures", 
#"Fig2.1_coef_meta_regress_biomass_2019.png"), biomass_coef, width=6, height=4,
#bg="white",dpi = 600, units = "in")


###############################################################################

###############################################################################

###############################################################################

#RESAMPLING APPROACH --- WORKING

#create boot fun

bootfun <- function(data, i){
  d <- data[i,]
  fit <- lm(RR ~ mpa_age + size_km2 + lat , data = d)
  coef(fit)
}

set.seed(1985)
R<- 5000
b_list <- by(targeted_dat, targeted_dat$group, \(X) {
  boot(X, bootfun, R#, strata = X$affiliated_mpa
       )
}
)


npars <- ncol(b_list$'deep reef'$t)
ci_list <- lapply(b_list, \(group) {
  ci <- lapply(seq.int(npars), \(index) {
    boot.ci(group, index = index, type = c("norm","basic", "perc", "bca"))
  })
  setNames(ci, c("Intercept", "mpa_age", "size_km2","lat"))
})



lapply(b_list, boot.ci)




set.seed(1985)
R<- 1000
b_list <- lapply( unique(targeted_dat$group), \(X) {   
  eval(bquote(boot(df(targeted_dat$group==.(X), 
                                bootfun, R, 
                                strata = df[df$group==.(X),]$subgroup)))) })







#bootstrap
bootfun <- function(x, i) {
  xSub <- x[i, ] #resample x
  LnRR <- log(xSub[, 1]/xSub[ ,2])
  return(mean(LnRR))
}


b <- boot(targeted_dat, bootfun, R=999)
plot(b) #always worth looking at

#Calculate ci's
boot.ci(b)










test <- lm(RR ~ lat, data=targeted_dat)

b <- test$coefficients

plot(RR ~ lat, data=targeted_dat)
abline(test)


res.List <- nlme::lmList(RR ~ landings_lb_sqkm_20002006 | affiliated_mpa, data=targeted_dat, 
                         na.action=na.omit)


plot(augPred(res.List), grid=TRUE)



summary(res.List)

plot(res.List, grid=TRUE)

#res1 <- lme(as.numeric(RR) ~ as.factor(mpa_age), random= ~ as.factor(mpa_age) | as.factor(affiliated_mpa), data=targeted_wide,
#            na.action=na.exclude)

res.list <- nlme::lmList(distance ~ age | Subject, data=Orthodont)
summary(res.list)
plot(res.list, grid=TRUE)

plot(augPred(res.list), grid=TRUE)

plot(res.list, distance ~ fitted(.) | Subject, abline = c(0,1))


dat <- as.data.frame(Orthodont)








