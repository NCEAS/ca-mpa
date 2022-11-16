#Joshua G. Smith
#November 9, 2022

rm(list=ls())

#required packages
require(dplyr)
require(vegan)
require(mvabund)
require(lme4)
require(lmerTest)
require(emmeans)

#set directories
datadir <- "/home/shares/ca-mpa/data/sync-data/monitoring/processed_data/community_climate_derived_data"
figdir <- here::here("analyses", "5community_climate_ecology", "figures")
outdir <- 
  
#read data
load(file.path(datadir,"all_groups_mpa_level_means_long.rda"))

#tidy up
CCFRP_join1[CCFRP_join1 == ""] <- NA
kelp_fish_join1[kelp_fish_join1 == ""] <- NA
kelp_combined_join1[kelp_combined_join1 == ""] <- NA
deep_reef_join1[deep_reef_join1 == ""]<- NA

################################################################################
#calculate %abund by affinity for each year


#step 1, calculate grand mean for each species, for each year, and thermal affin


CCFRP_affin_mean_total <- CCFRP_join1 %>% 
               filter(year >= 2007)%>%
               filter(!(is.na(thermal_affinity)))%>%
               group_by(year, species, thermal_affinity)%>%
               dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
               group_by(year, thermal_affinity)%>%
               dplyr::summarize(group_total = sum(mean))

kelp_fish_affin_mean_total <- kelp_fish_join1 %>% 
  filter(year >= 2007)%>%
  filter(!(is.na(thermal_affinity)))%>%
  group_by(year, species, thermal_affinity)%>%
  dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
  group_by(year, thermal_affinity)%>%
  dplyr::summarize(group_total = sum(mean))

kelp_combined_affin_mean_total <- kelp_combined_join1 %>% 
  filter(year >= 2007)%>%
  filter(!(is.na(thermal_affinity)))%>%
  group_by(year, species, thermal_affinity)%>%
  dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
  group_by(year, thermal_affinity)%>%
  dplyr::summarize(group_total = sum(mean))

deep_reef_mean_total <- deep_reef_join1 %>% 
  filter(year >= 2007)%>%
  filter(!(is.na(thermal_affinity)))%>%
  group_by(year, species, thermal_affinity)%>%
  dplyr::summarize(mean = mean(counts, na.rm = TRUE))%>%
  group_by(year, thermal_affinity)%>%
  dplyr::summarize(group_total = sum(mean))

################################################################################
#plot

my_theme <-  theme(axis.text=element_text(size=7),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=8, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   legend.background = element_rect(fill=alpha('blue', 0)))


color_set <- c("cold temperate" = "#80B1D3","cosmopolitan" = "#BEBADA",
               "subtropical" ="#FDB462", 
               "tropical" = "#FB8072",
               "warm temperate"= "#8DD3C7")

every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}


g1 <- ggplot(CCFRP_affin_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", title="Rocky reef fish") +
  guides(fill=guide_legend(title="Thermal affinity"))+
  scale_x_discrete(breaks=every_nth(n = 2))


g2 <- ggplot(kelp_fish_affin_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", title="Kelp forest fish")+
  guides(fill=guide_legend(title="Thermal affinity"))+
  scale_x_discrete(breaks=every_nth(n = 2))

g3 <- ggplot(kelp_combined_affin_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", title="Kelp forest inverts and algae")+
  guides(fill=guide_legend(title="Thermal affinity"))+
  scale_x_discrete(breaks=every_nth(n = 2))

g4 <- ggplot(deep_reef_mean_total, aes(x = factor(year), y = group_total, fill = thermal_affinity)) + 
  geom_bar(position = "fill",stat = "identity") +
  # or:
  # geom_bar(position = position_fill(), stat = "identity") 
  scale_y_continuous(labels = scales::percent_format())+
  scale_fill_manual(values=color_set)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", title="Deep reef fish")+
  guides(fill=guide_legend(title="Thermal affinity"))




# Merge plots
g <- ggpubr::ggarrange(g1, g2, g3, g4, nrow=2,ncol=2,common.legend=TRUE,
                       legend="right")
g

g_title<- ggpubr::annotate_figure(g, left = textGrob("Perc. of total abundance", 
                                                     rot = 90, vjust = 2, gp = gpar(cex = 0.7)),
                                  bottom = textGrob("Year", hjust=2, vjust=-2, gp = gpar(cex = 0.7)))

# Export
ggsave(g_title, filename=file.path(figdir, "spp_affinities_perc.png"), 
     width=6.5, height=4.5, units="in", dpi=600, bg="white")





################################################################################
#prep data for mixed model

CCFRP_mod <- CCFRP_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_designation)),
                                    year = as.numeric(year),
                                    MHW = factor(MHW, levels=c("before","during","after")))%>% #set reference level
                            filter(!(is.na(thermal_affinity)))
kelp_fish_mod <- kelp_fish_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_defacto_designation)),
                                            year = as.numeric(year),
                                            MHW = factor(MHW, levels=c("before","during","after")))%>% #set reference level
                            filter(!(is.na(thermal_affinity)))
kelp_invalg_mod <- kelp_combined_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_defacto_designation)),
                                            year = as.numeric(year),
                                            MHW = factor(MHW, levels=c("before","during","after")))%>% #set reference level
                            filter(!(is.na(thermal_affinity)))
deep_reef_mod <- deep_reef_join1 %>% mutate(site = as.factor(paste(affiliated_mpa,mpa_defacto_designation)),
                                            year = as.numeric(year),
                                            MHW = factor(MHW, levels=c("before","during","after")))%>% #set reference level
                            filter(!(is.na(thermal_affinity)))


################################################################################
#build mixed models

#build linear mixed model
library("lme4")
library("lmerTest")

########### CCFRP

#build model
ccfrp_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), data = CCFRP_mod)
ccfrp_sig<- anova(ccfrp_mixed)

#examine least square means
ccfrp_ls <- emmeans(ccfrp_mixed, specs = pairwise ~ MHW|thermal_affinity, adjust = "tukey") 
ccfrp_ef <- eff_size(ccfrp_ls, sigma = sigma(ccfrp_mixed), edf = Inf, method="identity")

########### kelp fish

#build model
kelp_fish_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), data = kelp_fish_mod)
kelp_fish_sig<- anova(kelp_fish_mixed)

#examine least square means
kelp_fish_ls <- emmeans(kelp_fish_mixed, specs = pairwise ~ MHW|thermal_affinity, adjust = "tukey") 
kelp_fish_ef <- eff_size(kelp_fish_ls, sigma = sigma(kelp_fish_mixed), edf = Inf, method="identity")

########### kelp inverts and algae

#build model
kelp_invalg_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), data = kelp_invalg_mod)
kelp_invalg_sig<- anova(kelp_invalg_mixed)

#examine least square means
kelp_invalg_ls <- emmeans(kelp_invalg_mixed, specs = pairwise ~ MHW|thermal_affinity, adjust = "tukey")
kelp_invalg_ef <- eff_size(kelp_invalg_ls, sigma = sigma(kelp_invalg_mixed), edf = Inf, method="identity")

########### deep reef

#build model

deep_reef_mixed <- lmer(counts ~ year+MHW+thermal_affinity+MHW*thermal_affinity + (1 | site), 
                        data = deep_reef_mod)

deep_reef_sig<- anova(deep_reef_mixed)

#examine least square means
library(emmeans)

#deep_reef_ls <- emmeans(deep_reef_mixed, specs = pairwise ~ MHW:thermal_affinity, adjust = "tukey") #pairwise across
deep_reef_ls <- emmeans(deep_reef_mixed, specs = pairwise ~ MHW|thermal_affinity, adjust = "tukey") #compare within heatwave period

deep_reef_ef <- eff_size(deep_reef_ls, sigma = sigma(deep_reef_mixed), edf = Inf, method="identity")

################################################################################
#collect ls means


# https://aosmith.rbind.io/2019/03/25/getting-started-with-emmeans/

collect_fun <- function(contrast_dat, group){
                contrast_dat$contrasts %>%
                summary(infer = TRUE)%>%
                filter(contrast == 'before - during'|
                  contrast == 'before - after'|
                    contrast == 'during - after')%>%
                mutate(group = group)
}

ccfrp_constrast <- collect_fun(ccfrp_ls, "Rocky reef fish")
kelp_fish_contrast <- collect_fun(kelp_fish_ls, "Kelp forest fish")
kelp_invalg_contrast <- collect_fun(kelp_invalg_ls, "Kelp forest inverts and algae")
deep_reef_contrast <- collect_fun(deep_reef_ls, "Deep reef fish") %>%
                        rename("asymp.LCL"=lower.CL,
                               "asymp.UCL"=upper.CL,
                               "z.ratio"=t.ratio)


contrast_results <- rbind(ccfrp_constrast, kelp_fish_contrast, kelp_invalg_contrast,
                          deep_reef_contrast)%>%
  mutate(sig_level = ifelse(p.value <= 0.05, "*",
                            ifelse(p.value <=0.01,"**",
                                   ifelse(p.value <= 0.001,"*",""))),
         thermal_affinity = factor(thermal_affinity, 
                                   levels = c("cold temperate",
                                              "warm temperate",
                                              "subtropical",
                                              "tropical",
                                              "cosmopolitan")),
         group = factor(group, level=c(
           "Rocky reef fish", "Deep reef fish",
           "Kelp forest fish","Kelp forest inverts and algae"
         )))






es_fun <- function(contrast_dat, group){
  contrast_dat %>%
    summary(infer = TRUE)%>%
    filter(contrast == '(before - during)'|
             contrast == '(before - after)'|
             contrast == '(during - after)')%>%
    mutate(group = group)
}

ccfrp_es <- es_fun(ccfrp_ef, "Rocky reef fish")
kelp_fish_es <- es_fun(kelp_fish_ef, "Kelp forest fish")
kelp_invalg_es <- es_fun(kelp_invalg_ef, "Kelp forest inverts and algae")
deep_reef_es <- es_fun(deep_reef_ef, "Deep reef fish") %>%
  rename("asymp.LCL"=lower.CL,
         "asymp.UCL"=upper.CL,
         "z.ratio"=t.ratio)

es_results <- rbind(ccfrp_es, kelp_fish_es, kelp_invalg_es,
                          deep_reef_es)%>%
  mutate(sig_level = ifelse(p.value <= 0.05, "*",
                            ifelse(p.value <=0.01,"**",
                                   ifelse(p.value <= 0.001,"*",""))),
         thermal_affinity = factor(thermal_affinity, 
                                   levels = c("cold temperate",
                                              "warm temperate",
                                              "subtropical",
                                              "tropical",
                                              "cosmopolitan")),
         group = factor(group, level=c(
           "Rocky reef fish", "Deep reef fish",
           "Kelp forest fish","Kelp forest inverts and algae"
         )),
         contrast = gsub("[()]", "", contrast))
        




################################################################################
#plot


my_theme <-  theme(axis.text=element_text(size=6),
                   axis.text.y = element_text(angle = 90, hjust = 0.5),
                   axis.title=element_text(size=8),
                   plot.tag=element_blank(), #element_text(size=8),
                   plot.title =element_text(size=6, face="bold"),
                   # Gridlines
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.text=element_text(size=5),
                   legend.title=element_text(size=7),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   #facets
                   strip.text = element_text(size=6)
                   )


color_set <- c("cold temperate" = "#80B1D3","cosmopolitan" = "#BEBADA",
               "subtropical" ="#FDB462", 
               "tropical" = "#FB8072",
               "warm temperate"= "#8DD3C7")
library(stringr)


g1 <- es_results %>% filter(group == "Rocky reef fish")%>%
  ggplot(aes(x = thermal_affinity, y = effect.size, color=contrast)) + 
  geom_point(position = position_dodge(width=0.7), 
             stat="identity", size=1) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),
                position = position_dodge(width=0.7), width=0, size=0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_text(aes(label=sig_level), size=4, hjust=-0.4, vjust=0.8,
            position = position_dodge(width=0.7),
            show.legend = FALSE)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", 
       title="Rock reef fish",
       color = "Contrast") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=10))+
  scale_y_continuous(limits=c(-1,0.8))+
  scale_color_brewer(palette = "Dark2")

g2 <- es_results %>% filter(group == "Deep reef fish")%>%
  ggplot(aes(x = thermal_affinity, y = effect.size, color=contrast)) + 
  geom_point(position = position_dodge(width=0.7), 
             stat="identity", size=1) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),
                position = position_dodge(width=0.7), width=0, size=0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_text(aes(label=sig_level), size=4, hjust=-0.4, vjust=0.8,
            position = position_dodge(width=0.7),
            show.legend = FALSE)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", 
       title="Deep reef fish",
       color = "Contrast") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=10))+
  scale_y_continuous(limits=c(-1,0.8))+
  scale_color_brewer(palette = "Dark2")

g3 <- es_results %>% filter(group == "Kelp forest fish")%>%
  ggplot(aes(x = thermal_affinity, y = effect.size, color=contrast)) + 
  geom_point(position = position_dodge(width=0.7), 
             stat="identity", size=1) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),
                position = position_dodge(width=0.7), width=0, size=0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_text(aes(label=sig_level), size=4, hjust=-0.4, vjust=0.8,
            position = position_dodge(width=0.7),
            show.legend = FALSE)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", 
       title="Kelp forest fish",
       color = "Contrast") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=10))+
  scale_y_continuous(limits=c(-1,0.8))+
  scale_color_brewer(palette = "Dark2")


g4 <- es_results %>% filter(group == "Kelp forest inverts and algae")%>%
  ggplot(aes(x = thermal_affinity, y = effect.size, color=contrast)) + 
  geom_point(position = position_dodge(width=0.7), 
             stat="identity", size=1) +
  geom_errorbar(aes(ymin=asymp.LCL, ymax=asymp.UCL),
                position = position_dodge(width=0.7), width=0, size=0.3
  ) +
  geom_hline(yintercept = 0, linetype = "dotted")+
  geom_text(aes(label=sig_level), size=4, hjust=-0.4, vjust=0.8,
            position = position_dodge(width=0.7),
            show.legend = FALSE)+
  theme_bw()+my_theme+
  labs(x="", 
       y="", tag="F", 
       title="Kelp forest inverts and algae",
       color = "Contrast") +
  scale_x_discrete(labels = function(x) str_wrap(x, width=10))+
  scale_y_continuous(limits=c(-1,0.8))+
  scale_color_brewer(palette = "Dark2")

library(grid)
g <- ggpubr::ggarrange(g1, g2, g3, g4, nrow=2, ncol=2, 
                       common.legend = TRUE, legend = "right")

g_title<- ggpubr::annotate_figure(g, left = textGrob("Standardzied estimated marginal means (Cohen)", 
                                                     rot = 90, vjust = 2, gp = gpar(cex = 0.6)),
                bottom = textGrob("Thermal affinity", hjust=1, vjust=-2, gp = gpar(cex = 0.6)))


#ggsave(g_title, filename=file.path(figdir, "spp_affinities_emms_es.png"), 
#       width=6.5, height=4.5, units="in", dpi=600, bg="white")





