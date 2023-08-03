#title: "CA MPA Performance biomass meta analyses"
#author: "Joshua G. Smith"
#date: "8/1/2023"

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
biomass_mod <- readRDS(file.path(dat_path, "biomass_with_moderators.Rds")) %>% 
                mutate(habitat = ifelse(habitat == "Rocky reef","Shallow reef",habitat))

################################################################################
#prep data

biomass_meta_build1 <- biomass_mod %>% mutate(target_status = ifelse(habitat == "Shallow reef","Targeted",target_status),
                                      affiliated_mpa = str_to_title(affiliated_mpa) %>% 
                                        str_replace(" Smr$", " SMR") %>% 
                                        str_replace(" Smca$", " SMCA")) %>%
                #calcualte mean for each MPA across years where age > 0
                filter(age_at_survey > 0 )%>%
                group_by(habitat,state_region, affiliated_mpa, target_status) %>%
                summarize(biomass_ref = mean(biomass_ref),
                          biomass_smr = mean(biomass_smr))
                
#now since we are interested in determining a region effect, calculate the mean
#effect size by region, the n MPAs, and sd

biomass_meta_build2 <- biomass_meta_build1 %>%
                        group_by(habitat, state_region, target_status) %>%
                    summarize(n_MPA = n(),
                              biomass_smr_region = mean(biomass_smr),
                              biomass_ref_region = mean(biomass_ref),
                              sd_smr = sd(biomass_smr, na.rm=TRUE),
                              sd_ref = sd(biomass_ref, na.rm=TRUE)) 



################################################################################

#calcualte effect size for each year and MPA
dat <- escalc(measure="ROM", m1i=biomass_smr_region, m2i=biomass_ref_region, sd1i=sd_smr, 
              sd2i=sd_ref, n1i=n_MPA, n2i=n_MPA, data = biomass_meta_build2,
              slab = paste(habitat))

dat <- dat %>%
  filter(!is.na(vi))    

################################################################################
#determine state pooled effect
rma.t.overall <- rma(yi,vi, method="REML", subset=(target_status == "Targeted"), data=dat) 
res.t.overall <- rma.t.overall %>%
                  tidy() %>%
                  mutate(ci.lb = rma.t.overall$ci.lb,
                         ci.ub = rma.t.overall$ci.ub,
                  target_status = "Targeted",
                  state_region = "Pooled",
                  habitat = "Pooled effect size")

rma.n.overall <- rma(yi,vi, method="REML", subset=(target_status == "Nontargeted"), data=dat) 
res.n.overall <- rma.n.overall %>%
  tidy() %>%
  mutate(ci.lb = rma.n.overall$ci.lb,
         ci.ub = rma.n.overall$ci.ub,
         target_status = "Nontargeted",
         state_region = "Pooled",
         habitat = "Pooled effect size")

#Merge

state_es <- rbind(#res.overall, 
  res.t.overall, res.n.overall
) %>% mutate(vi = std.error^2,
             n_MPA = 1)%>% #create dummy number for merge
  dplyr::select(yi = estimate,
                vi,
                p.value,
                habitat,
                target_status,
                state_region,
                n_MPA)

################################################################################
#determine region pooled effect
rma.t.north <- rma(yi,vi, method="REML",subset=(state_region == "North Coast" & target_status == "Targeted"), data=dat) 
res.t.north <- rma.t.north %>%
  tidy() %>%
  mutate(ci.lb = rma.t.north$ci.lb,
         ci.ub = rma.t.north$ci.ub,
         target_status = "Targeted",
         state_region = "North Coast",
         habitat = "Pooled effect size")

#determine region pooled effect
rma.n.north <- rma(yi,vi, method="REML",subset=(state_region == "North Coast" & target_status == "Nontargeted"), data=dat) 
res.n.north <- rma.n.north %>%
  tidy() %>%
  mutate(ci.lb = rma.n.north$ci.lb,
         ci.ub = rma.n.north$ci.ub,
         target_status = "Nontargeted",
         state_region = "North Coast",
         habitat = "Pooled effect size")

#determine region pooled effect
rma.t.northC <- rma(yi,vi, method="REML",subset=(state_region == "North Central Coast" & target_status == "Targeted"), data=dat) 
res.t.northC <- rma.t.northC %>%
  tidy() %>%
  mutate(ci.lb = rma.t.northC$ci.lb,
         ci.ub = rma.t.northC$ci.ub,
         target_status = "Targeted",
         state_region = "North Central Coast",
         habitat = "Pooled effect size")

#determine region pooled effect
rma.n.northC <- rma(yi,vi, method="REML",subset=(state_region == "North Central Coast" & target_status == "Nontargeted"), data=dat) 
res.n.northC <- rma.n.northC %>%
  tidy() %>%
  mutate(ci.lb = rma.n.northC$ci.lb,
         ci.ub = rma.n.northC$ci.ub,
         target_status = "Nontargeted",
         state_region = "North Central Coast",
         habitat = "Pooled effect size")

#determine region pooled effect
rma.t.central <- rma(yi,vi, method="REML",subset=(state_region == "Central Coast" & target_status == "Targeted"), data=dat) 
res.t.central <- rma.t.central %>%
  tidy() %>%
  mutate(ci.lb = rma.t.central$ci.lb,
         ci.ub = rma.t.central$ci.ub,
         target_status = "Targeted",
         state_region = "Central Coast",
         habitat = "Pooled effect size")

#determine region pooled effect
rma.n.central <- rma(yi,vi, method="REML",subset=(state_region == "Central Coast" & target_status == "Nontargeted"), data=dat) 
res.n.central <- rma.n.central %>%
  tidy() %>%
  mutate(ci.lb = rma.n.central$ci.lb,
         ci.ub = rma.n.central$ci.ub,
         target_status = "Nontargeted",
         state_region = "Central Coast",
         habitat = "Pooled effect size")

#determine region pooled effect
rma.t.south <- rma(yi,vi, method="REML",subset=(state_region == "South Coast" & target_status == "Targeted"), data=dat) 
res.t.south <- rma.t.south %>%
  tidy() %>%
  mutate(ci.lb = rma.t.south$ci.lb,
         ci.ub = rma.t.south$ci.ub,
         target_status = "Targeted",
         state_region = "South Coast",
         habitat = "Pooled effect size")

#determine region pooled effect
rma.n.south <- rma(yi,vi, method="REML",subset=(state_region == "South Coast" & target_status == "Nontargeted"), data=dat) 
res.n.south <- rma.n.south %>%
  tidy() %>%
  mutate(ci.lb = rma.n.south$ci.lb,
         ci.ub = rma.n.south$ci.ub,
         target_status = "Nontargeted",
         state_region = "South Coast",
         habitat = "Pooled effect size")

#Merge

region_es <- rbind(#res.overall, 
  res.t.north, res.n.north,
  res.t.northC, res.n.northC,
  res.t.central, res.n.central,
  res.t.south, res.n.south
) %>% mutate(vi = std.error^2,
             n_MPA = 1)%>% #create dummy number for merge
  dplyr::select(yi = estimate,
                vi,
                p.value,
                habitat,
                target_status,
                state_region,
                n_MPA)

################################################################################
#determine pooled effects for each habitat and region
rma.t.surf <- rma(yi, vi, method="REML", 
                  subset=(habitat == "Surf zone" & target_status == "Targeted"),
                  verbose=TRUE, digits=5, data=dat) 
res.t.surf <- rma.t.surf %>% 
              tidy() %>%
              mutate(ci.lb = rma.t.surf$ci.lb,
                     ci.ub = rma.t.surf$ci.ub,
                     habitat = "Surf zone",
                     target_status = "Targeted",
                     state_region = "Pooled")

rma.n.surf <- rma(yi, vi, method="REML", 
                  subset=(habitat == "Surf zone" & target_status == "Nontargeted"), 
                  verbose=TRUE, digits=5, data=dat)
res.n.surf <- rma.n.surf %>%
                  tidy() %>%
                  mutate(
                    ci.lb = rma.n.surf$ci.lb,
                    ci.ub = rma.n.surf$ci.ub,
                    habitat = "Surf zone",
                      target_status = "Nontargeted",
                    state_region = "Pooled")

rma.t.kelp <- rma(yi, vi, method="REML", 
                  subset=(habitat == "Kelp forest" & target_status == "Targeted"), 
                  verbose=TRUE, digits=5, data=dat) 
res.t.kelp<- rma.t.kelp %>%
                  tidy() %>%
                  mutate(ci.lb = rma.t.kelp$ci.lb,
                         ci.ub = rma.t.kelp$ci.ub,
                        habitat = "Kelp forest",
                         target_status = "Targeted",
                        state_region = "Pooled")

rma.n.kelp <- rma(yi, vi, method="REML", 
                  subset=(habitat == "Kelp forest" & target_status == "Nontargeted"), 
                  verbose=TRUE, digits=5, data=dat) 
res.n.kelp <- rma.n.kelp%>%
                tidy() %>%
                mutate(ci.lb = rma.n.kelp$ci.lb,
                       ci.ub = rma.n.kelp$ci.ub,
                  habitat = "Kelp forest",
                       target_status = "Nontargeted",
                  state_region = "Pooled")

rma.t.rocky <- rma(yi, vi, method="REML", 
                   subset=(habitat == "Shallow reef" & target_status == "Targeted"), 
                   verbose=TRUE, digits=5, data=dat) 
res.t.rocky <- rma.t.rocky %>% 
                tidy() %>%
                mutate(ci.lb = rma.t.rocky$ci.lb,
                       ci.ub = rma.t.rocky$ci.ub,
                  habitat = "Shallow reef",
                       target_status = "Targeted",
                  state_region = "Pooled")


rma.t.deep <- rma(yi, vi, method="REML", 
                  subset=(habitat == "Deep reef" & target_status == "Targeted"), 
                  verbose=TRUE, digits=5, data=dat) 
res.t.deep <- rma.t.deep %>% 
              tidy() %>%
              mutate(ci.lb = rma.t.deep$ci.lb,
                     ci.ub = rma.t.deep$ci.ub,
                habitat = "Deep reef",
                     target_status = "Targeted",
                state_region = "Pooled")

rma.n.deep <- rma(yi, vi, method="REML", 
                  subset=(habitat == "Deep reef" & target_status == "Nontargeted"), 
                  verbose=TRUE, digits=5, data=dat) 
res.n.deep <- rma.n.deep %>%
                tidy() %>%
                mutate(ci.lb = rma.n.deep$ci.lb,
                       ci.ub = rma.n.deep$ci.ub,
                  habitat = "Deep reef",
                       target_status = "Nontargeted",
                  state_region = "Pooled")

#merge

pooled_es <- rbind(#res.overall, 
                   res.t.surf, res.n.surf,
                   res.t.kelp, res.n.kelp,
                   res.t.rocky,
                   res.t.deep, res.n.deep
                   ) 


################################################################################
#ggplot approach


#merge data for plotting
pool_dat <- pooled_es %>% 
  mutate(vi = std.error^2,
         n_MPA = 1)%>% #create dummy number for merge
  dplyr::select(yi = estimate,
                vi,
                p.value,
                habitat,
                target_status,
                state_region,
                n_MPA)

combined_dat <- bind_rows(pool_dat, dat, region_es, state_es, .id = "source") %>%
                mutate(habitat = factor(habitat),
                       target_status = factor(target_status),
                       state_region = factor(state_region),
                       significance = ifelse(p.value < 0.05, "*","")
                       )



# Theme
my_theme <-  theme(axis.text=element_text(size=6, color = "black"),
                   axis.text.y = element_text(color = "black"),
                   axis.title=element_text(size=8, color = "black"),
                   plot.tag=element_text(size= 8, color = "black"), #element_text(size=8),
                   plot.title =element_text(size=7, face="bold", color = "black"),
                   # Gridlines 
                   panel.grid.major = element_blank(), 
                   panel.grid.minor = element_blank(),
                   panel.background = element_blank(), 
                   axis.line = element_line(colour = "black"),
                   # Legend
                   legend.key = element_blank(),
                   legend.background = element_rect(fill=alpha('blue', 0)),
                   legend.key.height = unit(1, "lines"), 
                   legend.text = element_text(size = 6, color = "black"),
                   legend.title = element_text(size = 7, color = "black"),
                   #legend.spacing.y = unit(0.75, "cm"),
                   #facets
                   strip.background = element_blank(),
                   strip.text = element_text(size = 6 , face="bold", color = "black")
)


combined_dat$habitat <- factor(combined_dat$habitat, levels = c("Surf zone", "Kelp forest", "Shallow reef", "Deep reef", "Pooled effect size"))
combined_dat$state_region <- factor(combined_dat$state_region, levels = c("Pooled","South Coast", "Central Coast", "North Central Coast","North Coast"))
combined_dat$target_status <- factor(combined_dat$target_status, levels = c("Nontargeted", "Targeted"))  # Reversed order

# labels
state_labels <- c(expression(italic("Pooled")), "South Coast", "Central Coast", "North Central Coast", "North Coast")

g <- ggplot(combined_dat, aes(x = yi, y = state_region, color = target_status)) +
  geom_point(aes(size = n_MPA), shape = 15, position = position_dodge(width = 0.7)) +
  geom_errorbarh(aes(xmin = yi - 1.96 * sqrt(vi), xmax = yi + 1.96 * sqrt(vi)), 
                 position = position_dodge(width = 0.7), height = 0, alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey") +
  geom_hline(yintercept = which(levels(combined_dat$state_region) == "South Coast") - 0.5, 
             linetype = "solid", color = "black", size = 0.2) +  
  geom_text(aes(label = significance), vjust = -0.2, size = 4, show.legend=FALSE) + 
  facet_grid(habitat ~ ., scales = "free_y", space = "free_y") +  
  xlab("Effect size (log response ratio)") +
  ylab("") +
  scale_color_manual(values = c("navyblue", "indianred"),
                     name = "Target status") +  
  scale_size_continuous(name = "No. MPAs", range = c(1, 3)) +  
  scale_y_discrete(labels = state_labels) +
  theme_minimal() +
  theme(strip.text = element_text(size = 10, face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1, "lines")) +
  theme_bw() + my_theme 

g



ggsave(g, filename=file.path(fig_dir, "Fig3_habitat_meta_forestplot2.png"), bg = "white",
      width=6, height=7, units="in", dpi=600) 








