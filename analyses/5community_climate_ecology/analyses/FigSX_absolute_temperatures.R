
# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
basedir <- here::here("analyses","5community_climate_ecology","output")
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read data
envi_orig <- readRDS(file.path(basedir, "envr_anomalies_at_mpas_new.Rds")) 
monthly_temp <- readRDS(file.path(basedir, "monthly_air_temp_central_coast.rds")) 

###############################################################################
#Process data
# Process data
envi <- envi_orig %>%
  # Filter
  filter(region3=='central' & year>=2002 & year <= 2020)%>%
  # Summarize by year and month
  group_by(year, month)%>%
  summarise(beuti_abs = mean(beuti_monthly_obs, na.rm=T),
            cuti_abs = mean(cuti_monthly_obs, na.rm=T),
            sst_abs = mean(sst_monthly_obs, na.rm=T),
            bottomT_abs = mean(as.numeric(as.character(bottomT_monthly_obs)),na.rm=T),
            quarterly_MOCI = mean(quarterly_MOCI, na.rm=T))  %>% 
  ungroup() %>% 
  # Gather
  gather(key="indicator", value="value", 3:ncol(.)) %>% 
  # Format indicator name
  mutate(indicator=recode(indicator, 
                          "beuti_abs"="BEUTI",
                          "cuti_abs"="CUTI",
                          "sst_abs"="SST",
                          "bottomT_abs" = "Bottom temp",
                          "quarterly_MOCI"="MOCI"
                          )) %>% 
  # Summarize by year
  group_by(indicator, year) %>% 
  summarize(value_avg=mean(value),
            value_lo=value_avg-1.96*sd(value)/sqrt(length(value)),
            value_hi=value_avg+1.96*sd(value)/sqrt(length(value))) %>% 
  # value_lo=quantile(value, probs=0.025, na.rm=T),
  # value_hi=quantile(value, probs=0.975, na.rm=T)) %>% 
  ungroup() %>% 
  # Remov missing value
  filter(!is.na(value_avg))

envi_dat <- envi_orig %>%
  # Filter
  filter(region3=='central' & year>=2002 & year <= 2020)%>%
  mutate( date_order = as.numeric(paste0(year,".",month))
          #Date = with(., sprintf("%d-%02d", year, month))
  ) %>%
  filter(date_order <= 2020.5) 

#Process air temp
# Convert "year" and "month" to Date format
monthly_temp$year_month <- as.Date(paste0(monthly_temp$year, "-", monthly_temp$month, "-01"))

yearly_stats <- monthly_temp %>%
  group_by(year) %>%
  summarize(
    mean_temp = mean(observed_monthly_mean),
    se = sd(observed_monthly_mean) / sqrt(n()),
    value_lo=mean_temp-1.96*se,
    value_hi=mean_temp+1.96*se)

#Date = with(., sprintf("%d-%02d", year, month))


# Plot air temp anomalies
################################################################################
# Theme
base_theme <-  theme(axis.text=element_text(size=7,colour = "black"),
                     axis.title=element_text(size=8,colour = "black"),
                     legend.text=element_text(size=7,colour = "black"),
                     legend.title=element_text(size=8,colour = "black"),
                     plot.tag=element_text(size=8,colour = "black"),
                     # Gridlines
                     panel.grid.major = element_blank(), 
                     panel.grid.minor = element_blank(),
                     panel.background = element_blank(), 
                     axis.line = element_line(colour = "black"),
                     # Legend
                     legend.key = element_rect(fill=alpha('blue', 0)),
                     legend.background = element_rect(fill=alpha('blue', 0)))

# Create the plot
#AT
ymax <- max(monthly_temp$observed_monthly_mean, na.rm = TRUE) 
g1 <- ggplot() +
  geom_point(data = monthly_temp %>% filter(year > 2002 & year < 2020),
             aes(x = as.numeric(year) + (as.numeric(month) - 1)/12, y = observed_monthly_mean),
             alpha = 0.6, color = 'lightgray', position = position_nudge(x = 0.015)) +
  geom_ribbon(data = yearly_stats %>% filter(year > 2002 & year < 2020), mapping=aes(x=as.numeric(year), ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line(data = yearly_stats %>% filter(year > 2002 & year < 2020),
            aes(x = as.numeric(year), y = mean_temp),
            color = "black", group = 1) +
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x=2008, y=ymax , size=2.5) +
  annotate("segment", x = 2010.5, y = ymax, xend = 2013.5, yend = ymax,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  labs(x = "Year", y = "Air temp (°C)",tag="A") +
  scale_x_continuous(breaks = seq(2000, 2020, 5)) +
  scale_y_continuous() +
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        #plot.tag.position = c(0.05, 0.98)
        )

g1

# SST
#ymax <- envi %>% filter(indicator=="SST") %>% pull(value_hi) %>% max() * 1.05   ##Chris
ymax <- max(envi_dat$sst_monthly_obs, na.rm=TRUE) 
g2 <- ggplot(data=envi %>% filter(indicator=="SST"), aes(x=year, y=value_avg)) + 
  #data series
  geom_point(data=envi_dat, aes(x=date_order, y=sst_monthly_obs), size=1, alpha=0.1, color='lightgray')+
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x=2008, y=ymax , size=2.5) +
  annotate("segment", x = 2010.5, y = ymax, xend = 2013.5, yend = ymax,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  # Plot data
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line() +
  #add data points
  # Labels
  labs(x="", y="Sea surface temp (°C)", tag="B") +
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        #plot.tag.position = c(0.05, 0.98)
        )
g2


# bottomT
#ymax <- envi %>% filter(indicator=="Bottom temp") %>% pull(value_hi) %>% max() * 1.05 #Chris
ymax <- max(envi_dat$bottomT_monthly_obs, na.rm=TRUE) 
g3 <- ggplot(data=envi %>% filter(indicator=="Bottom temp"), aes(x=year, y=value_avg)) + 
  #data series
  geom_point(data=envi_dat, aes(x=date_order, y=bottomT_monthly_obs), size=1, alpha=0.1, color='lightgray')+
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x=2008, y=ymax , size=2.5) +
  annotate("segment", x = 2010.5, y = ymax, xend = 2013.5, yend = ymax,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  # Plot data
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line() +
  # Labels
  labs(x="", y="Sea bottom temp (°C)", tag="C") +
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        #plot.tag.position = c(0.05, 0.98)
        )
g3


# BEUTI
#ymax <- envi %>% filter(indicator=="BEUTI") %>% pull(value_hi) %>% max() * 1.05 #Chris
ymax <- max(envi_dat$beuti_monthly_obs, na.rm=TRUE) 
g4 <- ggplot(data=envi %>% filter(indicator=="BEUTI"), aes(x=year, y=value_avg)) + 
  #data series
  geom_point(data=envi_dat, aes(x=date_order, y=beuti_monthly_obs), size=1, alpha=0.1, color='lightgray')+
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x=2008, y=ymax , size=2.5) +
  annotate("segment", x = 2010.5, y = ymax, xend = 2013.5, yend = ymax,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  # Plot data
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line() +
  # Labels
  #labs(x = "", y = expression("BEUTI anomaly\n"~(mmol~m^{-1}~s^{-1})), tag="E")+
  labs(x = "", y = "BEUTI (mmol m\u207B\u00B9 s\u207B\u00B9)", tag="D")+
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0),
        #plot.tag.position = c(0.05, 0.98)
        )
g4


g <- ggpubr::ggarrange(g1,g2,g3,g4, ncol=2, nrow=2)

g_final <- annotate_figure(g, bottom = textGrob("Year", vjust=-0.5, hjust=0))


ggsave(g_final, filename=file.path(plotdir, "FigSX_absolute_temps.png"), 
       width=6, height=6, units="in", dpi=600)


