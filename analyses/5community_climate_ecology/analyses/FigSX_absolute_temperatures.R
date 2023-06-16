
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
envi_orig <- readRDS(file.path(basedir, "envr_anomalies_at_mpas_new.Rds")) # Josh
monthly_temp <- readRDS(file.path(basedir, "monthly_air_temp_central_coast.rds")) # Josh

###############################################################################
#Process data
envi <- envi_orig %>%
  # Filter
  filter(region3=='central' & year>=2000) %>%
  # Summarize by year and month
  group_by(year, month)%>%
  summarise(beuti_anom = mean(beuti_monthly_anom, na.rm=T),
            cuti_anom = mean(cuti_monthly_anom, na.rm=T),
            sst_anom = mean(sst_monthly_anom, na.rm=T),
            bottomT_anom = mean(as.numeric(as.character(bottomT_monthly_anom)),na.rm=T),
            quarterly_MOCI = mean(quarterly_MOCI, na.rm=T))  %>% 
  ungroup() %>% 
  # Gather
  gather(key="indicator", value="value", 3:ncol(.)) %>% 
  # Format indicator name
  mutate(indicator=recode(indicator, 
                          "beuti_anom"="BEUTI",
                          "cuti_anom"="CUTI",
                          "sst_anom"="SST",
                          "bottomT_anom" = "Bottom temp",
                          "quarterly_MOCI"="MOCI")) %>% 
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
  filter(region3=='central' & year>=2000)%>%
  mutate( date_order = as.numeric(paste0(year,".",month))
          #Date = with(., sprintf("%d-%02d", year, month))
  ) %>%
  filter(date_order <= 2020.5)


###############################################################################
#Process air temp
# Convert "year" and "month" to Date format
monthly_temp$year_month <- as.Date(paste0(monthly_temp$year, "-", monthly_temp$month, "-01"))

# Calculate the mean and standard error for each year
yearly_stats <- monthly_temp %>%
  group_by(year) %>%
  summarize(
    mean_anomaly = mean(monthly_mean_anomaly),
    se = sd(monthly_mean_anomaly) / sqrt(n())
  )


# Plot air temp anomalies
################################################################################
my_theme <- theme(axis.text=element_text(size=8),
                  #axis.text.y = element_text(angle = 90, hjust = 0.5),
                  axis.title=element_text(size=10),
                  plot.tag=element_blank(), #element_text(size=8),
                  plot.title =element_text(size=10, face="bold"),
                  # Gridlines
                  panel.grid.major = element_line(colour = "transparent"), 
                  panel.grid.minor = element_line(colour = "transparent"), 
                  panel.background = element_blank(), 
                  axis.line = element_line(colour = "black"),
                  axis.ticks = element_line(colour = "black"),
                  # Legend
                  legend.key = element_blank(),
                  legend.text=element_text(size=8),
                  legend.title=element_text(size=10),
                  # legend.background = element_rect(fill=alpha('blue', 0)),
                  #facets
                  strip.text = element_text(size=7, hjust=0, face="bold"),
                  strip.background = element_blank()
                  #margins
                  #plot.margin=unit(c(0.01,0.01,0.01,0.01),"cm")
)

# Convert "year" and "month" to Date format
monthly_temp$year_month <- as.Date(paste0(monthly_temp$year, "-", monthly_temp$month, "-01"))

# Calculate the mean and standard error for each year
yearly_stats <- monthly_temp %>%
  group_by(year) %>%
  summarize(
    mean_anomaly = mean(monthly_mean_anomaly),
    se = sd(monthly_mean_anomaly) / sqrt(n())
  )

# Create the plot
ymax <- max(monthly_temp$monthly_mean_anomaly, na.rm=TRUE) * .90

g <- ggplot() +
  geom_point(data = monthly_temp %>% filter(year >2006 & year < 2020), aes(x = year_month, y = monthly_mean_anomaly), alpha = 0.6, color='lightgray') +
  geom_ribbon(data = yearly_stats %>% filter(year >2006 & year < 2021), aes(x = as.Date(paste0(year, "-01-01")), ymin = mean_anomaly - se, ymax = mean_anomaly + se), fill = "black", alpha = 0.3) +
  geom_line(data = yearly_stats %>% filter(year >2006 & year < 2021), aes(x = as.Date(paste0(year, "-01-01")), y = mean_anomaly), color = "black", group = 1) +
  annotate(geom="rect", xmin = as.Date("2014-01-01"), xmax = as.Date("2016-12-31"), ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x= as.Date("2008-01-01"), y=ymax , size=2.5) +
  annotate("segment", x = as.Date("2010-06-01"), y = ymax, xend = as.Date("2013-06-01"), yend = ymax,
          arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
   labs(x = "Year", y = "Air temperature anamoly (°C)") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous() +
  theme_bw() + my_theme
g

ggsave(g, filename=file.path(plotdir, "FigSX_air_temp_anamoly.png"), 
       width=6, height=7, units="in", dpi=600)

# SST
#ymax <- envi %>% filter(indicator=="SST") %>% pull(value_hi) %>% max() * 1.05   ##Chris
ymax <- max(envi_dat$sst_monthly_anom, na.rm=TRUE) * .90 #Josh
g2 <- ggplot(data=envi %>% filter(indicator=="SST"), aes(x=year, y=value_avg)) + 
  #data series
  geom_point(data=envi_dat, aes(x=date_order, y=sst_monthly_anom), size=1, alpha=0.1, color='lightgray')+
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
  # Reference line
  geom_hline(yintercept = 0, linetype="dashed", color="grey40") +
  # Labels
  labs(x="", y="SST anamoly (°C)", tag="B") +
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        plot.tag.position = c(0.05, 0.98))
g2


# bottomT
#ymax <- envi %>% filter(indicator=="Bottom temp") %>% pull(value_hi) %>% max() * 1.05 #Chris
ymax <- max(envi_dat$bottomT_monthly_anom, na.rm=TRUE) * .90 #Josh
g3 <- ggplot(data=envi %>% filter(indicator=="Bottom temp"), aes(x=year, y=value_avg)) + 
  #data series
  geom_point(data=envi_dat, aes(x=date_order, y=bottomT_monthly_anom), size=1, alpha=0.1, color='lightgray')+
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x=2008, y=ymax , size=2.5) +
  annotate("segment", x = 2010.5, y = ymax, xend = 2013.5, yend = ymax,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  # Plot data
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line() +
  # Reference line
  geom_hline(yintercept = 0, linetype="dashed", color="grey40") +
  # Labels
  labs(x="", y="SBT anamoly (°C)", tag="C") +
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        plot.tag.position = c(0.05, 0.98))
g3

# MOCI
#ymax <- envi %>% filter(indicator=="MOCI") %>% pull(value_hi) %>% max() * 1.05 #Chris
ymax <- max(envi_dat$quarterly_MOCI, na.rm=TRUE) * .90 #Josh
g4 <- ggplot(data=envi %>% filter(indicator=="MOCI"), aes(x=year, y=value_avg)) + 
  #data series
  geom_point(data=envi_dat, aes(x=date_order, y=quarterly_MOCI), size=1, alpha=0.1, color='lightgray')+
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x=2008, y=ymax , size=2.5) +
  annotate("segment", x = 2010.5, y = ymax, xend = 2013.5, yend = ymax,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  # Plot data
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line() +
  # Reference line
  geom_hline(yintercept = 0, linetype="dashed", color="grey40") +
  # Labels
  labs(x="", y="MOCI", tag="D") +
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0, hjust = 0.5),
        plot.tag.position = c(0.05, 0.98))
g4

# BEUTI
#ymax <- envi %>% filter(indicator=="BEUTI") %>% pull(value_hi) %>% max() * 1.05 #Chris
ymax <- max(envi_dat$beuti_monthly_anom, na.rm=TRUE) * .90 #Josh
g5 <- ggplot(data=envi %>% filter(indicator=="BEUTI"), aes(x=year, y=value_avg)) + 
  #data series
  geom_point(data=envi_dat, aes(x=date_order, y=beuti_monthly_anom), size=1, alpha=0.1, color='lightgray')+
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.2) +
  annotate(geom="text", label="MHW", x=2008, y=ymax , size=2.5) +
  annotate("segment", x = 2010.5, y = ymax, xend = 2013.5, yend = ymax,
           arrow = arrow(type = "closed", length = unit(0.02, "npc")))+
  # Plot data
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line() +
  # Reference line
  geom_hline(yintercept = 0, linetype="dashed", color="grey40") +
  # Labels
  #labs(x = "", y = expression("BEUTI anomaly\n"~(mmol~m^{-1}~s^{-1})), tag="E")+
  labs(x = "", y = "BEUTI anomaly \n(mmol m\u207B\u00B9 s\u207B\u00B9)", tag="E")+
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 0),
        plot.tag.position = c(0.05, 0.98))
g5



# CUTI
ymax <- envi %>% filter(indicator=="CUTI") %>% pull(value_hi) %>% max() * 1.05
g6 <- ggplot(data=envi %>% filter(indicator=="CUTI"), aes(x=year, y=value_avg)) + 
  # Heatwave
  annotate(geom="rect", xmin=2013.5, xmax=2016.5, ymin=-Inf, ymax=Inf, fill="red", alpha=0.5) +
  annotate(geom="text", label="MHW", x=2015, y=ymax , size=2.1) +
  # Plot data
  geom_ribbon(mapping=aes(x=year, ymin=value_lo, ymax=value_hi), 
              fill="grey60", color=NA, alpha=0.8) +
  geom_line() +
  # Reference line
  geom_hline(yintercept = 0, linetype="dashed", color="grey40") +
  # Labels
  labs(x="", y="CUTI anamoly", tag="F") +
  scale_x_continuous(breaks=seq(2000, 2020, 5)) +
  # scale_y_continuous(breaks=seq(-1.5, 1.5, 0.5), lim=c(-1.5, 1.5)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.title.x=element_blank(),
        axis.text.y = element_text(angle = 90, hjust = 0.5),
        plot.tag.position = c(0.02, 1))
g6
