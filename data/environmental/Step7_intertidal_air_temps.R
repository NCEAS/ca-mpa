
# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)

#load data
temp_raw <- read.csv("/home/shares/ca-mpa/data/sync-data/environmental/raw/intertidal_temperature_tide_data_20230615/josh_smith_temperature_tide_data_20230615.csv")


################################################################################
#drop outliers

temp_build1 <- temp_raw %>%
                #filter tides <0 to make sure logger is exposed to air
              filter(tide_f < 0) %>%

              #check for temp outliers using z-score
              mutate(temp_zscore = (temp_c - mean(temp_c)) / sd(temp_c))

temp_build2 <- temp_build1 %>%
                #drop outliers
                filter(temp_zscore > -4 & temp_zscore <6)

#check distribution
g1 <- ggplot(temp_build1, aes(y = temp_zscore)) +
  geom_boxplot() +
  labs(x = "Sites", y = "Z-Score Distribution") +
  ggtitle("Boxplot of Z-Score Distribution, uncorrected") + theme_bw()
g1

g2 <- ggplot(temp_build2, aes(x=1, y = temp_zscore)) +
  geom_boxplot() +
  labs(x = "Sites", y = "Z-Score Distribution") +
  ggtitle("Boxplot of Z-Score Distribution, corrected \n(+/- 3 S.D. of grand mean)") + theme_bw()
g2

ggpubr::ggarrange(g1, g2, nrow=1)


################################################################################
#aggregate temp data
library(dplyr)
library(ggplot2)


temp_build3 <- temp_build2 %>%
                group_by(site_code, marine_site_name, latitude, longitude,
                         georegion, replicate, date_utc) %>%
                dplyr::summarise(rep_mean_c = mean(temp_c))%>%
                group_by(site_code, date_utc, marine_site_name, latitude, longitude,
                         georegion)%>%
                dplyr::summarise(daily_mean_c = mean(rep_mean_c))

#Filter the temp_build3 dataset to include only the relevant data before and up to 2012:
baseline_data <- temp_build3 %>%
  filter(year(date_utc) <= 2012)

# Calculate the observed mean for each month and year at each site
observed_mean <- temp_build3 %>%
  mutate(year_month = format(as.Date(as.character(date_utc)), "%Y-%m"),
         year = format(as.Date(as.character(date_utc)), "%Y"),
         month = format(as.Date(as.character(date_utc)), "%m")) %>%
  group_by(georegion, site_code, marine_site_name, year, month, year_month) %>%
  summarize(observed_monthly_mean = mean(daily_mean_c)) %>%
  ungroup()

# Calculate the baseline average for each month up to 2012
baseline_average <- baseline_data %>%
  mutate(month = format(as.Date(as.character(date_utc)), "%m")) %>%
  group_by(site_code, month) %>%
  summarize(baseline_average = mean(daily_mean_c, na.rm = TRUE)) %>%
  ungroup()

# Determine the monthly anomaly for each observed monthly mean
monthly_anomaly <- observed_mean %>%
  left_join(baseline_average, by = c("month", "site_code")) %>%
  mutate(monthly_mean_anomaly = observed_monthly_mean - baseline_average) 

# Calculate the mean annual observed temp
annual_obs <- observed_mean %>% group_by(georegion, site_code, marine_site_name,
                                         year)%>%
  mutate(year = as.numeric(year))%>%
  summarize(annual_mean_observed = mean(observed_monthly_mean, na.rm=TRUE))

# Calculate the mean annual anomaly across the monthly anomalies for each year
annual_anomaly <- monthly_anomaly %>%
  mutate(year = as.numeric(substr(year_month, 1, 4))) %>%
  group_by(georegion, site_code, marine_site_name, year) %>%
  summarize(mean_annual_anomaly = mean(monthly_mean_anomaly,na.rm=TRUE))%>%
  left_join(annual_obs, by=c("year","georegion","site_code","marine_site_name"))%>%
  rename(AT_anom = mean_annual_anomaly,
         AT_abs = annual_mean_observed) %>%
  mutate_all(~ ifelse(. == "NaN", NA, .)) 

saveRDS(annual_anomaly, "/home/shares/ca-mpa/data/sync-data/environmental/processed/air_temp_intertidal_sites.rds")



################################################################################
#plot monthly mean anomalies

# Extract year from year_month
monthly_temp <- monthly_anomaly %>%
  filter(georegion == "CA Central")

saveRDS(monthly_temp, here::here("analyses","5community_climate_ecology","output","monthly_air_temp_central_coast.rds"))

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

library(ggplot2)

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
g <- ggplot() +
  geom_point(data = monthly_temp %>% filter(year >2006 & year < 2020), aes(x = year_month, y = monthly_mean_anomaly), alpha = 0.1) +
  geom_ribbon(data = yearly_stats %>% filter(year >2006 & year < 2021), aes(x = as.Date(paste0(year, "-01-01")), ymin = mean_anomaly - se, ymax = mean_anomaly + se), fill = "black", alpha = 0.3) +
  geom_line(data = yearly_stats %>% filter(year >2006 & year < 2021), aes(x = as.Date(paste0(year, "-01-01")), y = mean_anomaly), color = "black", group = 1) +
  labs(x = "Year", y = "Monthly Mean Anomaly") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous() +
  theme_bw() + my_theme



