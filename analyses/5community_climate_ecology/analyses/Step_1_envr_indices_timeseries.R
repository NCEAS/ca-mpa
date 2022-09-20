#Joshua G. Smith
#August 17, 2022


#clear workspace
rm(list=ls())

#load packages
require(ggplot2)
require(dplyr)

#read envr data
envr_vars_all <- readRDS("/home/shares/ca-mpa/data/sync-data/environmental/processed/envr_anomalies_at_mpas.Rds")



# Aggregate and filter by region ------------------------------------------

envr_plot <- envr_vars_all %>%
  filter(region3=='central')%>%
  group_by(year, month)%>%
  dplyr::summarise(beuti_anom = mean(beuti_monthly_anom),
                   cuti_anom = mean(cuti_monthly_anom),
                   sst_anom = mean(as.numeric(as.character(sst_monthly_anom)),na.rm=T),
                   annual_MOCI = mean(as.numeric(annual_MOCI), na.rm=T),
                   quarterly_MOCI = mean(as.numeric(quarterly_MOCI), na.rm=T),
  ) %>%
  pivot_longer(names_to = "index", cols=c("beuti_anom","cuti_anom","sst_anom","annual_MOCI", "quarterly_MOCI"))

envr_plot$index <- factor(envr_plot$index, levels = c("sst_anom", "annual_MOCI","quarterly_MOCI","beuti_anom", "cuti_anom"))



# Examine time series of indices as annual means -------------------------------


envr_ts <- envr_plot%>%
  filter(year>=2000)%>%
  filter(!(index=='annual_MOCI'))%>%
  ggplot(aes(x=as.numeric(year),y=value,color=index, fill=index, group=index))+
  #geom_point()+
  stat_summary(fun=mean, geom="line", aes(color = index), size=1)+
  stat_summary(fun.data = mean_cl_normal, geom = "ribbon", aes(color=index), colour=NA, alpha=0.3)+
  geom_rect(data = data.frame(year = 2015), aes(xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf), 
            alpha = 0.3, fill="#8a0606", inherit.aes = FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  #annotate("rect", xmin = 2014.5, xmax = 2016.5, ymin = -12, ymax = 12,
  #      alpha = .2, fill="pink")+
  facet_wrap(~index, ncol=2, nrow=2, scale="free_y",
             strip.position = "left",
             labeller = as_labeller(c(sst_anom = "SST anomaly (°C)",
                                      beuti_anom="BEUTI anomaly", 
                                      cuti_anom = "CUTI anomaly",
                                      #annual_MOCI = "annual MOCI",
                                      quarterly_MOCI = "MOCI")))+
  xlab("Year")+
  ylab(NULL)+
  #scale_y_continuous(
  #  "beuti_anom", 
  #  sec.axis = sec_axis(trans=~ . * .0001, name = "cuti_anom")
  #)+
  theme_minimal(base_size = 14, bgcolor("white"))+
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        strip.background = element_blank(),
        strip.placement = "outside")+
  scale_x_continuous(breaks= scales::pretty_breaks())
  #labs(title="Central CA oceanographic anomalies")
 # theme(aspect.ratio=0.5/1.5)

print(envr_ts)

#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "oceanographic_indices_timeseries.png"), 
#       bg="white",envr_ts,height=5, width = 8, units = "in", dpi = 600)





# Examine time series of indices as monthly means ------------------------------

envr_plot2 <- envr_vars_all %>%
  filter(region3=='central')%>%
  group_by(year, month)%>%
  pivot_longer(names_to = "index", cols=c("beuti_monthly_anom","cuti_monthly_anom","sst_monthly_anom","annual_MOCI", "quarterly_MOCI"))

envr_plot2$index <- factor(envr_plot2$index, levels = c("beuti_monthly_anom","cuti_monthly_anom","sst_monthly_anom","annual_MOCI", "quarterly_MOCI"))

MOCI_plot <- envr_plot2 %>% filter(index=='quarterly_MOCI')

envr_ts2 <- envr_plot2 %>%
  filter(year>=2000)%>%
  filter(!(index=='quarterly_MOCI'|index=='annual_MOCI'))%>%
  ggplot(aes(x=as.numeric(month),y=value,color=index, fill=index, group=index))+
  #geom_point()+
  stat_summary(fun=mean, geom="line", aes(color = index), size=1)+
  stat_summary(fun.data = mean_sdl, geom = "ribbon", aes(color=index), colour=NA, alpha=0.3)+
  #geom_rect(data = data.frame(year = 2015), aes(xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf), 
  #          alpha = 0.3, fill="#8a0606", inherit.aes = FALSE)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  #annotate("rect", xmin = 2014.5, xmax = 2016.5, ymin = -12, ymax = 12,
  #      alpha = .2, fill="pink")+
  facet_wrap(~index, ncol=2, nrow=2, scale="free_y",
             strip.position = "left",
             labeller = as_labeller(c(sst_monthly_anom = "SST anomaly (°C)",
                                      beuti_monthly_anom="BEUTI anomaly", 
                                      cuti_monthly_anom = "CUTI anomaly",
                                      #annual_MOCI = "annual MOCI",
                                      quarterly_MOCI = "MOCI"))
             )+
  xlab("Year")+
  ylab(NULL)+
  #scale_y_continuous(
  #  "beuti_anom", 
  #  sec.axis = sec_axis(trans=~ . * .0001, name = "cuti_anom")
  #)+
  theme_minimal(base_size = 14, bgcolor("white"))+
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        strip.background = element_blank(),
        strip.placement = "outside")+
  scale_x_continuous(breaks= scales::pretty_breaks())
#labs(title="Central CA oceanographic anomalies")
# theme(aspect.ratio=0.5/1.5)

print(envr_ts2)








