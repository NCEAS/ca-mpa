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
                   bottomT_anom = mean(as.numeric(as.character(bottomT_monthly_anom)),na.rm=T),
                   annual_MOCI = mean(as.numeric(annual_MOCI), na.rm=T),
                   quarterly_MOCI = mean(as.numeric(quarterly_MOCI), na.rm=T),
  ) %>%
  pivot_longer(names_to = "index", cols=c("beuti_anom","cuti_anom","sst_anom",
                                          "bottomT_anom","annual_MOCI", "quarterly_MOCI"))

envr_plot$index <- factor(envr_plot$index, levels = c("sst_anom","bottomT_anom",
                                                      "annual_MOCI",
                                                      "quarterly_MOCI",
                                                      "beuti_anom", "cuti_anom"))



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
  facet_wrap(~index, ncol=3, nrow=2, scale="free_y",
             strip.position = "left",
             labeller = as_labeller(c(sst_anom = "SST anomaly (°C)",
                                      bottomT_anom = "Bottom temp anomaly (°C)",
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
  pivot_longer(names_to = "index", cols=c("beuti_monthly_anom","cuti_monthly_anom",
                                          "sst_monthly_anom","annual_MOCI", "quarterly_MOCI",
                                          "bottomT_monthly_anom"))

envr_plot2$index <- factor(envr_plot2$index, levels = c("sst_monthly_anom",
                                                        "bottomT_monthly_anom",
                                                        "quarterly_MOCI","beuti_monthly_anom","cuti_monthly_anom","annual_MOCI"))

MOCI <- envr_plot2 %>% filter(index=='quarterly_MOCI',
                                    year>=2000)
SST <- envr_plot2 %>% filter(index=='sst_monthly_anom',
                             year>=2000)
BT <- envr_plot2 %>% filter(index=='bottomT_monthly_anom',
                            year>=2000)
BEUTI <- envr_plot2 %>% filter(index=='beuti_monthly_anom',
                             year>=2000)
CUTI <- envr_plot2 %>% filter(index=='cuti_monthly_anom',
                               year>=2000)


MOCI_plot<- ggplot(aes(x=as.numeric(qter),y=value),data=MOCI)+
  stat_summary(fun=mean, geom="line", color = "#93AA00", size=1)+
  stat_summary(fun.data = mean_sdl, geom = "ribbon", fill='#93AA00', color=NA, alpha=0.3)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  xlab("Quarter")+
  ylab("MOCI")+
  theme_minimal(base_size = 10, bgcolor("white"))+
  scale_x_continuous(breaks= c(1:4))+
  theme(legend.position="none")

SST_plot <- ggplot(aes(x=as.numeric(month),y=value), data=SST)+
  stat_summary(fun=mean, geom="line", color = "#F8766D", size=1)+
  stat_summary(fun.data = mean_sdl, geom = "ribbon", fill="#F8766D", colour=NA, alpha=0.3)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  xlab("Month")+
  ylab("SST anomaly (°C)")+
  theme_minimal(base_size = 10, bgcolor("white"))+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none")

BT_plot<- ggplot(aes(x=as.numeric(month),y=value),data=BT)+
  stat_summary(fun=mean, geom="line", color = "#DB72FB", size=1)+
  stat_summary(fun.data = mean_sdl, geom = "ribbon", fill='#DB72FB', color=NA, alpha=0.3)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  xlab("Month")+
  ylab("CUTI anomaly")+
  theme_minimal(base_size = 10, bgcolor("white"))+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none")

BEUTI_plot<- ggplot(aes(x=as.numeric(month),y=value),data=BEUTI)+
  stat_summary(fun=mean, geom="line", color = "#00B9E3", size=1)+
  stat_summary(fun.data = mean_sdl, geom = "ribbon", fill='#00B9E3', color=NA, alpha=0.3)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  xlab("Month")+
  ylab("BEUTI anomaly")+
  theme_minimal(base_size = 10, bgcolor("white"))+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none")

CUTI_plot<- ggplot(aes(x=as.numeric(month),y=value),data=CUTI)+
  stat_summary(fun=mean, geom="line", color = "#DB72FB", size=1)+
  stat_summary(fun.data = mean_sdl, geom = "ribbon", fill='#DB72FB', color=NA, alpha=0.3)+
  geom_hline(yintercept=0, linetype="dashed", color = "black")+
  xlab("Month")+
  ylab("CUTI anomaly")+
  theme_minimal(base_size = 10, bgcolor("white"))+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  theme(legend.position="none")


plots <- ggarrange(SST_plot, MOCI_plot,
          BEUTI_plot, CUTI_plot, ncol=3, nrow=2)


#ggsave(here::here("analyses", "5community_climate_ecology", "figures", "oceanographic_indices_range.png"), 
#       bg="white",plots,height=5, width = 8, units = "in", dpi = 600)
















