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
                   MOCI = mean(as.numeric(annual_MOCI), na.rm=T),
  ) %>%
  pivot_longer(names_to = "index", cols=c("beuti_anom","cuti_anom","sst_anom","MOCI"))

envr_plot$index <- factor(envr_plot$index, levels = c("beuti_anom", "cuti_anom", "sst_anom", "MOCI"))



# Examine time series of indices ------------------------------------------

envr_plot%>%
  filter(year>=2000)%>%
  ggplot(aes(x=as.numeric(year),y=value,color=index, fill=index, group=index))+
  geom_point()+
  stat_summary(fun=mean, geom="line", aes(color = index), size=2)+
  geom_rect(data = data.frame(year = 2015), aes(xmin = 2014, xmax = 2016, ymin = -Inf, ymax = Inf), alpha = 0.2, fill="red", inherit.aes = FALSE)+
  #annotate("rect", xmin = 2014.5, xmax = 2016.5, ymin = -12, ymax = 12,
  #      alpha = .2, fill="pink")+
  facet_wrap(~index, ncol=2, nrow=2, scale="free_y",
             strip.position = "left",
             labeller = as_labeller(c(beuti_anom="BEUTI (anom)", 
                                      cuti_anom = "CUTI (anom)",
                                      sst_anom = "SST (deg c anom)",
                                      MOCI = "MOCI")))+
  xlab("year")+
  ylab(NULL)+
  #scale_y_continuous(
  #  "beuti_anom", 
  #  sec.axis = sec_axis(trans=~ . * .0001, name = "cuti_anom")
  #)+
  theme_minimal(base_size = 22,)+
  theme(legend.position="none",
        plot.title=element_text(hjust=0.5),
        strip.background = element_blank(),
        strip.placement = "outside")+
  scale_x_continuous(breaks= scales::pretty_breaks())+
  labs(title="Central CA oceanographic anomalies")
