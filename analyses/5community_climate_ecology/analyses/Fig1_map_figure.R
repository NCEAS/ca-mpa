

# Read data
################################################################################

# Clear workspace
rm(list = ls())

# Packages
library(tidyverse)
library(patchwork)

# Directories
basedir <- "/Volumes/GoogleDrive/.shortcut-targets-by-id/1kCsF8rkm1yhpjh2_VMzf8ukSPf9d4tqO/MPA Network Assessment: Working Group Shared Folder/data/sync-data"
#basedir <- "/home/shares/ca-mpa/data/sync-data/" #aurora 
gisdir <- file.path(basedir, "gis_data/processed")
plotdir <- "analyses/5community_climate_ecology/figures"

# Read site locations
sites <- readRDS(file.path(basedir, "monitoring/monitoring_sites_clean.Rds")) %>% 
  mutate(habitat=recode(habitat, "Kelp"="Kelp forest"),
         habitat = recode(habitat, "Rocky reef"="Shallow reef"))

# Read data
state_waters_poly <- readRDS(file.path(gisdir, "CA_state_waters_polygons.Rds"))
state_waters_line <- readRDS(file.path(gisdir, "CA_state_waters_polyline.Rds"))
mpas_orig <- readRDS(file.path(gisdir, "CA_MPA_polygons.Rds"))
mpas_data <- readRDS(file.path(basedir, "mpa_traits/processed", "CA_mpa_metadata.Rds"))

# Get land
usa <- rnaturalearth::ne_states(country="United States of America", returnclass = "sf")
foreign <- rnaturalearth::ne_countries(country=c("Canada", "Mexico"), returnclass = "sf")


# Format data
################################################################################

# Reduce to MPAs of interest
mpas <- mpas_orig %>% 
  left_join(mpas_data %>% select(mpa, mlpa), by=c("name"="mpa")) %>% 
  filter(mlpa=="MLPA" & region=="CCSR") %>% 
  select(name, lat_dd, long_dd, type)

# Habitat stats
hab_stats <- sites %>% 
  count(mpa, habitat)

# Build habitat key
mpa_names <- mpas$name
habitats <- sort(unique(sites$habitat))
hab_key <- expand.grid(mpa=mpa_names, habitat=habitats) %>% 
  # Remove surf zone
  filter(habitat!="Surf zone") %>% 
  # Add habitat stats
  left_join(hab_stats) %>% 
  mutate(data_yn=!is.na(n)) %>% 
  # Add lat/long
  left_join(mpas_data %>% select(mpa, lat_dd, long_dd), by="mpa") %>% 
  # Order habitats
  mutate(habitat=factor(habitat, c("Rocky intertidal", 
                                   "Kelp forest", "Shallow reef", "Deep reef")),
         habitat_num=as.numeric(habitat)) %>% 
  # Adjust longitude
  mutate(long_dd_adj=long_dd-0.04-habitat_num*0.05) %>% 
  # Adjust latitude
  mutate(lat_dd_adj=case_when(# Elkohorn Slough (move both up)
                              mpa=="Elkhorn Slough SMR"~lat_dd+0.035,
                              mpa=="Elkhorn Slough SMCA"~lat_dd+0.015,
                              # Downtown Monterey
                              mpa=="Pacific Grove Marine Gardens SMCA"~lat_dd+0.03,
                              mpa=="Asilomar SMR"~lat_dd+0.01,
                              mpa=="Lovers Point - Julia Platt SMR"~lat_dd-0.02,
                              mpa=="Edward F. Ricketts SMCA"~lat_dd-0.04,
                              # Carmel (move both down)
                              mpa=="Carmel Pinnacles SMR"~lat_dd-0.01,
                              mpa=="Carmel Bay SMCA"~lat_dd-0.03,
                              # Point Lobos (move both down)
                              mpa=="Point Lobos SMR"~lat_dd-0.02,
                              mpa=="Point Lobos SMCA"~lat_dd-0.05,
                              # Point Sur
                              mpa=="Point Sur SMR"~lat_dd+0.02,
                              mpa=="Point Sur SMCA"~lat_dd-0.02,
                              # Big Creek
                              mpa=="Big Creek SMR"~lat_dd+0.031,
                              mpa=="Big Creek SMCA"~lat_dd-0.031,
                              # Piedras Blancas
                              mpa=="Piedras Blancas SMR"~lat_dd+0.02,
                              mpa=="Piedras Blancas SMCA"~lat_dd-0.02,
                              # Morro Bay
                              mpa=="Morro Bay SMR"~lat_dd+0.03,
                              mpa=="Morro Bay SMCA"~lat_dd-0.03,
                              # Point Buchon
                              mpa=="Point Buchon SMR"~lat_dd+0.01,
                              mpa=="Point Buchon SMCA"~lat_dd-0.01,
                              T ~ lat_dd)) %>% 
  # Arrange
  arrange(mpa, habitat) %>% 
  select(mpa, habitat, habitat_num, data_yn, n, lat_dd, long_dd, everything()) %>% 
  # Reduce to only available habitats
  filter(data_yn==T) %>% 
  group_by(mpa) %>% 
  mutate(habitat_num2=1:n()) %>% 
  ungroup() %>% 
  mutate(long_dd_adj2=long_dd-0.04-habitat_num2*0.05)


# Process environmental data using Josh's code
################################################################################

# Read data
envi_orig <- readRDS(file.path( basedir, "environmental/processed/envr_anomalies_at_mpas_new.Rds")) # Josh


# Process data
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

# Plot data
################################################################################

# Key cities
# Monterey	36.603056	-121.893611
# Morro Bay	35.367222	-120.846667
# Pismo Beach	35.148333	-120.648056
cities <- tibble(city=c("Monterey", "Morro\nBay", "Pismo\nBeach"),
                 long_dd=c(-121.893611, -120.846667, -120.648056),
                 lat_dd=c(36.603056, 35.367222, 35.148333),
                 hjust=c(-0.2, 0.5, 0.5),
                 vjust=c(0.5, -0.3, -0.3))

# MPA regions
# CA/OR, Alder Creek, Pigeon Point, Point Conception, CA/MEX 
region_lats <- c(39.0, 37.18, 34.5)
region_lats_long <- c(42, 39.0, 37.18, 34.5, 32)
region_labels <- tibble(region=paste0(c("North", "North\nCentral", "Central", "South"), "\nCoast"),
                        lat_dd=zoo::rollmean(region_lats_long, k = 2))

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

# Build inset
g1_inset <-  ggplotGrob(
  ggplot() +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats), lwd=0.4) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot box
  annotate("rect", xmin=-122.50, xmax=-120.4, ymin=34.5, ymax=37.2, color="indianred1", fill=NA, lwd=0.8) +
  # Label regions
  geom_text(data=region_labels, mapping=aes(y=lat_dd, label=region), x= -124.4, hjust=0, size=2) +
  #add north arrow
    ggsn::north(x.min = -124.5, x.max = -117, 
                y.min = 32.5, y.max = 42,
                location = "topright", 
                scale = 0.1, 
                symbol = 10)+
  # Labels
  labs(x="", y="") +
  # Crop
  coord_sf(xlim = c(-124.5, -117), ylim = c(32.5, 42)) +
  # Theme
  theme_bw() + base_theme +
  theme( plot.margin = unit(rep(0, 4), "null"),
         panel.margin = unit(rep(0, 4), "null"),
         panel.background = element_rect(fill='transparent'), #transparent panel bg
        # plot.background = element_rect(fill='transparent', color=NA), #transparent plot bg
        axis.ticks = element_blank(),
        axis.ticks.length = unit(0, "null"),
        axis.ticks.margin = unit(0, "null"),
        axis.text = element_blank(),
        axis.title=element_blank())
)
g1_inset

# Plot data
g1 <- ggplot() +
  # Plot state waters
  geom_sf(data=state_waters_line, color="grey60", lwd=0.1) +
  # Plot regions
  geom_hline(mapping=aes(yintercept=region_lats)) +
  # Plot land
  geom_sf(data=foreign, fill="grey80", color="white", lwd=0.3) +
  geom_sf(data=usa, fill="grey80", color="white", lwd=0.3) +
  # Plot MPAs
  geom_sf(data=mpas, fill=ifelse(mpas$type == "SMR","indianred1","lightblue"), color="black") +
  # ggrepel::geom_text_repel(data=mpas,
  #           mapping=aes(x=long_dd, y=lat_dd, label=name), direction="y", hjust=-0.1) +
  # Plot habitats
  # geom_point(data=hab_key, aes(x=long_dd_adj, y=lat_dd_adj, color=habitat, shape=data_yn)) +
  geom_point(data=hab_key, aes(x=long_dd_adj2, y=lat_dd_adj, color=habitat, shape=data_yn), pch=16, size=1.8) +
  # Plot cities
  geom_point(data=cities, mapping=aes(x=long_dd, y=lat_dd), size=2.2) +
  geom_text(data=cities, mapping=aes(x=long_dd, y=lat_dd, label=city, hjust=hjust, vjust=vjust),
            size=2.6) +
  # Labels
  labs(x="", y="", tag="A") +
  # Legend
  scale_color_manual(name="Habitat", values=c( "#f2cc8f", "#81b29a", "#e07a5f", "#3d405b")) +
  scale_shape_manual(name="Monitoring data?", values=c(1, 16)) +
  guides(color = guide_legend(order = 1), shape = guide_legend(order = 2)) +
  #add scalebar
  ggsn::scalebar(x.min = -122.6, x.max = -120.5, 
                 y.min = 34.44, y.max = 37.2,
                 #anchor=c(x=-124.7,y=41),
                 location="bottomleft",
                 dist = 50, dist_unit = "km",
                 transform=TRUE, 
                 model = "WGS84",
                 st.dist=0.012,
                 st.size=2,
                 border.size=.5,
                 height=.01
  )+
  # Crop
  coord_sf(xlim = c(-122.6, -120.5), ylim = c(34.5, 37.2)) +
  # Theme
  theme_bw() + base_theme +
  theme(axis.text.y = element_text(angle = 90, hjust = 0.5),
        axis.title=element_blank(),
        legend.position = c(0.2, 0.2), 
        legend.key.size = unit(0.4, "cm")) +
  # Add inset
  annotation_custom(grob = g1_inset, 
                    xmin = -121.4, 
                    xmax = -120.37,
                    ymin = 36.08)
  # Add inset
  # patchwork::inset_element(g1_inset, 
  #                          right = -120.6,
  #                          left=-121.4,
  #                          top=37.1, 
  #                          bottom=36.0)
  
g1

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

# Merge
#layout_matrix <- matrix(data=c(1, 2,
 #                              1, 3,
  #                             1, 4,
   #                            1, 5), byrow=T, ncol=2)
#g <- gridExtra::grid.arrange(g1, g2, g3, g4, g5, layout_matrix=layout_matrix,
 #                            widths=c(0.7, 0.3))
#g

# Export figure

#ggsave(g, filename=file.path(plotdir, "Fig1_map_figure_new2.png"), 
 #     width=5.8, height=6.5, units="in", dpi=600)

######use patchwork to align plot area 
# Merge
library(patchwork)
g <- g1 + (g2 / g3 / g4 / g5 + plot_layout(ncol = 1)) + plot_layout(widths = c(0.75, 0.25))


ggsave(g, filename=file.path(plotdir, "Fig1_map_figure.png"), 
            width=6, height=7, units="in", dpi=600)











