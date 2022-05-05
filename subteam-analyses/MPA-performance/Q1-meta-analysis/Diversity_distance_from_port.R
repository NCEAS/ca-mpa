rm(list = ls())



require(sf)
require(tidyverse)
require(raster)
require(dplyr)
require(ggplot2)
require(ggpmisc)

data_path <- "/home/shares/ca-mpa/data/sync-data"

# read in port locations --------------------------------------------------

input_file <- "ports_and_harbors.xlsx" 
port.locations <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

# read in performance metrics ---------------------------------------------


input_file <- "Ecol_perform_metrics_means_working.xlsx" 
perform.metrics <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

perform.metrics <- perform.metrics %>%
                  drop_na("lat_wgs84")

# identify closest port to each site ---------------------------------------

perform.metrics.sp <- st_as_sf(perform.metrics, coords = c("lon_wgs84","lat_wgs84"), crs = 4326) #load into WGS84
ports.sp <- st_as_sf(port.locations, coords = c("long","lat"), crs = 4326) #load into WGS84


#transform to TA

metrics.albers <- st_transform(perform.metrics.sp, crs=3310)
st_crs(metrics.albers)

ports.albers <- st_transform(ports.sp, crs=3310) #transform to albers
st_crs(ports.albers)


# add coords for points in DF 
a_coord <- st_coordinates(metrics.albers)
a <- cbind(perform.metrics.sp, a_coord) 
    

b_coord <- st_coordinates(ports.albers)
b <- cbind(ports.sp, b_coord)


#get closest feature in B to A

A_B <- a %>%
  st_join(b %>%
            #select(port, size, X, Y) %>%
            rename(B_X = X, B_Y = Y), join = st_nearest_feature)

#create a WKT from the coords of A and closes feature in B 
A_B$line_wkt <- paste('linestring(',A_B$X,A_B$Y,',',A_B$B_X, A_B$B_Y,')')


#Converty WKT into Geom-
A_B <- A_B %>%
  st_drop_geometry() %>%
  st_as_sf(wkt = 'line_wkt', crs = 3310)

#Get the length (distance of each line in meters)

A_B$length <- as.numeric(st_length(A_B))


eco_metrics <- as.data.frame(A_B)

eco_metrics <- eco_metrics %>%
                dplyr::select(-c("X","Y","B_X","B_Y","line_wkt"))%>%
                mutate(port_size = size,
                       port_distance = (length/1000))








# Part 2 - calculate response ratio by distance from port -----------------------


region.yr.means<- eco_metrics%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_class == "ref"|mpa_class =="smr",
         #year=="2016" | year=="2017" | year=="2018" | year=="2019"
         )%>%
  group_by(group,mlpa_region, affiliated_mpa, mpa_designation,variable,indicator,year,port_distance)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean),
                   m.port_distance=mean(port_distance),# standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(id_cols = c(affiliated_mpa, group, mlpa_region, variable, indicator, year),
              names_from = mpa_designation,
              values_from = c(yr.mean, sd, n, port_distance)) 


region.yr.means$yr.mean_ref <- as.numeric(as.character(region.yr.means$yr.mean_ref))
region.yr.means$yr.mean_smr <- as.numeric(as.character(region.yr.means$yr.mean_smr))

mu_site <- region.yr.means %>% 
  drop_na(yr.mean_ref)%>%
  drop_na(yr.mean_smr)%>%
  mutate(RR        = log(yr.mean_smr/yr.mean_ref)
         #lower.CI  = log((RR - (qt(0.975, df)*n_total/sqrt(n_total))+1)+1),
         #upper.CI  = log((RR + (qt(0.975, df)*n_total/sqrt(n_total)))+1)
  )

mu_site$RR <- as.numeric(as.character(mu_site$RR))

mu_site <- mu_site %>%
  mutate(group = reorder_within(group, RR, mlpa_region)) %>%
  #filter(year=="2016" | year=="2017" | year=="2018" | year=="2019") %>%
  group_by(group,mlpa_region, affiliated_mpa,variable,indicator)%>%
  mutate_if(is.numeric, list(~na_if(RR, Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(RR, -Inf))) %>%
  drop_na(RR)%>%
  summarize(mean = mean(RR),
            port_distance = mean(as.numeric(port_distance_smr)))

View(mu_site)

mu_site$mlpa_region <- factor(mu_site$mlpa_region, levels = c("north","central","south"))

ggplot(mu_site, aes(x=port_distance, y=mean))+
  geom_hline(yintercept=0, linetype="dashed", color = "gray", size = 0.5) +
  geom_point(shape=19, size=3) +
  geom_smooth(method='lm', se = T, formula=y~x, linetype='solid', size=0.5) +
  stat_poly_eq(formula = y ~ x, 
               aes(label = paste(..rr.label.., ..p.value.label.., sep = "*`,`~")), 
               parse = TRUE,
               label.x.npc = "right",
               vstep = 0.05) + # sets vertical spacing
  scale_color_manual(values=c("blue", "red"), drop = FALSE) +
  scale_fill_manual(values=c("blue", "red"), drop = FALSE) +
  scale_linetype_manual(values=c("solid","twodash"), drop = FALSE) +
  labs(x="distance from port (km)", y="diversity log-RR (Shannon-Wiener)") +
  labs(colour = 'Site type', linetype = 'Slope') +
  facet_wrap(mlpa_region~group, scales="free_x", nrow=3)+
  scale_x_continuous()+
  lims(y=c(-0.5,0.5))+
  theme_classic() 








