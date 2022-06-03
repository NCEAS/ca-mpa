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


region.smr.means<- eco_metrics%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_class =="smr",
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
 region.smr.means <- region.smr.means %>%
        mutate(class="smr",
         ref_mean = yr.mean_ref,
         mpa_mean = yr.mean_smr,
         ref_port_distance = port_distance_ref,
         mpa_port_distance = port_distance_smr)%>%
        dplyr::select(-c("yr.mean_ref","yr.mean_smr","port_distance_ref","port_distance_smr","sd_ref","sd_smr","n_ref","n_smr"))



 region.smca.means<- eco_metrics%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_class =="smca",
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
 region.smca.means <- region.smca.means %>%
   mutate(class="smca",
          ref_mean = yr.mean_ref,
          mpa_mean = yr.mean_smca,
          ref_port_distance = port_distance_ref,
          mpa_port_distance = port_distance_smca)%>%
   dplyr::select(-c("yr.mean_ref","yr.mean_smca","port_distance_ref","port_distance_smca","sd_ref","sd_smca","n_ref","n_smca"))
 
 
 combined_means<-rbind(region.smr.means, region.smca.means)
 

combined_means$ref_mean <- as.numeric(as.character(combined_means$ref_mean))
combined_means$mpa_mean <- as.numeric(as.character(combined_means$mpa_mean))

mu_site <- combined_means %>% 
  drop_na(ref_mean)%>%
  drop_na(mpa_mean)%>%
  mutate(RR        = log(mpa_mean/ref_mean)
         #lower.CI  = log((RR - (qt(0.975, df)*n_total/sqrt(n_total))+1)+1),
         #upper.CI  = log((RR + (qt(0.975, df)*n_total/sqrt(n_total)))+1)
  )

mu_site$RR <- as.numeric(as.character(mu_site$RR))

mu_site <- mu_site %>%
  mutate(group = reorder_within(group, RR, mlpa_region)) %>%
  #filter(year=="2016" | year=="2017" | year=="2018" | year=="2019") %>%
  group_by(group,class, mlpa_region, affiliated_mpa,variable,indicator)%>%
  mutate_if(is.numeric, list(~na_if(RR, Inf))) %>% 
  mutate_if(is.numeric, list(~na_if(RR, -Inf))) %>%
  drop_na(RR)%>%
  summarize(mean = mean(RR),
            port_distance = mean(as.numeric(mpa_port_distance)))

View(mu_site)

mu_site$mlpa_region <- factor(mu_site$mlpa_region, levels = c("north","central","south"))

ggplot(mu_site, aes(x=port_distance, y=mean, color=class))+
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








