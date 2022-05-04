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








# Part 2 - calculate response by distance from port -----------------------


#First, calculate raw mean for ref vs smr per monitoring group averaged 2016-2019

region.yr.means<- eco_metrics%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_designation == "ref"|mpa_designation =="smr",
         year=="2016" | year=="2017" | year=="2018" | year=="2019")%>%
  group_by(group,mlpa_region, affiliated_mpa, mpa_designation,variable,indicator)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean),
                   m.port_distance=mean(port_distance),# standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) 




ggplot(region.yr.means, aes(x=m.port_distance, y=yr.mean, color=mpa_designation))+
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
  labs(x="MPA age", y="mean diversity (Shannon-Weiner)") +
  labs(colour = 'Site type', linetype = 'Slope') +
  facet_wrap(mlpa_region~group, scales="fixed", nrow=3)+
  scale_x_continuous()+
  lims(y=c(0.5,2.5))+
  theme_classic() 












# Log ratios --------------------------------------------------------------


region.yr.means<- eco_metrics%>%
  filter(variable=="all species"| variable=="all fish"| variable=="invalg",
         indicator == "diversity",
         mpa_class == "ref"|mpa_class =="smr",
         #year=="2016" | year=="2017" | year=="2018" | year=="2019"
         )%>%
  group_by(group,mlpa_region, affiliated_mpa, mpa_designation,variable,indicator,year, port_distance)%>%
  dplyr::summarize(yr.mean = mean(mean,na.rm=TRUE), 
                   sd=sd(mean),
                   m.port_distance=mean(port_distance),# standard deviation of across MPAs where indicator was observed/recorded
                   n=n()) %>%
  pivot_wider(id_cols = c(affiliated_mpa, group, mlpa_region, variable, indicator, year),
              names_from = mpa_designation,
              values_from = c(yr.mean, sd, n)) 




mu_site <- region.yr.means %>% 
  mutate(HedgeG    = sqrt((sd_smr^2/(n_smr*yr.mean_smr))+(sd_ref^2/(n_ref*yr.mean_ref))),
         t_score   = mean_diff/se,
         df        = n_smr + n_ref - 2,
         p_value   = pt(t_score, df, lower.tail = FALSE),
         sig       = if_else(p_value < 0.05, 1, 0),
         cohens_d  = abs(mean_diff)/(sqrt(((sd_smr^2*(n_smr-1))+(sd_ref^2*(n_ref-1)))/df)),
         RR        = log(yr.mean_smr/yr.mean_ref),
         n_total   = n_smr + n_ref
         #lower.CI  = log((RR - (qt(0.975, df)*n_total/sqrt(n_total))+1)+1),
         #upper.CI  = log((RR + (qt(0.975, df)*n_total/sqrt(n_total)))+1)
  )

mu_site <- mu_site %>%
  mutate(group = reorder_within(group, RR, mlpa_region))

View(region.yr.means)










