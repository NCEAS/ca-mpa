rm(list = ls())



library(tidyverse)
library(sf)


#load CDFW mpa polygons
data_path <- "/home/shares/ca-mpa/data/sync-data/"
input_file <- "CA_MPA_boundaries/ds582/ds582.shp" 

location.data <- st_read(file.path(data_path,input_file), stringsAsFactors = F)

data.sf <- st_transform(x = location.data, crs=4326) #transform to WGS84

#calculate centroids
data.sf$centroids <- data.sf %>%
                            st_centroid() %>%
                            st_geometry() 


plot(st_geometry(data.sf))
plot(st_set_geometry(data.sf, 'centroids')[,0], add = T, col = 'red', pch = 3)

#st_coordinates(data.sf$centroids)

#add coordinates to data frame


separated_coord <- data.sf %>%
    mutate(Name = NAME,
           long = unlist(map(data.sf$centroids,1)),
           lat = unlist(map(data.sf$centroids,2))
           ) %>%
    select(Name, lat, long)



#read in mpa-attribute table and port locations
input_file <- "mpa-attributes.xlsx" 
mpa.attributes <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

input_file <- "ports_and_harbors.xlsx" 
port.locations <- readxl::read_excel(file.path(data_path, input_file), sheet=1, skip = 0, na="NA")

mpa.coords <- left_join(separated_coord, mpa.attributes,
                        by = 'Name')  %>%
              drop_na("lat")

#clean up new mpa-attribute table

mpa.attrib <- mpa.coords %>%
              select(-c("Latitude","Annual_citations","Enforcement_metric","Compliance_estimate","Scientific_permits","Distance_to_port"))
              





#================calculate closest port for each MPA

# mpa.attrib.sp <- st_as_sf(mpa.attrib, coords = c("long","lat"), crs = 4326) #load into WGS84

#transform to TA

mpa.attrib.albers <- st_transform(mpa.attrib, crs=3310)
st_crs(mpa.attrib.albers)


ports <- st_as_sf(port.locations, coords = c("long","lat"), crs = 4326) #load into WGS84
ports.albers <- st_transform(ports, crs=3310) #transform to albers

  

#add coordinates for points in dataframes --------------------------------------
a_coord <- st_coordinates(mpa.attrib.albers)
a <- cbind(mpa.attrib.sp, a_coord)

b_coord <- st_coordinates(ports.albers)
b <- cbind(ports, b_coord)

#get closes feature in B to A

A_B <- a %>%
  st_join(b %>%
            select(port, size, X, Y) %>%
            rename(B_X = X, B_Y = Y), join = st_nearest_feature)


#create a WKT from the coords of A and closes feature in B ---------------------

A_B$line_wkt <- paste('linestring(',A_B$X,A_B$Y,',',A_B$B_X, A_B$B_Y,')')


#Converty WKT into Geom---------------------------------------------------------

A_B <- A_B %>%
  st_drop_geometry() %>%
  st_as_sf(wkt = 'line_wkt', crs = 3310)


#Get the length (distance of each line in meters)-------------------------------

A_B$length <- as.numeric(st_length(A_B))

#Join results with original A---------------------------------------------------
mpa_attributes <- a %>%
  left_join(A_B %>%
              st_drop_geometry() %>%
              select(Name, port, size, length), by = 'Name') 

mpa_attributes <-  mpa_attributes %>%
                    mutate(
                      distance_to_port = length,
                      long = unlist(map(mpa_attributes$geometry,1)),
                      lat = unlist(map(mpa_attributes$geometry,2))) %>%
                      select(-c("length", "geometry")) 


View(mpa_attributes)





plot(st_geometry(a), col = 'red', pch = 3) #mpa locations
plot(st_geometry(b), add = T, col = 'black', pch = 17) #ports and harbors
