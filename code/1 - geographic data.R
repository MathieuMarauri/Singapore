
# Geographical data is retrieved. Shapes for singapore area, streets, bus and rail lines
# and stops. 

# Outputs: singapore map

# Packages --------------------------------------------------------------------------

library('data.table') # dataset manipulation
library('rgdal') # read shape files
library('rgeos') # simplify spatial polygons
library('ggplot2') # plot maps
library('osmdata') # extracting spatial data
library('dplyr') # sf/dataframe manipulation
library('sf') # make sf class sticky to dplyr
library('rjson') # read json files
library('xml2') # read xml files


# Singapore areas -------------------------------------------------------------------

# Data from: https://data.gov.sg/dataset/master-plan-2014-planning-area-boundary-no-sea

# read shape file
area <- readOGR(dsn = "data/geography/raw/singapore_areas")

# transform coordinates system to regular decimal degrees
area <- spTransform(area, CRS("+proj=longlat +datum=WGS84"))

# simplify to avoid the topology exception
area <- gSimplify(area, tol = 0.00001)

# plot
area_df <- fortify(area, region = 'OBJECTID')
ggplot(data = area_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'white', fill = 'gray20')

# clean session
rm(area)


# Streets ---------------------------------------------------------------------------

# Data from: http://www.mapcruzin.com/free-singapore-country-city-place-gis-shapefiles.htm

# read shape file
streets <- readOGR(dsn = "data/geography/raw/singapore_highway")

# plot
streets_df <- fortify(streets, region = 'NAME')
ggplot(data = area_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'black', fill = 'white') + 
  geom_line(data = streets_df, mapping = aes(x = long, y = lat, group = group), 
            colour = 'gray40')

# singapore bounding box
singapore_bbox <- matrix(c(103.604201, 
                           1.182086, 
                           104.089178, 
                           1.477715), nrow = 2)

# get the streets shape
query <- opq(bbox = singapore_bbox)
query <- add_osm_feature(opq = query, 
                         key = 'highway',
                         key_exact = FALSE,
                         value_exact = FALSE,
                         match_case = FALSE)
streets <- osmdata_sf(q = query)
streets <- streets$osm_lines

# select informative columns to then filter data
streets <- streets %>% 
  select(osm_id, addr.city, highway, railway, geometry)

# filter out some streets 
streets <- streets %>% 
  filter(addr.city != 'Johor Bahru' | is.na(addr.city)) %>% 
  filter(railway != 'abandoned' | is.na(railway)) %>% 
  filter(!highway %in% c('footway', 'path', 'bridleway'))

# coerce sf to data frame for the plot
geometry <- st_geometry(streets)
streets_df <- lapply(X = 1:nrow(streets), 
                     FUN = function(i) cbind(id = streets$osm_id[i], geometry[[i]])) 
streets_df <- do.call(rbind, streets_df) %>% 
  as.data.frame() %>% 
  setNames(c('id', 'long', 'lat'))

# coerce to spatialpoints to use over function
streets_sp <- SpatialPointsDataFrame(coords = streets_df[, c(2,3)], 
                                     data = streets_df,
                                     proj4string = CRS("+proj=longlat +datum=WGS84"),
                                     match.ID = FALSE)

overlay <- over(streets_sp, as(area, 'Spatial'))


# map
ggplot(data = area_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'black', fill = 'white') + 
  geom_line(data = streets_df, mapping = aes(x = long, y = lat, group = id),
            colour = 'gray40')


# Bus lanes -------------------------------------------------------------------------

# Bus lanes from https://github.com/cheeaun/busrouter-sg

#'
#' This function takes a json bus route file as input and returns the corresponding data
#' table with lat and long columns along with an id and the name of the bus line. If
#' something goes wrong it outputs the name of the bus.
#' 
busRoute <- function(file, bus) {
  result <- tryCatch(expr = {
    route <- fromJSON(file = file)
    route <- strsplit(route[[1]]$route, split = ',')
    route <- do.call(what = rbind, args = route)
    route <- as.data.table(route)
    names(route) <- c('lat', 'long')
    route[, c('lat', 'long') := lapply(X = .SD, FUN = function(x) as.numeric(as.character(x))), 
          .SDcols = c('lat', 'long')]
    route[, c('bus', 'id') := list(bus, paste(bus, 1:nrow(route), sep = '_'))]
    return(route)
  }, error = function(err) {
    return(bus)
  })
  return(result)
}

# bus route files 
bus_files <- list.files(path = 'data/geography/raw/bus_routes/bus-services')
bus_names <- gsub(pattern = '.json', replacement = '', x = bus_files)
bus_files <- file.path('data/geography/raw/bus_routes/bus-services', bus_files)

# bus route datasets
bus_route <- lapply(X = seq(length(bus_files)),
                    FUN = function(i) busRoute(file = bus_files[i], bus = bus_names[i]))

# filter out bus routes without data
bus_route <- bus_route[!sapply(bus_route, is.character)]

# bind all tables
bus_routes <- rbindlist(bus_route)

# plot on Singapore map
ggplot(data = area_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'white', fill = 'gray20') + 
  geom_point(data = bus_routes, mapping = aes(x = long, y = lat, group = id, color = bus),
             show.legend = FALSE)


# Bus travel times ------------------------------------------------------------------

# Data from https://www.mytransport.sg/content/mytransport/home/dataMall.html

# import data
travel_time <- read_xml(x = "data/geography/raw/EstTravelTimes.xml")

# parse data
name <- travel_time %>% 
  xml_find_all('//m:properties/d:Name') %>% 
  xml_text() %>% 
  tolower()
startpoint <- travel_time %>% 
  xml_find_all('//m:properties/d:StartPoint') %>% 
  xml_text() %>% 
  tolower()
endpoint <- travel_time %>% 
  xml_find_all('//m:properties/d:EndPoint') %>% 
  xml_text() %>% 
  tolower()
time <- travel_time %>% 
  xml_find_all('//m:properties/d:EstTime') %>% 
  xml_text() %>% 
  as.numeric()

travel_time <- data.table(name = name, startpoint = startpoint, endpoint = endpoint, 
                          time = time)
# data is useless

# Using osm to get the data 
