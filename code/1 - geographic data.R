
# Geographical data is retrieved. Shapes for singapore areas, streets, bus and rail lines
# and stops. 

# Inputs: shape files from different sources.

# Packages --------------------------------------------------------------------------

library('data.table') # dataset manipulation
library('sf') # sf class sticky to dplyr
library('sp') # change coordinates projection
library('rgdal') # read shape files
library('rgeos') # simplify spatial polygons
library('ggplot2') # plot maps
library('osmdata') # extracting spatial data
library('dplyr') # sf/dataframe manipulation
library('rjson') # read json files


# Singapore boundary ----------------------------------------------------------------

# Base singapore boundary from global administrative areas. 

# import data
singapore <- readRDS('data/geography/raw/SGP_adm0.rds')
singapore_df <- fortify(singapore, region = 'ISO')


# Singapore areas -------------------------------------------------------------------

# Data from: https://data.gov.sg/dataset/master-plan-2014-planning-area-boundary-no-sea

# read shape file
area <- readOGR(dsn = "data/geography/raw/singapore_areas")

# transform coordinates system to regular decimal degrees
area <- spTransform(area, CRS("+proj=longlat +datum=WGS84"))

# simplify to avoid the topology exception created with the new projection
area <- gSimplify(area, tol = 0.00001)

# plot
area_df <- fortify(area, region = 'OBJECTID')
ggplot(data = area_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'white', fill = 'gray20')

# save result and clean session
saveRDS(area_df, 'data/geography/clean/area.rds')
rm(area, area_df)


# Singapore subzones -------------------------------------------------------------------

# Data from: https://data.gov.sg/dataset/master-plan-2014-subzone-boundary-no-sea

# read shape file
subzone <- readOGR(dsn = "data/geography/raw/singapore_subzones")

# transform coordinates system to regular decimal degrees
subzone <- spTransform(subzone, CRS("+proj=longlat +datum=WGS84"))

# simplify to avoid the topology exception created with the new projection
subzone <- gSimplify(subzone, tol = 0.00001)
subzone_df <- fortify(subzone, region = 'OBJECTID')

# plot
ggplot(data = subzone_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'white', fill = 'gray20')

# save result and clean session
saveRDS(subzone_df, 'data/geography/clean/subzone.rds')
rm(subzone, subzone_df)


# Streets ---------------------------------------------------------------------------

# Data from osm. Get the streets shape in filter out streets not inside the singapore
# boudaries.

# singapore bounding box
singapore_bbox <- matrix(c(103.604201, 
                           1.182086, 
                           104.089178, 
                           1.477715), nrow = 2)

# get the streets inside bbox from osm
query <- opq(bbox = singapore_bbox)
query <- add_osm_feature(opq = query, 
                         key = 'highway',
                         key_exact = FALSE,
                         value_exact = FALSE,
                         match_case = FALSE)

# create sf object and keeps lines
streets_sf <- osmdata_sf(q = query)
streets_sf <- streets_sf$osm_lines

# which streets are contained in the Singapore boundaries
contained <- st_covers(x = st_as_sf(singapore), y = streets_sf)

streets_sf <- streets_sf[contained[[1]], ] 
streets_sp <- as(streets_sf, 'Spatial')
streets_df <- fortify(streets_sp, region = 'osm_id')

# plot 
ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'black', fill = 'white') + 
  geom_line(data = streets_df, mapping = aes(x = long, y = lat, group = id),
            colour = 'gray40')

# save results and clean session
saveRDS(streets_df, 'data/geography/clean/streets.rds')
rm(streets_df, streets_sf, streets_sp, contained, query, singapore_bbox, singapore)


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
bus_route <- rbindlist(bus_route)

# plot on Singapore map
ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'white', fill = 'gray20') + 
  geom_point(data = bus_routes, mapping = aes(x = long, y = lat, group = id, color = bus),
             show.legend = FALSE)

# save resuts and clean session
saveRDS(bus_route, 'data/geography/clean/bus.rds')
rm(bus_route, bus_files, bus_names, busRoute)


# Bicycle rack ----------------------------------------------------------------------

# Data from https://www.mytransport.sg/content/mytransport/home/dataMall.html

# read shape file
cycle <- st_read("data/geography/raw/bicyclerack.kml")

# coerce to sp 
cycle <- as(cycle, 'Spatial')
cycle <- data.frame(cycle)

# plot on singapore map
ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'black', fill = 'white') + 
  geom_point(data = cycle, mapping = aes(x = coords.x1, y = coords.x2, group = Name),
            colour = 'dodgerblue4')

# save results and clean session
saveRDS(cycle, 'data/geography/clean/cycle.rds')
rm(cycle)


# Bus travel times ------------------------------------------------------------------

# Using osm to get the travel time (and shortest route) between two points from the bus
# route. Complete bus route and get the travel time between stops.

# clean session
rm(singapore_df)