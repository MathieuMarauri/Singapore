
# import singapore boundaries from shape file from gadm
singapore_sp <- readRDS('data/geography/raw/SGP_adm0.rds')

# coerce to sf object
singapore_sf <- st_as_sf(singapore_sp)

# coerce back to sp
singapore_sp <- as(singapore_sf, 'Spatial')


# Complete bus routes ---------------------------------------------------------------

# Complete bus routes as data retrieved consists in points only. Osm used to get the
# shortest path between two consecutive points of the same bus route.

# import bus data
bus_routes <- readRDS("data/geography/clean/bus.rds")

library('osrm') # get shortest path 

bus <- unique(bus_routes$bus)
result <- vector(mode = 'list', length = length(bus))
names(result) <- bus
for (i in 1:length(bus)) {
  bus_route <- bus_routes[bus == bus[i]]
  nrow_bus <- nrow(bus_route)
  for (j in 1:(nrow_bus - 1)) {
    tryCatch(expr = {
      route <- osrmRoute(src = as.numeric(bus_route[j, .(id, long, lat)]),
                         dst = as.numeric(bus_route[j + 1, .(id, long, lat)]),
                         sp = TRUE)
      route <- st_as_sf(route)
      route <- route %>% 
        mutate(src = bus_route[j]$id,
               dst = bus_route[j + 1]$id)
      if (exists("shortest_path")) {
        shortest_path <- rbind(shortest_path, route)
      } else {
        shortest_path <- route
      }
      cat(j, ' - ', sep = '')
    }, error = function(message) {
      cat('Error')
    })
  }
  shortest_path <- shortest_path %>% 
    mutate(bus = i)
  result[[i]] <- shortest_path
  rm(shortest_path)
  Sys.sleep(5)
  cat('\n', bus[i], '(', i, '/', length(bus), '): ', sep = '')
}

saveRDS(result, 'data/geography/clean/bus_complete.rds')


# Read xml --------------------------------------------------------------------------

# BUs travel time
library('xml2') # read xml files
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



# Hexagon  --------------------------------------------------------------------------

# https://stackoverflow.com/questions/42893734/create-hexagonal-grid-over-city-and-associate-with-lon-lat-points-in-r
# https://cran.r-project.org/web/packages/dggridR/vignettes/dggridR.html