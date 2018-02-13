
# Visualization maps of some indicators. 

# Packages --------------------------------------------------------------------------

library('sp') # spatial polygons class
library('sf') # sf class
library('rgdal') # read shape files
library('ggplot2') # plot maps
library('dplyr') # combine demographic and geogrpahic data
library('rmapzen') # get isochrone data
library('leaflet') # dynamic maps


# Base map --------------------------------------------------------------------------

# Base Singapore map

# import data
singapore <- readRDS('data/geography/raw/SGP_adm0.rds')
singapore <- fortify(singapore, region = 'ISO')

# map
(map <- ggplot(data = singapore, mapping = aes(x = long, y = lat, group = group)) + 
    geom_polygon() + 
    coord_equal() + 
  theme_void())

# clean session
rm(singapore)


# Subzone population ----------------------------------------------------------------

# Add heatmap of population. 

# import population data
subzone_pop <- readRDS('data/demographic/clean/demographic_subzone.rds')

# import shapes
subzone <- readOGR(dsn = "data/geography/raw/singapore_subzones")

# transform coordinates system to regular decimal degrees
subzone <- spTransform(subzone, CRS("+proj=longlat +datum=WGS84"))
subzone_data <- data.frame(subzone)

# simplify to avoid the topology exception created with the new projection
subzone <- gSimplify(subzone, tol = 0.00001)
subzone <- SpatialPolygonsDataFrame(Sr = subzone, data = subzone_data, match.ID = FALSE)

subzone_df <- fortify(subzone, region = 'SUBZONE_N')

# add population data
subzone_df$id <- tolower(subzone_df$id)
subzone_pop$subzone <- tolower(subzone_pop$subzone)
subzone_df <- merge(x = subzone_df,
                      y = subzone_pop[, .(subzone, total)], 
                      by.x = 'id',
                      by.y = 'subzone',
                      all.x = TRUE,
                      sort = FALSE)
setorderv(x = subzone_df, cols = c('group', 'order'))

# map
(map <- ggplot(data = subzone_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(data = subzone_df, mapping = aes(x = long, y = lat, group = group, fill = total),
               colour = 'black', size = 0.1) + 
  scale_fill_continuous(name = 'Population') + 
    coord_equal() + 
    theme_void())

# clean session
rm(subzone_df, subzone_pop, subzone, subzone_data)


# Streets ---------------------------------------------------------------------------

# Add streets to the base map.

# import streets data
streets <- readRDS("data/geography/clean/streets.rds")

# map
(map <- map + 
    geom_line(data = streets, mapping = aes(x = long, y = lat, group = group),
              colour = 'gray20', size = 0.1))

# clean session
rm(streets)

# Bus lines -------------------------------------------------------------------------

# Add the bus lines to the map.

# import bus data
bus_routes <- readRDS("data/geography/clean/bus.rds")

# map
(map <- map + 
    geom_point(data = bus_routes, mapping = aes(x = long, y = lat, group = bus),
              colour = 'maroon4', size = 0.1))

# clean session
rm(bus_routes)


# Isochrone -------------------------------------------------------------------------

# Get isochrone data and plot it on dynamic maps.

# get the isochrone for each station
station1 <- mz_location(lat = 1.299597, lon = 103.8396382)
station1 <- mz_isochrone(
  locations = station1,
  costing_model = mz_costing$pedestrian(),
  contours = mz_contours(c(5, 10, 15)),
  polygons = TRUE
)

station2 <- mz_location(lat = 1.300094, lon = 103.853883)
station2 <- mz_isochrone(
  locations = station2,
  costing_model = mz_costing$pedestrian(),
  contours = mz_contours(c(5, 10, 15)),
  polygons = TRUE
)

station3 <- mz_location(lat = 1.309508, lon = 103.853166)
station3 <- mz_isochrone(
  locations = station3,
  costing_model = mz_costing$pedestrian(),
  contours = mz_contours(c(5, 10, 15)),
  polygons = TRUE
)

station4 <- mz_location(lat = 1.293097, lon = 103.841369)
station4 <- mz_isochrone(
  locations = station4,
  costing_model = mz_costing$pedestrian(),
  contours = mz_contours(c(5, 10, 15)),
  polygons = TRUE
)

# plot the map
leaflet(data = as_sp(station1)) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(data = as_sp(station1), color = ~color, weight = 1) %>%
  addPolygons(data = as_sp(station2), color = ~color, weight = 1) %>% 
  addPolygons(data = as_sp(station3), color = ~color, weight = 1) %>%  
  addPolygons(data = as_sp(station4), color = ~color, weight = 1) %>% 
  addMarkers(lng = 103.8396382, lat = 1.299597, popup = "Rochor Road Station") %>% 
  addMarkers(lng = 103.853883, lat = 1.300094, popup = "Queen Street Station") %>% 
  addMarkers(lng = 103.853166, lat = 1.309508, popup = "Race Course Station") %>% 
  addMarkers(lng = 103.841369, lat = 1.293097, popup = "Mohamed Sultan Station") %>% 
  addLegend(colors = ~color, 
            labels = ~paste(contour, "minutes"),
            title = "Walking time from <br/> blueSG stations") %>% 
  addScaleBar(position = 'bottomleft')

# save results and clean session
saveRDS(list(station1, station2, station3, station4), 'data/geography/clean/isochrone.rds')
rm(station1, station2, station3, station4)

