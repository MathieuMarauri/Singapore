
# Visualization maps of some indicators. 

# Packages --------------------------------------------------------------------------

library('sp') # spatial polygons class
library('sf') # sf class
library('rgdal') # read shape files
library('ggplot2') # plot maps
library('dplyr') # combine demographic and geogrpahic data


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


# Streets ---------------------------------------------------------------------------

# Add streets to the base map.

# import streets data
streets <- readRDS("data/geography/clean/streets.rds")

# map
(map <- map + 
    geom_line(data = streets, mapping = aes(x = long, y = lat, group = group),
              colour = 'gray20', size = 0.1))


# Bus lines -------------------------------------------------------------------------

# Add the bus lines to the map.

# import bus data
bus_routes <- readRDS("data/geography/clean/bus.rds")

# map
(map <- map + 
    geom_point(data = bus_routes, mapping = aes(x = long, y = lat, group = bus),
              colour = 'maroon4', size = 0.1))
  