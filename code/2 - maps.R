
# Visualization maps of some indicators. 


# Packages --------------------------------------------------------------------------

library('sp') # spatial polygons class
library('rgdal') # read shape files
library('ggplot2') # plot maps


# Base map --------------------------------------------------------------------------

# Base Singapore map

# import data
singapore <- readRDS('data/geography/raw/SGP_adm0.rds')
singapore <- fortify(singapore, region = 'ISO')

# map
(map <- ggplot(data = singapore, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon() + 
  coord_fixed() + 
  theme_void())


# Streets ---------------------------------------------------------------------------

# Add streets to the base map.

# import streets data
streets <- readOGR(dsn = "data/geography/raw/singapore_highway")
streets <- fortify(streets, region = 'NAME')

# map
(map <- map + 
    geom_line(data = streets, mapping = aes(x = long, y = lat, group = group),
              colour = 'dodgerblue4'))
  