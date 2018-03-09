
# Get available taxi in real time 

# Packages --------------------------------------------------------------------------

library('httr') # api call
library('jsonlite') # coerce api call response to workable list
library('purrr') # extract element from list easily
library('ggplot2') # data viz


# Taxi data -------------------------------------------------------------------------

# call the api
response <- GET('https://api.data.gov.sg/v1/transport/taxi-availability')

# get the datetime of the taxi data
time <- response$date

# extract useful information
response <- fromJSON(content(response, "text"), simplifyVector = FALSE)

# get the crs 
taxi_crs <- response$crs$properties$type

# get the coordinates
coordinates <- response$features[[1]]$geometry$coordinates
coordinates <- coordinates %>% {
  data.frame(
    long = map_dbl(., 1),
    lat = map_dbl(., 2)
   )
}

# map
ggplot(data = coordinates, mapping = aes(x = long, y = lat)) + 
  geom_point(color = 'white', size = 0.5) + 
  theme(panel.background = element_rect(fill = 'grey30'))
