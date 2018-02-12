
library('rmapzen')

ucb <- mz_location(lat = 1.303540, lon = 103.876791)
isos <- mz_isochrone(
  ucb,
  costing_model = mz_costing$auto(),
  contours = mz_contours(c(10, 20, 30)),
  polygons = TRUE
)

library(leaflet)
leaflet(as_sp(isos)) %>%
  addProviderTiles("CartoDB.DarkMatter") %>%
  addPolygons(color = ~color, weight = 1) %>%
  addMarkers(lng = 103.876791, lat = 1.303540, popup = "The National Stadium") %>% 
  addLegend(colors = ~color, 
            labels = ~paste(contour, "minutes"),
            title = "Drive times from <br/> The National Stadium")
