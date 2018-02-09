
# import singapore boundaries from shape file from gadm
singapore_sp <- readRDS('data/geography/raw/SGP_adm0.rds')

# coerce to sf object
singapore_sf <- st_as_sf(singapore_sp)

# coerce back to sp
singapore_sp <- as(singapore_sf, 'Spatial')

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
contained_2 <- st_covers(x = singapore_sf, y = streets_sf)

streets_sf <- streets_sf[contained_2[[1]], ] 
streets_sp <- as(streets_sf, 'Spatial')
streets_df <- fortify(streets_sp, region = 'osm_id')

singapore_df <- fortify(singapore_sp, region = 'ISO')
ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'black', fill = 'white') + 
  geom_line(data = streets_df, mapping = aes(x = long, y = lat, group = id),
            colour = 'gray40')


