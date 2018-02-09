pts = st_sfc(st_point(c(.5,.5)), st_point(c(1.5, 1.5)), st_point(c(2.5, 2.5)))
pol = st_polygon(list(rbind(c(0,0), c(2,0), c(2,2), c(0,2), c(0,0))))
(lst = st_intersects(pts, pol))
(mat = st_intersects(pts, pol, sparse = FALSE))
# which points fall inside a polygon?
apply(mat, 1, any)
lengths(lst) > 0
# which points fall inside the first polygon?
st_intersects(pol, pts)[[1]]


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

# coerce sf lines to spatialPoints to use over function 
# streets_points_sp <- st_geometry(streets_sf)
# streets_points_sp <- lapply(X = 1:nrow(streets_sf), 
#                      FUN = function(i) cbind(id = streets_sf$osm_id[i], streets_points_sp[[i]])) 
# streets_points_sp <- do.call(rbind, streets_points_sp) %>% 
#   as.data.frame() %>% 
#   setNames(c('id', 'long', 'lat'))
# coordinates(streets_points_sp) <- ~ long + lat
# proj4string(streets_points_sp) <- proj4string(singapore_sp)
# streets_points_sf <- st_as_sf(streets_points_sp)

# which streets are contained in the Singapore boundaries
contained <- st_intersects(x = streets_points_sf, y = singapore_sf)

contained_2 <- st_covers(x = singapore_sf, y = streets_sf)

streets_sf <- streets_sf[contained_2[[1]], ] 
streets_sp <- as(streets_sf, 'Spatial')
streets_df <- fortify(streets_sp, region = 'osm_id')

singapore_df <- fortify(singapore_sp, region = 'ISO')
ggplot(data = singapore_df, mapping = aes(x = long, y = lat, group = group)) + 
  geom_polygon(colour = 'black', fill = 'white') + 
  geom_line(data = streets_df, mapping = aes(x = long, y = lat, group = id),
            colour = 'gray40')


