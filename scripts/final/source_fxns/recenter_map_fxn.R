# function written by Michael Sumner, to re-center sf-data to a chosen longitude
# code here: https://github.com/AustralianAntarcticDivision/SOmap/issues/34
# 2019-AUG-29

# alse see this solution: http://strimas.com/spatial/long-flights/#bounding-box-and-graticules (older, 'fortify' solution)

# e.g.
# pacman::p_load(sf, tidyverse, viridis, cowplot, lwgeom)
# 
# x <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
# world <- rmapshaper::ms_simplify(x, keep = 0.99)
# 
# shift <- -152 
# 
# re_world <- recentre(world, shift) %>% group_by(sovereignt) %>% summarize()
# 
# ggplot() + geom_sf(data = re_world, fill="grey", color="black")
recentre <- function(x, clon = NULL, ..., tryfix = TRUE) {
  if (is.null(clon)) return(x)
  if (!st_is_longlat(x)) stop("recentring not appropriate for non longlat data")
  ## save the crs while we do our munging
  crs <- st_crs(x)
  x <- st_set_crs(x, NA)
  
  
  ## try to fix problematic geometry
  if (tryfix) {
    if (all(grepl("POLYGON", st_geometry_type(x)))) x <- suppressWarnings(st_buffer(sf::st_as_sf(x), 0))
    x <- st_wrap_dateline(x, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
  }
  wbox <- st_bbox(c(xmin = -180, ymin = -90, xmax = (clon)%%360 - 180, ymax = 90))
  west <- suppressWarnings(st_crop(x, wbox))
  west <- st_set_geometry(west, st_geometry(west) + c(360, 0))
  east <- suppressWarnings(st_crop(x, st_bbox(c(xmin = (clon)%%360 - 180, xmax = 180, ymin = -90, ymax = 90))))
  xx <- rbind(
    west, east
  ) 
  ## ensure geometries are of consistent type
  xx <- sf::st_cast(xx)
  bb <- st_bbox(xx)
  ## hmmm
  # if (bb["xmax"] > 180 && !grepl("\\+over", crs) && !grepl("init", crs)) {
  #   crs <- sprintf("%s +over", crs)
  # }
  st_set_crs(xx, crs)
}




## OLD VERSION (before Nov. 2019)

# recenter <- recentre <- function(x, clon = NULL, ..., tryfix = TRUE) {
#   if (is.null(clon)) return(x)
#   if (!st_is_longlat(x)) stop("recentring not appropriate for non longlat data")
#   ## save the crs while we do our munging
#   crs <- st_crs(x)
#   x <- st_set_crs(x, NA)
#   
#   
#   ## try to fix problematic geometry
#   if (tryfix) {
#     if (all(grepl("POLYGON", st_geometry_type(x)))) x <- suppressWarnings(st_buffer(sf::st_as_sf(x), 0))
#     x <- st_wrap_dateline(x, options = c("WRAPDATELINE=YES", "DATELINEOFFSET=180"))
#   }
#   wbox <- st_bbox(c(xmin = -180, ymin = -90, xmax = (clon)%%360 - 180, ymax = 90))
#   west <- suppressWarnings(st_crop(x, wbox))
#   west <- st_set_geometry(west, st_geometry(west) + c(360, 0))
#   east <- suppressWarnings(st_crop(x, st_bbox(c(xmin = (clon)%%360 - 180, xmax = 180, ymin = -90, ymax = 90))))
#   xx <- rbind(
#     west, east
#   ) 
#   ## ensure geometries are of consistent type
#   xx <- sf::st_cast(xx)
#   bb <- st_bbox(xx)
#   st_set_crs(xx, crs)
# }
