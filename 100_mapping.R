library(tmap)
library(mapview)
library(rgdal)
library(ggmap)

shape <- readOGR(dsn = "C:/Users/Alice/Uni/Projekte/Kili/data/lidar_mid_poles_forfigures.shp", 
                 layer = "lidar_mid_poles_forfigures")

shp_wgs <- spTransform(shape, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

e <- extent(shp_wgs)
g <- gmap(e, type='satellite')



mapview(shape)

World
tm_shape(World) +
  tm_polygons(World$HPI)

# mapImageData1 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
#                          color = "color",
#                          source = "google",
#                          maptype = "satellite",
#                          zoom = 17)

tm_symbols ("")

library(OpenStreetMap)

library(Orcs)
ext_gimms <- projectExtent(template, "+init=epsg:4326")

kili.map <- openproj(openmap(upperLeft = c(ymax(ext_gimms), xmin(ext_gimms)), 
                             lowerRight = c(ymin(ext_gimms), xmax(ext_gimms)), 
                             type = "bing", minNumTiles = 12L), 
                     projection = "+init=epsg:4326")

library(dismo)
r <- raster('world_avg.tif')
e <- extent(-122.6, -122.3, 37.75, 37.9)
rc <- crop(r, e)
g <- gmap(e, type='satellite')
mrc <- projectRaster(rc, g)
plot(g, interpolate=TRUE)
contour(mrc, add=TRUE, lwd=2, levels=1:7 * 20, col=rev(heat.colors(7)))
pt <- cbind(-122.45, 37.8)
mpt <- Mercator(pt)
points(mpt, col='light blue', pch='+', cex=2)


library(ggmap)
library(rgeos)
library(plyr)
register_google(key = "...")

base_map <- get_map(location = c(lon = 37.5, lat = -3.2), maptype = "roadmap", source = "google")
library(RgoogleMaps)
GetBingMap(c(lon = 37.5, lat = -3.2), maptype = c("Aerial"), zoom = 1)
