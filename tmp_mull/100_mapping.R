library(tmap)
library(mapview)
library(rgdal)
library(ggmap)
library(sf)
library(dismo)

shape <- st_read("C:/Users/Alice/Uni/Projekte/Kili/data/lidar_mid_poles_forfigures.shp")

shp_wgs <- st_transform(shape, "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")

e <- extent(shp_wgs)
g <- ggmap(e, type='satellite')
a <- get_map(location = c(left = e[1]-0.05, bottom = e[3]-0.05, right = e[2]+0.05, top = e[4]+0.05), 
            color = "color", 
            source = "google", 
            maptype = "satellite")
ggmap(a)

tmap_mode("plot")
tm_basemap(leaflet::providers$Stamen.Watercolor)+ 
tm_shape(shp_wgs)+
  tm_dots("cat")+ 
  tm_tiles("Stamen.TonerLabels")

p <- ggmap(a)
p + geom_point(aes(x = X, y = Y), data = shp_wgs, size = 0.5) + 
  theme(legend.position="bottom")



  # tm_fill(shp_wgs)



mapview(shape)

data("World")
tm_shape(World) +
  tm_polygons("HPI")

mapImageData1 <- get_map(location = c(lon = -0.016179, lat = 51.538525),
                         color = "color",
                         source = "google",
                         maptype = "satellite",
                         zoom = 17)
ggmap(mapImageData1)
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

base_map <- get_map(location = c(lon = 37.5, lat = -3.2), maptype = "roadmap", source = "google")
library(RgoogleMaps)
GetBingMap(c(lon = 37.5, lat = -3.2), maptype = c("Aerial"), zoom = 1)

library(tidyverse)
locations <- c("Hoensbroek", "Johannesburg", "Barrow-in-Furness",
               "Hong Kong", "Singapore", "Tangail", "Maastricht", "Bendigo") %>%
  geocode()
world <- map_data("world")
ggplot() +
  geom_polygon(data = world,  aes(long, lat, group = group), fill = "grey") +
  geom_point(data = locations, aes(lon, lat), colour = "red", size = 5) + 
  coord_map("ortho", orientation = c(30, 80, 0)) +
  theme_void()

