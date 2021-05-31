library(rayshader) 
library(rayvista)
library(rayrender)
library(sp)
library(dplyr)
library(geosphere) #to measure distances 
#import data
gps <- readr::read_csv("bearalberta1.csv")
#filter by species
Amelie <- filter(gps, animal == "Amelie")
Emma <- filter(gps, animal == "Emma")

# Determine map center & radius
.lat <- (max(gps$lat)-min(gps$lat))/2 + min(gps$lat)
.long <- (max(gps$long)-min(gps$long))/2 + min(gps$long)

.radius <- max(distm(c(.long, .lat), c(.long, max(gps$lat)), fun = distHaversine),
               distm(c(.long, .lat), c(max(gps$long),.lat), fun = distHaversine))
.radius <- .radius * 1.5 # add some buffer

#Plot 3d Map 
map <- plot_3d_vista(lat = .lat, long = .long, radius=.radius, phi = 30, zoom = .4, 
                     fov = 00, theta= 0, zscale = 3, windowsize = c(1200, 800), 
                     elevation_detail = 13, overlay_detail = 13, baseshape = "circle", 
                     background = "slategrey")
render_camera(theta=0, phi = 30,zoom = .4)

#render paths 
render_path(extent = attr(map,"extent"),
            lat = Amelie$lat, long = Amelie$long,
            heightmap = map, zscale=3,color="coral3",
            clear_previous = TRUE,
            offset = 200, linewidth = 2)

render_path(extent = attr(map,"extent"),
            lat = Emma$lat, long = Emma$long,
            heightmap = map, zscale=3,color="cadetblue4", antialias=TRUE,
            offset = 200, linewidth = 2)
#Render Labels

render_label(map,lat = Amelie$lat[1], long = Amelie$long[1],
             extent = attr(map, "extent"),
             zscale=5, textcolor="black", linecolor="black", text = "Amelie", 
             dashed = TRUE,textsize = 2, clear_previous = TRUE)

render_label(map,lat = Emma$lat[length(Emma$lat)], long = Emma$long[length(Emma$long)],
             extent = attr(map, "extent"),
             zscale=5, textcolor="black", linecolor="black", text = "Emma",
             dashed = TRUE, textsize = 2)
# Render Movie
render_movie(filename = "bear.mp4", type = "orbit",
             frames = 720, fps = 60)
