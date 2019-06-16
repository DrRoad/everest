# bon j'ai pas réussi encore à downloader l'elevation du chemin parcouru, et pas tout à fait à utiliser le package pour mettre la trail
# 

library(sf) #pour manipulation des données géospatiales
library(leaflet) #pour leaflet
library(lubridate) #pour manipulation des dates
library(viridis) #pour la palette de couleur
library(RColorBrewer) #pour la palette de couleur
library(htmlwidgets) #pour sauvegarder le leaflet
library(htmltools) # pour htmlEscape dans les popups
library(stringr) # pour remplacer les caractères dans les string avant export vers kml
library(mapview)
library(elevatr)
library(raster)
library(dplyr)
library(rayshader)
#install.packages("remotes")
#remotes::install_github("vinayudyawer/KUD3D")
library(KUD3D) # pour add_points
library(sf)
library(tidyverse)
library(osmdata) # pour osm_elevation

# Specify projection.
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
crs_utm<-CRS("+init=epsg:28348") ## projected WA (in meters)
# import gps path ----
# everest climb by http://www.movescount.com/moves/move159990476
## importer le GPX avec sf:st_read, rgdal::readOGR ou plotKML::readGPX 
# perd les datetime  des points. J'ai oublié où j'ai lu ça, mais apparemment 
# c'est normal car les shapefile ont juste la date.  Faisons le quand même.
st_layers("route4636025-Untitled_route.gpx")     
everest_gpx <- st_read("route4636025-Untitled_route.gpx", layer = "route_points") %>% st_transform(., "+proj=longlat +datum=WGS84") 
bbox <- st_bbox(everest_gpx)
mapview(everest_gpx)

# get elevation raster around path
# créer le raster de la montagne ----

#https://stackoverflow.com/questions/54165356/create-topographic-map-in-r

# Generate a data frame of lat/long coordinates.
ex.df <- data.frame(x=seq(from=bbox$xmin - 0.03, to=bbox$xmax+0.03, length.out=100), 
                    y=seq(from=bbox$ymin - 0.03, to=bbox$ymax+0.03, length.out=100))



# Use elevatr package to get elevation data for each point.
elev <- get_elev_raster(ex.df, prj = prj, z = 10, clip = "bbox")

elev_utm <- projectRaster(elev, crs=crs_utm)

# contourmap 2d
raster::contour(elev)


# créer kilian_path ----

# get elevation for path from elevation raster
latlons <- map_df(everest_gpx$geometry, ~ st_coordinates(.x) %>% as_tibble() %>% rename(x = X, y=Y ) ) %>% as.data.frame

z <- raster::extract(elev, latlons %>% .[, c("x","y")])
# alternative 1 that doesnt work: get_elev_point doesnt work outside usa
# utiliser elevatr pour elevation? non parce que outside usa
#elev_trail <- get_elev_point(latlons, prj = prj)  -1e6 parce que outside usa

# alternative2 , also useless : I could also have created a raster from SRTM 30meters tif: 
# what about osm_elevation? C'est nouveau du mois d'avril!
# https://ropensci.github.io/osmdata/articles/osmdata-sc.html
# https://github.com/ropensci/osmdata/issues/157
# download.file("http://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_54_07.zip",
#               destfile = "srtm_54_07.zip")
#utils::unzip("srtm_54_07.zip")
#r <- raster::raster("srtm_54_07.tif")
#z <- raster::extract(r, latlons %>% .[, c("x","y")])

kilian_path  <- latlons %>% 
  add_column(z= z) %>%
  select(lat = y, lon = x, dep = z) %>%
  as_tibble() %>%
  mutate(is_max =  ifelse(dep == max(dep),1 ,0 ), # chop return
         cumsum_is_max = cumsum(is_max)) %>%
  filter(cumsum_is_max ==0 | (is_max==1 & cumsum_is_max == 1)) %>%
  select(lat,lon,dep)  

kilian_path_utm <-  kilian_path %>%
  st_as_sf(coords=c("lon","lat"), crs=4326) %>%
  st_transform(crs=28348) %>%
  as_Spatial() %>%
  as_tibble() %>%
  transmute(lat = coords.x2,
            lon = coords.x1,
            dep
  )


# rayshader de l'everest, because we can

elmat = matrix(raster::extract(elev,
                               raster::extent(elev)#,
                               #buffer=1000
                               ),
               nrow=ncol(elev),
               ncol=nrow(elev))

# plot_map 2d
elmat %>%
  sphere_shade() %>%
  plot_map() 

# plot_3d
elmat %>% 
  sphere_shade(zscale=50,texture = "imhof1") %>% 
  plot_3d(elmat,zscale=50,fov=0,theta=-45,phi=45,windowsize=c(1000,800),zoom=0.6,
          water=FALSE)

kilian_path %>%
  add_points(
    ras = elev,
    det = .,
    zscale = 50,
    cont = c(95, 50),
    alphavec = c(0.1, 0.9),
    drawpoints = T,
    size = 1,
    col.pt = "black",
    colors = c("red","red")
  )

## add axes
add_axes(elev,
         zscale = 50,
         axis.col = grey(0.5))

#render_snapshot("pouet.gif")

# demo USGS api  ---- 
#https://wcmbishop.github.io/rayshader-demo/


## ok on investigue KUD3D pour ajouter la trail ----
#https://rdrr.io/github/vinayudyawer/KUD3D/f/README.md 
