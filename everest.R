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


# everest climb by http://www.movescount.com/moves/move159990476
## importer le GPX avec sf:st_read, rgdal::readOGR ou plotKML::readGPX 
# perd les datetime  des points. J'ai oublié où j'ai lu ça, mais apparemment 
# c'est normal car les shapefile ont juste la date.  Faisons le quand même.
st_layers("route4636025-Untitled_route.gpx")     
everest_gpx <- st_read("route4636025-Untitled_route.gpx", layer = "route_points") %>% st_transform(., "+proj=longlat +datum=WGS84") 
latlons <- map_df(everest_gpx$geometry, ~ st_coordinates(.x) %>% as_tibble() %>% rename(x = X, y=Y ) ) %>% as.data.frame

# utiliser elevatr pour elevation? non parce que outside usa
#elev_trail <- get_elev_point(latlons, prj = prj_dd)  -1e6 parce que outside usa

# what about osm_elevation? C'est nouveau du mois d'avril!
# https://ropensci.github.io/osmdata/articles/osmdata-sc.html
# https://github.com/ropensci/osmdata/issues/157
# download.file("http://srtm.csi.cgiar.org/wp-content/uploads/files/srtm_5x5/TIFF/srtm_54_07.zip",
#               destfile = "srtm_54_07.zip")
#utils::unzip("srtm_54_07.zip")
r <- raster::raster("srtm_54_07.tif")
z <- raster::extract(r, latlons %>% .[, c("x","y")])

kilian_path  <- latlons %>% 
  add_column(z= z) %>%
  select(lat = y, lon = x, dep = z) %>%
  as_tibble()



osm_elevation
check_elev_file <- function (elev_file)
{
  if (!methods::is (elev_file, "character"))
    stop ("elev_file must be one of more character strings")
  
  base_dir <- dirname (elev_file [1])
  lf <- list.files (base_dir, full.names = TRUE)
  ret <- NULL
  for (f in elev_file)
  {
    if (!file.exists (f))
      stop ("file ", f, " does not exist")
    fe <- tools::file_ext (f)
    if (!fe %in% c ("tif", "zip"))
      stop ("Unrecognised file format [.", fe, "]; must be .zip or .tif")
    
    if (fe == "zip")
    {
      ftif <- paste0 (basename (tools::file_path_sans_ext (f)), ".tif")
      index <- grepl (ftif, lf, ignore.case = TRUE)
      if (!any (index))
      {
        message ("File ", f, " has not been unzipped; ",
                 "this may take a while ... ", appendLF = FALSE)
        utils::unzip (f, exdir = base_dir)
        message ("done.")
        lf <- list.files (base_dir, full.names = TRUE)
        index <- grepl (ftif, lf, ignore.case = TRUE)
      }
      ret <- c (ret, lf [which (index)])
    } else
      ret <- c (ret, f)
  }
  return (unique (ret))
}

check_elev_file("srtm_54_07.zip")
library(osmdata)


r <- raster::raster("srtm_54_07.tif")

dat <- opq ("mount everest") %>%
  
  osmdata_sc ()


xyz <- tibble(latlons) %>% mutate(Z = 8000)

everest_ll<-
  rasterFromXYZ(xyz, crs=ll) %>%
  raster::disaggregate(x=., fact=3, method='bilinear')

heron_utm<-
  projectRaster(heron_ll, crs=utm_heron) %>%
  raster::crop(.,
               stat_utm %>%
                 as_Spatial() %>%
                 extent() + 300)




everest_gpx <- everest_gpx %>% bind_cols(latlons)
### les kml il faut enlever l'axe des Z avec st_zm, sinon on a une erreur opaque
# https://github.com/r-spatial/mapview/issues/98
# Error in if (length(nms) != n || any(nms == "")) stop("'options' must be a fully named list, or have no names (NULL)") : 
#missing value where TRUE/FALSE needed
everest_trail <- st_read("route4636025-Untitled_route.kml") %>% st_zm() %>% rename(name = Name) %>% select(name, geometry)

everest_ll<-
  rasterFromXYZ(everest_gpx, crs=ll) %>%
  raster::disaggregate(x=., fact=3, method='bilinear')


mapview(everest_trail)

bbox <- st_bbox(everest_trail)

#https://stackoverflow.com/questions/54165356/create-topographic-map-in-r

# Generate a data frame of lat/long coordinates.
ex.df <- data.frame(x=seq(from=bbox$xmin-0.1, to=bbox$xmax+0.1, length.out=100), 
                    y=seq(from=bbox$ymin-0.1, to=bbox$ymax+0.1, length.out=100))

# Specify projection.
prj_dd <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# Use elevatr package to get elevation data for each point.
elev <- get_elev_raster(ex.df, prj = prj_dd, z = 10, clip = "bbox")
get_elev_point(ex.df %>% head, prj = prj_dd)
#  contour map 2d
raster::contour(elev)

# rayshader de l'everest, because we can

elmat = matrix(raster::extract(elev,raster::extent(elev),buffer=1000),
               nrow=ncol(elev),ncol=nrow(elev))

# plot_map 2d
elmat %>%
  sphere_shade() %>%
  plot_map() 

# plot_3d
elmat %>% 
  sphere_shade(zscale=10,texture = "imhof1") %>% 
  plot_3d(elmat,zscale=50,fov=0,theta=-45,phi=45,windowsize=c(1000,800),zoom=0.6,
          water=TRUE, waterdepth = 0, wateralpha = 0.5,watercolor = "lightblue",
          waterlinecolor = "white",waterlinealpha = 0.5,baseshape = "circle")

# demo USGS api  ---- 
#https://wcmbishop.github.io/rayshader-demo/


## ok on investigue KUD3D pour ajouter la trail ----
#https://rdrr.io/github/vinayudyawer/KUD3D/f/README.md 

ll<-CRS("+proj=longlat +datum=WGS84") ## lat/long
utm_heron<-CRS("+init=epsg:32755") ## projected GBR (in meters) 
utm_ningaloo<-CRS("+init=epsg:28348") ## projected WA (in meters)

data(GPSdata)

shark_ll <-
  GPSdata %>%
  st_as_sf(coords=c("Longitude","Latitude"), crs=4326)

shark_utm <-
  shark_ll %>%
  st_transform(crs=28348)

data(ningaloo_bath)
ningaloo_ll<- rasterFromXYZ(ningaloo_bath, crs=ll)
ningaloo_utm <-
  projectRaster(ningaloo_ll, crs=utm_ningaloo) %>%
  crop(.,
       shark_utm %>%
         as_Spatial() %>%
         extent() + 500)

#Now we have the detection data and bathymetry properly formatted we can start calculating the 3D KUD volumes and visualise them

## 3D KUD calculations

kud_df<-
  shark_utm %>%
  as_Spatial() %>%
  as_tibble() %>%
  transmute(X = coords.x1,
            Y = coords.x2,
            Z = - Depth,
            dt = Date.Time)

H.pi <- Hpi(kud_df[1:3], binned = TRUE)
fhat <- kde(kud_df[1:3], H = H.pi)

#We can use the vol3d() helper function in the KUD3D package to quickly estimate volume of particular voxels of our animals 3D KUD

vol3d(fhat, cont = 50) ## in m3
vol3d(fhat, cont = 95)

#Now lets visualise the 3DKUD using the funtions in the rayshader package

## Set depth exaggeration
depth_exaggeration <- 0.1

## reconfigure bathymetry data for 3D plotting (** to correct mirrored plotting in rayshader)
bath_mat <-
  as.matrix(ningaloo_utm) %>%
  apply(., 2, rev)


bath_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(bath_mat, zscale = 1/depth_exaggeration), 0.1) %>%
  add_shadow(ambient_shade(bath_mat, zscale = 1/depth_exaggeration), 0.1) %>%
  plot_3d(
    bath_mat,
    baseshape = "rectangle",
    water = T,                 ## render water
    zscale = 1/depth_exaggeration,
    wateralpha = 0.2,
    waterlinecolor = "white",
    waterlinealpha = 0.5,
    windowsize = c(1200, 700),  ## Size of window
    theta = 80, 
    phi = 20,
    fov = 60,
    zoom = 0.8
  )

#We can use the helper functions in KUD3D to overlay the fixed 3DKUD, and an axis on the plot

## Plot 3DKUD ontop of bathymetry
kud_df %>%
  transmute(lat = Y,
            lon = X,
            dep = Z) %>%
  add_fkud(
    ras = ningaloo_utm,
    det = .,
    zscale = 1 / depth_exaggeration,
    cont = c(95, 50),
    alphavec = c(0.1, 0.9),
    drawpoints = T,
    size = 1,
    col.pt = "black",
    colors = c("red","red")
  )

## add axes
add_axes(ningaloo_utm,
         zscale = 1/depth_exaggeration,
         axis.col = grey(0.5))

#Passive telemetry data
#This example dataset is from a Coral Trout tracked using a passive telemetry array at Heron Island, Australia. We will look at diurnal patterns in space use by this example individual.

## Input and process files
## Station information
data(statinfo)

stat_ll <-
  statinfo %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

stat_utm <-
  stat_ll %>%
  st_transform(crs = 32755)

## Centre of activity data
data(COAdata)

trout_ll <-
  COAdata %>%
  st_as_sf(coords = c("Longitude.coa", "Latitude.coa"), crs = 4326)

trout_utm <-
  trout_ll %>%
  st_transform(crs = 32755)

## Bathymetry data for Heron Island Reef
data(heron_bath)

heron_ll<-
  rasterFromXYZ(heron_bath, crs=ll) %>%
  raster::disaggregate(x=., fact=3, method='bilinear')

heron_utm<-
  projectRaster(heron_ll, crs=utm_heron) %>%
  raster::crop(.,
               stat_utm %>%
                 as_Spatial() %>%
                 extent() + 300)

#Now that the data is correctly formatted, lets calculate 3DKUD like we did for the shark example. We can then use the vol3d() helper function in the KUD3D package to quickly estimate volume of particular voxels of our animals 3D KUD at night and during the day

## 3D KUD calculations

kud_df<-
  trout_utm %>%
  as_Spatial() %>%
  as_tibble() %>%
  transmute(X = coords.x1,
            Y = coords.x2,
            Z = - Sensor.Value.coa,
            subset =
              factor(
                case_when(
                  lubridate::hour(TimeStep.coa) %in% c(7:17) ~ "Day",
                  lubridate::hour(TimeStep.coa) %in% c(0:6, 18:23) ~ "Night"
                ), levels=c("Night", "Day")))


## Calculate KUD volume during the day
day_df <-
  kud_df %>%
  filter(subset %in% "Day")

H.pi_day <- Hpi(day_df[1:3], binned = TRUE)
fhat_day <- kde(day_df[1:3], H = H.pi)

vol3d(fhat_day, cont = 50) ## in m3
vol3d(fhat_day, cont = 95)

## Calculate KUD volume during the night
night_df <-
  kud_df %>%
  filter(subset %in% "Night")

H.pi_night <- Hpi(night_df[,1:3], binned = TRUE)
fhat_night <- kde(night_df[,1:3], H = H.pi)

vol3d(fhat_night, cont = 50) ## in m3
vol3d(fhat_night, cont = 95)

#Now lets plot these 3D KUDs over a semi-transparent bathymetry using the helper function plot_bath()

## Plotting 3D KUD and bathymetry

## reconfigure bathymetry data for 3D plotting (** to correct mirrored plotting in rayshader)
bath_mat <-
  as.matrix(heron_utm) %>%
  apply(., 2, rev)

## Set depth exaggeration
depth_exaggeration <- 1.5

## Plotting using our modified plot_bath() function to control transparency
bath_mat %>%
  sphere_shade(texture = "desert") %>%
  add_shadow(ray_shade(bath_mat, zscale = 1/depth_exaggeration), 0.1) %>%
  add_shadow(ambient_shade(bath_mat, zscale = 1/depth_exaggeration), 0.1) %>%
  plot_bath(
    bath_mat,
    water = TRUE,      ## render water surface
    zscale = 1/depth_exaggeration,
    waterdepth = 0,
    watercolor = "#88DDFF",
    wateralpha = 0.2,
    windowsize = c(1200, 700),  ## Size of window
    theta = 80,               ## Play around with the theta, phi, fov and zoom to orient the plot (or you can adjust it manually)
    phi = 20,
    fov = 60,
    zoom = 0.8,
    alpha = 0.4              ## transparency of bathymetry
  )

#We can use the do() function to run the add_fkud() helper function across all our temporal subsets

## Divides COA data into subsets, calculates KUD and then plots it on bathymetry
kud_df %>%
  group_by(subset) %>%
  transmute(lon = X,
            lat = Y,
            dep = Z) %>%
  do(
    add_fkud(
      ras = heron_utm,
      det = .,
      zscale = 1,
      cont = c(95, 50),                        ## you can add multiple contours, but the plot might get a bit cluttered
      alphavec = c(0.1, 0.7),
      drawpoints = F,
      size = 1,
      col.pt = rainbow(2)[as.numeric(.$subset[1])],
      colors = rep(rainbow(2)[as.numeric(.$subset[1])], 2)
    )
  )

#Now lets add the receiver stations and an axes for context

## add receiver stations

stat_utm %>%
  as_Spatial() %>%
  data.frame() %>%
  transmute(lon = coords.x1,
            lat = coords.x2,
            dep = - Depth) %>%      ## the depth data for receivers if you have it otherwise you can plot them just under the water surface (-1)
  add_points(ras = heron_utm,
             line = TRUE,
             det = .,
             zscale = 1,
             size = 6,
             col = "black")

## add legend
legend3d("topright",
         legend = c(levels(kud_df$subset), NA , "Receiver stations"),
         border = c(rainbow(2), NA, NA),
         fill = c(rainbow(2), NA, NA),
         pch = c(NA,NA,NA,19),
         col = c(NA,NA,NA,1),
         cex=1, inset=c(0.06))

## add axes
add_axes(heron_utm,
         zscale = 1/depth_exaggeration,
         axis.col = grey(0.5))

The 3D rgl outputs can be saved as snapshots or interactive WebGL html documents. You can even create short movies using spin animations to highlight different activity spaces and animal movements

##### Saving output in different formats (leave rgl window open) #####

### Save output as a .png
snapshot3d("RGLsnapshot.png")

### .GIF animation
movie3d(
  spin3d(axis = c(0, 1, 0), rpm = 1),
  duration = 60,
  fps = 10,
  movie = "Spin animation"
)

### Export rgl window to a web browser
browseURL(paste("file://", writeWebGL(dir = file.path(tempdir(), "webGL")), sep = ""))

### save as a WebGL()
writeWebGL(dir="Interactive plot", snapshot=T)

### Write to .OBJ so can be uploaded to p3d.in server or pdf document
writeOBJ("output.OBJ")

### Write to .PLY and .STL format for 3D printing (combines all objects to one single object)
writePLY("output.PLY")
writeSTL("output.STL")

More functions to calculate and visualise 3D animal movement coming soon!!
  
  Author

Vinay Udyawer v.udyawer@aims.gov.au Australian Institute of Marine Science

Vignette version 0.0.1 (20 Dec 2018)
