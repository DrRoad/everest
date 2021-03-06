# 
#gpx_file <- "draveurs.gpx"
#my_layer <- "track_points" 
gpx_file <- "route4636025-Untitled_route.gpx"
my_layer <- "route_points" 
elevation_raster_zoom <- 10
# librairies ----

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
library(magick) # to rotate png overlay
# functions by Will Bishop @wcmbishop ----
# https://wcmbishop.github.io/rayshader-demo/ 
#' Download a map image from the ArcGIS REST API
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param map_type map type to download - options are World_Street_Map, World_Imagery, World_Topo_Map
#' @param file file path to save to. Default is NULL, which will create a temp file.
#' @param width image width (pixels)
#' @param height image height (pixels)
#' @param sr_bbox Spatial Reference code for bounding box
#' 
#' @details This function uses the ArcGIS REST API, specifically the 
#' "Execute Web Map Task" task. You can find links below to a web UI for this
#' rest endpoint and API documentation.
#' 
#' Web UI: https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute
#' API docs: https://developers.arcgis.com/rest/services-reference/export-web-map-task.htm
#'
#' @return file path for the downloaded .png map image
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' overlay_file <- get_arcgis_map_image(bbox, width = image_size$width,
#'                                      height = image_size$height)
#' 
get_arcgis_map_image <- function(bbox, map_type = "World_Street_Map", file = NULL, 
                                 width = 400, height = 400, sr_bbox = 4326) {
  require(httr)
  require(glue) 
  require(jsonlite)
  
  url <- parse_url("https://utility.arcgisonline.com/arcgis/rest/services/Utilities/PrintingTools/GPServer/Export%20Web%20Map%20Task/execute")
  
  # define JSON query parameter
  web_map_param <- list(
    baseMap = list(
      baseMapLayers = list(
        list(url = jsonlite::unbox(glue("https://services.arcgisonline.com/ArcGIS/rest/services/{map_type}/MapServer",
                                        map_type = map_type)))
      )
    ),
    exportOptions = list(
      outputSize = c(width, height)
    ),
    mapOptions = list(
      extent = list(
        spatialReference = list(wkid = jsonlite::unbox(sr_bbox)),
        xmax = jsonlite::unbox(max(bbox$p1$long, bbox$p2$long)),
        xmin = jsonlite::unbox(min(bbox$p1$long, bbox$p2$long)),
        ymax = jsonlite::unbox(max(bbox$p1$lat, bbox$p2$lat)),
        ymin = jsonlite::unbox(min(bbox$p1$lat, bbox$p2$lat))
      )
    )
  )
  
  res <- GET(
    url, 
    query = list(
      f = "json",
      Format = "PNG32",
      Layout_Template = "MAP_ONLY",
      Web_Map_as_JSON = jsonlite::toJSON(web_map_param))
  )
  
  if (status_code(res) == 200) {
    body <- content(res, type = "application/json")
    message(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
    if (is.null(file)) 
      file <- tempfile("overlay_img", fileext = ".png")
    
    img_res <- GET(body$results[[1]]$value$url)
    img_bin <- content(img_res, "raw")
    writeBin(img_bin, file)
    message(paste("image saved to file:", file))
  } else {
    message(res)
  }
  invisible(file)
}

#' Define image size variables from the given bounding box coordinates.
#'
#' @param bbox bounding box coordinates (list of 2 points with long/lat values)
#' @param major_dim major image dimension, in pixels. 
#'                  Default is 400 (meaning larger dimension will be 400 pixels)
#'
#' @return list with items "width", "height", and "size" (string of format "<width>,<height>")
#'
#' @examples
#' bbox <- list(
#'   p1 = list(long = -122.522, lat = 37.707),
#'   p2 = list(long = -122.354, lat = 37.84)
#' )
#' image_size <- define_image_size(bbox, 600)
#' 
define_image_size <- function(bbox, major_dim = 400) {
  # calculate aspect ration (width/height) from lat/long bounding box
  aspect_ratio <- abs((bbox$p1$long - bbox$p2$long) / (bbox$p1$lat - bbox$p2$lat))
  # define dimensions
  img_width <- ifelse(aspect_ratio > 1, major_dim, major_dim*aspect_ratio) %>% round()
  img_height <- ifelse(aspect_ratio < 1, major_dim, major_dim/aspect_ratio) %>% round()
  size_str <- paste(img_width, img_height, sep = ",")
  list(height = img_height, width = img_width, size = size_str)
}




#' Build a gif of 3D rayshader plots
#'
#' @param hillshade Hillshade/image to be added to 3D surface map.
#' @param heightmap A two-dimensional matrix, where each entry in the matrix is the elevation at that point.
#' @param file file path for .gif
#' @param duration gif duration in seconds (framerate will be duration/n_frames)
#' @param ... additional arguments passed to rayshader::plot_3d(). See Details for more info.
#'
#' @details This function is designed to be a pipe-in replacement for rayshader::plot_3d(),
#' but it will generate a 3D animated gif. Any inputs with lengths >1 will 
#' be interpreted as "animation" variables, which will be used to generate 
#' individual animation frames -- e.g. a vector of theta values would produce
#' a rotating gif. Inputs to plot_3d() that are meant to have length >1 
#' (specifically "windowsize") will be excluded from this process.
#'
#' @return file path of .gif file created
#' 
#' @examples
#' # MONTEREREY BAY WATER DRAINING
#' # ------------------------------
#' # define transition variables
#' n_frames <- 180
#' waterdepths <- transition_values(from = 0, to = min(montereybay), steps = n_frames) 
#' thetas <- transition_values(from = -45, to = -135, steps = n_frames)
#' # generate gif
#' zscale <- 50
#' montereybay %>% 
#'   sphere_shade(texture = "imhof1", zscale = zscale) %>%
#'   add_shadow(ambient_shade(montereybay, zscale = zscale), 0.5) %>%
#'   add_shadow(ray_shade(montereybay, zscale = zscale, lambert = TRUE), 0.5) %>%
#'   save_3d_gif(montereybay, file = "montereybay.gif", duration = 6,
#'               solid = TRUE, shadow = TRUE, water = TRUE, zscale = zscale,
#'               watercolor = "imhof3", wateralpha = 0.8, 
#'               waterlinecolor = "#ffffff", waterlinealpha = 0.5,
#'               waterdepth = waterdepths/zscale, 
#'               theta = thetas, phi = 45)
#' 
save_3d_gif <- function(hillshade, heightmap, file, duration = 5, ...) {
  require(rayshader)
  require(magick)
  require(rgl)
  require(gifski)
  require(rlang)
  
  # capture dot arguments and extract variables with length > 1 for gif frames
  dots <- rlang::list2(...)
  var_exception_list <- c("windowsize")
  dot_var_lengths <- purrr::map_int(dots, length)
  gif_var_names <- names(dots)[dot_var_lengths > 1 & 
                                 !(names(dots) %in% var_exception_list)]
  # split off dot variables to use on gif frames
  gif_dots <- dots[gif_var_names]
  static_dots <- dots[!(names(dots) %in% gif_var_names)]
  gif_var_lengths <- purrr::map_int(gif_dots, length)
  # build expressions for gif variables that include index 'i' (to use in the for loop)
  gif_expr_list <- purrr::map(names(gif_dots), ~rlang::expr(gif_dots[[!!.x]][i]))
  gif_exprs <- exprs(!!!gif_expr_list)
  names(gif_exprs) <- names(gif_dots)
  message(paste("gif variables found:", paste(names(gif_dots), collapse = ", ")))
  
  # TODO - can we recycle short vectors?
  if (length(unique(gif_var_lengths)) > 1) 
    stop("all gif input vectors must be the same length")
  n_frames <- unique(gif_var_lengths)
  
  # generate temp .png images
  temp_dir <- tempdir()
  img_frames <- file.path(temp_dir, paste0("frame-", seq_len(n_frames), ".png"))
  on.exit(unlink(img_frames))
  message(paste("Generating", n_frames, "temporary .png images..."))
  for (i in seq_len(n_frames)) {
    message(paste(" - image", i, "of", n_frames))
    rgl::clear3d()
    hillshade %>%
      plot_3d_tidy_eval(heightmap, !!!append(gif_exprs, static_dots))
    rgl::snapshot3d(img_frames[i])
  }
  
  # build gif
  message("Generating .gif...")
  magick::image_write_gif(magick::image_read(img_frames), 
                          path = file, delay = duration/n_frames)
  message("Done!")
  invisible(file)
}


plot_3d_tidy_eval <- function(hillshade, ...) {
  dots <- rlang::enquos(...)
  plot_3d_call <- rlang::expr(plot_3d(hillshade, !!!dots))
  rlang::eval_tidy(plot_3d_call)
}


#' Create a numeric vector of transition values.
#' @description This function helps generate a sequence 
#' of numeric values to transition "from" a start point
#' "to" some end point. The transition can be "one_way" 
#' (meaning it ends at the "to" point) or "two_way" (meaning
#' we return back to end at the "from" point).
#'
#' @param from starting point for transition values
#' @param to ending point (for one-way transitions) or turn-around point 
#'           (for two-way transitions)
#' @param steps the number of steps to take in the transation (i.e. the length
#'              of the returned vector)
#' @param one_way logical value to determine if we should stop at the "to" value
#'                (TRUE) or turn around and return to the "from" value (FALSE)
#' @param type string defining the transition type - currently suppoerts "cos"
#'             (for a cosine curve) and "lin" (for linear steps)
#'
#' @return a numeric vector of transition values
#' 
transition_values <- function(from, to, steps = 10, 
                              one_way = FALSE, type = "cos") {
  if (!(type %in% c("cos", "lin")))
    stop("type must be one of: 'cos', 'lin'")
  
  range <- c(from, to)
  middle <- mean(range)
  half_width <- diff(range)/2
  
  # define scaling vector starting at 1 (between 1 to -1)
  if (type == "cos") {
    scaling <- cos(seq(0, 2*pi / ifelse(one_way, 2, 1), length.out = steps))
  } else if (type == "lin") {
    if (one_way) {
      xout <- seq(1, -1, length.out = steps)
    } else {
      xout <- c(seq(1, -1, length.out = floor(steps/2)), 
                seq(-1, 1, length.out = ceiling(steps/2)))
    }
    scaling <- approx(x = c(-1, 1), y = c(-1, 1), xout = xout)$y 
  }
  
  middle - half_width * scaling
}



# Specify projection. (utm not used) ----
prj <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
#crs_utm<-CRS("+init=epsg:28348") ## projected WA (in meters)

# import gps tracking data  ----
# everest climb by http://www.movescount.com/moves/move159990476
## importer le GPX avec sf:st_read, rgdal::readOGR ou plotKML::readGPX 
# perd les datetime  des points. J'ai oublié où j'ai lu ça, mais apparemment 
# c'est normal car les shapefile ont juste la date.  Faisons le quand même.
st_layers(gpx_file)     
everest_gpx <- st_read(gpx_file, layer = my_layer) %>% st_transform(., "+proj=longlat +datum=WGS84") 
bbox <- st_bbox(everest_gpx)
bbox["xmin"] <- bbox$xmin - 0.1   # add buffer around path
bbox["xmax"] <- bbox$xmax + 0.1
bbox["ymin"] <- bbox$ymin - 0.07
bbox["ymax"] <- bbox$ymax + 0.07

# bbox["xmin"] <- bbox$xmin - 0.1   # add buffer around path
# bbox["xmax"] <- bbox$xmax + 0.1
# bbox["ymin"] <- bbox$ymin - 0.1
# bbox["ymax"] <- bbox$ymax + 0.1


mapview(everest_gpx)


# get elevation raster around path  ----

#https://stackoverflow.com/questions/54165356/create-topographic-map-in-r

# Generate a data frame of lat/long coordinates for get_elev_raster()
ex.df <- data.frame(x=seq(from=bbox$xmin , to=bbox$xmax, length.out=100), 
                    y=seq(from=bbox$ymin , to=bbox$ymax, length.out=100))



# Use elevatr package to get elevation data for each point.
#elev <- get_elev_raster(ex.df, prj = prj, z = 10, clip = "bbox")
elev <- get_elev_raster(ex.df, prj = prj, z = elevation_raster_zoom, clip = "bbox")

#elev_utm <- projectRaster(elev, crs=crs_utm)

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
  select(lat,lon,dep)  %>%
  mutate(dep = dep+30)

# kilian_path_utm <-  kilian_path %>%
#   st_as_sf(coords=c("lon","lat"), crs=4326) %>%
#   st_transform(crs=28348) %>%
#   as_Spatial() %>%
#   as_tibble() %>%
#   transmute(lat = coords.x2,
#             lon = coords.x1
#   )
# z_utm  <- raster::extract(elev_utm, kilian_path_utm %>% .[, c("lon","lat")])
# kilian_path_utm <- kilian_path_utm %>% add_column(dep = z_utm)

# get image overlay ----



bbox2 <- list(
  p1 = list(long = bbox$xmin, lat = bbox$ymin),
  p2 =  list(long = bbox$xmax, lat = bbox$ymax)
)

#image_size <- define_image_size(bbox2, major_dim = 600)
#image_size <- define_image_size(bbox2, major_dim = max(dim(elev)))


overlay_file <- "images.png"
get_arcgis_map_image(bbox2, map_type = "World_Imagery", file = overlay_file,
                     width = dim(elev)[2], height = dim(elev)[1], 
                     sr_bbox = 4326)


image_write(image_rotate(image_read(overlay_file), 90) ,"rotated.png")


#overlay_img <- png::readPNG(overlay_file)
overlay_img <- png::readPNG("rotated.png")



# rayshader de l'everest, because we can ----

# elmat = matrix(raster::extract(elev,
#                                raster::extent(elev)#,
#                                #buffer=1000
#                                ),
#                nrow=ncol(elev),
#                ncol=nrow(elev))
# 

## reconfigure bathymetry data for 3D plotting (** to correct mirrored plotting in rayshader)
elmat <-
  as.matrix(elev) %>%
  apply(., 2, rev)


myzscale = 50000 / (elevation_raster_zoom * elevation_raster_zoom * elevation_raster_zoom)
shadow = ray_shade(elmat,zscale=myzscale,lambert=FALSE)
ambient = ambient_shade(elmat,zscale=myzscale)


# plot_3d
# sphere_shade retourne RGB array of hillshaded texture mappings.
sphere_shade(heightmap = elmat, zscale=myzscale,texture = "imhof4") %>% 
  add_shadow(shadow) %>%
  add_shadow(ambient)  %>% 
  #add_water(detect_water(elmat)) %>%
  add_overlay(., overlay = overlay_img, alphacolor = NULL,alphalayer = 0.9)  %>%
  plot_3d(., heightmap = elmat,
          zscale=myzscale,fov=0,theta=80,phi=29,windowsize=c(1000,800),zoom=0.6,
          water=FALSE,          baseshape = "circle" 
  )


# render labels
# locaton founds by trial and error
render_label(heightmap = elmat,x=363,y=281, z=1000,zscale=myzscale,
             text = "Base Camp",textsize = 2,linewidth = 5)


render_label(heightmap = elmat,x=124,y=178, z=1000,zscale=myzscale,
             text = "Summit",textsize = 2,linewidth = 5)


#KUD3D adds points and axes
add_points(
  ras = elev,
  det = kilian_path,
  zscale=myzscale,
  cont = c(95, 5),
  alphavec = c(0.1, 0.9),
  drawpoints = T,
  size = 3,
  col.pt = "black",
  colors = c("red","red")
)  
add_axes(elev,
         zscale=myzscale,
         axis.col = grey(0.5))

render_snapshot("trailmap")
# gif !! ----


# montery water gif ====
elev_matrix <- elmat
n_frames <- 60
zscale <- myzscale
# frame transition variables
thetavalues <- transition_values(from = 80, to = 35, steps = n_frames, 
                                 one_way = TRUE, type = "lin")

phivalues <- transition_values(from = 28, to = 73, steps = n_frames, 
                               one_way = FALSE, type = "cos")

zoomvalues <- transition_values(from = 0.6, to = 0.8, steps = n_frames, 
                                one_way = FALSE, type = "cos")
# shadow layers
ambmat <- ambient_shade(elev_matrix, zscale = zscale)
raymat <- ray_shade(elev_matrix, zscale = zscale, lambert = TRUE)

# generate .png frame images
img_frames <- paste0("drain", seq_len(n_frames), ".png")
for (i in seq_len(n_frames)) {
  message(paste(" - image", i, "of", n_frames))
  
  sphere_shade(heightmap = elmat, zscale=myzscale,texture = "imhof4") %>% 
    add_shadow(shadow) %>%
    add_shadow(ambient)  %>% 
    #add_water(detect_water(elmat)) %>%
    add_overlay(., overlay = overlay_img, alphacolor = NULL,alphalayer = 0.9)  %>%
    plot_3d(., heightmap = elmat,
            zscale=myzscale,fov=0,theta=thetavalues[i],phi=phivalues[i],windowsize=c(1000,800),zoom=zoomvalues[i],
            water=FALSE,          baseshape = "circle" 
    )
  
  
  # render labels
  # locaton founds by trial and error
  render_label(heightmap = elmat,x=363,y=281, z=1000,zscale=myzscale,
               text = "Base Camp",textsize = 2,linewidth = 5)
  
  
  render_label(heightmap = elmat,x=124,y=178, z=1000,zscale=myzscale,
               text = "Summit",textsize = 2,linewidth = 5)
  
  
  #KUD3D adds points and axes
  add_points(
    ras = elev,
    det = kilian_path,
    zscale=myzscale,
    cont = c(95, 5),
    alphavec = c(0.1, 0.9),
    drawpoints = T,
    size = 3,
    col.pt = "black",
    colors = c("red","red")
  )  
  add_axes(elev,
           zscale=myzscale,
           axis.col = grey(0.5))
  
  
  render_snapshot(img_frames[i])
  rgl::clear3d()
}

# build gif
magick::image_write_gif(magick::image_read(img_frames), 
                        path = "everest.gif", 
                        delay = 2/n_frames)
