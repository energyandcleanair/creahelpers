#' Extract parameter values from proj4 strings
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
get_crs_par <- function(crsstring, parname) {
  search_string <- paste0('.*\\+',parname,'=')
  if(grepl(search_string, crsstring)) {
    crsstring %>% gsub(search_string, '', .) %>% gsub(' .*', '', .)
  } else return(as.character(NA))
}


#' Repair UTM projections with km as units
#'
#' New versions of GDAL convert UTM projections with km as units into generic transverse Mercator, which causes no end of problems.
#'
#' This function takes an object that has a crs and converts it back to UTM/
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
fixproj <- function(x) {
  crs0 <- crs(x)
  if(grepl("\\+proj=tmerc", crs0)) {
    UTMZ <- get_crs_par(crs0, 'lon_0') %>% as.numeric %>% add(177) %>% divide_by(6) %>% add(1)
    UTMH <- ifelse(get_crs_par(crs0, 'y_0')=='0', '', ' +south')
    u <- get_crs_par(crs0, 'units')
    datum <- get_crs_par(crs0, 'datum')
    if(is.na(datum)) datum <- 'WGS84'
    crs(x) <- paste0("+proj=utm +datum=", datum,
                     " +no_defs +zone=", UTMZ, UTMH,
                     " +units=", u)
  }
  return(x)
}


#' Alternative for raster::projectExtent that works with UTM with +units=km
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
projectExtent2 <- function(r, targetcrs, ...) {
  bb <- r %>% extent()
  bbpol <- bb %>% as('SpatialPolygons')
  crs(bbpol) <- crs(r)
  r.out <- bbpol %>% spTransform(crs(targetcrs)) %>% extent %>% raster(...)
  crs(r.out) <- targetcrs
  return(r.out)
}


#' Alternative for sp::spTransform that works with UTM with +units=km
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
spTransform2 <- function(obj, targetcrs, ...) {
  if(grepl('\\+units=km', targetcrs)) obj <- obj %>% spTransform(gsub('\\+units=km', '\\+units=m', targetcrs, ...))
  obj %>% spTransform(targetcrs, ...)
}


#' Reproject a spatial object, cropping to the extent of target raster
#'
#' This function reprojects the extent of the target raster, crops the spatial object to this extent, and then reprojects it to the crs of the target raster.
#'
#' Using standard functions raster::projectRaster or sp::spTransform directly causes an error when the object cannot be entirely represented in the target crs, f.ex. when reprojecting a global dataset to UTM. Cropping first is also faster.
#'
#' @param shapeobj Object to be reprojected
#' @param rasterobj Raster from which to take the crs and extent
#' @param expand Factor by which to expand the extent before cropping
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @author Danny Hartono \email{danny@@energyandcleanair.org}
#' @export
cropProj <- function(shapeobj, rasterobj, expand = 1.25, ...) {
  bb <- rasterobj %>% projectExtent2(crs(shapeobj)) %>% extent %>%
    multiply_by(expand)
  shapeobj <- shapeobj %>% crop(bb)
  if(grepl("Raster", class(shapeobj))) {
    result <- project(as(shapeobj, 'SpatRaster'), rast(rasterobj), ...) %>% raster
  } else {
    shapeobj %>% spTransform2(crs(rasterobj))
  }
}


#' Simplify administrative area boundaries
#'
#' Requires that the GADM RDS files exist in a path specified in either the environment variable GISpath or in the function parameter path. Tries to load first the RDS file and secondarily the shp file. If RDS file does not exist, it will convert the shp into RDS and write to disk.
#'
#' The required files can be downloaded from https://gadm.org/
#' @param level The level of admin boundaries to load.
#' @param path Path to the file
#' @return SpatialPolygonsDataFrame
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
simplify_adm <- function(level = 0, resname = 'low', version = '410', method) {

  print(method)

  adm <- get_adm(level, res='full', version)

  # Original approach
  # adm_sf %>% sf::st_simplify(dTolerance = tol) -> adm_sf_coarse
  # adm %>% sf::st_as_sf() -> adm_sf
  # creahelpers::to_spdf(adm) -> adm_sp
  if(method == 'rgeos') {
    if(resname == 'low') {
      tol <- .005
      threshold_km2 <- 5
    }

    if(resname == 'coarse'){
      tol <- .05
      threshold_km2 <- 250
    }

    # RGEOS approach
    # gSimplify doesn't preserve the @data frame, though, so we should re-create it:
    adm_gsimplify <- rgeos::gSimplify(adm, tol = tol, topologyPreserve = TRUE)
    adm_df <- adm@data
    adm_simplified <- sp::SpatialPolygonsDataFrame(adm_gsimplify, adm_df)
    adm_simplified <- remove_small_polygons(adm_simplified, threshold_km2 = threshold_km2)
  }

  if(method == 'rmapshaper'){
    # Trying to fix orphaned holes in R
    adm <- rgeos::gBuffer(adm, byid = T, width = 0)

    # RMAPSHAPER approach
    if(resname == 'low') keep <- .1
    if(resname == 'coarse') keep <- .05

    adm_simplified <- rmapshaper::ms_simplify(input = adm, keep = keep, sys = T)
  }

  adm_simplified_sf <- sf::st_as_sf(adm_simplified)

  f <- get_boundaries_path(paste0('gadm/gadm', version, '_', level, '_', resname, '_', method, '.gpkg'))
  sf::st_write(sf::st_as_sf(adm_simplified_sf), f, overwrite = T, append = F)
  adm_simplified <- sf::read_sf(f) %>% creahelpers::to_spdf()
  saveRDS(adm_simplified, gsub('\\.gpkg', '.RDS', f))
  return(adm_simplified)
}

remove_small_polygons <- function(spdf, threshold_km2){

  if(is.null(threshold_km2)){
    return(spdf)
  }

  spdf_dis <- sp::disaggregate(spdf)

  # Use terra. Facing errors with rgeos::gArea
  # Threshold: A tenth of Luxembourg ~ 250km2
  # areas <- rgeos::gArea(adm_simplified, byid = TRUE)
  spdf_dis_vect <- terra::vect(spdf_dis)
  areas <- terra::expanse(spdf_dis_vect, unit = 'km')
  # threshold <- 250 #areas[which(adm_simplified_vect$GID_0=='LUX')] %>% sum() / 10
  spdf_dis_vect <- spdf_dis_vect[which(areas >= threshold_km2)]

  # Re aggregate
  spdf_dis <- as(spdf_dis_vect, "Spatial")
  spdf <- aggregate(spdf_dis, by = names(spdf_dis))
  return(spdf)
}


identify_coordinate_columns <- function(data) {
  llcols <- unlist(sapply(c("^longitude","^latitude"), grep,tolower(names(data))))

  if(length(llcols) != 2)
    llcols <- unlist(sapply(c("^lon", "^lat"), grep, tolower(names(data))))

  if(length(llcols) != 2)
    llcols <- unlist(sapply(c("x", "y"), function(str) {which(str == tolower(names(data)))}))

  if(length(llcols) != 2)
    llcols <- unlist(sapply(c("^x", "^y"), grep, tolower(names(data))))

  if(length(llcols) < 2)
    stop("could not identify coordinate columns, need to start with lat, lon or x, y")

  if(length(llcols) > 2)
    stop("could not identify coordinate columns, too many starting with lat, lon or x, y")

  return(llcols)
}


#' Convert to spatialpointsdataframe
#'
#' @param data
#' @param crs
#' @param llcols
#' @param na.action
#'
#' @return
#' @export
#'
#' @examples
to_spdf <- function(data, crs = NULL, llcols = NULL, na.action = na.omit) {

  if(any(grepl('^Spatial', class(data)))) {
    warning('Data is already of type Spatial*')
    return(data)
  }

  if(class(data)[1] == "sf"){
    return(as(data, "Spatial"))
  }

  if(all(class(data) != 'data.frame')){
    data <- as.data.frame(data)
  }

  if(is.null(llcols)) llcols <- identify_coordinate_columns(data)

  if(anyNA(data[,llcols])) warning("NAs in coordinates")
  data <- data[row.names(na.action(data[,llcols])),]

  if(is.null(crs)) {
    crs <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
    warning("assuming lat-lon WGS84 coordinate system")
  }

  if(class(crs) == "character")
    crs <- sp::CRS(crs)

  return(SpatialPointsDataFrame(coords = data[,llcols], data = data, proj4string = crs))
}


to_sf_points <- function(data, crs = NULL, llcols = NULL, na.action = na.omit) {

  if('sf' %in% class(data)) {
    message('Data is already of type sf')
    return(data)
  }

  if(any(grepl('^Spatial',class(data)))) {
    message('Converting from type Spatial*')
    return(sf::st_as_sf(data))
  }

  if(all(class(data) != 'data.frame')){
    data <- tibble::as_tibble(data)
  }

  if(is.null(llcols)) llcols <- identify_coordinate_columns(data)

  if(anyNA(data[,llcols])) warning("NAs in coordinates")
  data <- data[row.names(na.action(data[,llcols])),]

  if(is.null(crs)) {
    crs <- 4326
    warning("assuming lat-lon WGS84 coordinate system")
  }

  sf::st_as_sf(data, coords = llcols, crs = crs)
}


#' Convert to raster RasterLayer format if not already in this format. Also works with lists.
#' (using raster::raster loses values if already a raster)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
to_raster <- function(x){

  if(is.null(x)) return(NULL)

  if(class(x)[1] == "list"){
    return(sapply(x, to_raster, USE.NAMES = T))
  }

  if(class(x)[1] %in% c("RasterStack", "RasterBrick")) {
    return(x)
  }

  if(class(x)[1] != "RasterLayer") {
    raster(x)
  } else {
    x
  }
}

#' Convert to terra SpatRaster if not already in this format. Also works with lists.
#' (using terra::rast loses values if already a rast)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
to_rast <- function(x) {

  if(is.null(x)) return(NULL)

  if(class(x)[1] == "list"){
    return(sapply(x, to_rast, USE.NAMES = T))
  }

  if(class(x)[1] != "SpatRaster") {
    terra::rast(x)
  } else {
    x
  }
}

#' Convert a raster stack to a list of
#'
#' @return
#' @export
#'
#' @examples
unrasterstack <- function(x){

  if(class(x)[1]=='RasterStack'){
    names <- names(x)
    x <- raster::unstack(x) %>%
      `names<-`(names)
  }

  x
}

focal.loop <- function(r, w, fun, filename = raster::rasterTmpFile(), ...) {

  # compile in hopes of speeding up
  cmp_fun <- compiler::cmpfun(fun)

  bs <- raster::blockSize(r)

  # adjust chunksize down to account for extra "padding" rows
  bs <- raster::blockSize(r, chunksize = ncol(r) * (bs$nrows[1]-dim(w)[1]))

  #initialize
  r.out <- raster(r)
  r.out <- raster::writeStart(r.out, filename = filename, overwrite = TRUE)
  pad.rows <- (dim(w)[2] - 1) / 2 # number of rows to add above and below
  for(i in 1:bs$n) {
    # calculate number of padding rows, avoid trying to read outside raster
    padUp <- min(c(bs$row[i] - 1, pad.rows))
    padDown <- min(c(nrow(r) - bs$row[i] - bs$nrows[i] + 1, pad.rows))

    #read a chunk of rows and convert to matrix
    raster::getValues(r, bs$row[i] - padUp,
                      bs$nrows[i] + padUp + padDown) %>%
      matrix(ncol=ncol(r), byrow=T) -> r.sub


    # convert to raster, run focal and convert to vector for writing
    r.sub %>% raster %>%
      raster::focal(w=w,fun=cmp_fun, ...) %>%
      matrix(ncol=ncol(r), byrow=T) -> r.sub
    r.sub[(padUp+1):(nrow(r.sub)-padDown),] %>%
      t %>% as.vector-> r.sub

    # write the rows back
    r.out <- raster::writeValues(r.out, r.sub, bs$row[i])
    cat('\rprocessed chunk ', i, ' out of ', bs$n)
  }
  # save and exit
  r.out <- raster::writeStop(r.out)
  return(r.out)
}

focalcircle <- function(r, d,
                        fun = function(x) weighted.mean(x, fw.c, na.rm = T),
                        ...) {
  fw.r <- raster::focalWeight(r, d = d, type = "rectangle")
  fw.c <- raster::focalWeight(r, d = d, type = "circle")
  fw.r[,] <- 1
  fw.c[fw.c > 0] <- 1
  focal.loop(r, w = fw.r, pad = T, fun = fun, ...)
}


rasterize_lines <- function(lines, grid) {

  if(is.na(crs(lines)))
  lines <- to_spdf(lines)

  if(is.na(raster::crs(lines))){
    message("Assuming crs are the same")
    raster::crs(lines) <- raster::crs(raster::raster(pop))
  }

  # Cut lines along grid cells
  print("Polygonizing...")
  rs <- grid
  rs[] <- 1:ncell(rs)
  names(rs) <- "i_cell"
  # TERRA much faster than raster::rasterToPolygons
  rsp <- terra::as.polygons(rast(rs)) %>% sf::st_as_sf()
  print("Done")

  # Add temporary feature id for grouping
  lines$feature_id_tmp <- 1:nrow(lines)

  print("Cutting along grid...")
  # sf much less memory intensive than raster::intersect
  # and faster

  # Chunking it to avoid rgeos_binpredfunc_prepared: maximum returned dense matrix size exceeded
  cutting_successful <- F
  chunk_size <- 1E10
  while(!cutting_successful){
    tryCatch({
      rsp$chunk <- rsp$i_cell %/% chunk_size
      emission.sf <- sf::st_as_sf(lines)
      rp <- pbapply::pblapply(split(sf::st_as_sf(rsp), rsp$chunk),
                              function(rsp_chunk){
                                sf::st_intersection(emission.sf,rsp_chunk)
                              }) %>%
        do.call("bind_rows",.)
      cutting_successful <- T
    }, error = function(e) {
      if("size exceeded" %in% as.character(e)) {
        chunk_size <- chunk_size / 100
        warning("Cutting failed: ", e, "\n Trying with smaller chunk size", )
      } else {
        stop(e)
      }
    })
  }

  print("Done")

  print("Calculating length...")
  rp$length <- sf::st_length(rp, byid = TRUE)
  print("Done")

  # # Weighting accordingly
  # print("Weighting by length...")
  # rp <- rp %>%
  #   group_by(feature_id_tmp) %>%
  #   do(mutate(., emission=.$emission * length / sum(.$length)))
  # print("Done")

  print("Rasterizing...")
  rp.sum <- rp %>%
    group_by(i_cell = as.integer(i_cell)) %>%
    summarise(length = sum(length, na.rm = T))

  # Print into raster directly!
   cells_x <- rep(0,ncell(rs))
   cells_x[rp.sum$i_cell] <- rp.sum$length
   grid_result <- grid
   grid_result[] <- cells_x

  return(grid_result)
}


cluster <- function(sp, distKM) {
  require(sp)
  require(geosphere)
  sp <- to_spdf(sp)
  hc <- sp %>% coordinates %>% distm %>% as.dist %>% hclust
  cutree(hc,h=distKM*1000)
}

#' Buffer a sf without overlapping amongst features. Mainly useful
#' to extend region boundaries into sea
#'
#' @param g_sf
#' @param buffer_km
#'
#' @return
#' @export
#'
#' @examples
buffer <- function(g_sf, buffer_km, id_col, grouping_cols) {

    g_coast <- cartomisc::regional_seas(g_sf %>% sf::st_transform(3857),
                                        group = id_col,
                                        dist = buffer_km * 1000) %>%
      left_join(g %>% as.data.frame() %>% dplyr::select(id, name, province))

    g_sf <- bind_rows(g_sf %>% sf::st_transform(3857), g_coast) %>%
      # st_snap(x = ., y = ., tolerance = 0.0001) %>% # for sliver polygons but too slow
      group_at(grouping_cols) %>%
      summarise() %>%
      sf::st_transform(sf::st_crs(g_sf)) %>%
      sf::st_make_valid()

    return(g_sf)
}
