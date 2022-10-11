#' Extract parameter values from proj4 strings
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
get_crs_par = function(crsstring, parname) {
  search_string = paste0('.*\\+',parname,'=')
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
fixproj = function(x) {
  crs0 = crs(x)
  if(grepl("\\+proj=tmerc", crs0)) {
    UTMZ = get_crs_par(crs0, 'lon_0') %>% as.numeric %>% add(177) %>% divide_by(6) %>% add(1)
    UTMH = ifelse(get_crs_par(crs0, 'y_0')=='0', '', ' +south')
    u = get_crs_par(crs0, 'units')
    datum=get_crs_par(crs0, 'datum')
    if(is.na(datum)) datum='WGS84'
    crs(x)=paste0("+proj=utm +datum=",datum,
                  " +no_defs +zone=",UTMZ,UTMH,
                  " +units=",u)
  }
  return(x)
}



#' Alternative for raster::projectExtent that works with UTM with +units=km
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
projectExtent2 = function(r, targetcrs, ...) {
  r %>% extent() -> bb
  bb %>% as('SpatialPolygons') -> bbpol
  crs(bbpol) = crs(r)
  bbpol %>% spTransform(crs(targetcrs)) %>% extent %>% raster(...) -> r.out
  crs(r.out) = targetcrs
  return(r.out)
}

#' Alternative for sp::spTransform that works with UTM with +units=km
#'
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
spTransform2 = function(obj, targetcrs, ...) {
  if(grepl('\\+units=km', targetcrs)) obj %<>% spTransform(gsub('\\+units=km', '\\+units=m', targetcrs, ...))
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
#' @export
cropProj <- function(shapeobj, rasterobj, expand=1.25, ...) {
  bb = rasterobj %>% projectExtent2(crs(shapeobj)) %>%
    extent %>% multiply_by(expand)
  shapeobj %<>% crop(bb)
  if(grepl("Raster", class(shapeobj))) {
    shapeobj %>% projectRaster(rasterobj, ...) %>% return
  } else shapeobj %>% spTransform2(crs(rasterobj)) %>% return
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
simplify_adm = function(level=0, resname='low', version='36') {

  if(resname=='low') tol=.005
  if(resname=='coarse') tol=.025

  get_adm(level, res='full', version) -> adm
  adm %>% sf::st_as_sf() -> adm_sf


  adm_sf_coarse <- rmapshaper::ms_simplify(input = as(adm, 'SpatialPolygonsDataFrame')) %>% sf::st_as_sf()

  # adm_sf %>% sf::st_simplify(dTolerance = tol) -> adm_sf_coarse

  f = get_boundaries_path(paste0('gadm',version,'_',level,'_',resname,'.shp'))
  sf::st_write(adm_sf_coarse, f, overwrite=T, append=F)
  raster::shapefile(f) -> adm_coarse

  saveRDS(adm_coarse, gsub('\\.shp', '.RDS', f))
  return(adm_coarse)
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
to_spdf <- function(data, crs=NULL, llcols=NULL, na.action=na.omit) {

  if(any(grepl('^Spatial',class(data)))) {
    warning('Data is already of type Spatial*')
    return(data)
  }

  if(class(data)[1]=="sf"){
    return(as(data, "Spatial"))
  }

  if(all(class(data) != 'data.frame')){
    as.data.frame(data) -> data
  }

  if(is.null(llcols)) {
    llcols <- unlist(sapply(c("^longitude","^latitude"), grep,tolower(names(data))))

    if(length(llcols)!=2)
      llcols <- unlist(sapply(c("^lon","^lat"), grep, tolower(names(data))))

    if(length(llcols)!=2)
      llcols <- unlist(sapply(c("x","y"),function(str) { which(str == tolower(names(data))) }))

    if(length(llcols)!=2)
      llcols <- unlist(sapply(c("^x","^y"), grep, tolower(names(data))))

    if(length(llcols)<2)
      stop("could not identify coordinate columns, need to start with lat, lon or x, y")

    if(length(llcols)>2)
      stop("could not identify coordinate columns, too many starting with lat, lon or x, y")
  }

  if(anyNA(data[,llcols])) warning("NAs in coordinates")
  data[row.names(na.action(data[,llcols])),] -> data

  if(is.null(crs)) {
    crs <- creapuff.env$llproj
    warning("assuming lat-lon WGS84 coordinate system")
  }

  if(class(crs) == "character")
    crs <- CRS(crs)

  return(SpatialPointsDataFrame(coords = data[,llcols],data=data,proj4string = crs))
}

#' Convert to raster format if not.
#' (using raster::raster loses values if already a raster)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
to_raster <- function(x){
  if(class(x)[1]!="RasterLayer"){
    raster(x)
  }else{
    x
  }
}

#' Convert to terra::rast format if not.
#' (using terra::rast loses values if already a rast)
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
to_rast <- function(x){
  if(class(x)[1]!="SpatRaster"){
    terra::rast(x)
  }else{
    x
  }
}

focal.loop <- function(r, w, fun, filename=raster::rasterTmpFile(), ...) {

  #compile in hopes of speeding up
  cmp_fun <- compiler::cmpfun(fun)

  bs <- raster::blockSize(r)

  #adjust chunksize down to account for extra "padding" rows
  bs <- raster::blockSize(r, chunksize = ncol(r) * (bs$nrows[1]-dim(w)[1]))

  #initialize
  r.out <- raster(r)
  r.out <- raster::writeStart(r.out, filename=filename, overwrite=TRUE)
  pad.rows = (dim(w)[2]-1)/2 #number of rows to add above and below
  for(i in 1:bs$n) {
    #calculate number of padding rows, avoid trying to read outside raster
    padUp = min(c(bs$row[i]-1, pad.rows))
    padDown = min(c(nrow(r) - bs$row[i] - bs$nrows[i] + 1, pad.rows))

    #read a chunk of rows and convert to matrix
    raster::getValues(r, bs$row[i] - padUp,
              bs$nrows[i] + padUp + padDown) %>%
      matrix(ncol=ncol(r), byrow=T) -> r.sub

    #convert to raster, run focal and convert to vector for writing
    r.sub %>% raster %>%
      raster::focal(w=w,fun=cmp_fun, ...) %>%
      matrix(ncol=ncol(r), byrow=T) -> r.sub
    r.sub[(padUp+1):(nrow(r.sub)-padDown),] %>%
      t %>% as.vector-> r.sub

    #write the rows back
    r.out <- raster::writeValues(r.out, r.sub, bs$row[i])
    cat('\rprocessed chunk ', i,' out of ', bs$n)
  }
  #save and exit
  r.out <- raster::writeStop(r.out)
  return(r.out)
}

focalcircle <- function(r, d,
                        fun=function(x) weighted.mean(x, fw.c, na.rm=T),
                        ...) {
  fw.r=raster::focalWeight(r, d=d, type="rectangle")
  fw.c=raster::focalWeight(r, d=d, type="circle")
  fw.r[,] <- 1
  fw.c[fw.c>0] <- 1
  focal.loop(r, w=fw.r, pad=T, fun=fun, ...)
}


rasterize_lines <- function(lines, grid){

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
    }, error=function(e){
      if("size exceeded" %in% as.character(e)){
        chunk_size <- chunk_size / 100
        warning("Cutting failed: ", e, "\n Trying with smaller chunk size", )
      }else{
        stop(e)
      }
    })
  }

  print("Done")

  print("Calculating length...")
  rp$length <- sf::st_length(rp, byid=TRUE)
  print("Done")

  # # Weighting accordingly
  # print("Weighting by length...")
  # rp <- rp %>%
  #   group_by(feature_id_tmp) %>%
  #   do(mutate(., emission=.$emission * length / sum(.$length)))
  # print("Done")

  print("Rasterizing...")
  rp.sum <- rp %>%
    group_by(i_cell=as.integer(i_cell)) %>%
    summarise(length=sum(length, na.rm=T))

  # Print into raster directly!
   cells_x <- rep(0,ncell(rs))
   cells_x[rp.sum$i_cell] <- rp.sum$length
   grid_result <- grid
   grid_result[] <- cells_x

  return(grid_result)
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
buffer <- function(g_sf, buffer_km, id_col, grouping_cols){

    g_coast <- cartomisc::regional_seas(g_sf %>% sf::st_transform(3857),
                                        group=id_col,
                                        dist=buffer_km*1000) %>%
      left_join(g %>% as.data.frame() %>% dplyr::select(id, name, province))

    g_sf <-  bind_rows(g_sf %>% sf::st_transform(3857),
                    g_coast) %>%
      # st_snap(x = ., y = ., tolerance = 0.0001) %>% # for sliver polygons but too slow
      group_at(grouping_cols) %>%
      summarise() %>%
      sf::st_transform(sf::st_crs(g_sf)) %>%
      sf::st_make_valid()

    return(g_sf)
}
