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



#' Load administrative area boundaries
#'
#' Requires that the GADM RDS or shp files exist in a path specified in either the environment variable GISpath or in the function parameter path. Tries to load first the RDS file and secondarily the shp file. If RDS file does not exist, it will convert the shp into RDS and write to disk.
#'
#' The required files can be downloaded from https://gadm.org/
#' @param level The level of admin boundaries to load.
#' @param path Path to the file
#' @return SpatialPolygonsDataFrame
#' @author Lauri Myllyvirta \email{lauri@@energyandcleanair.org}
#' @export
get_adm <- function(level=0, res='full', version='36') {

  resext=''
  if(res!='full') resext=paste0('_', res)

  f <- get_boundaries_path(paste0('gadm',version,'_',level, resext,'.RDS'))

  if(!file.exists(f) & res=='full') {
    message('RDS not found, trying shapefile')
    raster::shapefile(gsub('\\.RDS','.shp',f),
                      encoding='UTF-8', use_iconv=TRUE) -> adm
    saveRDS(adm, f)
  }

  if(!file.exists(f) & res!='full') {
    message('simplifying the admin features')
    simplify_adm(level, res, version)
  }

  readRDS(f)
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

  adm_sf %>% sf::st_simplify(dTolerance = tol) -> adm_sf_coarse

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

  if(grepl('^Spatial',class(data))) {
    warning('Data is already of type Spatial*')
    return(data)
  }

  if(class(data) != 'data.frame')
    as.data.frame(data) -> data


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
