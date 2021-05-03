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
