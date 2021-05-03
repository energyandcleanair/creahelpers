
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

